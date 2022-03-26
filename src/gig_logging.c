// Copyright (C) 2018, 2019, 2022 Michael L. Gran

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <glib.h>
#include <libguile.h>
#include "core.h"
#include "gig_logging.h"

_Thread_local int logger_initialized = 0;

static const LogField *
field_ref(const char *needle, const LogField *fields, size_t n_fields)
{
    for (size_t i = 0; i < n_fields; i++)
        if (!(strcmp(fields[i].key, needle)))
            return fields + i;
    return NULL;
}

static bool
is_enabled(const LogField *domain, const char *domains, bool allow_empty)
{
    if (!domain || !domain->value)
        return allow_empty;
    if (domains == NULL)
        return false;
    if (!strcmp(domains, "all"))
        return true;
    if (strstr(domains, domain->value))
        return true;
    return false;
}

static LogWriterOutput
gig_log_writer(LogLevelFlags flags, const LogField *fields, size_t n_fields, void *user_data)
{
#define LOG_FIELD(f) field_ref(f, fields, n_fields)
#define ENV_MESSAGES_DEBUG (getenv("G_MESSAGES_DEBUG"))
#define ENV_GIG_DEBUG (getenv("GIG_DEBUG"))
    const LogField *message;

    const char *prefix = NULL, *color = NULL;
    if (!logger_initialized) {
        scm_init_guile();
        logger_initialized = 1;
    }
    switch (flags & LOG_LEVEL_MASK) {
    case LOG_LEVEL_ERROR:
        color = "\033[1;31m%s\033[0m";
        prefix = "ERROR";
        break;
    case LOG_LEVEL_CRITICAL:
        color = "\033[1;35m%s\033[0m";
        prefix = "CRITICAL";
        break;
    case LOG_LEVEL_WARNING:
        color = "\033[1;33m%s\033[0m";
        prefix = "WARNING";
        break;
    case LOG_LEVEL_MESSAGE:
        color = "\033[1;32m%s\033[0m";
        prefix = "MESSAGE";
        break;
    case LOG_LEVEL_INFO:
        color = "\033[1;32m%s\033[0m";
        if (!is_enabled(LOG_FIELD("GLIB_DOMAIN"), ENV_MESSAGES_DEBUG, FALSE)
            || !is_enabled(LOG_FIELD("GIG_DOMAIN"), ENV_GIG_DEBUG, TRUE))
            return LOG_WRITER_HANDLED;
        prefix = "INFO";
        break;
    case LOG_LEVEL_DEBUG:
        color = "\033[1;32m%s\033[0m";
        if (!is_enabled(LOG_FIELD("GLIB_DOMAIN"), ENV_MESSAGES_DEBUG, FALSE)
            || !is_enabled(LOG_FIELD("GIG_DOMAIN"), ENV_GIG_DEBUG, TRUE))
            return LOG_WRITER_HANDLED;
        prefix = "DEBUG";
        break;
    }

    message = field_ref("MESSAGE", fields, n_fields);
    assert(message != NULL);

    SCM port = SCM_PACK_POINTER(user_data);

    if (scm_is_true(scm_output_port_p(port))) {
        if (scm_is_true(scm_file_port_p(port))) {
            int fd = scm_to_int(scm_fileno(port));
            char *colored_prefix =
                decorate_string(log_writer_supports_color(fd) ? color : "%s", prefix);
            scm_c_write(port, colored_prefix, strlen(colored_prefix));
            scm_c_write(port, ": ", 2);
            scm_c_write(port, message->value, strlen(message->value));
            scm_newline(port);
            free(colored_prefix);
        }
        else
            scm_printf(port, "%s: %s\n", prefix, (const char *)message->value);
    }
    else
        scm_printf(scm_current_error_port(), "%s: %s\n", prefix, (const char *)message->value);

    return LOG_WRITER_HANDLED;
#undef LOG_FIELD
#undef ENV_MESSAGES_DEBUG
#undef ENV_GIG_DEBUG
}

static SCM
gig_log_to_port(SCM port)
{
    SCM_ASSERT_TYPE(SCM_OPOUTPORTP(port), port, SCM_ARG1, "install-port-logger!",
                    "open output port");
    log_set_writer_func(gig_log_writer, SCM_UNPACK_POINTER(port), NULL);
    return SCM_UNSPECIFIED;
}

static SCM
gig_log_to_journal(void)
{
    scm_misc_error("install-journal-logger!", "journald unimplemented", SCM_EOL);
    return SCM_UNSPECIFIED;
}

static void
gig_unprotect_func(void *func)
{
    scm_gc_unprotect_object(SCM_PACK_POINTER(func));
}

SCM kwd_log_level;

static LogWriterOutput
gig_log_custom_helper(LogLevelFlags log_level, const LogField *fields, size_t n_fields,
                      void *user_data)
{
    if (!logger_initialized) {
        scm_init_guile();
        logger_initialized = 1;
    }

    SCM args = scm_make_list(scm_from_size_t(4 * n_fields + 2), SCM_UNDEFINED);
    SCM it = args;
    scm_set_car_x(it, kwd_log_level);
    scm_set_car_x(scm_cdr(it), scm_from_size_t(log_level));

    for (size_t i = 0; i < n_fields; i++) {
        it = scm_cddr(it);
        char *key = make_scm_name(fields[i].key);
        scm_set_car_x(it, scm_from_utf8_keyword(key));
        // TODO: add more conversions
        if (                    /* the message itself is a string */
               !strcmp(fields[i].key, "MESSAGE") ||
               /* log level as string */
               !strcmp(fields[i].key, "PRIORITY") ||
               /* domains */
               !strcmp(fields[i].key, "GLIB_DOMAIN") || !strcmp(fields[i].key, "GIG_DOMAIN") ||
               /* source information inserted by g_debug, etc */
               !strcmp(fields[i].key, "CODE_FILE") ||
               !strcmp(fields[i].key, "CODE_FUNC") || !strcmp(fields[i].key, "CODE_LINE") ||
               /* end on a false statement  */
               0) {
            if (fields[i].value)
                scm_set_car_x(scm_cdr(it), scm_from_utf8_string(fields[i].value));
            else
                scm_set_car_x(scm_cdr(it), scm_from_utf8_string(""));

        }
        else {
            scm_set_car_x(scm_cdr(it), scm_from_pointer((void *)fields[i].value, NULL));
            char *length = concatenate(key, "-length");
            it = scm_cddr(it);
            scm_set_car_x(it, scm_from_utf8_keyword(length));
            scm_set_car_x(scm_cdr(it), scm_from_size_t(fields[i].length));
            free(length);
        }
        free(key);
    }
    scm_set_cdr_x(scm_cdr(it), SCM_EOL);
    scm_apply_0(SCM_PACK_POINTER(user_data), args);

    return LOG_WRITER_HANDLED;
}

static SCM
gig_install_custom_logger(SCM func)
{
    func = scm_gc_protect_object(func);
    log_set_writer_func(gig_log_custom_helper, SCM_UNPACK_POINTER(func), gig_unprotect_func);
    return SCM_UNSPECIFIED;
}

void
gig_init_logging(void)
{
    kwd_log_level = scm_from_utf8_keyword("log-level");
    scm_c_define_gsubr("install-port-logger!", 1, 0, 0, gig_log_to_port);
    scm_c_define_gsubr("install-journal-logger!", 0, 0, 0, gig_log_to_journal);
    scm_c_define_gsubr("install-custom-logger!", 1, 0, 0, gig_install_custom_logger);
}
