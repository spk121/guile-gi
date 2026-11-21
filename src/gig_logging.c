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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <girepository.h>
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

// Logging is called from all over the C code, including callbacks and
// atexit handlers, so a Scheme error raised while writing a message
// (a closed port, or a custom logger that throws) must not escape.
static SCM
log_error_handler(void *data, SCM key, SCM args)
{
    // Say so once, rather than once per message.
    static int reported = 0;
    if (!reported) {
        reported = 1;
        fprintf(stderr, "guile-gi: error while writing a log message; "
                "further logging errors are ignored\n");
    }
    return SCM_BOOL_F;
}

struct port_message
{
    SCM port;
    const char *prefix;
    const char *color;
    const char *message;
};

static SCM
write_to_port(void *data)
{
    struct port_message *m = data;
    SCM port = m->port;

    if (scm_is_true(scm_output_port_p(port)) && SCM_OPOUTPORTP(port)) {
        if (scm_is_true(scm_file_port_p(port))) {
            int fd = scm_to_int(scm_fileno(port));
            char *colored_prefix =
                decorate_string(log_writer_supports_color(fd) ? m->color : "%s", m->prefix);
            scm_c_write(port, colored_prefix, strlen(colored_prefix));
            scm_c_write(port, ": ", 2);
            scm_c_write(port, m->message, strlen(m->message));
            scm_newline(port);
            free(colored_prefix);
        }
        else
            scm_printf(port, "%s: %s\n", m->prefix, m->message);
    }
    else
        scm_printf(scm_current_error_port(), "%s: %s\n", m->prefix, m->message);
    return SCM_UNSPECIFIED;
}

static LogWriterOutput
gig_log_writer(LogLevelFlags flags, const LogField *fields, size_t n_fields, void *user_data)
{
#define LOG_FIELD(f) field_ref(f, fields, n_fields)
#define ENV_MESSAGES_DEBUG (getenv("G_MESSAGES_DEBUG"))
#define ENV_GIG_DEBUG (getenv("GIG_DEBUG"))
    const LogField *message;

    // Defaults for a level outside the switch below.
    const char *prefix = "LOG", *color = "%s";
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

    struct port_message m = {
        .port = SCM_PACK_POINTER(user_data),
        .prefix = prefix,
        .color = color,
        .message = message->value,
    };
    scm_internal_catch(SCM_BOOL_T, write_to_port, &m, log_error_handler, NULL);

    return LOG_WRITER_HANDLED;
#undef LOG_FIELD
#undef ENV_MESSAGES_DEBUG
#undef ENV_GIG_DEBUG
}

static void gig_unprotect_func(void *func);

static SCM
gig_log_to_port(SCM port)
{
    SCM_ASSERT_TYPE(SCM_OPOUTPORTP(port), port, SCM_ARG1, "install-port-logger!",
                    "open output port");
    // Keep the port alive for as long as it is the logger.
    port = scm_gc_protect_object(port);
    log_set_writer_func(gig_log_writer, SCM_UNPACK_POINTER(port), gig_unprotect_func);
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

struct custom_call
{
    SCM func;
    SCM args;
};

static SCM
call_custom_logger(void *data)
{
    struct custom_call *call = data;
    return scm_apply_0(call->func, call->args);
}

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
    struct custom_call call = { SCM_PACK_POINTER(user_data), args };
    scm_internal_catch(SCM_BOOL_T, call_custom_logger, &call, log_error_handler, NULL);

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
