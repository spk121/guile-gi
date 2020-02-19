// Copyright (C) 2018, 2019 Michael L. Gran

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

#include <string.h>
#include <girepository.h>
#include <glib.h>
#include <libguile.h>
#include "gig_util.h"

_Thread_local int logger_initialized = 0;

static const GLogField *
field_ref(const gchar *needle, const GLogField *fields, gsize n_fields)
{
    for (gsize i = 0; i < n_fields; i++)
        if (!(strcmp(fields[i].key, needle)))
            return fields + i;
    return NULL;
}

static gboolean
is_enabled(const GLogField *domain, const gchar *domains, gboolean allow_empty)
{
    if (!domain || !domain->value)
        return allow_empty;
    if (domains == NULL)
        return FALSE;
    if (!strcmp(domains, "all"))
        return TRUE;
    if (strstr(domains, domain->value))
        return TRUE;
    return FALSE;
}

static GLogWriterOutput
gig_log_writer(GLogLevelFlags flags, const GLogField *fields, gsize n_fields, gpointer user_data)
{
#define LOG_FIELD(f) field_ref(f, fields, n_fields)
#define ENV_MESSAGES_DEBUG (g_getenv("G_MESSAGES_DEBUG"))
#define ENV_GIG_DEBUG (g_getenv("GIG_DEBUG"))
    const GLogField *message;

    const gchar *prefix = NULL, *color = NULL;
    if (!logger_initialized) {
        scm_init_guile();
        logger_initialized = 1;
    }
    switch (flags & G_LOG_LEVEL_MASK) {
    case G_LOG_LEVEL_ERROR:
        color = "\033[1;31m%s\033[0m";
        prefix = "ERROR";
        break;
    case G_LOG_LEVEL_CRITICAL:
        color = "\033[1;35m%s\033[0m";
        prefix = "CRITICAL";
        break;
    case G_LOG_LEVEL_WARNING:
        color = "\033[1;33m%s\033[0m";
        prefix = "WARNING";
        break;
    case G_LOG_LEVEL_MESSAGE:
        color = "\033[1;32m%s\033[0m";
        prefix = "MESSAGE";
        break;
    case G_LOG_LEVEL_INFO:
        color = "\033[1;32m%s\033[0m";
        if (!is_enabled(LOG_FIELD("GLIB_DOMAIN"), ENV_MESSAGES_DEBUG, FALSE)
            || !is_enabled(LOG_FIELD("GIG_DOMAIN"), ENV_GIG_DEBUG, TRUE))
            return G_LOG_WRITER_HANDLED;
        prefix = "INFO";
        break;
    case G_LOG_LEVEL_DEBUG:
        color = "\033[1;32m%s\033[0m";
        if (!is_enabled(LOG_FIELD("GLIB_DOMAIN"), ENV_MESSAGES_DEBUG, FALSE)
            || !is_enabled(LOG_FIELD("GIG_DOMAIN"), ENV_GIG_DEBUG, TRUE))
            return G_LOG_WRITER_HANDLED;
        prefix = "DEBUG";
        break;
    }

    message = field_ref("MESSAGE", fields, n_fields);
    g_assert(message != NULL);

    SCM port = SCM_PACK_POINTER(user_data);

    if (scm_is_true(scm_output_port_p(port))) {
        if (scm_is_true(scm_file_port_p(port))) {
            gint fd = scm_to_int(scm_fileno(port));
            gchar *colored_prefix =
                g_strdup_printf(g_log_writer_supports_color(fd) ? color : "%s", prefix);
            scm_c_write(port, colored_prefix, strlen(colored_prefix));
            scm_c_write(port, ": ", 2);
            scm_c_write(port, message->value, strlen(message->value));
            scm_newline(port);
            g_free(colored_prefix);
        }
        else
            scm_printf(port, "%s: %s\n", prefix, (const gchar *)message->value);
    }
    else
        scm_printf(scm_current_error_port(), "%s: %s\n", prefix, (const gchar *)message->value);

    return G_LOG_WRITER_HANDLED;
#undef LOG_FIELD
#undef ENV_MESSAGES_DEBUG
#undef ENV_GIG_DEBUG
}

SCM
gig_log_to_port(SCM port)
{
    SCM_ASSERT_TYPE(SCM_OPOUTPORTP(port), port, SCM_ARG1, "log-to-port", "open output port");
    g_log_set_writer_func(gig_log_writer, SCM_UNPACK_POINTER(port), NULL);
    return SCM_UNSPECIFIED;
}

SCM
gig_log_to_journal(void)
{
    g_log_set_writer_func(g_log_writer_journald, NULL, NULL);
    return SCM_UNSPECIFIED;
}

void
gig_init_logging()
{
    scm_c_define_gsubr("log-to-port", 1, 0, 0, gig_log_to_port);
    scm_c_define_gsubr("log-to-journal", 0, 0, 0, gig_log_to_journal);
}
