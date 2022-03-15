/* GLIB - Library of useful routines for C programming
 * Copyright (C) 1995-1997  Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */

/*
 * Modified by the GLib Team and others 1997-2000.  See the AUTHORS
 * file for a list of people on the GLib Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GLib at ftp://ftp.gtk.org/pub/gtk/.
 */

/*
 * Modified by MichaeL Gran 2022 */

#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include "logging.h"

LogWriterOutput log_writer_fallback(LogLevelFlags flags, const LogField *fields, size_t n_fields,
                                    void *_user_data);

static LogWriterFunc log_writer_func = log_writer_fallback;
static void *log_writer_user_data = NULL;
DestroyNotify log_writer_user_data_free = NULL;

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

#define N_TERMINALS 455
static char color_terminals[N_TERMINALS][24] = {
    "xterm", "xterm-256color", "xterm-new", "konsole",
    "konsole-256color", "linux", "gnome", "gnome-256color", "screen",
    "screen-16color", "screen-256color",

#if 1
    "alacritty", "ansi", "ansi80x25", "ansis", "Apple_Terminal",
    "aterm", "bterm", "cons25", "cygwin", "Eterm", "Eterm-256color",
    "Eterm-88color", "eterm-color", "Eterm-color", "hurd", "jfbterm",
    "kitty", "kon", "kon2", "mach-color", "mach-gnu-color", "mlterm",
    "mrxvt", "nsterm", "nsterm-256color", "nxterm", "pcansi", "putty",
    "putty-256color", "rxvt", "rxvt-16color", "rxvt-256color",
    "rxvt-88color", "rxvt-color", "rxvt-cygwin", "rxvt-cygwin-native",
    "rxvt-unicode", "rxvt-unicode-256color", "rxvt-xpm",
    "screen.Eterm", "screen.gnome", "screen.konsole",
    "screen.konsole-256color", "screen.linux", "screen.linux-s",
    "screen.mlterm", "screen.mlterm-256color", "screen.mrxvt",
    "screen.putty", "screen.putty-256color", "screen.rxvt",
    "screen.teraterm", "screen.vte", "screen.vte-256color",
    "screen.xterm-256color", "screen.xterm-new",
    "screen.xterm-xfree86", "st", "st-16color", "st-256color",
    "stterm", "stterm-16color", "stterm-256color", "teraterm",
    "teraterm2.3", "tmux", "tmux-256color", "tmux-direct", "vte",
    "vte-256color", "vwmterm", "wsvt25", "wsvt25m", "xfce",
    "xterm-1002", "xterm-1003", "xterm-1005", "xterm-1006",
    "xterm-16color", "xterm-88color", "xterm-8bit", "xterm-basic",
    "xterm-color", "xterm-direct", "xterm-direct16", "xterm-direct2",
    "xterm-direct256", "xterm-hp", "xterm-nic", "xterm-noapp",
    "xterm-sco", "xterm-sun", "xterm-utf8", "xterm-vt220",
    "xterm-x10mouse", "xterm-x11hilite", "xterm-x11mouse",
    "xterm-xf86-v32", "xterm-xf86-v33", "xterm-xf86-v333",
    "xterm-xf86-v40", "xterm-xf86-v43", "xterm-xf86-v44",
    "xterm-xfree86", "xterm-xi"
#endif
};

static bool
is_color_term(const char *str)
{
    if (str == NULL)
        return false;

    for (int i = 0; i < N_TERMINALS; i++) {
        if (strcmp(str, color_terminals[i]) == 0)
            return true;
    }
    return false;
}

bool
log_writer_supports_color(int fd)
{
    static int first = 1;
    bool color = false;
    if (first) {
        first = 0;
        color = is_color_term(getenv("TERM"));
    }

    if ((fd == STDOUT_FILENO || fd == STDERR_FILENO) && color)
        return true;
    return false;
}

static FILE *
log_level_to_file(LogLevelFlags log_level)
{
    if (log_level & (LOG_LEVEL_ERROR | LOG_LEVEL_CRITICAL | LOG_LEVEL_WARNING | LOG_LEVEL_MESSAGE))
        return stderr;
    else
        return stdout;
}

LogWriterOutput
log_writer_fallback(LogLevelFlags flags, const LogField *fields, size_t n_fields, void *_user_data)
{
#define LOG_FIELD(f) field_ref(f, fields, n_fields)
#define ENV_MESSAGES_DEBUG (getenv("G_MESSAGES_DEBUG"))
#define ENV_GIG_DEBUG (getenv("GIG_DEBUG"))
    const LogField *message;

    FILE *outfp = log_level_to_file(flags);
    const char *prefix = NULL;
    switch (flags & LOG_LEVEL_MASK) {
    case LOG_LEVEL_ERROR:
        prefix = "ERROR";
        break;
    case LOG_LEVEL_CRITICAL:
        prefix = "CRITICAL";
        break;
    case LOG_LEVEL_WARNING:
        prefix = "WARNING";
        break;
    case LOG_LEVEL_MESSAGE:
        prefix = "MESSAGE";
        break;
    case LOG_LEVEL_INFO:
        if (!is_enabled(LOG_FIELD("GLIB_DOMAIN"), ENV_MESSAGES_DEBUG, false)
            || !is_enabled(LOG_FIELD("GIG_DOMAIN"), ENV_GIG_DEBUG, true))
            return LOG_WRITER_HANDLED;
        prefix = "INFO";
        break;
    case LOG_LEVEL_DEBUG:
        if (!is_enabled(LOG_FIELD("GLIB_DOMAIN"), ENV_MESSAGES_DEBUG, false)
            || !is_enabled(LOG_FIELD("GIG_DOMAIN"), ENV_GIG_DEBUG, true))
            return LOG_WRITER_HANDLED;
        prefix = "DEBUG";
        break;
    }

    message = field_ref("MESSAGE", fields, n_fields);
    if (message)
        fprintf(outfp, "%s: %s\n", prefix, (const char *)message->value);
    else
        fprintf(outfp, "%s: %s\n", prefix, "Unknown error");

    return LOG_WRITER_HANDLED;
#undef LOG_FIELD
#undef ENV_MESSAGES_DEBUG
#undef ENV_GIG_DEBUG
}

void
log_set_writer_func(LogWriterFunc func, void *_user_data, DestroyNotify _user_data_free)
{
    log_writer_func = func;
    log_writer_user_data = _user_data;
    log_writer_user_data_free = _user_data_free;
}

#if 0
static LogWriterOutput
log_writer_debug(LogLevelFlags log_level, const LogField *fields, size_t n_fields, void *user_data)
{
    FILE *stream;
    size_t i;

    stream = log_level_to_file(log_level);

    for (i = 0; i < n_fields; i++) {
        const LogField *field = &fields[i];

        if (strcmp(field->key, "MESSAGE") != 0 &&
            strcmp(field->key, "MESSAGE_ID") != 0 &&
            strcmp(field->key, "PRIORITY") != 0 &&
            strcmp(field->key, "CODE_FILE") != 0 &&
            strcmp(field->key, "CODE_LINE") != 0 &&
            strcmp(field->key, "CODE_FUNC") != 0 &&
            strcmp(field->key, "ERRNO") != 0 &&
            strcmp(field->key, "SYSLOG_FACILITY") != 0 &&
            strcmp(field->key, "SYSLOG_IDENTIFIER") != 0 &&
            strcmp(field->key, "SYSLOG_PID") != 0 && strcmp(field->key, "GLIB_DOMAIN") != 0)
            continue;

        fputs(field->key, stream);
        fputs("=", stream);
        if (field->length < 0)
            fputs(field->value, stream);
        else
            fwrite(field->value, 1, field->length, stream);
    }

    fprintf(stream, "_PID=%d", getpid());

    return LOG_WRITER_HANDLED;
}
#endif

static void
log_structured_array(LogLevelFlags log_level, const LogField *fields, size_t n_fields)
{
    if (n_fields == 0)
        return;

    // FIXME: need mutex

    log_writer_func(log_level, fields, n_fields, log_writer_user_data);

    if (log_level & LOG_FATAL_MASK)
        exit(1);
}

static const char *
log_level_to_priority(LogLevelFlags log_level)
{
    if (log_level & LOG_LEVEL_ERROR)
        return "3";
    else if (log_level & LOG_LEVEL_CRITICAL)
        return "4";
    else if (log_level & LOG_LEVEL_WARNING)
        return "4";
    else if (log_level & LOG_LEVEL_MESSAGE)
        return "5";
    else if (log_level & LOG_LEVEL_INFO)
        return "6";
    else if (log_level & LOG_LEVEL_DEBUG)
        return "7";

    /* Default to LOG_NOTICE for custom log levels. */
    return "5";
}

void
log_structured(const char *log_domain, LogLevelFlags log_level, ...)
{
    va_list args;
    char buffer[1025];
    const char *format;
    const char *message;
    void *p;
    size_t n_fields, i;
    LogField stack_fields[16];
    LogField *fields = stack_fields;

    va_start(args, log_level);

    /* MESSAGE and PRIORITY are a given */
    n_fields = 2;

    if (log_domain)
        n_fields++;

    for (p = va_arg(args, char *), i = n_fields;
         strcmp(p, "MESSAGE") != 0; p = va_arg(args, char *), i++)
    {
        LogField field;
        const char *key = p;
        const void *value = va_arg(args, void *);

        field.key = key;
        field.value = value;
        field.length = -1;

        if (i < 16)
            stack_fields[i] = field;
    }

    n_fields = i;

    format = va_arg(args, char *);

    vsnprintf(buffer, sizeof(buffer), format, args);
    message = buffer;

    /* Add MESSAGE, PRIORITY and GLIB_DOMAIN. */
    fields[0].key = "MESSAGE";
    fields[0].value = message;
    fields[0].length = -1;

    fields[1].key = "PRIORITY";
    fields[1].value = log_level_to_priority(log_level);
    fields[1].length = -1;

    if (log_domain) {
        fields[2].key = "GLIB_DOMAIN";
        fields[2].value = log_domain;
        fields[2].length = -1;
    }

    /* Log it. */
    log_structured_array(log_level, fields, n_fields);

    va_end(args);
}
