// Copyright (C) 2019, 2022 Michael L. Gran

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

#ifndef CORE_LOGGING_H
#define CORE_LOGGING_H

#include <assert.h>
#include <stdio.h>
#include <stdbool.h>
#include <unistd.h>

typedef struct _LogField
{
    const char *key;
    const void *value;
    ssize_t length;
} LogField;

typedef enum _LogWriterOutput
{
    LOG_WRITER_HANDLED = 1,
    LOG_WRITER_UNHANDLED = 0
} LogWriterOutput;

typedef enum _LogLeveFlags
{
    /* log flags */
    LOG_FLAG_RECURSION = 1 << 0,
    LOG_FLAG_FATAL = 1 << 1,

    /* GLib log levels */
    LOG_LEVEL_ERROR = 1 << 2,   /* always fatal */
    LOG_LEVEL_CRITICAL = 1 << 3,
    LOG_LEVEL_WARNING = 1 << 4,
    LOG_LEVEL_MESSAGE = 1 << 5,
    LOG_LEVEL_INFO = 1 << 6,
    LOG_LEVEL_DEBUG = 1 << 7,

    LOG_LEVEL_MASK = ~(LOG_FLAG_RECURSION | LOG_FLAG_FATAL),
    LOG_FATAL_MASK = LOG_FLAG_FATAL | LOG_LEVEL_ERROR
} LogLevelFlags;

typedef LogWriterOutput(*LogWriterFunc) (LogLevelFlags log_level, const LogField *fields,
                                         size_t n_fields, void *user_data);
typedef void (*DestroyNotify)(void *data);

bool log_writer_supports_color(int fd);
void log_set_writer_func(LogWriterFunc func, void *user_data, DestroyNotify user_data_free);
LogWriterOutput log_writer_journald(LogLevelFlags log_level, const LogField *fields,
                                    size_t n_fields, void *user_data);
LogWriterOutput log_writer_default(LogLevelFlags log_level, const LogField *fields,
                                   size_t n_fields, void *user_data);
void log_structured(const char *log_domain, LogLevelFlags log_level, ...);

#define assert_not_reached() (assert(0))

#define gig_debug_internal(level, domain, ...)                                                    \
    do                                                                                            \
    {                                                                                             \
        log_structured(G_LOG_DOMAIN, level, "CODE_FILE", __FILE__, "CODE_LINE",                   \
                       G_STRINGIFY(__LINE__), "CODE_FUNC", __func__, "GIG_DOMAIN", domain,        \
                       "MESSAGE", __VA_ARGS__);                                                   \
    } while (false)
#define gig_debug_transfer(...) gig_debug_internal(LOG_LEVEL_DEBUG, "transfers", __VA_ARGS__)
#define gig_debug_load(...) gig_debug_internal(LOG_LEVEL_DEBUG, "load", __VA_ARGS__)
#define gig_warning_load(...) gig_debug_internal(LOG_LEVEL_WARNING, "load", __VA_ARGS__)
#define gig_critical_load(...) gig_debug_internal(LOG_LEVEL_CRITICAL, "load", __VA_ARGS__)
#define gig_debug(...) gig_debug_internal(LOG_LEVEL_DEBUG, "general", __VA_ARGS__)
#define gig_warning(...) gig_debug_internal(LOG_LEVEL_WARNING, "general", __VA_ARGS__)
#define gig_critical(...) gig_debug_internal(LOG_LEVEL_CRITICAL, "general", __VA_ARGS__)
#define gig_error(...) gig_debug_internal(LOG_LEVEL_ERROR, "general", __VA_ARGS__)
#define gig_warn_if_reached() gig_warning("unexpected condition reached")
#define gig_return_val_if_reached(val)                                                            \
    do                                                                                            \
    {                                                                                             \
        gig_warning("unexpected return condition reached");                                       \
        return val;                                                                               \
    } while (0)
#define gig_return_val_if_fail(test, val)                                                         \
    do                                                                                            \
    {                                                                                             \
        if (!(test))                                                                              \
        {                                                                                         \
            gig_warning("unexpected return val condition");                                       \
            return val;                                                                           \
        }                                                                                         \
    } while (0)


#endif
