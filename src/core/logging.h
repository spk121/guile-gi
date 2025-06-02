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
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// Forward declarations
typedef struct log_field LogField;
typedef enum log_writer_output LogWriterOutput;
typedef enum log_level_flags LogLevelFlags;
typedef LogWriterOutput (*LogWriterFunc)(LogLevelFlags log_level, const LogField *fields,
                                        size_t n_fields, void *user_data);
typedef void (*DestroyNotify)(void *data);

// Log field structure - now with more explicit sizing
struct log_field {
    const char *key;
    const void *value;
    ssize_t length;  // Use ssize_t for better compatibility with standard library
};

// Writer output enumeration
enum log_writer_output {
    LOG_WRITER_UNHANDLED = 0,
    LOG_WRITER_HANDLED = 1
};

// Log level flags with explicit values for clarity
enum log_level_flags {
    // Log flags
    LOG_FLAG_RECURSION = 1U << 0,
    LOG_FLAG_FATAL     = 1U << 1,

    // Log levels (compatible with syslog priorities)
    LOG_LEVEL_ERROR    = 1U << 2,   // always fatal
    LOG_LEVEL_CRITICAL = 1U << 3,
    LOG_LEVEL_WARNING  = 1U << 4,
    LOG_LEVEL_MESSAGE  = 1U << 5,
    LOG_LEVEL_INFO     = 1U << 6,
    LOG_LEVEL_DEBUG    = 1U << 7,

    // Masks
    LOG_LEVEL_MASK = ~(LOG_FLAG_RECURSION | LOG_FLAG_FATAL),
    LOG_FATAL_MASK = LOG_FLAG_FATAL | LOG_LEVEL_ERROR
};

// Core logging functions
bool log_writer_supports_color(int fd);
void log_set_writer_func(LogWriterFunc func, void *user_data, DestroyNotify user_data_free);
void log_structured(const char *log_domain, LogLevelFlags log_level, ...);

// Enhanced assertion macros with better error reporting
#define ASSERT_NOT_REACHED() \
    do { \
        gig_critical("Code should not be reached at %s:%d in %s", \
                    __FILE__, __LINE__, __func__); \
        assert(0); \
    } while (0)

// Utility macros
#define STRINGIFY(x) STRINGIFY_INNER(x)
#define STRINGIFY_INNER(x) #x

// Enhanced logging macros with better parameter validation
#define GIG_LOG_INTERNAL(level, domain, format, ...) \
    do { \
        static_assert(level > 0, "Invalid log level"); \
        log_structured(G_LOG_DOMAIN, (level), \
                      "CODE_FILE", __FILE__, \
                      "CODE_LINE", STRINGIFY(__LINE__), \
                      "CODE_FUNC", __func__, \
                      "GIG_DOMAIN", (domain), \
                      "MESSAGE", (format), ##__VA_ARGS__); \
    } while (0)

// Domain-specific logging macros
#define gig_debug_transfer(format, ...) \
    GIG_LOG_INTERNAL(LOG_LEVEL_DEBUG, "transfers", format, ##__VA_ARGS__)

#define gig_debug_load(format, ...) \
    GIG_LOG_INTERNAL(LOG_LEVEL_DEBUG, "load", format, ##__VA_ARGS__)

#define gig_warning_load(format, ...) \
    GIG_LOG_INTERNAL(LOG_LEVEL_WARNING, "load", format, ##__VA_ARGS__)

#define gig_critical_load(format, ...) \
    GIG_LOG_INTERNAL(LOG_LEVEL_CRITICAL, "load", format, ##__VA_ARGS__)

#define gig_error_load(format, ...) \
    GIG_LOG_INTERNAL(LOG_LEVEL_ERROR, "load", format, ##__VA_ARGS__)

// General logging macros
#define gig_debug(format, ...) \
    GIG_LOG_INTERNAL(LOG_LEVEL_DEBUG, "general", format, ##__VA_ARGS__)

#define gig_warning(format, ...) \
    GIG_LOG_INTERNAL(LOG_LEVEL_WARNING, "general", format, ##__VA_ARGS__)

#define gig_critical(format, ...) \
    GIG_LOG_INTERNAL(LOG_LEVEL_CRITICAL, "general", format, ##__VA_ARGS__)

#define gig_error(format, ...) \
    GIG_LOG_INTERNAL(LOG_LEVEL_ERROR, "general", format, ##__VA_ARGS__)

// Enhanced control flow macros with better error reporting
#define gig_warn_if_reached() \
    gig_warning("Unexpected condition reached at %s:%d in %s", \
               __FILE__, __LINE__, __func__)

#define gig_return_val_if_reached(val) \
    do { \
        gig_warning("Unexpected return condition reached at %s:%d in %s", \
                   __FILE__, __LINE__, __func__); \
        return (val); \
    } while (0)

#define gig_return_if_reached() \
    do { \
        gig_warning("Unexpected return condition reached at %s:%d in %s", \
                   __FILE__, __LINE__, __func__); \
        return; \
    } while (0)

#define gig_return_val_if_fail(expr, val) \
    do { \
        if (!(expr)) { \
            gig_warning("Assertion '%s' failed at %s:%d in %s", \
                       #expr, __FILE__, __LINE__, __func__); \
            return (val); \
        } \
    } while (0)

#define gig_return_if_fail(expr) \
    do { \
        if (!(expr)) { \
            gig_warning("Assertion '%s' failed at %s:%d in %s", \
                       #expr, __FILE__, __LINE__, __func__); \
            return; \
        } \
    } while (0)

// Thread safety macros (can be customized based on threading library)
#ifndef GIG_LOG_USE_LOCKS
#define GIG_LOG_USE_LOCKS 0
#endif

#if GIG_LOG_USE_LOCKS
#define GIG_LOG_LOCK() /* Define based on your threading library */
#define GIG_LOG_UNLOCK() /* Define based on your threading library */
#else
#define GIG_LOG_LOCK()
#define GIG_LOG_UNLOCK()
#endif

#ifdef __cplusplus
}
#endif

#endif // CORE_LOGGING_H
