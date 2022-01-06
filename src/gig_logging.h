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

#ifndef GIG_LOGGING_H
#define GIG_LOGGING_H

#define GIG_LOG_ERROR       1
#define GIG_LOG_CRITICAL    2
#define GIG_LOG_WARNING     3
#define GIG_LOG_NOTICE      4   /* normal but significant condition */
#define GIG_LOG_INFO        5   /* informational */
#define GIG_LOG_DEBUG       6   /* debug-level messages */

void gig_log(int level, const char *file, int line, const char *func,
             const char *domain, const char *template, ...);

#define gig_debug_internal(level,domain,...)  gig_log((level), __FILE__, __LINE__, __func__, (domain), __VA_ARGS__)

#define gig_debug_init(...) gig_debug_internal(G_LOG_LEVEL_DEBUG, "init", __VA_ARGS__)

#define gig_debug_transfer(...) gig_debug_internal(G_LOG_LEVEL_DEBUG, "transfers", __VA_ARGS__)
#define gig_warning_transfer(...) gig_debug_internal(G_LOG_LEVEL_WARNING, "transfers", __VA_ARGS__)
#define gig_critical_transfer(...) gig_debug_internal(G_LOG_LEVEL_CRITICAL, "transfers", __VA_ARGS__)
#define gig_error_transfer(...) gig_debug_internal(G_LOG_LEVEL_ERROR, "transfers", __VA_ARGS__)
#define gig_debug_load(...)     gig_debug_internal(G_LOG_LEVEL_DEBUG, "load", __VA_ARGS__)
#define gig_warning_load(...)   gig_debug_internal(G_LOG_LEVEL_WARNING, "load", __VA_ARGS__)
#define gig_critical_load(...)  gig_debug_internal(G_LOG_LEVEL_CRITICAL, "load", __VA_ARGS__)
#define gig_error_load(...)  gig_debug_internal(G_LOG_LEVEL_ERROR, "load", __VA_ARGS__)
#define gig_debug_ffi(...) gig_debug_internal(G_LOG_LEVEL_DEBUG, "ffi", __VA_ARGS__)
#define gig_critical_ffi(...) gig_debug_internal(G_LOG_LEVEL_CRITICAL, "ffi", __VA_ARGS__)

#define gig_return_val_if_fail(a,b)                                     \
    do {                                                                \
        if (!(a)) {                                                     \
            gig_debug_internal(G_LOG_LEVEL_CRITICAL, "return", "unexpected failure: %s", #a); \
            return (b);                                                 \
        }                                                               \
    } while(0)

#define gig_return_val_if_reached(x)                                    \
    do {                                                                \
        gig_debug_internal(G_LOG_LEVEL_CRITICAL, "return", "unexpected reach: %s", #x); \
        return (x);                                                     \
    } while (0)

#define gig_assert_not_reached()                                        \
    do {                                                                \
        gig_debug_internal(G_LOG_LEVEL_CRITICAL, "reached", "unexpected reach"); \
        exit(1);                                                        \
    } while (0)


#endif
