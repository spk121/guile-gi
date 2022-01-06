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

#define gig_debug_amap(...) \
    gig_debug_internal(GIG_LOG_DEBUG, "amap", __VA_ARGS__)

#define gig_debug_invoke(...)                                   \
    gig_debug_internal(GIG_LOG_DEBUG, "invoke", __VA_ARGS__)

#define gig_debug_transfer(...) gig_debug_internal(GIG_LOG_DEBUG, "transfers", __VA_ARGS__)
#define gig_debug_load(...)     gig_debug_internal(GIG_LOG_DEBUG, "load", __VA_ARGS__)
#define gig_warning_load(...)   gig_debug_internal(GIG_LOG_WARNING, "load", __VA_ARGS__)
#define gig_critical_load(...)  gig_debug_internal(GIG_LOG_CRITICAL, "load", __VA_ARGS__)

#endif
