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

#define LOG_ERROR       1
#define LOG_CRITICAL    2
#define LOG_WARNING     3
#define LOG_NOTICE      4   /* normal but significant condition */
#define LOG_INFO        5   /* informational */
#define LOG_DEBUG       6   /* debug-level messages */

void internal_log(int level, const char *file, int line, const char *func,
             const char *domain, const char *template, ...);

#define debug_internal(level,domain,...) internal_log((level), __FILE__, __LINE__, __func__, (domain), __VA_ARGS__)

#define debug_amap(...) \
    debug_internal(LOG_DEBUG, "amap", __VA_ARGS__)

#define debug_invoke(...)                                   \
    debug_internal(LOG_DEBUG, "invoke", __VA_ARGS__)

#define debug_transfer(...) debug_internal(LOG_DEBUG, "transfers", __VA_ARGS__)
#define debug_load(...)     debug_internal(LOG_DEBUG, "load", __VA_ARGS__)
#define warning_load(...)   debug_internal(LOG_WARNING, "load", __VA_ARGS__)
#define critical_load(...)  debug_internal(LOG_CRITICAL, "load", __VA_ARGS__)

#endif
