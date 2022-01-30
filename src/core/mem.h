// Copyright (C) 2019, 2020, 2021, 2022 Michael L. Gran

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

#ifndef X_UTIL_H
#define X_UTIL_H

#include <stddef.h>
#define MALLOC __attribute__((malloc))

MALLOC void *xcalloc(size_t nmemb, size_t siz);
MALLOC void *xmalloc(size_t siz);
char *xstrdup(const char *S);
char *xstrndup(const char *S, size_t siz);
void *xmemdup(const void *mem, size_t len);
#endif
