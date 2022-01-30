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

#define _XOPEN_SOURCE 700       /* For strdup, strndup */
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mem.h"

void *
xcalloc(size_t nmemb, size_t siz)
{
    void *x;
    x = calloc(nmemb, siz);
    if (x == 0) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    return x;
}

void *
xmalloc(size_t siz)
{
    void *x;
    x = malloc(siz);
    if (x == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    return x;
}

void *
xmemdup(const void *mem, size_t len)
{
    void *new_mem;

    new_mem = malloc(len);
    if (new_mem == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    memcpy(new_mem, mem, len);

    return new_mem;
}

char *
xstrdup(const char *S)
{
    char *x;
    x = strdup(S);
    if (x == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    return x;
}

char *
xstrndup(const char *S, size_t siz)
{
    char *x;
    x = strndup(S, siz);
    if (x == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    return x;
}
