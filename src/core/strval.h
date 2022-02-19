// Copyright (C) 2022 Michael L. Gran

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

#ifndef CORE_STRVAL_H
#define CORE_STRVAL_H

#include <stdint.h>

typedef struct strval_item_t_
{
    char *key;
    uint64_t val;
} strval_item_t;

typedef struct strval_t_
{
    strval_item_t *entries;
    int len;
    int alloc;
} strval_t;

strval_t *strval_new(void);
uint64_t strval_find_entry(strval_t *kv, const char *key);
void strval_add_entry(strval_t *kv, const char *key, uint64_t val);
void strval_free(strval_t *kv, void (*valfree)(uint64_t val));
int strval_size(strval_t *kv);

#endif
