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

#ifndef CORE_KEYVAL_H
#define CORE_KEYVAL_H

#include <stdint.h>

typedef struct _keyval_item_t
{
    uint64_t key;
    uint64_t val;
} keyval_item_t;

typedef struct _keyval_t
{
    keyval_item_t *entries;
    int len;
    int alloc;
} keyval_t;

keyval_t *keyval_new(void);
uint64_t keyval_find_entry(keyval_t *kv, uint64_t key);
void keyval_add_entry(keyval_t *kv, uint64_t key, uint64_t val);
void keyval_free(keyval_t *kv, void (*keyfree)(uint64_t key), void (*valfree)(uint64_t val));
int keyval_size(keyval_t *kv);

#endif
