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

#include <stdio.h>
#include <stdlib.h>
#include "slist.h"

void
slist_prepend(slist_t **lst, void *data)
{
    slist_t *cur;
    cur = malloc(sizeof(slist_t));
    if (cur == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    cur->data = data;
    cur->next = *lst;
    *lst = cur;
}

void
slist_free(slist_t **lst, void (*free_func)(void *))
{
    slist_t *cur, *next;
    if (lst == NULL)
        return;
    cur = *lst;
    do {
        if (cur == NULL)
            break;
        if (free_func)
            free_func(cur->data);
        next = cur->next;
        free(cur);
        cur = next;
    } while (1);
    *lst = NULL;
}
