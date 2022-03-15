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

#ifndef CORE_SLIST_H
#define CORE_SLIST_H

typedef struct slist_t_
{
    void *data;
    struct slist_t_ *next;
} slist_t;

void slist_prepend(slist_t **lst, void *data);
void slist_free(slist_t **lst, void (*free_func)(void *));

#endif
