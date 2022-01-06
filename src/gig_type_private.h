// Copyright (C) 2019, 2020 Michael L. Gran

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
#ifndef _GIG_TYPE_PRIVATE_H_
#define _GIG_TYPE_PRIVATE_H_

#include <ffi.h>
#include <libguile.h>

typedef struct _GigBoxedFuncs
{
    ffi_type *atypes[1];

    ffi_closure *copy_closure;
    ffi_cif copy_cif;
    void *copy;

    ffi_closure *free_closure;
    ffi_cif free_cif;
    void *free;
} GigBoxedFuncs;

GigBoxedFuncs *_boxed_funcs_for_type(size_t type);
void _free_boxed_funcs();

extern SCM sym_obarray;

#endif
