// Copyright (C) 2018, 2019 Michael L. Gran

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

#ifndef _GIR_FUNCTION_H_
#define _GIR_FUNCTION_H_

#include <glib.h>
#include <girepository.h>
#include <ffi.h>
#include <libguile.h>

extern SCM gir_function_type;
typedef SCM (*gir_gsubr_t)(void);

typedef struct _GirFunction
{
    GIFunctionInfo *function_info;
    ffi_closure *closure;
    ffi_cif cif;
    void *function_ptr;
    int n_required;
    int n_optional;
    char *name;
    ffi_type **atypes;
} GirFunction;

gchar*
gir_function_make_name(const char *parent, GIFunctionInfo *info);
SCM gir_function_invoke (char *name, GICallableInfo *info, GObject *object, SCM args, GError **error);
void gir_function_define_gsubr(const char *parent, GIFunctionInfo *info);
void gir_init_function(void);
#endif
