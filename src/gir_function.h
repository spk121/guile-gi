// Copyright (C) 2018 Michael L. Gran

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
} GirFunction;

SCM gir_function_info_convert_output_args(const char *func_name, const GIFunctionInfo *func_info, int n_output_args, GIArgument *out_args);
void gir_function_info_convert_args(GIFunctionInfo *func_info, SCM s_args, int *n_input_args, GIArgument **in_args, unsigned **in_args_free, int *n_output_args, GIArgument **out_args);
void gir_function_define_gsubr(const char *namespace_, const char *parent, GIFunctionInfo *info);
void gir_init_function(void);
#endif