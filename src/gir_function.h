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
#include <libguile.h>

void gir_function_define_gsubr(GIFunctionInfo *info, const char *prefix);
SCM gir_function_invoke(GIFunctionInfo *info, const char *name, GObject *object, SCM args,
                        GError **error);
void gir_init_function(void);
#endif
