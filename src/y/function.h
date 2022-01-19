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

#ifndef Y_FUNCTION_H
#define Y_FUNCTION_H

#include <girepository.h>
#include <libguile.h>
#include "y/arg_map.h"

typedef SCM (*Gsubr)(void);
typedef struct _Function Function;

SCM define_function(GType type, GICallableInfo *info, const char *_namespace, SCM defs);
SCM invoke_callable(GICallableInfo *callable_info, void *callable, Arg_map *amap,
                    const char *name, GObject *self, SCM args, GError **error);
void init_function(void);
#endif