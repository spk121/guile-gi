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

#ifndef GIG_REPOSITORY_H
#define GIG_REPOSITORY_H

#include <girepository.h>
#include "core.h"

typedef GIBaseInfo *(*GigRepositoryNested)(GIBaseInfo *info, int n);

void gig_repository_nested_infos(GIBaseInfo *base,
                                 int *n_methods,
                                 GigRepositoryNested *method,
                                 int *n_properties,
                                 GigRepositoryNested *property,
                                 int *n_signals, GigRepositoryNested *signal);

GigArgMap *callable_info_make_amap(GICallableInfo *function_info, const char *name);
char *callable_info_make_name(GICallableInfo *info, const char *prefix);

GIG_API void gig_init_repository(void);

#endif
