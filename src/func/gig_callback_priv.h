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

#ifndef GIG_CALLBACK_H
#define GIG_CALLBACK_H

#include <libguile.h>
#include "gig_callback.h"
#include "../core.h"

SCM gig_callback_to_scm(const char *name, GigArgMap *amap, void *proc);
void *gig_callback_to_c(const char *name, GigArgMap *amap, SCM s_func);

GIG_API void gig_init_callback(void);

#endif
