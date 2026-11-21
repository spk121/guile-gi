// Copyright (C) 2026 Michael L. Gran

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

#ifndef GIG_VFUNC_H
#define GIG_VFUNC_H

#include <girepository.h>
#include <libguile.h>

// The name of the procedure that calls virtual function INFO of the
// type named CONTAINER, e.g. "widget:do-snapshot".  Free it.
char *gig_vfunc_make_name(GIVFuncInfo *info, const char *container);

// Defines that procedure in the current module and returns a list of
// its name, or the empty list if the virtual function can't be
// represented.
SCM gig_vfunc_define(GIVFuncInfo *info, const char *container);

void gig_init_vfunc(void);

#endif
