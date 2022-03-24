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

#ifndef GIG_OBJECT_H
#define GIG_OBJECT_H

#include <libguile.h>
#include <girepository.h>
#include "../core.h"

extern SCM gig_il_property_func;

SCM gig_il_property(SCM s_gtype_name, SCM s_long_name, SCM s_short_name, SCM s_symbol);

GIG_API void gig_init_object(void);

#endif
