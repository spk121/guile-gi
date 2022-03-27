// Copyright (C) 2018, 2022 Michael L. Gran

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

#ifndef GIG_CONSTANT_H
#define GIG_CONSTANT_H

#include <libguile.h>
#include "../core.h"

GIG_API extern SCM gig_il_constant_func;

SCM gig_il_constant(SCM s_name, SCM s_value);
void gig_constant_init(void);

#endif
