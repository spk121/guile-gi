// Copyright (C) 2019, 2020, 2021 Michael L. Gran

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

#ifndef GIG_UTIL_H
#define GIG_UTIL_H

#include "core.h"
#include <girepository.h>
#include <glib.h>

// *INDENT-OFF*
G_BEGIN_DECLS
// *INDENT-ON*

G_GNUC_MALLOC char *gig_callable_info_make_name(GICallableInfo *info, const char *prefix);
const char *g_base_info_get_name_safe(GIBaseInfo *info);
char *g_registered_type_info_get_qualified_name(GIRegisteredTypeInfo *info);

G_END_DECLS
#endif
