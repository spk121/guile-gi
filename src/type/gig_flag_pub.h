// Copyright (C) 2018, 2019, 2022 Michael L. Gran

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

#ifndef GIG_FLAG_PUBLIC_H
#define GIG_FLAG_PUBLIC_H

#include <girepository.h>
#include <libguile.h>

SCM gig_define_enum_conversions(GIEnumInfo *info, GType type, SCM defs);
int gig_enum_to_int(SCM _enum);
unsigned gig_flags_to_uint(SCM _flags);
SCM gig_int_to_enum(int value, GType type);
SCM gig_int_to_enum_with_info(int val, GIEnumInfo *info);
SCM gig_uint_to_flags(unsigned value, GType type);
SCM gig_uint_to_flags_with_info(unsigned val, GIEnumInfo *info);

void gig_init_flag(void);

#endif
