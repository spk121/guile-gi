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

#ifndef Y_FLAG_H
#define Y_FLAG_H

#include <girepository.h>
#include <libguile.h>

int enum_to_int(SCM _enum);
unsigned flags_to_uint(SCM _flags);
SCM int_to_enum(int value, GType type);
SCM uint_to_flags(unsigned value, GType type);
SCM int_to_enum_with_info(int val, GIEnumInfo *info);
SCM uint_to_flags_with_info(unsigned val, GIEnumInfo *info);
SCM symbol_to_enum(SCM type, SCM symbol);
SCM list_to_flags(SCM type, SCM symbol);

SCM define_enum_conversions(GIEnumInfo *info, GType type, SCM defs);
SCM define_enum(GIEnumInfo *info, SCM defs);

void init_flag(void);
#endif
