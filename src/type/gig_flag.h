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

#ifndef GIG_FLAG_H
#define GIG_FLAG_H

#include <libguile.h>
#include <glib.h>
#include <glib-object.h>

GIG_API extern SCM gig_il_untyped_enum_conversions_func;
GIG_API extern SCM gig_il_untyped_flag_conversions_func;
GIG_API extern SCM gig_il_enum_conversions_func;
GIG_API extern SCM gig_il_flag_conversions_func;

int gig_enum_to_int(SCM _enum);
unsigned gig_flags_to_uint(SCM _flags);
SCM gig_int_to_enum(int value, GType type);
SCM gig_int_to_enum_with_qname(int val, const char *qname);
SCM gig_uint_to_flags(unsigned value, GType type);
SCM gig_uint_to_flags_with_qname(unsigned val, const char *qname);

void gig_init_flag(void);

#endif
