// Copyright (C) 2018, 2019, 2020, 2021, 2022 Michael L. Gran

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

#ifndef GIG_TYPE_H
#define GIG_TYPE_H

#include <stdbool.h>
#include <girepository.h>
#include "../core.h"
#include "gig_type_pub.h"

SCM gig_enum_type(void);
SCM gig_flags_type(void);
SCM gig_object_type(void);
SCM gig_paramspec_type(void);
SCM gig_value_type(void);
SCM gig_closure_type(void);

char *gig_type_class_name_from_gtype(GType gtype);

void gig_type_register(GType gtype, SCM stype);
SCM gig_type_define_full(GType gtype, SCM extra_supers);

GType gig_type_get_gtype_from_obj(SCM x);
SCM gig_type_get_scheme_type_with_info(GIRegisteredTypeInfo *info);

bool gig_type_check_object(SCM obj);
void *gig_type_peek_typed_object(SCM obj, SCM expected);

#endif
