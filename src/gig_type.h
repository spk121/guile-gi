// Copyright (C) 2018, 2019, 2020, 2021 Michael L. Gran

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

#include <girepository.h>
#include "core.h"
#include "gig_types.h"

extern SCM gig_enum_type;
extern SCM gig_flags_type;
extern SCM gig_object_type;
extern SCM gig_interface_type;
extern SCM gig_paramspec_type;
extern SCM gig_value_type;
extern SCM gig_closure_type;

typedef void *(*GigTypeRefFunction)(void *);
typedef void (*GigTypeUnrefFunction)(void *);

G_GNUC_MALLOC char *gig_type_class_name_from_gtype(gtype_t gtype);

void gig_type_register(gtype_t gtype, SCM stype);
SCM gig_type_define(gtype_t gtype, SCM defs);
SCM gig_type_define_full(gtype_t gtype, SCM defs, SCM extra_supers);
SCM gig_type_define_with_info(GIRegisteredTypeInfo *info, SCM supers, SCM slots);
void
gig_type_define_fundamental(gtype_t type, SCM extra_supers,
                            GigTypeRefFunction ref, GigTypeUnrefFunction unref);

gtype_t scm_to_gtype(SCM x);
gtype_t scm_to_gtype_full(SCM x, const char *subr, int argpos);
SCM scm_from_gtype(gtype_t x);
gtype_t gig_type_get_gtype_from_obj(SCM x);
SCM gig_type_get_scheme_type(gtype_t gtype);
SCM gig_type_get_scheme_type_with_info(GIRegisteredTypeInfo *info);

intbool_t gig_type_check_object(SCM obj);
intbool_t gig_type_check_typed_object(SCM obj, SCM expected_type);
SCM gig_type_transfer_object(gtype_t gtype, void *obj, GITransfer transfer);
void *gig_type_peek_object(SCM obj);
void *gig_type_peek_typed_object(SCM obj, SCM expected);

GIG_API void gig_init_types(void);
#endif
