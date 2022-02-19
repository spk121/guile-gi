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

extern SCM gig_enum_type;
extern SCM gig_flags_type;
extern SCM gig_object_type;
extern SCM gig_paramspec_type;
extern SCM gig_value_type;
extern SCM gig_closure_type;

typedef void *(*GigTypeRefFunction)(void *);
typedef void (*GigTypeUnrefFunction)(void *);

char *gig_type_class_name_from_gtype(GType gtype);

void gig_type_register(GType gtype, SCM stype);
SCM gig_type_define(GType gtype, SCM defs);
SCM gig_type_define_full(GType gtype, SCM defs, SCM extra_supers);
SCM gig_type_define_with_info(GIRegisteredTypeInfo *info, SCM supers, SCM slots);
void gig_type_define_fundamental(GType type, SCM extra_supers,
                                 GigTypeRefFunction ref, GigTypeUnrefFunction unref);

GType scm_to_gtype(SCM x);
GType scm_to_gtype_full(SCM x, const char *subr, int argpos);
SCM scm_from_gtype(GType x);
GType gig_type_get_gtype_from_obj(SCM x);
SCM gig_type_get_scheme_type(GType gtype);
SCM gig_type_get_scheme_type_with_info(GIRegisteredTypeInfo *info);

bool gig_type_check_object(SCM obj);
bool gig_type_check_typed_object(SCM obj, SCM expected_type);
SCM gig_type_transfer_object(GType gtype, void *obj, GITransfer transfer);
void *gig_type_peek_object(SCM obj);
void *gig_type_peek_typed_object(SCM obj, SCM expected);

GIG_API void gig_init_types(void);

#endif
