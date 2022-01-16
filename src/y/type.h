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

#ifndef TYPE_H
#define TYPE_H

#include <girepository.h>
#include "x.h"

typedef void *(*Type_ref_function)(void *);
typedef void (*Type_unref_function)(void *);


GType scm_to_gtype(SCM x);
SCM get_closure_type(void);
SCM get_scheme_type(GType gtype);
SCM get_scheme_type_with_info(GIRegisteredTypeInfo *info);
SCM get_value_type(void);
SCM get_enum_type(void);
SCM get_flags_type(void);
SCM transfer_object(GType gtype, void *obj, GITransfer transfer);
SCM define_type(GType gtype, SCM defs);
void
define_fundamental_type(GType type, SCM extra_supers,
                        Type_ref_function ref, Type_unref_function unref);
SCM define_type_with_info(GIRegisteredTypeInfo *info, SCM supers, SCM slots);
void *peek_typed_object(SCM obj, SCM expected);

SCM sym_obarray;
#if 0
#include "types.h"

extern SCM enum_type;
extern SCM flags_type;
extern SCM object_type;
extern SCM interface_type;
extern SCM paramspec_type;
extern SCM value_type;
extern SCM closure_type;


G_GNUC_MALLOC char *type_class_name_from_gtype(GType gtype);

void type_register(GType gtype, SCM stype);
SCM type_define_full(GType gtype, SCM defs, SCM extra_supers);
SCM type_define_with_info(GIRegisteredTypeInfo *info, SCM supers, SCM slots);
void
type_define_fundamental(GType type, SCM extra_supers,
                            GigTypeRefFunction ref, GigTypeUnrefFunction unref);

GType scm_to_gtype(SCM x);
GType scm_to_gtype_full(SCM x, const char *subr, int argpos);
SCM scm_from_gtype(GType x);
GType type_get_gtype_from_obj(SCM x);

intbool_t type_check_object(SCM obj);
intbool_t type_check_typed_object(SCM obj, SCM expected_type);
SCM type_transfer_object(GType gtype, void *obj, GITransfer transfer);
void *type_peek_object(SCM obj);

API void init_types(void);
#endif
#endif
