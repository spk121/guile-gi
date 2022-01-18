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

#ifndef Y_TYPE_H
#define Y_TYPE_H

#include <girepository.h>
#include "x.h"

extern SCM enum_type;
extern SCM flags_type;
extern SCM object_type;
extern SCM interface_type;
extern SCM paramspec_type;
extern SCM value_type;
extern SCM closure_type;

typedef void *(*Type_ref_function)(void *);
typedef void (*Type_unref_function)(void *);

char *class_name_from_gtype(GType gtype);

char *type_class_name_from_gtype(GType gtype);

void register_type(GType gtype, SCM stype);
SCM define_type(GType gtype, SCM defs);
SCM define_type_full(GType gtype, SCM defs, SCM extra_supers);
SCM define_type_with_info(GIRegisteredTypeInfo *info, SCM supers, SCM slots);
void define_fundamental_type(GType type, SCM extra_supers,
                             Type_ref_function ref, Type_unref_function unref);


GType scm_to_gtype(SCM x);
GType get_gtype_full(SCM x, const char *subr, int argpos);
SCM scm_from_gtype(GType x);
GType get_gtype_from_obj(SCM x);
SCM get_scheme_type(GType gtype);
SCM get_scheme_type_with_info(GIRegisteredTypeInfo *info);

int check_object(SCM obj);
int check_typed_object(SCM obj, SCM expected_type);
SCM transfer_object(GType gtype, void *obj, GITransfer transfer);
void *peek_object(SCM obj);
void *peek_typed_object(SCM obj, SCM expected);

GIG_API void init_types(void);
#endif
