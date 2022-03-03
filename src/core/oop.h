// Copyright (C) 2022 Michael L. Gran

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

#ifndef CORE_OOP_H
#define CORE_OOP_H

#include <libguile.h>

SCM scm_get_applicable_class(void);
SCM scm_get_applicable_struct_class(void);
SCM scm_get_applicable_struct_with_setter_class(void);
SCM scm_get_char_class(void);
SCM scm_get_foreign_class(void);
SCM scm_get_hashtable_class(void);
SCM scm_get_integer_class(void);
SCM scm_get_list_class(void);
SCM scm_get_real_class(void);
SCM scm_get_method_class(void);
SCM scm_get_string_class(void);
SCM scm_get_top_class(void);

SCM scm_add_method(SCM target, SCM method);
SCM scm_define_methods_from_procedure(const char *name, SCM proc, int opt, SCM formals,
                                      SCM specializers);
SCM scm_ensure_accessor_with_name(SCM proc, SCM name);
SCM scm_get_class_ref_slot(SCM cls);
SCM scm_get_class_size_slot(SCM cls);
SCM scm_get_class_unref_slot(SCM cls);
SCM scm_get_value_slot(SCM instance);
SCM scm_make_class_with_name(SCM supers, SCM slots, SCM name);
SCM scm_make_with_value(SCM type, SCM value);
SCM scm_make_method(SCM specializers, SCM formals, SCM proc);
SCM scm_set_class_obarray_slot(SCM cls, SCM obarray);
SCM scm_set_class_ref_slot(SCM cls, SCM func);
SCM scm_set_class_size_slot(SCM cls, SCM size);
SCM scm_set_class_unref_slot(SCM cls, SCM func);
SCM scm_set_procedure_slot(SCM instance, SCM proc);

void init_core_oop(void);

#endif
