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

#ifndef GIR_TYPE_H
#define GIR_TYPE_H

#include <girepository.h>

#define SLOT_COUNT 8
#define OB_TYPE_SLOT 0
#define OB_REFCNT_SLOT 1
#define OBJ_SLOT 2
#define DEALLOC_SLOT 3
#define FREE_FUNC_SLOT 4
#define INST_DICT_SLOT 5
#define WEAKREFLIST_SLOT 6
#define FLAGS_SLOT 7

void gir_type_register(GType gtype);
void gir_type_define(GType gtype);
GType scm_to_gtype(SCM x);
GType gir_type_get_gtype_from_obj(SCM x);
SCM gir_type_get_scheme_type(GType gtype);
SCM gir_type_make_object(GType gtype, gpointer obj, GITransfer transfer);
void gir_init_types(void);

#endif
