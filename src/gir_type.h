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

SCM gig_object_type;
SCM gig_boxed_type;
SCM gig_compact_type;
SCM gig_interface_type;
SCM gig_paramspec_type;

G_GNUC_MALLOC gchar *gir_type_document_type_from_gtype(GType gtype);
G_GNUC_MALLOC char *gir_type_class_name_from_gtype(GType gtype);
void gir_type_register(GType gtype);
void gir_type_define(GType gtype);
GType scm_to_gtype(SCM x);
GType gir_type_get_gtype_from_obj(SCM x);
SCM gir_type_get_scheme_type(GType gtype);
SCM gir_type_transfer_object(GType gtype, gpointer obj, GITransfer transfer);
gpointer gir_type_peek_object(SCM obj);
void gir_init_types(void);

#endif
