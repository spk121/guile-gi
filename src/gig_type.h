// Copyright (C) 2018, 2019, 2020 Michael L. Gran

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

// *INDENT-OFF*
G_BEGIN_DECLS
// *INDENT-ON*

extern SCM gig_enum_type;
extern SCM gig_flags_type;
extern SCM gig_object_type;
extern SCM gig_interface_type;
extern SCM gig_paramspec_type;
extern SCM gig_value_type;
extern SCM gig_closure_type;

G_GNUC_MALLOC gchar *gig_type_class_name_from_gtype(GType gtype);

void gig_type_register(GType gtype, SCM stype);
SCM gig_type_define(GType gtype, SCM defs);
SCM gig_type_define_full(GType gtype, SCM defs, SCM extra_supers);
SCM gig_type_define_with_info(GIRegisteredTypeInfo *info, SCM supers, SCM slots);

GType scm_to_gtype(SCM x);
GType scm_to_gtype_full(SCM x, const gchar *subr, gint argpos);
SCM scm_from_gtype(GType x);
GType gig_type_get_gtype_from_obj(SCM x);
SCM gig_type_get_scheme_type(GType gtype);
SCM gig_type_get_scheme_type_with_info(GIRegisteredTypeInfo *info);

SCM gig_type_transfer_object(GType gtype, gpointer obj, GITransfer transfer);
gpointer gig_type_peek_object(SCM obj);
gpointer gig_type_peek_typed_object(SCM obj, SCM expected);

void gig_init_types(void);

G_END_DECLS
#endif
