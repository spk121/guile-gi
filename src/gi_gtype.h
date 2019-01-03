// Copyright (C) 2018 Michael L. Gran

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

#ifndef _GI_GTYPE_H_
#define _GI_GTYPE_H_

#include <libguile.h>
#include <glib-object.h>
#include "__gi_gtype.h"

G_BEGIN_DECLS

extern GQuark gtype_base_info_key;
extern GQuark gtype_class_wrapper;

GType     gi_gtype_from_scm (SCM obj);
GType     gi_infer_gtype_from_scm(SCM obj);
SCM       gi_gtype_c2g (GType type);
void      gi_gtype_add_info(GType type, GIBaseInfo *info);
SCM       gi_gtype_define_wrapper(GType gtype, GIBaseInfo *info, SCM fo_type);
SCM       gi_gtype_get_scheme_type(GType type);
void      gi_gtype_set_scheme_type_x(GType type, SCM value);
SCM       scm_gtype_get_scheme_type(SCM self);
SCM       scm_gtype_set_scheme_type_x(SCM self, SCM value);

void      gi_init_gtype (void);

G_END_DECLS

#endif
