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

GType gig_type_get_c_array_type(void);

#define G_TYPE_PRIV_C_ARRAY (gig_type_get_c_array_type())

typedef void *(*GigTypeRefFunction)(void *);
typedef void (*GigTypeUnrefFunction)(void *);

bool gig_type_is_registered(GType gtype);

SCM gig_type_define(GType gtype);
void gig_type_define_fundamental(GType type, SCM extra_supers,
                                 GigTypeRefFunction ref, GigTypeUnrefFunction unref);
SCM gig_type_define_with_info(GIRegisteredTypeInfo *info, SCM slots);
SCM gig_type_get_scheme_type(GType gtype);
void *gig_type_peek_object(SCM obj);
bool gig_type_check_typed_object(SCM obj, SCM expected_type);
SCM gig_type_transfer_object(GType gtype, void *obj, GITransfer transfer);
SCM scm_from_gtype(GType x);
GType scm_to_gtype(SCM x);
GType scm_to_gtype_full(SCM x, const char *subr, int argpos);

GIG_API void gig_init_types(void);

#endif
