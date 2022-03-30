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
#include "../core.h"
#include <glib.h>
#include <glib-object.h>

typedef enum GigTransfer_
{
    GIG_TRANSFER_NOTHING,
    GIG_TRANSFER_CONTAINER,
    GIG_TRANSFER_EVERYTHING
} GigTransfer;

typedef void *(*GigTypeRefFunction)(void *);
typedef void (*GigTypeUnrefFunction)(void *);

GIG_API extern SCM gig_il_type_func;
GIG_API extern SCM gig_il_untyped_flags_func;
GIG_API extern SCM gig_il_untyped_enum_func;

bool gig_type_is_registered(GType gtype);

SCM gig_type_define(GType gtype);
SCM gig_il_type(SCM s_name, SCM s_gtype_name, SCM boxed_size);
SCM gig_il_untyped_flags(SCM s_name, SCM s_qname, SCM alist);
SCM gig_il_untyped_enum(SCM s_name, SCM s_qname, SCM alist);

SCM gig_type_get_scheme_type(GType gtype);
void *gig_type_peek_object(SCM obj);
bool gig_type_check_typed_object(SCM obj, SCM expected_type);
SCM gig_type_transfer_object(GType gtype, void *obj, GigTransfer transfer);
SCM scm_from_gtype(GType x);
GType scm_to_gtype(SCM x);
GType scm_to_gtype_full(SCM x, const char *subr, int argpos);
char *gig_type_class_name_from_gtype(GType gtype);

void gig_init_type_stage1(void);
void gig_init_type_stage2(void);

#endif
