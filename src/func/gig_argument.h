// Copyright (C) 2021, 2022 Michael L. Gran

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

#ifndef GIG_ARGUMENT_H
#define GIG_ARGUMENT_H

#include <girepository.h>
#include <libguile.h>
#include "../core.h"
#include "gig_arg_map.h"

#define S2C_ARG_DECL const char *subr, int argpos,    \
        GigTypeMeta *meta, SCM object,               \
        slist_t **must_free, GIArgument *arg, size_t *size
#define S2C_ARGS subr, argpos, meta, object, must_free, arg, size

#define C2S_ARG_DECL const char *subr, int argpos,    \
        GigTypeMeta *meta, GIArgument *arg,             \
        SCM *object, size_t size
#define C2S_ARGS subr, argpos, meta, arg, object, size

void gig_argument_scm_to_c(S2C_ARG_DECL);
void gig_argument_c_to_scm(C2S_ARG_DECL);
char *gig_argument_describe_arg(GIArgInfo *arg_info);
char *gig_argument_describe_return(GITypeInfo *type_info, GITransfer transfer, bool null_ok,
                                   bool skip);

#endif
