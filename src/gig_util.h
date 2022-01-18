// Copyright (C) 2019, 2020, 2021, 2022 Michael L. Gran

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

#ifndef GIG_UTIL_H
#define GIG_UTIL_H

#include <libguile.h>
#include <girepository.h>
#include "gig_types.h"

#define MALLOC __attribute__((malloc))

size_t strvlen(const char **x);

MALLOC char *gig_callable_info_make_name(GICallableInfo *info, const char *prefix);
const char *gig_constant_strip_prefix(const char *name, const char *strip_prefix);
char *gig_gname_to_scm_name(const char *gname);
SCM scm_c_list_ref(SCM list, size_t k);
intbool_t scm_is_list(SCM obj);
size_t scm_c_length(SCM list);
void *scm_dynwind_or_bust(const char *subr, void *mem);
SCM scm_class_ref(SCM cls, SCM slot);
SCM scm_class_set_x(SCM cls, SCM slot, SCM val);
SCM scm_drop_right_1(SCM lst);
SCM scm_c_reexport(const char *name, ...);
void scm_printf(SCM port, const char *fmt, ...);
const char *g_base_info_get_name_safe(GIBaseInfo *info);
char *g_registered_type_info_get_qualified_name(GIRegisteredTypeInfo *info);
char *scm_write_to_utf8_stringn(SCM x, size_t max_len);

#define scm_is_equal(a,b) scm_is_true(scm_equal_p(a,b))

#define SCM_UNBND_TO_BOOL_F(obj) \
    do {                         \
        if (SCM_UNBNDP (obj))    \
            obj = SCM_BOOL_F;    \
    } while (0)                  \

#if (SCM_MAJOR_VERSION == 2) || (SCM_MAJOR_VERSION == 3 && SCM_MINOR_VERSION == 0 && SCM_MICRO_VERSION < 4)
#define scm_c_bitvector_count(x) scm_to_size_t(scm_bit_count(SCM_BOOL_T, (x)))
#endif
#endif
