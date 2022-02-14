// Copyright (C) 2019, 2020, 2021 Michael L. Gran

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

#include "core.h"
#include <girepository.h>
#include <glib.h>
#include <libguile.h>
#include <stdbool.h>

// *INDENT-OFF*
G_BEGIN_DECLS
// *INDENT-ON*

G_GNUC_MALLOC char *gig_callable_info_make_name(GICallableInfo *info, const char *prefix);
SCM scm_class_ref(SCM cls, SCM slot);
SCM scm_class_set_x(SCM cls, SCM slot, SCM val);
SCM scm_drop_right_1(SCM lst);
SCM scm_c_reexport(const char *name, ...);
const char *g_base_info_get_name_safe(GIBaseInfo *info);
char *g_registered_type_info_get_qualified_name(GIRegisteredTypeInfo *info);

#define gig_debug_internal(level, domain, ...)                                                    \
    do                                                                                            \
    {                                                                                             \
        log_structured(G_LOG_DOMAIN, level, "CODE_FILE", __FILE__, "CODE_LINE",                   \
                       G_STRINGIFY(__LINE__), "CODE_FUNC", __func__, "GIG_DOMAIN", domain,        \
                       "MESSAGE", __VA_ARGS__);                                                   \
    } while (false)
#define gig_debug_transfer(...) gig_debug_internal(LOG_LEVEL_DEBUG, "transfers", __VA_ARGS__)
#define gig_debug_load(...) gig_debug_internal(LOG_LEVEL_DEBUG, "load", __VA_ARGS__)
#define gig_warning_load(...) gig_debug_internal(LOG_LEVEL_WARNING, "load", __VA_ARGS__)
#define gig_critical_load(...) gig_debug_internal(LOG_LEVEL_CRITICAL, "load", __VA_ARGS__)
#define gig_debug(...) gig_debug_internal(LOG_LEVEL_DEBUG, "general", __VA_ARGS__)
#define gig_warning(...) gig_debug_internal(LOG_LEVEL_WARNING, "general", __VA_ARGS__)
#define gig_critical(...) gig_debug_internal(LOG_LEVEL_CRITICAL, "general", __VA_ARGS__)
#define gig_error(...) gig_debug_internal(LOG_LEVEL_ERROR, "general", __VA_ARGS__)
#define gig_warn_if_reached() gig_warning("unexpected condition reached")
#define gig_return_val_if_reached(val)                                                            \
    do                                                                                            \
    {                                                                                             \
        gig_warning("unexpected return condition reached");                                       \
        return val;                                                                               \
    } while (0)
#define gig_return_val_if_fail(test, val)                                                         \
    do                                                                                            \
    {                                                                                             \
        if (!(test))                                                                              \
        {                                                                                         \
            gig_warning("unexpected return val condition");                                       \
            return val;                                                                           \
        }                                                                                         \
    } while (0)
#if (SCM_MAJOR_VERSION == 2) ||                                                                   \
    (SCM_MAJOR_VERSION == 3 && SCM_MINOR_VERSION == 0 && SCM_MICRO_VERSION < 4)
#define scm_c_bitvector_count(x) scm_to_size_t(scm_bit_count(SCM_BOOL_T, (x)))
#endif

G_END_DECLS
#endif
