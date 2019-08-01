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

#include <libguile.h>
#include <glib.h>
#include <glib-object.h>
#include <girepository.h>
#include "gig_type.h"
#include "gig_util.h"
#include "gi_callable_info.h"
#include "gig_arg_map.h"

static void
do_document(GIBaseInfo *info, GIBaseInfo *parent)
{
#define FUNC "%document"
    gchar *scheme_name;
    GigArgMap *arg_map;
    gint in, out, req, opt;

    scm_dynwind_begin(0);
    switch (g_base_info_get_type(info)) {
    case GI_INFO_TYPE_FUNCTION:
        scm_printf(SCM_UNDEFINED, "<function name=\"%s\">", g_base_info_get_name(info));
        scheme_name = scm_dynwind_or_bust(FUNC, gi_callable_info_make_name(info, NULL));
        scm_printf(SCM_UNDEFINED, "<scheme><procedure name=\"%s\">", scheme_name);

        arg_map = gig_arg_map_new(info);
        scm_dynwind_unwind_handler((scm_t_pointer_finalizer)gig_arg_map_free,
                                   arg_map, SCM_F_WIND_EXPLICITLY);
        gig_arg_map_get_cinvoke_args_count(arg_map, &in, &out);
        gig_arg_map_get_gsubr_args_count(arg_map, &req, &opt);

        if (g_callable_info_is_method(info)) {
            scm_printf(SCM_UNDEFINED, "<argument name=\"self\">");
            scm_printf(SCM_UNDEFINED, "</argument>");
        }

        for (gint i = 0; i < req + opt; i++) {
            GigArgMapEntry *entry = gig_arg_map_get_entry(arg_map, i);
            scm_printf(SCM_UNDEFINED, "<argument name=\"%s\">", entry->name);
            scm_printf(SCM_UNDEFINED, "</argument>");
        }
        for (gint i = 0; i < out; i++) {
            GigArgMapEntry *entry = gig_arg_map_get_output_entry(arg_map, i);
            scm_printf(SCM_UNDEFINED, "<return name=\"%s\">", entry->name);
            scm_printf(SCM_UNDEFINED, "</return>");
        }
        scm_printf(SCM_UNDEFINED, "</procedure></scheme></function>");


    case GI_INFO_TYPE_STRUCT:
    case GI_INFO_TYPE_ENUM:
    case GI_INFO_TYPE_OBJECT:
    case GI_INFO_TYPE_INTERFACE:
    case GI_INFO_TYPE_CONSTANT:
    case GI_INFO_TYPE_UNION:

    case GI_INFO_TYPE_CALLBACK:
    case GI_INFO_TYPE_VALUE:
    case GI_INFO_TYPE_SIGNAL:
    case GI_INFO_TYPE_VFUNC:
    case GI_INFO_TYPE_PROPERTY:
    case GI_INFO_TYPE_FIELD:
    case GI_INFO_TYPE_ARG:
    case GI_INFO_TYPE_TYPE:
        break;
    case GI_INFO_TYPE_INVALID:
    case GI_INFO_TYPE_INVALID_0:
    default:
        g_critical("Unsupported irepository type %d", g_base_info_get_type(info));
        break;
    }
    scm_dynwind_end();
#undef FUNC
}

static SCM
_document(SCM info)
{
    GIBaseInfo *real_info = (GIBaseInfo *)gig_type_peek_object(info);
    do_document(real_info, NULL);
    return SCM_UNSPECIFIED;
}

void
gig_init_document()
{
    scm_c_define_gsubr("%document", 1, 0, 0, _document);
}
