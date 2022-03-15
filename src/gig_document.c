// Copyright (C) 2018, 2019, 2022 Michael L. Gran

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
#include <girepository.h>
#include "type.h"
#include "func.h"
#include "gig_repository.h"

static void do_document(GIBaseInfo *info, const char *parent);
GIG_API void gig_init_document(void);


static void
document_nested(GIBaseInfo *parent)
{
#define DOCUMENT_NESTED(N, I)                           \
    do {                                                \
        for (int i = 0; i < N; i++)                    \
            do_document(I(parent, i), _namespace);      \
    } while (0)

    char *_namespace = make_scm_name(g_base_info_get_name(parent));
    scm_dynwind_free(_namespace);

    int n_methods, n_properties, n_signals;
    GigRepositoryNested method, property, nested_signal;

    gig_repository_nested_infos(parent, &n_methods, &method, &n_properties, &property,
                                &n_signals, &nested_signal);

    DOCUMENT_NESTED(n_methods, method);
    DOCUMENT_NESTED(n_properties, property);
    DOCUMENT_NESTED(n_signals, nested_signal);

#undef DOCUMENT_NESTED
}

static void
document_arg_entry(const char *kind, GigArgMapEntry *entry)
{
    char *name = make_scm_name(entry->name);
    scm_dynwind_free(name);
    scm_printf(SCM_UNDEFINED, "<%s name=\"%s\" c:name=\"%s\">", kind, name, entry->name);
    scm_printf(SCM_UNDEFINED, "</%s>", kind);
}

static void
do_document(GIBaseInfo *info, const char *_namespace)
{
#define FUNC "%document"
    char *scheme_name;
    const char *kind;
    GigArgMap *arg_map;
    int in, out, req, opt;
    GIInfoType type = g_base_info_get_type(info);

    scm_dynwind_begin(0);
    switch (type) {
    case GI_INFO_TYPE_FUNCTION:
        if (g_callable_info_is_method(info))
            kind = "method";
        else
            kind = "function";

        arg_map = gig_amap_new(NULL, info);
        if (arg_map)
            scm_dynwind_unwind_handler((scm_t_pointer_finalizer) gig_amap_free,
                                       arg_map, SCM_F_WIND_EXPLICITLY);
        else
            break;


        scm_printf(SCM_UNDEFINED, "<%s name=\"%s\">", kind, g_base_info_get_name(info));
        scheme_name = scm_dynfree(gig_callable_info_make_name(info, NULL));

        if (g_callable_info_is_method(info))
            scm_printf(SCM_UNDEFINED, "<scheme><procedure name=\"%s\" long-name=\"%s:%s\">",
                       scheme_name, _namespace, scheme_name);
        else if (_namespace != NULL)
            scm_printf(SCM_UNDEFINED, "<scheme><procedure name=\"%s:%s\">", _namespace,
                       scheme_name);
        else
            scm_printf(SCM_UNDEFINED, "<scheme><procedure name=\"%s\">", scheme_name);

        gig_amap_c_count(arg_map, &in, &out);
        gig_amap_s_input_count(arg_map, &req, &opt);

        if (g_callable_info_is_method(info)) {
            scm_printf(SCM_UNDEFINED, "<argument name=\"self\">");
            scm_printf(SCM_UNDEFINED, "</argument>");
        }
        for (int i = 0; i < req + opt; i++) {
            GigArgMapEntry *entry = gig_amap_get_input_entry_by_s(arg_map, i);
            document_arg_entry("argument", entry);
        }
        if (arg_map->return_val.meta.gtype != G_TYPE_NONE)
            document_arg_entry("return", &arg_map->return_val);

        for (int i = 0; i < out; i++) {
            GigArgMapEntry *entry = gig_amap_get_output_entry_by_c(arg_map, i);
            document_arg_entry("return", entry);
        }
        scm_printf(SCM_UNDEFINED, "</procedure></scheme>");

        scm_printf(SCM_UNDEFINED, "<parameters>");
        if (g_callable_info_is_method(info))
            scm_printf(SCM_UNDEFINED, "<instance-parameter name=\"self\">"      // name will be dropped
                       "<inferred argument=\"self\" />" "</instance-parameter>");

        for (int i = 0; i < arg_map->len; i++) {
            GigArgMapEntry *entry = arg_map->pdata + i;
            scm_printf(SCM_UNDEFINED, "<parameter name=\"%s\">", entry->name);
            if (entry->parent) {
                char *parent = make_scm_name(entry->parent->name);
                scm_printf(SCM_UNDEFINED, "<inferred parent=\"%s\"/>", parent);
                free(parent);
            }
            else {
                char *arg = make_scm_name(entry->name);
                scm_printf(SCM_UNDEFINED, "<inferred argument=\"%s\"/>", arg);
                free(arg);
            }
            scm_printf(SCM_UNDEFINED, "</parameter>");
        }
        scm_printf(SCM_UNDEFINED, "</parameters>");
        scm_printf(SCM_UNDEFINED, "</%s>", kind);

        break;

    case GI_INFO_TYPE_STRUCT:
    case GI_INFO_TYPE_OBJECT:
    case GI_INFO_TYPE_INTERFACE:
    case GI_INFO_TYPE_UNION:
    {
        GType gtype = g_registered_type_info_get_g_type(info);
        if (gtype == G_TYPE_NONE)
            break;

        switch (type) {
        case GI_INFO_TYPE_STRUCT:
            kind = "record";
            break;
        case GI_INFO_TYPE_UNION:
            kind = "union";
            break;
        case GI_INFO_TYPE_INTERFACE:
            kind = "interface";
            break;
        case GI_INFO_TYPE_OBJECT:
            kind = "class";
            break;
        default:
            assert_not_reached();
        }

        scm_printf(SCM_UNDEFINED, "<%s name=\"%s\">", kind, g_base_info_get_name(info));
        scm_printf(SCM_UNDEFINED, "<scheme><type name=\"&lt;%s&gt;\" /></scheme>",
                   g_type_name(gtype));

        document_nested(info);

        scm_printf(SCM_UNDEFINED, "</%s>", kind);
        break;
    }

    case GI_INFO_TYPE_ENUM:
    case GI_INFO_TYPE_FLAGS:
    {
        if (type == GI_INFO_TYPE_FLAGS)
            kind = "bitfield";
        else
            kind = "enumeration";

        scm_printf(SCM_UNDEFINED, "<%s name=\"%s\">", kind, g_base_info_get_name(info));

        GType gtype = g_registered_type_info_get_g_type(info);

        scm_printf(SCM_UNDEFINED, "<scheme>");

        if (gtype != G_TYPE_NONE)
            scm_printf(SCM_UNDEFINED, "<type name=\"&lt;%s&gt;\" />", g_type_name(gtype));
        else
            scm_printf(SCM_UNDEFINED, "<type name=\"&lt;%%%s%s&gt;\" />",
                       g_base_info_get_namespace(info), g_base_info_get_name(info));

        scm_printf(SCM_UNDEFINED, "</scheme>");

        document_nested(info);

        int n_values = g_enum_info_get_n_values(info);
        for (int i = 0; i < n_values; i++)
            do_document(g_enum_info_get_value(info, i), g_base_info_get_name(info));

        scm_printf(SCM_UNDEFINED, "</%s>", kind);
    }
        break;

    case GI_INFO_TYPE_SIGNAL:
        scm_printf(SCM_UNDEFINED, "<signal name=\"%s\" />", g_base_info_get_name(info));
        break;
    case GI_INFO_TYPE_VFUNC:
    case GI_INFO_TYPE_PROPERTY:
    {
        const char *name = g_base_info_get_name(info);
        GParamFlags flags = g_property_info_get_flags(info);
        scm_printf(SCM_UNDEFINED, "<property name=\"%s\"><scheme>"
                   "<accessor name=\"%s\" long-name=\"%s:%s\" readable=\"%d\" writable=\"%d\"/>"
                   "</scheme></property>", name, name, _namespace, name,
                   ((flags & G_PARAM_READABLE) != 0), ((flags & G_PARAM_WRITABLE) != 0));
        break;
    }
    case GI_INFO_TYPE_VALUE:
    {
        scheme_name = scm_dynfree(make_scm_name(g_base_info_get_name(info)));
        scm_printf(SCM_UNDEFINED, "<member name=\"%s\">", g_base_info_get_name(info));
        scm_printf(SCM_UNDEFINED, "<scheme><symbol name=\"%s\"", scheme_name);
        GIAttributeIter iter = { 0, };
        char *name, *value;
        while (g_base_info_iterate_attributes(info, &iter, &name, &value))
            scm_printf(SCM_UNDEFINED, "%s=\"%s\"", name, value);
        scm_printf(SCM_UNDEFINED, "/></scheme></member>");
    }
        break;
    case GI_INFO_TYPE_FIELD:
    case GI_INFO_TYPE_CONSTANT:
    case GI_INFO_TYPE_CALLBACK:
    case GI_INFO_TYPE_ARG:
    case GI_INFO_TYPE_TYPE:
        break;
    case GI_INFO_TYPE_INVALID:
    case GI_INFO_TYPE_INVALID_0:
    default:
        gig_critical("Unsupported irepository type %d", type);
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

GIG_API void
gig_init_document()
{
    scm_c_define_gsubr("%document", 1, 0, 0, _document);
}
