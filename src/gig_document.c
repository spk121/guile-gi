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
#include "func/gig_arg_map_priv.h"

static void do_document(GIBaseInfo *info, const char *parent);
GIG_API void gig_init_document(void);


// The field accessors that src/func/gig_field.c defines.
static void
document_fields(GIBaseInfo *container, const char *prefix)
{
    GIInfoType type = g_base_info_get_type(container);
    int n;
    GIFieldInfo *(*get)(GIBaseInfo *, int);
    switch (type) {
    case GI_INFO_TYPE_STRUCT:
    case GI_INFO_TYPE_BOXED:
        n = g_struct_info_get_n_fields(container);
        get = (GIFieldInfo *(*)(GIBaseInfo *, int))g_struct_info_get_field;
        break;
    case GI_INFO_TYPE_UNION:
        n = g_union_info_get_n_fields(container);
        get = (GIFieldInfo *(*)(GIBaseInfo *, int))g_union_info_get_field;
        break;
    case GI_INFO_TYPE_OBJECT:
        n = g_object_info_get_n_fields(container);
        get = (GIFieldInfo *(*)(GIBaseInfo *, int))g_object_info_get_field;
        break;
    default:
        return;
    }
    GType gtype = g_registered_type_info_get_g_type(container);
    if (gtype == G_TYPE_NONE || gtype == G_TYPE_INVALID || !gig_fields_are_reliable(container))
        return;
    for (int i = 0; i < n; i++) {
        GIFieldInfo *fi = get(container, i);
        if (gig_field_is_readable(fi) && !gig_field_is_shadowed(container, fi)) {
            char *name = gig_field_make_name(fi);
            scm_printf(SCM_UNDEFINED, "<field name=\"%s\"><scheme>"
                       "<accessor name=\"%s\" long-name=\"%s:%s\" readable=\"1\" writable=\"%d\"/>"
                       "</scheme></field>", g_base_info_get_name(fi), name, prefix, name,
                       gig_field_is_writable(fi));
            free(name);
        }
        g_base_info_unref(fi);
    }
}

static void
document_nested(GIBaseInfo *parent)
{
#define DOCUMENT_NESTED(N, I)                           \
    do {                                                \
        for (int i = 0; i < N; i++) {                   \
            GIBaseInfo *nested = I(parent, i);          \
            do_document(nested, _namespace);            \
            g_base_info_unref(nested);                  \
        }                                               \
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

    GIInfoType type = g_base_info_get_type(parent);
    document_fields(parent, _namespace);
    if (type == GI_INFO_TYPE_OBJECT)
        DOCUMENT_NESTED(g_object_info_get_n_vfuncs(parent), g_object_info_get_vfunc);
    else if (type == GI_INFO_TYPE_INTERFACE)
        DOCUMENT_NESTED(g_interface_info_get_n_vfuncs(parent), g_interface_info_get_vfunc);

#undef DOCUMENT_NESTED
}

static void
document_arg_entry(const char *kind, GigArgMapEntry *entry, bool optional)
{
    char *name = make_scm_name(entry->name);
    scm_dynwind_free(name);
    scm_printf(SCM_UNDEFINED, "<%s name=\"%s\" c:name=\"%s\"%s>", kind, name, entry->name,
               optional ? " optional=\"1\"" : "");
    scm_printf(SCM_UNDEFINED, "</%s>", kind);
}

// One procedure that src/type/gig_foreign_struct.c defines: its
// arguments, and its return value if RET is not NULL.
static void
document_foreign_procedure(const char *name, const char **args, size_t n_args,
                           size_t n_required, const char *ret)
{
    scm_printf(SCM_UNDEFINED, "<function name=\"%s\"><scheme><procedure name=\"%s\">",
               name, name);
    for (size_t i = 0; i < n_args; i++) {
        char *arg = make_scm_name(args[i]);
        scm_printf(SCM_UNDEFINED, "<argument name=\"%s\" c:name=\"%s\"%s></argument>", arg,
                   args[i], i >= n_required ? " optional=\"1\"" : "");
        free(arg);
    }
    if (ret != NULL) {
        char *r = make_scm_name(ret);
        scm_printf(SCM_UNDEFINED, "<return name=\"%s\" c:name=\"%s\"></return>", r, ret);
        free(r);
    }
    scm_printf(SCM_UNDEFINED, "</procedure></scheme></function>");
}

// A plain struct without a GType that guile-gi wraps as a foreign
// object: its class, constructor, field accessors and methods.
static void
document_foreign_struct(GIBaseInfo *info, const GigForeignStruct *fs)
{
    char *prefix = make_scm_name(gig_foreign_struct_name(fs));
    char *name;
    size_t n = gig_foreign_struct_n_fields(fs);
    const char *self = "self";

    scm_printf(SCM_UNDEFINED, "<record name=\"%s\">", g_base_info_get_name(info));
    scm_printf(SCM_UNDEFINED, "<scheme><type name=\"&lt;%s&gt;\" /></scheme>",
               gig_foreign_struct_c_name(fs));

    if (gig_foreign_struct_is_opaque(fs)) {
        const char *ptr = "pointer";
        name = concatenate("pointer->", prefix);
        document_foreign_procedure(name, &ptr, 1, 1, "struct");
        free(name);
    }
    else {
        const char **fields = xcalloc(n, sizeof(char *));
        for (size_t i = 0; i < n; i++)
            fields[i] = gig_foreign_struct_field_name(fs, i);
        name = concatenate("make-", prefix);
        document_foreign_procedure(name, fields, n, 0, "struct");
        free(name);

        for (size_t i = 0; i < n; i++) {
            char *field = make_scm_name(fields[i]);
            const char *setter_args[2] = { self, fields[i] };
            name = concatenate3(prefix, ":", field);
            document_foreign_procedure(name, &self, 1, 1, fields[i]);
            free(name);
            name = concatenate4(prefix, ":set-", field, "!");
            document_foreign_procedure(name, setter_args, 2, 2, NULL);
            free(name);
            free(field);
        }
        free(fields);
    }
    free(prefix);

    document_nested(info);
    scm_printf(SCM_UNDEFINED, "</record>");
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
    case GI_INFO_TYPE_VFUNC:
    {
        // A virtual function's procedure takes the class whose
        // implementation to call, then the instance.
        bool is_vfunc = (type == GI_INFO_TYPE_VFUNC);
        if (is_vfunc)
            kind = "virtual-method";
        else if (g_callable_info_is_method(info))
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

        if (is_vfunc)
            scm_printf(SCM_UNDEFINED, "<scheme><procedure name=\"%s:do-%s\">",
                       _namespace, scm_dynfree(make_scm_name(g_base_info_get_name(info))));
        else if (g_callable_info_is_method(info) && scm_is_core_syntax_name(scheme_name))
            // It has no shorthand, which would replace Guile's syntax.
            scm_printf(SCM_UNDEFINED, "<scheme><procedure name=\"%s:%s\">", _namespace,
                       scheme_name);
        else if (g_callable_info_is_method(info))
            scm_printf(SCM_UNDEFINED, "<scheme><procedure name=\"%s\" long-name=\"%s:%s\">",
                       scheme_name, _namespace, scheme_name);
        else if (_namespace != NULL)
            scm_printf(SCM_UNDEFINED, "<scheme><procedure name=\"%s:%s\">", _namespace,
                       scheme_name);
        else
            scm_printf(SCM_UNDEFINED, "<scheme><procedure name=\"%s\">", scheme_name);

        gig_amap_c_count(arg_map, &in, &out);
        gig_amap_s_input_count(arg_map, &req, &opt);

        if (is_vfunc)
            scm_printf(SCM_UNDEFINED, "<argument name=\"implementor\"></argument>");
        if (g_callable_info_is_method(info)) {
            scm_printf(SCM_UNDEFINED, "<argument name=\"self\">");
            scm_printf(SCM_UNDEFINED, "</argument>");
        }
        // These must describe the procedure that src/func builds from
        // the same argument map: the Scheme inputs in order, of which
        // the trailing OPT may be omitted...
        for (int i = 0; i < req + opt; i++) {
            GigArgMapEntry *entry = gig_amap_get_input_entry_by_s(arg_map, i);
            document_arg_entry("argument", entry, i >= req);
        }
        // ...and its values: the return value, unless void or skipped,
        // then each C output that is returned to Scheme, as
        // convert_output_args decides.  Outputs that only give an array's
        // length, and skipped outputs, are not.
        if (gig_amap_return_is_returned(arg_map))
            document_arg_entry("return", &arg_map->return_val, false);

        for (int i = 0; i < out; i++) {
            if (!gig_amap_output_is_returned(arg_map, i))
                continue;
            GigArgMapEntry *entry = gig_amap_get_output_entry_by_c(arg_map, i);
            document_arg_entry("return", entry, false);
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
    }

    case GI_INFO_TYPE_STRUCT:
    case GI_INFO_TYPE_BOXED:
    case GI_INFO_TYPE_OBJECT:
    case GI_INFO_TYPE_INTERFACE:
    case GI_INFO_TYPE_UNION:
    {
        GType gtype = g_registered_type_info_get_g_type(info);
        if (gtype == G_TYPE_NONE) {
            const GigForeignStruct *fs = gig_foreign_struct_find(info);
            if (fs != NULL)
                document_foreign_struct(info, fs);
            break;
        }

        switch (type) {
        case GI_INFO_TYPE_STRUCT:
        case GI_INFO_TYPE_BOXED:
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
        for (int i = 0; i < n_values; i++) {
            GIValueInfo *value = g_enum_info_get_value(info, i);
            do_document(value, g_base_info_get_name(info));
            g_base_info_unref(value);
        }

        scm_printf(SCM_UNDEFINED, "</%s>", kind);
    }
        break;

    case GI_INFO_TYPE_SIGNAL:
        scm_printf(SCM_UNDEFINED, "<signal name=\"%s\" />", g_base_info_get_name(info));
        break;
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
        while (g_base_info_iterate_attributes(info, &iter, &name, &value)) {
            // Attribute values are arbitrary text from the typelib.
            char *_name = g_markup_escape_text(name, -1);
            char *_value = g_markup_escape_text(value, -1);
            scm_printf(SCM_UNDEFINED, " %s=\"%s\"", _name, _value);
            g_free(_name);
            g_free(_value);
        }
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
    if (real_info == NULL)
        scm_wrong_type_arg_msg("%document", SCM_ARG1, info, "GIBaseInfo");
    do_document(real_info, NULL);
    return SCM_UNSPECIFIED;
}

GIG_API void
gig_init_document()
{
    scm_c_define_gsubr("%document", 1, 0, 0, _document);
}
