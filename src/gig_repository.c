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

#include <string.h>
#include <inttypes.h>
#include <libguile.h>
#include <girepository.h>
#include "type.h"
#include "func.h"
#include "gig_repository.h"

static SCM get_shared_library_list(const char *lib);
static void constant_define(GIConstantInfo *info, SCM *defs, SCM *ils);
static void type_define(GType gtype, SCM *defs, SCM *ils);

static SCM il_output_port = SCM_UNDEFINED;
static SCM pretty_print_func = SCM_UNDEFINED;

static SCM
set_il_output_port(SCM s_port)
{
#define FUNC_NAME "set-il-output-port!"
    SCM_ASSERT_TYPE(scm_is_true(scm_output_port_p(s_port)), s_port,
                    SCM_ARG1, FUNC_NAME, "output port");
    il_output_port = s_port;
    return SCM_UNSPECIFIED;
#undef FUNC_NAME
}

static SCM
output_il(SCM il)
{
    if (SCM_UNBNDP(il_output_port))
        return SCM_UNSPECIFIED;

    if (!scm_is_null(il)) {
        scm_call_2(pretty_print_func, il, il_output_port);
    }
    return SCM_UNSPECIFIED;
}

static SCM
output_exports(SCM defs)
{
    if (!scm_is_null(defs)) {
        SCM lst = scm_cons(scm_from_utf8_symbol("export"), defs);
        scm_call_2(pretty_print_func, lst, il_output_port);
    }
    return SCM_UNSPECIFIED;
}

static SCM
require(SCM s_namespace, SCM s_version)
{
#define FUNC_NAME "require"
    SCM_ASSERT_TYPE(scm_is_string(s_namespace), s_namespace, SCM_ARG1, FUNC_NAME, "string");
    SCM_ASSERT_TYPE(SCM_UNBNDP(s_version) ||
                    scm_is_string(s_version), s_version, SCM_ARG2, FUNC_NAME, "string");

    char *namespace_, *version = NULL;
    GITypelib *tl;
    GError *error = NULL;

    scm_dynwind_begin(0);
    namespace_ = scm_dynfree(scm_to_utf8_string(s_namespace));
    if (!SCM_UNBNDP(s_version) && scm_is_true(s_version))
        version = scm_dynfree(scm_to_utf8_string(s_version));

    gig_debug_load("requiring %s-%s", namespace_, version != NULL ? version : "latest");
    tl = g_irepository_require(NULL, namespace_, version, 0, &error);

    if (tl == NULL) {
        SCM err = scm_from_utf8_string(error->message);
        g_error_free(error);
        scm_misc_error(FUNC_NAME, "~A", scm_list_1(err));
    }
    SCM path_list = get_shared_library_list(namespace_);
    SCM il = scm_list_3(scm_from_utf8_symbol("^library"),
                        s_namespace,
                        path_list);
    scm_apply(gig_il_library_func, scm_cdr(il), SCM_EOL);
    scm_dynwind_end();

    output_il(il);
    return SCM_UNSPECIFIED;
#undef FUNC_NAME
}

static SCM
get_shared_library_list(const char *namespace_)
{
    const char *csv_paths;
    const char *start, *end;
    SCM path_list = SCM_EOL;

    if (namespace_ == NULL)
        return SCM_EOL;
    csv_paths = g_irepository_get_shared_library(NULL, namespace_);
    start = csv_paths;
    while ((end = strchr(start, ',')) != NULL) {
        path_list = scm_cons(scm_from_locale_stringn(start, end - start), path_list);
        start = end + 1;        // Skipping over comma.
    }
    path_list = scm_cons(scm_from_locale_string(start), path_list);
    return scm_reverse(path_list);
}

static SCM
infos(SCM lib)
{
    SCM_ASSERT_TYPE(scm_is_string(lib), lib, SCM_ARG1, "infos", "string");

    scm_dynwind_begin(0);
    char *_lib = scm_dynfree(scm_to_utf8_string(lib));
    int n = g_irepository_get_n_infos(NULL, _lib);
    SCM infos = SCM_EOL;

    for (int i = 0; i < n; i++) {
        GIBaseInfo *info = g_irepository_get_info(NULL, _lib, i);
        if (g_base_info_is_deprecated(info)) {
            g_base_info_unref(info);
            continue;
        }
        infos =
            scm_cons(gig_type_transfer_object(GI_TYPE_BASE_INFO, info, GIG_TRANSFER_EVERYTHING),
                     infos);
    }
    scm_dynwind_end();

    return scm_reverse_x(infos, SCM_EOL);
}

typedef enum _LoadFlags
{
    LOAD_INFO_ONLY = 0,
    LOAD_METHODS = 1 << 0,
    LOAD_PROPERTIES = 1 << 1,
    LOAD_SIGNALS = 1 << 2,
    LOAD_EVERYTHING = LOAD_METHODS | LOAD_PROPERTIES | LOAD_SIGNALS
} LoadFlags;

void
gig_repository_nested_infos(GIBaseInfo *base,
                            int *n_methods,
                            GigRepositoryNested *method,
                            int *n_properties,
                            GigRepositoryNested *property,
                            int *n_signals, GigRepositoryNested *signal)
{
    switch (g_base_info_get_type(base)) {
    case GI_INFO_TYPE_STRUCT:
        *n_methods = g_struct_info_get_n_methods(base);
        *method = (GigRepositoryNested)g_struct_info_get_method;
        *n_properties = *n_signals = 0;
        *property = *signal = NULL;
        break;
    case GI_INFO_TYPE_UNION:
        *n_methods = g_union_info_get_n_methods(base);
        *method = (GigRepositoryNested)g_union_info_get_method;
        *n_properties = *n_signals = 0;
        *property = *signal = NULL;
        break;
    case GI_INFO_TYPE_ENUM:
    case GI_INFO_TYPE_FLAGS:
        *n_methods = g_enum_info_get_n_methods(base);
        *method = (GigRepositoryNested)g_enum_info_get_method;
        *n_properties = *n_signals = 0;
        *property = *signal = NULL;
        break;
    case GI_INFO_TYPE_INTERFACE:
        *n_methods = g_interface_info_get_n_methods(base);
        *method = (GigRepositoryNested)g_interface_info_get_method;
        *n_signals = g_interface_info_get_n_signals(base);
        *signal = (GigRepositoryNested)g_interface_info_get_signal;
        *n_properties = g_interface_info_get_n_properties(base);
        *property = (GigRepositoryNested)g_interface_info_get_property;
        {
            GType gtype = g_registered_type_info_get_g_type(base);
            const char *name = g_base_info_get_name(base);
            if (!g_type_is_a(gtype, G_TYPE_OBJECT)) {
                if (*n_properties != 0)
                    gig_warning_load("%s - non-Object interface wants properties", name);
                if (*n_signals != 0)
                    gig_warning_load("%s - non-Object interface wants signals", name);
                *n_properties = *n_signals = 0;
            }
        }
        break;
    case GI_INFO_TYPE_OBJECT:
        *n_methods = g_object_info_get_n_methods(base);
        *method = (GigRepositoryNested)g_object_info_get_method;
        *n_properties = g_object_info_get_n_properties(base);
        *property = (GigRepositoryNested)g_object_info_get_property;
        *n_signals = g_object_info_get_n_signals(base);
        *signal = (GigRepositoryNested)g_object_info_get_signal;
        break;
    default:
        *n_methods = *n_properties = *n_signals = 0;
        *method = *property = *signal = NULL;
    }
}

static char *
g_registered_type_info_get_qualified_name(GIRegisteredTypeInfo *info)
{
    const char *_name = g_base_info_get_attribute(info, "c:type");
    if (_name != NULL)
        return xstrdup(_name);

    const char *_namespace = g_base_info_get_namespace(info);
    const char *prefix = g_irepository_get_c_prefix(NULL, _namespace);

    // add initial % to ensure that the name is private
    return concatenate3("%", prefix, g_base_info_get_name(info));
}

static SCM
make_flag_enum_alist(GIRegisteredTypeInfo *info)
{
    GIInfoType t = g_base_info_get_type(info);
    int n_values = g_enum_info_get_n_values(info);
    int i = 0;
    SCM alist = SCM_EOL;

    while (i < n_values) {
        GIValueInfo *vi;
        char *_key;
        int64_t _val;
        SCM key, val;

        vi = g_enum_info_get_value(info, i);
        _key = make_scm_name(g_base_info_get_name(vi));
        key = scm_from_utf8_symbol(_key);
        _val = g_value_info_get_value(vi);

        switch (t) {
        case GI_INFO_TYPE_ENUM:
            val = scm_from_int(_val);
            break;
        case GI_INFO_TYPE_FLAGS:
            val = scm_from_uint(_val);
            break;
        default:
            assert_not_reached();
        }
        alist = scm_cons(scm_cons(key, val), alist);

        g_base_info_unref(vi);
        free(_key);
        i++;
    }
    return scm_reverse(alist);
}

/*
;; API
;; (^library "lib" "version" ("lib.so.1" "lib2.so.1"))
;; (^type <Symbol> "GType-name" (extra-supers))
;; (^constant key val)
;; (^typed-enum <Symbol> GTypeName key-val-list)
;; (^typed-flags <Symbol> GTypeName key-val-list)
;; (^enum <Symbol> qname key-val-list)
;; (^flags <Symbol> qname key-val-list)
;; (^property parent-type-name name type-name)
;; (^function ...)
;; (^method ...)
;; (^signal ...)
*/

static SCM
load_info(GIBaseInfo *info, LoadFlags flags)
{
    SCM defs = SCM_EOL;
    SCM ils = SCM_EOL;
    g_return_val_if_fail(info != NULL, defs);

    GIBaseInfo *parent = g_base_info_get_container(info);
    GType parent_gtype = G_TYPE_INVALID;
    const char *parent_name = NULL;
    if (parent) {
        parent_gtype = g_registered_type_info_get_g_type(parent);
        parent_name = g_base_info_get_name(parent);
    }

    GIInfoType t = g_base_info_get_type(info);
    switch (t) {
    case GI_INFO_TYPE_CALLBACK:
        gig_debug_load("%s - not loading %s type because callbacks are not supported",
                       g_base_info_get_name(info), g_info_type_to_string(t));
        break;
    case GI_INFO_TYPE_FUNCTION:
    case GI_INFO_TYPE_SIGNAL:
    {
        GType *types = NULL;
        size_t len = 0;
        bool args_ok = true;
        char *long_name;
        const char *namespace_ = g_base_info_get_namespace(info);
        GigArgMap *amap;
        if (parent_name)
            long_name = gig_callable_info_make_name(info, parent_name);
        else
            long_name = gig_callable_info_make_name(info, namespace_);
        amap = gig_amap_new(long_name, info);
        if (amap)
            types = gig_amap_get_gtype_list(amap, &len);
        else
            args_ok = false;

        // Pre-load all the types used by the function's arguments.
        for (size_t i = 0; i < len; i++) {
            if (!gig_type_is_registered(types[i])) {
                GIBaseInfo *typeinfo;
                typeinfo = g_irepository_find_by_gtype(NULL, types[i]);
                if (typeinfo) {
                    gig_debug_load("%s - loading prerequisite arg type %s",
                                   g_base_info_get_name(info), g_type_name(types[i]));
                    defs = scm_append2(defs, load_info(typeinfo, LOAD_INFO_ONLY));
                    g_base_info_unref(typeinfo);
                }
                else {
                    gig_debug_load
                        ("%s - not loading %s because it has an argument of type %s that has no typeinfo",
                         g_base_info_get_name(info), g_info_type_to_string(t),
                         g_type_name(types[i]));
                    args_ok = false;
                }
            }
        }
        free(types);

        if (args_ok == true) {
            char *short_name;
            const char *symbol_name;

            short_name = gig_callable_info_make_name(info, NULL);
            if (amap != NULL) {
                if (t == GI_INFO_TYPE_FUNCTION) {
                    symbol_name = g_function_info_get_symbol(info);
                    defs =
                        scm_append2(defs,
                                    gig_function_define_full(namespace_, parent_gtype, long_name,
                                                             short_name, symbol_name, amap));
                }
                else if (t == GI_INFO_TYPE_SIGNAL) {
                    symbol_name = g_base_info_get_name(info);
                    defs =
                        scm_append2(defs,
                                    gig_signal_define_full(namespace_, parent_gtype, long_name,
                                                           short_name, symbol_name, amap));
                }
            }
            free(short_name);
        }
        free(long_name);
    }
        break;
    case GI_INFO_TYPE_PROPERTY:
        defs =
            scm_append2(defs,
                        gig_property_define(g_type_name(parent_gtype), g_base_info_get_name(info),
                                            parent_name));
        break;
    case GI_INFO_TYPE_BOXED:
    {
        GType gtype = g_registered_type_info_get_g_type(info);
        if (gtype == G_TYPE_NONE) {
            gig_debug_load("%s - not loading boxed type because is has no GType",
                           g_base_info_get_name(info));
            break;
        }
        type_define(gtype, &defs, &ils);
        goto recursion;
    }
    case GI_INFO_TYPE_STRUCT:
    case GI_INFO_TYPE_UNION:
    case GI_INFO_TYPE_INTERFACE:
    case GI_INFO_TYPE_OBJECT:
    {
        GType gtype = g_registered_type_info_get_g_type(info);
        if (gtype == G_TYPE_NONE) {
            gig_debug_load("%s - not loading struct type because is has no GType",
                           g_base_info_get_name(info));
            gig_debug_load("%s - not loading %s type because is has no GType",
                           g_base_info_get_name(info), g_info_type_to_string(t));
            break;
        }
        if (gtype == G_TYPE_INVALID) {
            gig_debug_load("%s - not loading %s type because its GType is invalid",
                           g_base_info_get_name(info), g_info_type_to_string(t));
            break;
        }
        GType par_gtype = g_type_parent(gtype);
        bool parent_ok = true;
        if (par_gtype && !gig_type_is_registered(par_gtype)) {
            GIBaseInfo *typeinfo;
            typeinfo = g_irepository_find_by_gtype(NULL, par_gtype);
            if (typeinfo) {
                gig_debug_load("%s - loading prerequisite type %s",
                               g_base_info_get_name(info), g_type_name(par_gtype));
                defs = scm_append2(defs, load_info(typeinfo, LOAD_INFO_ONLY));
                g_base_info_unref(typeinfo);
            }
            else {
                gig_debug_load
                    ("%s - not loading %s type because its parent %s has no introspection information",
                     g_base_info_get_name(info), g_info_type_to_string(t), g_type_name(par_gtype));
                parent_ok = false;
                break;
            }
        }
        if (parent_ok) {
            GType *interfaces = NULL;
            unsigned n_interfaces = 0;
            bool interface_ok = true;

            if (t == GI_INFO_TYPE_OBJECT)
                interfaces = g_type_interfaces(gtype, &n_interfaces);
            else if (t == GI_INFO_TYPE_INTERFACE)
                interfaces = g_type_interface_prerequisites(gtype, &n_interfaces);

            for (unsigned n = 0; n < n_interfaces; n++) {
                if (interfaces[n] && !gig_type_is_registered(interfaces[n])) {
                    GIBaseInfo *typeinfo;
                    typeinfo = g_irepository_find_by_gtype(NULL, interfaces[n]);
                    if (typeinfo) {
                        gig_debug_load("%s - loading prerequisite interface type %s",
                                       g_base_info_get_name(info), g_type_name(interfaces[n]));
                        defs = scm_append2(defs, load_info(typeinfo, LOAD_INFO_ONLY));
                        g_base_info_unref(typeinfo);
                    }
                    else {
                        gig_debug_load
                            ("%s - not loading %s type because its interface %s has no introspection information",
                             g_base_info_get_name(info), g_info_type_to_string(t),
                             g_type_name(interfaces[n]));
                        interface_ok = false;
                        break;
                    }
                }
            }
            free(interfaces);
            if (interface_ok)
                type_define(gtype, &defs, &ils);
        }
        goto recursion;
    }
    case GI_INFO_TYPE_ENUM:
    case GI_INFO_TYPE_FLAGS:
    {
        GType gtype = g_registered_type_info_get_g_type(info);
        if (gtype == G_TYPE_NONE)
            defs = scm_append2(defs, gig_type_define_with_info(info, SCM_EOL));
        else
            type_define(gtype, &defs, &ils);

        defs = scm_append2(defs, gig_define_enum_conversions(info, gtype));
        goto recursion;
    }
    case GI_INFO_TYPE_CONSTANT:
        constant_define(info, &defs, &ils);
        break;
    case GI_INFO_TYPE_VALUE:
        gig_critical_load("Unsupported irepository type 'VALUE'");
        break;
    case GI_INFO_TYPE_VFUNC:
        gig_critical_load("Unsupported irepository type 'VFUNC'");
        break;
    case GI_INFO_TYPE_FIELD:
        gig_critical_load("Unsupported irepository type 'FIELD'");
        break;
    case GI_INFO_TYPE_ARG:
        gig_critical_load("Unsupported irepository type 'ARG'");
        break;
    case GI_INFO_TYPE_TYPE:
        gig_critical_load("Unsupported irepository type 'TYPE'");
        break;
    case GI_INFO_TYPE_INVALID:
    case GI_INFO_TYPE_INVALID_0:
    default:
        gig_critical_load("Unsupported irepository type %d '%s'", g_base_info_get_type(info),
                          g_base_info_get_name(info));
        break;
    }

  end:
    output_il(ils);
    output_exports(defs);
    return defs;

  recursion:
    {
#define LOAD_NESTED(F, N, I)                                    \
        do {                                                    \
            if (flags & F)                                      \
                for (int i = 0; i < N; i++) {                  \
                    GIBaseInfo *nested_info = I(info, i);       \
                    defs = scm_append2(defs, load_info(nested_info, flags)); \
                    g_base_info_unref(nested_info);             \
                }                                               \
        } while (0)

        int n_methods, n_properties, n_signals;
        GigRepositoryNested method, property, nested_signal;

        gig_repository_nested_infos(info, &n_methods, &method, &n_properties, &property,
                                    &n_signals, &nested_signal);

        LOAD_NESTED(LOAD_METHODS, n_methods, method);
        LOAD_NESTED(LOAD_PROPERTIES, n_properties, property);
        LOAD_NESTED(LOAD_SIGNALS, n_signals, nested_signal);
#undef LOAD_NESTED
        goto end;
    }
}

static SCM
load(SCM info, SCM flags)
{
    SCM_ASSERT_TYPE(SCM_UNBNDP(flags) ||
                    scm_is_unsigned_integer(flags, 0, LOAD_EVERYTHING), flags, SCM_ARG2, "load",
                    "integer");

    LoadFlags load_flags;
    if (SCM_UNBNDP(flags))
        load_flags = LOAD_EVERYTHING;
    else
        load_flags = scm_to_uint(flags);

    GIBaseInfo *base_info = (GIBaseInfo *)gig_type_peek_object(info);

    return load_info(base_info, load_flags);
}

static SCM
info(SCM lib, SCM name)
{
    SCM_ASSERT_TYPE(scm_is_string(lib), lib, SCM_ARG1, "info", "string");
    SCM_ASSERT_TYPE(scm_is_string(name), name, SCM_ARG2, "info", "string");

    char *_lib, *_name;
    GIBaseInfo *info;
    scm_dynwind_begin(0);
    _lib = scm_dynfree(scm_to_utf8_string(lib));
    _name = scm_dynfree(scm_to_utf8_string(name));

    info = g_irepository_find_by_name(NULL, _lib, _name);
    if (info == NULL)
        scm_misc_error("info",
                       "could not load ~A from ~A, did you forget to require or perhaps misspell?",
                       scm_list_2(name, lib));
    scm_dynwind_end();

    return gig_type_transfer_object(GI_TYPE_BASE_INFO, info, GIG_TRANSFER_EVERYTHING);
}

static SCM
get_search_path(void)
{
    GSList *slist = g_irepository_get_search_path();
    SCM entry;
    SCM output = SCM_EOL;

    if (slist == NULL)
        return SCM_EOL;
    do {
        entry = scm_from_utf8_string(slist->data);
        output = scm_cons(entry, output);
    } while ((slist = g_slist_next(slist)));
    return scm_reverse_x(output, SCM_EOL);
}

static SCM
prepend_search_path(SCM s_dir)
{
    char *dir;

    SCM_ASSERT_TYPE(scm_is_string(s_dir), s_dir, SCM_ARG1, "prepend-search-path!", "string");

    dir = scm_to_utf8_string(s_dir);
    g_irepository_prepend_search_path(dir);
    free(dir);

    return SCM_UNSPECIFIED;
}

static SCM
get_dependencies(SCM namespace)
{
    char *_namespace;
    char **_dependencies;
    int i;
    SCM dependencies = SCM_EOL;

    SCM_ASSERT_TYPE(scm_is_string(namespace), namespace, SCM_ARG1, "get-dependencies", "string");
    _namespace = scm_to_utf8_string(namespace);
    _dependencies = g_irepository_get_dependencies(NULL, _namespace);
    free(_namespace);
    if (_dependencies == NULL)
        return SCM_EOL;
    i = 0;
    while (_dependencies[i] != NULL) {
        SCM entry = scm_from_utf8_string(_dependencies[i]);
        dependencies = scm_cons(entry, dependencies);
        free(_dependencies[i]);
        i++;
    }
    free(_dependencies);
    return scm_reverse_x(dependencies, SCM_EOL);
}

void
gig_init_repository()
{
    pretty_print_func = scm_c_public_ref("ice-9 pretty-print", "pretty-print");

    scm_c_define_gsubr("require", 1, 1, 0, require);
    scm_c_define_gsubr("infos", 1, 0, 0, infos);
    scm_c_define_gsubr("info", 2, 0, 0, info);
    scm_c_define_gsubr("%load-info", 1, 1, 0, load);
    scm_c_define_gsubr("get-search-path", 0, 0, 0, get_search_path);
    scm_c_define_gsubr("prepend-search-path!", 1, 0, 0, prepend_search_path);
    scm_c_define_gsubr("get-dependencies", 1, 0, 0, get_dependencies);
    scm_c_define_gsubr("set-il-output-port", 1, 0, 0, set_il_output_port);

#define D(x) scm_permanent_object(scm_c_define(#x, scm_from_uint(x)))

    D(LOAD_INFO_ONLY);
    D(LOAD_METHODS);
    D(LOAD_PROPERTIES);
    D(LOAD_SIGNALS);
    D(LOAD_EVERYTHING);
}

static void
constant_define(GIConstantInfo *info, SCM *defs, SCM *ils)
{
    const char *name = g_base_info_get_name(info);

    GITypeInfo *typeinfo;
    typeinfo = g_constant_info_get_type(info);
    GITypeTag typetag;
    typetag = g_type_info_get_tag(typeinfo);

    GIArgument value;
    SCM s_value;

    g_constant_info_get_value(info, &value);

    switch (typetag) {
    case GI_TYPE_TAG_BOOLEAN:
        gig_debug_load("%s - boolean constant %d", name, value.v_boolean);
        s_value = scm_from_bool(value.v_boolean);
        break;
    case GI_TYPE_TAG_DOUBLE:
        gig_debug_load("%s - double constant %lf", name, value.v_double);
        s_value = scm_from_double(value.v_double);
        break;
    case GI_TYPE_TAG_INT8:
        gig_debug_load("%s - int8 constant %d", name, (int)value.v_int8);
        s_value = scm_from_int8(value.v_int8);
        break;
    case GI_TYPE_TAG_INT16:
        gig_debug_load("%s - int16 constant %d", name, (int)value.v_int16);
        s_value = scm_from_int16(value.v_int16);
        break;
    case GI_TYPE_TAG_INT32:
        gig_debug_load("%s - int32 constant %d", name, (int)value.v_int32);
        s_value = scm_from_int32(value.v_int32);
        break;
    case GI_TYPE_TAG_INT64:
        gig_debug_load("%s - int64 constant %" PRId64, name, value.v_int64);
        s_value = scm_from_int64(value.v_int64);
        break;
    case GI_TYPE_TAG_UINT8:
        gig_debug_load("%s - uint8 constant %d", name, (int)value.v_uint8);
        s_value = scm_from_uint8(value.v_uint8);
        break;
    case GI_TYPE_TAG_UINT16:
        gig_debug_load("%s - uint16 constant %d", name, (int)value.v_uint16);
        s_value = scm_from_uint16(value.v_uint16);
        break;
    case GI_TYPE_TAG_UINT32:
        gig_debug_load("%s - uint32 constant %d", name, (int)value.v_uint32);
        s_value = scm_from_uint32(value.v_uint32);
        break;
    case GI_TYPE_TAG_UINT64:
        gig_debug_load("%s - uint64 constant %" PRIu64, name, value.v_uint64);
        s_value = scm_from_uint64(value.v_uint64);
        break;
    case GI_TYPE_TAG_UTF8:
        gig_debug_load("%s - UTF8 constant %s", name, value.v_string);
        s_value = scm_from_utf8_string(value.v_string);
        break;
    default:
        gig_critical_load("%s - unsupported constant type '%s'", name,
                          g_type_tag_to_string(typetag));
        s_value = SCM_BOOL_F;
    }
    g_constant_info_free_value(info, &value);
    g_base_info_unref(typeinfo);

    SCM s_name = scm_from_utf8_symbol(name);
    SCM il = scm_list_3(scm_from_utf8_symbol("^constant"),
                        s_name, s_value);
    SCM def = scm_apply(gig_il_constant_func, scm_cdr(il), SCM_EOL);
    *defs = scm_append2(*defs, def);
    *ils = scm_append2(*ils, il);
}

static void
type_define(GType gtype, SCM *defs, SCM *ils)
{
    char *type_class_name = gig_type_class_name_from_gtype(gtype);
    SCM s_type_class_name = scm_from_utf8_symbol(type_class_name);
    free(type_class_name);
    SCM s_gtype_name = scm_from_utf8_string(g_type_name(gtype));
    SCM il = scm_list_3(scm_from_utf8_symbol("^type"),
                        s_type_class_name, s_gtype_name);
    SCM def = scm_apply(gig_il_type_func, scm_cdr(il), SCM_EOL);

    *defs = scm_append2(*defs, def);
    *ils = scm_append2(*ils, il);
}
