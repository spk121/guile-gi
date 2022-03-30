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
#include "core.h"
#include "type.h"
#include "func.h"
#include "gig_parser.h"

static SCM get_shared_library_list(const char *lib);
static void constant_define(GIConstantInfo *info, SCM *ils);
static void type_define(GIRegisteredTypeInfo *info, GType gtype, size_t size, SCM *ils);
static void property_define(GIBaseInfo *info, SCM *ils);
static void untyped_flags_define(GIRegisteredTypeInfo *info, SCM *ils);
static void untyped_enum_define(GIRegisteredTypeInfo *info, SCM *ils);
static void untyped_enum_conversions_define(GIRegisteredTypeInfo *info, SCM *ils);
static void untyped_flag_conversions_define(GIRegisteredTypeInfo *info, SCM *ils);
static void enum_conversions_define(GIRegisteredTypeInfo *info, SCM *ils);
static void flag_conversions_define(GIRegisteredTypeInfo *info, SCM *ils);
static SCM make_baseinfo_fo(GIBaseInfo *info);

// utilities
static void arg_map_compute_c_invoke_positions(GigArgMap *amap);
static void arg_map_compute_s_call_positions(GigArgMap *amap);
static void arg_map_determine_argument_presence(GigArgMap *amap, GICallableInfo *info);
static char *base_info_get_qualified_name(GIRegisteredTypeInfo *info);
static bool callable_info_is_destructive(GICallableInfo *info);
static bool callable_info_is_predicate(GICallableInfo *info);
GigArgMap *callable_info_make_amap(GICallableInfo *function_info, const char *name);
static GigTransfer convert_transfer(GITransfer x);
static void type_meta_add_child_params(GigTypeMeta *meta, GITypeInfo *type_info, int n);
static void type_meta_init_from_basic_type_tag(GigTypeMeta *meta, GITypeTag tag);
static void type_meta_init_from_callable_info(GigTypeMeta *meta, GICallableInfo *ci);
static SCM type_meta_to_il(GigTypeMeta *meta);

static SCM baseinfo_fo_type = SCM_UNDEFINED;

static SCM
scm_irepository_search_path(void)
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
scm_irepository_prepend_search_path(SCM s_dir)
{
#define FUNC_NAME "irepository-prepend-search-path"
    char *dir;

    SCM_ASSERT_TYPE(scm_is_string(s_dir), s_dir, SCM_ARG1, "prepend-search-path!", "string");

    dir = scm_to_utf8_string(s_dir);
    g_irepository_prepend_search_path(dir);
    free(dir);

    return SCM_UNSPECIFIED;
#undef FUNC_NAME
}

static SCM
scm_namespace_load(SCM s_namespace, SCM s_version)
{
#define FUNC_NAME "namespace-load"
    SCM_ASSERT_TYPE(scm_is_string(s_namespace), s_namespace, SCM_ARG1, FUNC_NAME, "string");
    SCM_ASSERT_TYPE(scm_is_string(s_version), s_version, SCM_ARG2, FUNC_NAME, "string");

    char *namespace_, *version = NULL;
    GITypelib *tl;
    GError *error = NULL;

    scm_dynwind_begin(0);
    namespace_ = scm_dynfree(scm_to_utf8_string(s_namespace));
    if (scm_is_false(scm_string_null_p(s_version)))
        version = scm_dynfree(scm_to_utf8_string(s_version));
    
    gig_debug_load("requiring %s-%s", namespace_, version != NULL ? version : "latest");
    tl = g_irepository_require(NULL, namespace_, version, 0, &error);

    if (tl == NULL) {
        SCM err = scm_from_utf8_string(error->message);
        g_error_free(error);
        scm_misc_error(FUNC_NAME, "~A", scm_list_1(err));
    }
    scm_dynwind_end();
    return SCM_UNSPECIFIED;
#undef FUNC_NAME
}

static SCM
scm_namespace_version(SCM s_namespace)
{
#define FUNC_NAME "namespace-version"
    SCM_ASSERT_TYPE(scm_is_string(s_namespace), s_namespace, SCM_ARG1, FUNC_NAME, "string");

    char *namespace_;
    const char *version;

    scm_dynwind_begin(0);
    namespace_ = scm_dynfree(scm_to_utf8_string(s_namespace));
    version = g_irepository_get_version(NULL, namespace_);

    if (version == NULL)
        scm_misc_error(FUNC_NAME, "namespace '~A' is not loaded", scm_list_1(s_namespace));
    scm_dynwind_end();
    return scm_from_utf8_string(version);
    
#undef FUNC_NAME
}

static SCM
scm_namespace_shared_library(SCM s_namespace)
{
#define FUNC_NAME "namespace-shared-library"
    SCM_ASSERT_TYPE(scm_is_string(s_namespace), s_namespace, SCM_ARG1, FUNC_NAME, "string");

    char *namespace_;

    scm_dynwind_begin(0);
    namespace_ = scm_dynfree(scm_to_utf8_string(s_namespace));

    const char *csv_paths;
    const char *start, *end;
    SCM path_list = SCM_EOL;

    csv_paths = g_irepository_get_shared_library(NULL, namespace_);
    if (csv_paths == NULL)
        scm_misc_error(FUNC_NAME, "namespace '~A' is not loaded", scm_list_1(s_namespace));
    start = csv_paths;
    while ((end = strchr(start, ',')) != NULL) {
        path_list = scm_cons(scm_from_locale_stringn(start, end - start), path_list);
        start = end + 1;        // Skipping over comma.
    }
    path_list = scm_cons(scm_from_locale_string(start), path_list);
    scm_dynwind_end();
    return scm_reverse(path_list);
#undef FUNC_NAME
}

static SCM
scm_namespace_infos(SCM s_namespace)
{
#define FUNC_NAME "namespace-infos"
    SCM_ASSERT_TYPE(scm_is_string(s_namespace), s_namespace, SCM_ARG1, FUNC_NAME, "string");

    scm_dynwind_begin(0);
    char *namespace_ = scm_dynfree(scm_to_utf8_string(s_namespace));
    int n = g_irepository_get_n_infos(NULL, namespace_);
    SCM infos = SCM_EOL;

    for (int i = 0; i < n; i++) {
        GIBaseInfo *info = g_irepository_get_info(NULL, namespace_, i);
        if (g_base_info_is_deprecated(info)) {
            g_base_info_unref(info);
            continue;
        }
        infos = scm_cons(make_baseinfo_fo(info), infos);
    }
    scm_dynwind_end();

    return scm_reverse_x(infos, SCM_EOL);
#undef FUNC_NAME
}

static SCM
scm_namespace_info_by_name(SCM s_namespace, SCM s_name)
{
#define FUNC_NAME "namespace-info-by-name"
    SCM_ASSERT_TYPE(scm_is_string(s_namespace), s_namespace, SCM_ARG1, FUNC_NAME, "string");
    SCM_ASSERT_TYPE(scm_is_string(s_name), s_name, SCM_ARG2, FUNC_NAME, "string");

    scm_dynwind_begin(0);
    char *namespace_ = scm_dynfree(scm_to_utf8_string(s_namespace));
    char *name = scm_dynfree(scm_to_utf8_string(s_name));
    GIBaseInfo *info = g_irepository_find_by_name(NULL, namespace_, name);
    scm_dynwind_end();
    if (info == NULL)
        scm_misc_error(FUNC_NAME,
                       "could not load ~A from ~A, did you forget to require or perhaps misspell?",
                       scm_list_2(s_name, s_namespace));
    return scm_make_foreign_object_1(baseinfo_fo_type, info);
#undef FUNC_NAME
}

static SCM
scm_info_name(SCM s_info)
{
#define FUNC_NAME "info-name"
    scm_assert_foreign_object_type(baseinfo_fo_type, s_info);
    GIBaseInfo *info = scm_foreign_object_ref(s_info, 0);
    const char *name = g_base_info_get_name(info);
    return scm_from_utf8_string(name);
#undef FUNC_NAME
}

static SCM
scm_info_namespace(SCM s_info)
{
#define FUNC_NAME "info-namespace"
    scm_assert_foreign_object_type(baseinfo_fo_type, s_info);
    GIBaseInfo *info = scm_foreign_object_ref(s_info, 0);
    const char *namespace_ = g_base_info_get_namespace(info);
    return scm_from_utf8_string(namespace_);
#undef FUNC_NAME
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
    scm_dynwind_end();
    return il;
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

static SCM
load_info(GIBaseInfo *info, LoadFlags flags, SCM *s_types, SCM *ils)
{
    assert(info != NULL);

#define IS_REGISTERED(x)                                    \
    scm_is_true(scm_member(scm_from_size_t(x), *s_types))
#define REGISTER(x)                                     \
    *s_types = scm_cons(scm_from_size_t(x), *s_types)
        
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
            long_name = callable_info_make_name(info, parent_name);
        else
            long_name = callable_info_make_name(info, namespace_);
        amap = callable_info_make_amap(info, long_name);
        if (amap)
            types = gig_amap_get_gtype_list(amap, &len);
        else
            args_ok = false;

        // Pre-load all the types used by the function's arguments.
        for (size_t i = 0; i < len; i++) {
            if (!IS_REGISTERED(types[i])) {
                GIBaseInfo *typeinfo;
                typeinfo = g_irepository_find_by_gtype(NULL, types[i]);
                if (typeinfo) {
                    gig_debug_load("%s - loading prerequisite arg type %s",
                                   g_base_info_get_name(info), g_type_name(types[i]));
                    SCM il = load_info(typeinfo, LOAD_INFO_ONLY, s_types, ils);
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

            short_name = callable_info_make_name(info, NULL);
            if (amap != NULL) {
                if (t == GI_INFO_TYPE_FUNCTION) {
                    symbol_name = g_function_info_get_symbol(info);
                    SCM il = scm_list_n(scm_from_utf8_symbol("^function"),
                                        (namespace_ ? scm_from_utf8_symbol(namespace_) :
                                         SCM_BOOL_F),
                                        (parent_gtype ?
                                         scm_from_utf8_symbol(g_type_name(parent_gtype)) :
                                         SCM_BOOL_F),
                                        scm_from_utf8_symbol(long_name),
                                        scm_from_utf8_symbol(short_name),
                                        scm_from_utf8_symbol(symbol_name),
                                        gig_amap_to_il(amap),
                                        SCM_UNDEFINED);
                    *ils = scm_cons(il, *ils);
                }
                else if (t == GI_INFO_TYPE_SIGNAL) {
                    symbol_name = g_base_info_get_name(info);
                    SCM il = scm_list_n(scm_from_utf8_symbol("^signal"),
                                        (namespace_ ? scm_from_utf8_symbol(namespace_) :
                                         SCM_BOOL_F),
                                        (parent_gtype ?
                                         scm_from_utf8_symbol(g_type_name(parent_gtype)) :
                                         SCM_BOOL_F),
                                        scm_from_utf8_symbol(long_name),
                                        scm_from_utf8_symbol(short_name),
                                        scm_from_utf8_symbol(symbol_name),
                                        gig_amap_to_il(amap),
                                        SCM_UNDEFINED);
                    *ils = scm_cons(il, *ils);
                }
            }
            free(short_name);
        }
        free(long_name);
    }
    break;
    case GI_INFO_TYPE_PROPERTY:
        property_define(info, ils);
        break;
    case GI_INFO_TYPE_BOXED:
    {
        GType gtype = g_registered_type_info_get_g_type(info);
        if (!IS_REGISTERED(gtype)) {
            if (gtype == G_TYPE_NONE) {
                gig_debug_load("%s - not loading boxed type because is has no GType",
                               g_base_info_get_name(info));
                break;
            }
            size_t size = 0;
            if (GI_IS_STRUCT_INFO(info))
                size = g_struct_info_get_size((GIStructInfo *) info);
            if (GI_IS_UNION_INFO(info))
                size = g_union_info_get_size((GIStructInfo *) info);
            type_define(info, gtype, size, ils);
            REGISTER(gtype);
        }
        goto recursion;
    }
    case GI_INFO_TYPE_STRUCT:
    case GI_INFO_TYPE_UNION:
    case GI_INFO_TYPE_INTERFACE:
    case GI_INFO_TYPE_OBJECT:
    {
        GType gtype = g_registered_type_info_get_g_type(info);
        if (!IS_REGISTERED(gtype)) {
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
            if (par_gtype == G_TYPE_BOXED
                || par_gtype == G_TYPE_OBJECT
                || par_gtype == G_TYPE_INTERFACE) {
                parent_ok = true;
            }
            else if (par_gtype && !IS_REGISTERED(par_gtype)) {
                GIBaseInfo *typeinfo;
                typeinfo = g_irepository_find_by_gtype(NULL, par_gtype);
                if (typeinfo) {
                    gig_debug_load("%s - loading prerequisite type %s",
                                   g_base_info_get_name(info), g_type_name(par_gtype));
                
                    SCM il = load_info(typeinfo, LOAD_INFO_ONLY, s_types, ils);
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
                    if (interfaces[n] && !IS_REGISTERED(interfaces[n])) {
                        GIBaseInfo *typeinfo;
                        typeinfo = g_irepository_find_by_gtype(NULL, interfaces[n]);
                        if (typeinfo) {
                            gig_debug_load("%s - loading prerequisite interface type %s",
                                           g_base_info_get_name(info), g_type_name(interfaces[n]));
                            SCM il = load_info(typeinfo, LOAD_INFO_ONLY, s_types, ils);
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
                if (interface_ok) {
                    size_t size = 0;
                    if (t == GI_INFO_TYPE_STRUCT)
                        size = g_struct_info_get_size((GIStructInfo *) info);
                    if (t == GI_INFO_TYPE_UNION)
                        size = g_union_info_get_size((GIStructInfo *) info);
                    type_define(info, gtype, size, ils);
                    REGISTER(gtype);
                }
            }
        }
        goto recursion;
    }
    case GI_INFO_TYPE_ENUM:
    case GI_INFO_TYPE_FLAGS:
    {
        GType gtype = g_registered_type_info_get_g_type(info);
        if (gtype == G_TYPE_NONE) {
            if (t == GI_INFO_TYPE_ENUM) {
                untyped_enum_define(info, ils);
                untyped_enum_conversions_define(info, ils);
            }
            else {
                untyped_flags_define(info, ils);
                untyped_flag_conversions_define(info, ils);
            }
        }
        else {
            if (!IS_REGISTERED(gtype)) {
                type_define(info, gtype, 0, ils);
                REGISTER(gtype);
                if (t == GI_INFO_TYPE_ENUM)
                    enum_conversions_define(info, ils);
                else
                    flag_conversions_define(info, ils);
            }
        }
        goto recursion;
    }
    case GI_INFO_TYPE_CONSTANT:
        constant_define(info, ils);
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
    // output_il(ils);
    // output_exports(defs);
    return SCM_UNSPECIFIED;

recursion:
    {
#define LOAD_NESTED(F, N, I)                                        \
        do {                                                        \
            if (flags & F)                                          \
                for (int i = 0; i < N; i++) {                       \
                    GIBaseInfo *nested_info = I(info, i);           \
                    load_info(nested_info, flags, s_types, ils);    \
                    g_base_info_unref(nested_info);                 \
                }                                                   \
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
scm_parser_add_info_x(SCM s_parser, SCM s_info, SCM s_flags)
{
    SCM_ASSERT_TYPE(scm_is_unsigned_integer(s_flags, 0, LOAD_EVERYTHING), s_flags, SCM_ARG2, "parser-add-info!", "integer");

    LoadFlags flags;
    flags = scm_to_uint(s_flags);

    SCM s_types = scm_c_vector_ref(s_parser, 1);
    SCM s_ils = scm_c_vector_ref(s_parser, 3);
    GIBaseInfo *base_info = scm_foreign_object_ref(s_info, 0);

#ifdef DEBUG
    if (!scm_is_true(s_ils))
        scm_misc_error("parser-add-info!", "internal error #f IL", SCM_EOL);
    if (!scm_is_list(s_ils))
        scm_misc_error("parser-add-info!", "internal error non-list IL", SCM_EOL);
    for (int i = 0; i < scm_c_length(s_ils); i ++)
        if (!scm_is_list(scm_c_list_ref(s_ils, i)))
            scm_misc_error("parser-add-info!", "internal error non-list entry IL", SCM_EOL);
#endif
            
    load_info(base_info, flags, &s_types, &s_ils);
    scm_c_vector_set_x(s_parser, 1, s_types);
    scm_c_vector_set_x(s_parser, 3, s_ils);
    return SCM_UNSPECIFIED;
}

static SCM
scm_namespace_dependencies(SCM s_namespace)
{
#define FUNC_NAME "namespace-dependencies"
    char *namespace_;
    char **dependencies;
    int i;
    SCM s_dependencies = SCM_EOL;

    SCM_ASSERT_TYPE(scm_is_string(s_namespace), s_namespace, SCM_ARG1, FUNC_NAME, "string");
    namespace_ = scm_to_utf8_string(s_namespace);
    dependencies = g_irepository_get_dependencies(NULL, namespace_);
    free(namespace_);
    if (dependencies == NULL)
        return SCM_EOL;
    i = 0;
    while (dependencies[i] != NULL) {
        SCM entry = scm_from_utf8_string(dependencies[i]);
        s_dependencies = scm_cons(entry, s_dependencies);
        free(dependencies[i]);
        i++;
    }
    free(dependencies);
    return scm_reverse_x(s_dependencies, SCM_EOL);
#undef FUNC_NAME
}

static void
constant_define(GIConstantInfo *info, SCM *ils)
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
    *ils = scm_cons(il, *ils);
}

static char *
type_class_name_from_gtype(GType gtype)
{
    char *class_name;
#ifdef FIX_GTYPE_CLASS_NAME_BUG
    GIBaseInfo *info = g_irepository_find_by_gtype(NULL, gtype);
    if (info != NULL) {
        class_name = bracketize(g_base_info_get_name(info));
        g_base_info_unref(info);
    }
    else
        class_name = bracketize(g_type_name(gtype));
#else
    class_name = bracketize(g_type_name(gtype));
#endif
    return class_name;
}

static void
type_define(GIRegisteredTypeInfo *info, GType gtype, size_t size, SCM *ils)
{
    char *type_class_name = type_class_name_from_gtype(gtype);
    SCM s_type_class_name = scm_from_utf8_symbol(type_class_name);
    free(type_class_name);
    SCM s_gtype_name = scm_from_utf8_string(g_type_name(gtype));
    const char *namespace_ = g_base_info_get_namespace(info);
    SCM s_namespace = namespace_ ? scm_from_utf8_symbol(namespace_) : SCM_BOOL_F;
        
    
    
    const char *ref = NULL;
    const char *unref = NULL;
    SCM s_ref = SCM_BOOL_F;
    SCM s_unref = SCM_BOOL_F;
    bool custom = false;
    
    if(GI_IS_OBJECT_INFO(info)) {
        ref = g_object_info_get_ref_function(info);
        unref = g_object_info_get_unref_function(info);
        if (ref && unref) {
            s_ref = scm_from_utf8_string(ref);
            s_unref = scm_from_utf8_string(unref);
            custom = true;
        }
    }
    
    SCM il;
    if (!custom && !size)
        il = scm_list_4(scm_from_utf8_symbol("^type"),
                        s_namespace,
                        s_type_class_name, s_gtype_name);
    else if (!custom && size)
        il = scm_list_5(scm_from_utf8_symbol("^sized-type"),
                        s_namespace,
                        s_type_class_name, s_gtype_name, scm_from_size_t(size));
    else if (custom && !size)
        il = scm_list_n(scm_from_utf8_symbol("^custom-type"),
                        s_namespace,
                        s_type_class_name, s_gtype_name,
                        s_ref, s_unref, SCM_UNDEFINED);
    *ils = scm_cons(il, *ils);
}

static void
property_define(GIBaseInfo *info, SCM *ils)
{
    GIBaseInfo *parent = g_base_info_get_container(info);
    GType parent_gtype = g_registered_type_info_get_g_type(parent);
    const char *parent_name = g_base_info_get_name(parent);
    const char *short_name = g_base_info_get_name(info);
    const char *symbol = g_base_info_get_name(info);
    char *tmp = concatenate3(parent_name, ":", short_name);
    char *long_name = make_scm_name(tmp);
    free(tmp);
    SCM il = scm_list_5(scm_from_utf8_symbol("^property"),
                        scm_from_utf8_symbol(g_type_name(parent_gtype)),
                        scm_from_utf8_symbol(long_name),
                        scm_from_utf8_symbol(short_name), scm_from_utf8_symbol(symbol));
    *ils = scm_cons(il, *ils);
}

static void
untyped_flags_define(GIRegisteredTypeInfo *info, SCM *ils)
{
    char *qname = base_info_get_qualified_name(info);
    SCM il = SCM_EOL;
    SCM s_qname = scm_from_utf8_symbol(qname);
    char *class_name = bracketize(qname);
    SCM s_class_name = scm_from_utf8_symbol(class_name);
    free(class_name);
    SCM alist = make_flag_enum_alist(info);
    il = scm_list_4(scm_from_utf8_symbol("^untyped-flags"),
                    s_class_name, s_qname, alist);
    
    free(qname);
    *ils = scm_cons(il, *ils);
}

static void
untyped_enum_define(GIRegisteredTypeInfo *info, SCM *ils)
{
    char *qname = base_info_get_qualified_name(info);
    SCM il = SCM_EOL;
    SCM s_qname = scm_from_utf8_symbol(qname);
    char *class_name = bracketize(qname);
    SCM s_class_name = scm_from_utf8_symbol(class_name);
    free(class_name);
    SCM alist = make_flag_enum_alist(info);
    il = scm_list_4(scm_from_utf8_symbol("^untyped-enum"),
                    s_class_name, s_qname, alist);

    free(qname);
    *ils = scm_cons(il, *ils);
}

static void
untyped_enum_conversions_define(GIRegisteredTypeInfo *info, SCM *ils)
{
    char *qname = base_info_get_qualified_name(info);
    SCM s_qname = scm_from_utf8_symbol(qname);
    char *conversion_name = make_scm_name(g_base_info_get_name(info));
    SCM s_cname = scm_from_utf8_symbol(conversion_name);
    free(conversion_name);
    free(qname);
    SCM il = scm_list_3(scm_from_utf8_symbol("^untyped-enum-conv"),
                        s_cname, s_qname);
    *ils = scm_cons(il, *ils);
}

static void
untyped_flag_conversions_define(GIRegisteredTypeInfo *info, SCM *ils)
{
    char *qname = base_info_get_qualified_name(info);
    SCM s_qname = scm_from_utf8_symbol(qname);
    char *conversion_name = make_scm_name(g_base_info_get_name(info));
    SCM s_cname = scm_from_utf8_symbol(conversion_name);
    free(conversion_name);
    free(qname);
    SCM il = scm_list_3(scm_from_utf8_symbol("^untyped-flags-conv"),
                        s_cname, s_qname);
    *ils = scm_cons(il, *ils);
}

static void
enum_conversions_define(GIRegisteredTypeInfo *info, SCM *ils)
{
    char *conversion_name = make_scm_name(g_base_info_get_name(info));
    SCM s_cname = scm_from_utf8_symbol(conversion_name);
    size_t gtype = g_registered_type_info_get_g_type(info);
    SCM s_gtype_name = scm_from_utf8_symbol(g_type_name(gtype));
    free(conversion_name);
    SCM il = scm_list_3(scm_from_utf8_symbol("^enum-conv"),
                        s_cname, s_gtype_name);
    *ils = scm_cons(il, *ils);
}

static void
flag_conversions_define(GIRegisteredTypeInfo *info, SCM *ils)
{
    char *conversion_name = make_scm_name(g_base_info_get_name(info));
    SCM s_cname = scm_from_utf8_symbol(conversion_name);
    size_t gtype = g_registered_type_info_get_g_type(info);
    SCM s_gtype_name = scm_from_utf8_symbol(g_type_name(gtype));
    free(conversion_name);
    SCM il = scm_list_3(scm_from_utf8_symbol("^flags-conv"),
                        s_cname, s_gtype_name);
    *ils = scm_cons(il, *ils);
}

static SCM
make_baseinfo_fo(GIBaseInfo *info)
{
    g_base_info_ref(info);
    return scm_make_foreign_object_1(baseinfo_fo_type, info);
}

static void
gc_free_baseinfo(SCM x)
{
    GIBaseInfo *info = scm_foreign_object_ref(x, 0);
    g_base_info_unref(info);
}

void
gig_init_parser()
{
    baseinfo_fo_type = scm_make_foreign_object_type(scm_from_utf8_symbol("<GIBaseInfo>"),
                                                    scm_list_1(scm_from_utf8_symbol("ptr")),
                                                    gc_free_baseinfo);
    scm_c_define("<GIBaseInfo>", baseinfo_fo_type);

    scm_c_define_gsubr("require", 1, 1, 0, require);
    scm_c_define_gsubr("%irepository-search-path", 0, 0, 0, scm_irepository_search_path);
    scm_c_define_gsubr("%irepository-prepend-search-path", 1, 0, 0, scm_irepository_prepend_search_path);
    scm_c_define_gsubr("%namespace-load", 2, 0, 1, scm_namespace_load);
    scm_c_define_gsubr("%namespace-version", 1, 0, 0, scm_namespace_version);
    scm_c_define_gsubr("%namespace-shared-library", 1, 0, 0, scm_namespace_shared_library);
    scm_c_define_gsubr("%namespace-infos", 1, 0, 0, scm_namespace_infos);
    scm_c_define_gsubr("%namespace-info-by-name", 2, 0, 0, scm_namespace_info_by_name);
    scm_c_define_gsubr("%namespace-dependencies", 1, 0, 0, scm_namespace_dependencies);
    scm_c_define_gsubr("%info-name", 1, 0, 0, scm_info_name);
    scm_c_define_gsubr("%info-namespace", 1, 0, 0, scm_info_namespace);
    // scm_c_define_gsubr("get-all-info", 1, 0, 0, get_all_info);
    // scm_c_define_gsubr("get-info-by-name", 2, 0, 0, get_info_by_name);
    scm_c_define_gsubr("%parser-add-info!", 3, 0, 0, scm_parser_add_info_x);

#define D(x) scm_permanent_object(scm_c_define(#x, scm_from_uint(x)))

    D(LOAD_INFO_ONLY);
    D(LOAD_METHODS);
    D(LOAD_PROPERTIES);
    D(LOAD_SIGNALS);
    D(LOAD_EVERYTHING);
#undef D
}

static void
gig_type_meta_init_from_type_info(GigTypeMeta *meta, GITypeInfo *type_info)
{
    GITypeTag tag = g_type_info_get_tag(type_info);
    meta->is_ptr = g_type_info_is_pointer(type_info);

    if (tag == GI_TYPE_TAG_VOID) {
        if (meta->is_ptr)
            meta->arg_type = GIG_ARG_TYPE_POINTER;
        else
            meta->arg_type = GIG_ARG_TYPE_VOID;
    }
    else if (tag == GI_TYPE_TAG_ARRAY) {
        GIArrayType array_type = g_type_info_get_array_type(type_info);
        type_meta_add_child_params(meta, type_info, 1);

        switch (array_type) {
        case GI_ARRAY_TYPE_C:
        {
            meta->arg_type = GIG_ARG_TYPE_ARRAY;
            int length_arg = g_type_info_get_array_length(type_info);
            int fixed_size = g_type_info_get_array_fixed_size(type_info);

            if (length_arg != -1) {
                meta->has_length_arg = true;
                meta->length_arg = length_arg;
            }
            if (fixed_size != -1) {
                meta->has_fixed_size = true;
                meta->fixed_size = fixed_size;
            }
            if (g_type_info_is_zero_terminated(type_info))
                meta->is_zero_terminated = true;
            if (!meta->has_length_arg && !meta->has_fixed_size && !meta->is_zero_terminated) {
                gig_debug_load
                    ("no way of determining array size of C array %s of %s, coercing to pointer",
                     g_base_info_get_namespace(type_info), g_type_name(meta->params[0].gtype));
                meta->arg_type = GIG_ARG_TYPE_POINTER;
            }
        }
        break;
        case GI_ARRAY_TYPE_ARRAY:
            meta->arg_type = GIG_ARG_TYPE_GARRAY;
            break;
        case GI_ARRAY_TYPE_BYTE_ARRAY:
            meta->arg_type = GIG_ARG_TYPE_GBYTEARRAY;
            break;
        case GI_ARRAY_TYPE_PTR_ARRAY:
            meta->arg_type = GIG_ARG_TYPE_GPTRARRAY;
            break;
        }
    }
    else if (tag == GI_TYPE_TAG_GHASH) {
        meta->arg_type = GIG_ARG_TYPE_GHASH;
        meta->item_size = sizeof(GHashTable *);
        type_meta_add_child_params(meta, type_info, 2);
    }
    else if (tag == GI_TYPE_TAG_GLIST) {
        meta->arg_type = GIG_ARG_TYPE_GLIST;
        type_meta_add_child_params(meta, type_info, 1);
    }
    else if (tag == GI_TYPE_TAG_GSLIST) {
        meta->arg_type = GIG_ARG_TYPE_GSLIST;
        type_meta_add_child_params(meta, type_info, 1);
    }
    else if (tag == GI_TYPE_TAG_INTERFACE) {
        GIBaseInfo *referenced_base_info;
        GIInfoType itype;
        GType fundamental_gtype;
        GigArgMap *_amap;

        referenced_base_info = g_type_info_get_interface(type_info);
        if (referenced_base_info == NULL)
            meta->is_invalid = true;
        else {
            itype = g_base_info_get_type(referenced_base_info);
            switch (itype) {
            case GI_INFO_TYPE_INVALID:
            case GI_INFO_TYPE_FUNCTION:
                meta->is_invalid = true;
                break;
            case GI_INFO_TYPE_CALLBACK:
                meta->arg_type = GIG_ARG_TYPE_CALLBACK;
                _amap =
                    callable_info_make_amap(referenced_base_info, g_base_info_get_name(referenced_base_info));
                if (_amap == NULL)
                    meta->is_invalid = true;
                else {
                    meta->callable_arg_map = _amap;
                    meta->item_size = sizeof(void *);
                }
                break;
            case GI_INFO_TYPE_STRUCT:
                meta->arg_type = GIG_ARG_TYPE_BOXED;
                meta->item_size = g_struct_info_get_size(referenced_base_info);
                meta->gtype = g_registered_type_info_get_g_type(referenced_base_info);
                break;
            case GI_INFO_TYPE_BOXED:
                meta->is_invalid = true;
                break;
            case GI_INFO_TYPE_ENUM:
                meta->arg_type = GIG_ARG_TYPE_ENUM;
                meta->gtype = g_registered_type_info_get_g_type(referenced_base_info);
                if (meta->gtype == G_TYPE_NONE) {
                    meta->qname = base_info_get_qualified_name(referenced_base_info);
                    meta->gtype = G_TYPE_INVALID;
                }
                break;
            case GI_INFO_TYPE_FLAGS:
                meta->arg_type = GIG_ARG_TYPE_FLAGS;
                meta->gtype = g_registered_type_info_get_g_type(referenced_base_info);
                if (meta->gtype == G_TYPE_NONE) {
                    meta->qname = base_info_get_qualified_name(referenced_base_info);
                    meta->gtype = G_TYPE_INVALID;
                }
                break;
            case GI_INFO_TYPE_OBJECT:
                meta->arg_type = GIG_ARG_TYPE_OBJECT;
                meta->gtype = g_registered_type_info_get_g_type(referenced_base_info);
                break;
            case GI_INFO_TYPE_INTERFACE:
                meta->arg_type = GIG_ARG_TYPE_INTERFACE;
                meta->gtype = g_registered_type_info_get_g_type(referenced_base_info);
                fundamental_gtype = G_TYPE_FUNDAMENTAL(meta->gtype);
                if (fundamental_gtype == G_TYPE_BOXED) {
                    meta->arg_type = GIG_ARG_TYPE_BOXED;
                    meta->item_size = g_struct_info_get_size(referenced_base_info);
                }
                else if (fundamental_gtype == G_TYPE_INTERFACE)
                    meta->arg_type = GIG_ARG_TYPE_INTERFACE;
                else {
                    printf("FOOBAR\n");
                    meta->is_invalid = true;
                }
                break;
            case GI_INFO_TYPE_CONSTANT:
            case GI_INFO_TYPE_INVALID_0:
                meta->is_invalid = true;
            case GI_INFO_TYPE_UNION:
                meta->arg_type = GIG_ARG_TYPE_BOXED;
                meta->item_size = g_union_info_get_size(referenced_base_info);
                meta->gtype = g_registered_type_info_get_g_type(referenced_base_info);
                break;
            case GI_INFO_TYPE_VALUE:
                meta->arg_type = GIG_ARG_TYPE_VALUE;
                meta->gtype = g_registered_type_info_get_g_type(referenced_base_info);
                break;
            case GI_INFO_TYPE_SIGNAL:
            case GI_INFO_TYPE_VFUNC:
            case GI_INFO_TYPE_PROPERTY:
            case GI_INFO_TYPE_FIELD:
            case GI_INFO_TYPE_ARG:
                meta->is_invalid = true;
                break;
            case GI_INFO_TYPE_TYPE:
                meta->arg_type = GIG_ARG_TYPE_GTYPE;
                break;
            case GI_INFO_TYPE_UNRESOLVED:
                meta->is_invalid = true;
                break;
            }
        }
        assert(meta->arg_type != 0 || meta->is_invalid);
        g_base_info_unref(referenced_base_info);
    }
    else
        type_meta_init_from_basic_type_tag(meta, tag);

    assert(meta->arg_type <= GIG_ARG_TYPE_GHASH);
    assert(meta->arg_type != 0 || meta->is_invalid);
}

void
gig_type_meta_init_from_arg_info(GigTypeMeta *meta, GIArgInfo *ai)
{
    GITypeInfo *type_info = g_arg_info_get_type(ai);
    GIDirection dir = g_arg_info_get_direction(ai);
    GITransfer transfer = g_arg_info_get_ownership_transfer(ai);

    gig_type_meta_init_from_type_info(meta, type_info);

    meta->is_in = (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT);
    meta->is_out = (dir == GI_DIRECTION_OUT || dir == GI_DIRECTION_INOUT);
    meta->is_skip = g_arg_info_is_skip(ai);

    meta->is_caller_allocates = g_arg_info_is_caller_allocates(ai);
    meta->is_optional = g_arg_info_is_optional(ai);
    meta->is_nullable = g_arg_info_may_be_null(ai);

    meta->transfer = convert_transfer(transfer);
    g_base_info_unref(type_info);
}

static void
arg_map_apply_function_info(GigArgMap *amap, GIFunctionInfo *func_info)
{
    int i, n;
    GIArgInfo *arg_info;

    n = amap->len;
    amap->is_method = g_callable_info_is_method(func_info);
    amap->can_throw_gerror = g_callable_info_can_throw_gerror(func_info);

    for (i = 0; i < n; i++) {
        arg_info = g_callable_info_get_arg(func_info, i);
        gig_type_meta_init_from_arg_info(&amap->pdata[i].meta, arg_info);
        free(amap->pdata[i].name);
        amap->pdata[i].name = xstrdup(g_base_info_get_name(arg_info));
        g_base_info_unref(arg_info);
        amap->is_invalid |= amap->pdata[i].meta.is_invalid;
    }

    type_meta_init_from_callable_info(&amap->return_val.meta, func_info);
    free(amap->return_val.name);
    amap->return_val.name = xstrdup("%return");
    amap->is_invalid |= amap->return_val.meta.is_invalid;
}





// Returns a list of GTypes that used by this function call
GType *
gig_function_get_arg_gtypes(GICallableInfo *info, size_t *len)
{
    GigArgMap *amap;
    GType *types;

    amap = callable_info_make_amap(info, g_base_info_get_name(info));
    *len = 0;
    if (amap == NULL)
        return NULL;
    types = gig_amap_get_gtype_list(amap, len);
    //gig_amap_free(amap);
    return types;
}

// This procedure counts the number of arguments that the
// GObject Introspection FFI call is expecting.
static void
count_args(GICallableInfo *info, int *in, int *out)
{
    // Count the number of required input arguments, and store
    // the arg info in a newly allocate array.
    int n_args = g_callable_info_get_n_args(info);
    int n_input_args = 0;
    int n_output_args = 0;

    for (int i = 0; i < n_args; i++) {
        GIArgInfo *ai = g_callable_info_get_arg(info, i);
        GIDirection dir = g_arg_info_get_direction(ai);
        g_base_info_unref(ai);

        if (dir == GI_DIRECTION_IN)
            n_input_args++;
        else if (dir == GI_DIRECTION_OUT)
            n_output_args++;
        else if (dir == GI_DIRECTION_INOUT) {
            n_input_args++;
            n_output_args++;
        }
    }
    *in = n_input_args;
    *out = n_output_args;
}

const char *
g_base_info_get_name_safe(GIBaseInfo *info)
{
    GIInfoType type = g_base_info_get_type(info);
    switch (type) {
    case GI_INFO_TYPE_FUNCTION:
    case GI_INFO_TYPE_CALLBACK:
    case GI_INFO_TYPE_STRUCT:
    case GI_INFO_TYPE_BOXED:
    case GI_INFO_TYPE_ENUM:
    case GI_INFO_TYPE_FLAGS:
    case GI_INFO_TYPE_OBJECT:
    case GI_INFO_TYPE_INTERFACE:
    case GI_INFO_TYPE_CONSTANT:
    case GI_INFO_TYPE_UNION:
    case GI_INFO_TYPE_VALUE:
    case GI_INFO_TYPE_SIGNAL:
    case GI_INFO_TYPE_PROPERTY:
    case GI_INFO_TYPE_VFUNC:
    case GI_INFO_TYPE_FIELD:
    case GI_INFO_TYPE_ARG:
    case GI_INFO_TYPE_UNRESOLVED:
        return g_base_info_get_name(info);
        break;
    case GI_INFO_TYPE_TYPE:
    default:
        return "(unnamed)";
        break;
    }
}




static void
arg_map_determine_array_length_index(GigArgMap *amap, GigArgMapEntry *entry, GITypeInfo *info)
{
    if (entry->meta.arg_type == GIG_ARG_TYPE_ARRAY && entry->meta.has_length_arg) {
        int idx = entry->meta.length_arg;

        assert(idx >= 0);

        entry->tuple = GIG_ARG_TUPLE_ARRAY;
        GigArgMapEntry *child = amap->pdata + idx;
        child->tuple = GIG_ARG_TUPLE_ARRAY_SIZE;
        child->presence = GIG_ARG_PRESENCE_IMPLICIT;
        child->is_s_output = 0;
    }
}

////////////////////////////////////////////////////////////////

static void
arg_map_compute_c_invoke_positions(GigArgMap *amap)
{
    int i, n;
    GigArgMapEntry *entry;
    n = amap->len;

    int c_input_pos = 0;
    int c_output_pos = 0;
    for (i = 0; i < n; i++) {
        entry = &amap->pdata[i];

        // Here we find the positions of this argument in the
        // g_function_info_invoke call.  Also, some output parameters
        // require a SCM container to be passed in to the SCM GSubr
        // call.
        if (entry->meta.is_in && !entry->meta.is_out) {
            entry->s_direction = GIG_ARG_DIRECTION_INPUT;
            entry->is_c_input = 1;
            entry->c_input_pos = c_input_pos++;
        }
        else if (entry->meta.is_in && entry->meta.is_out) {
            entry->s_direction = GIG_ARG_DIRECTION_INOUT;
            entry->is_c_input = 1;
            entry->c_input_pos = c_input_pos++;
            entry->is_c_output = 1;
            entry->c_output_pos = c_output_pos++;
        }
        else if (entry->meta.is_out && entry->meta.is_caller_allocates) {
            entry->s_direction = GIG_ARG_DIRECTION_PREALLOCATED_OUTPUT;
            entry->is_c_output = 1;
            entry->c_output_pos = c_output_pos++;
        }
        else {
            entry->s_direction = GIG_ARG_DIRECTION_OUTPUT;
            entry->is_c_output = 1;
            entry->c_output_pos = c_output_pos++;
        }
    }

    if (amap->return_val.meta.is_out)
        amap->return_val.s_direction = GIG_ARG_DIRECTION_OUTPUT;
    else
        amap->return_val.s_direction = GIG_ARG_DIRECTION_VOID;

    amap->c_input_len = c_input_pos;
    amap->c_output_len = c_output_pos;
}

static void
arg_map_compute_s_call_positions(GigArgMap *amap)
{
    int i, n;
    GigArgMapEntry *entry;
    n = amap->len;

    int s_input_pos = 0;
    int s_output_pos = 0;
    // We now can decide where these arguments appear in the SCM GSubr
    // call.
    for (i = 0; i < n; i++) {
        entry = &amap->pdata[i];

        // TODO: Why check entry->tuple instead of entry->presence?
        //       The latter appears to be buggy in some way.
        if (entry->tuple == GIG_ARG_TUPLE_ARRAY_SIZE)
            continue;

        switch (entry->s_direction) {
        case GIG_ARG_DIRECTION_INPUT:
        case GIG_ARG_DIRECTION_INOUT:
        case GIG_ARG_DIRECTION_PREALLOCATED_OUTPUT:
            entry->is_s_input = 1;
            entry->s_input_pos = s_input_pos++;
            if (entry->presence == GIG_ARG_PRESENCE_REQUIRED)
                amap->s_input_req++;
            else if (entry->presence == GIG_ARG_PRESENCE_OPTIONAL)
                amap->s_input_opt++;

            if (entry->s_direction == GIG_ARG_DIRECTION_INPUT)
                break;
            /* fallthrough */
        case GIG_ARG_DIRECTION_OUTPUT:
            entry->is_s_output = 1;
            entry->s_output_pos = s_output_pos++;
            break;
        default:
            assert_not_reached();
        }
    }

    amap->s_output_len = s_output_pos;
    assert(amap->s_input_req + amap->s_input_opt == s_input_pos);
}

static void
arg_map_determine_argument_presence(GigArgMap *amap, GICallableInfo *info)
{
    GigArgMapEntry *entry;
    bool opt_flag = true;
    int i, n;

    n = amap->len;

    // may-be-null parameters at the end of the C call can be made
    // optional parameters in the gsubr call.
    for (i = n - 1; i >= 0; i--) {
        entry = &amap->pdata[i];
        entry->tuple = GIG_ARG_TUPLE_SINGLETON;
        if (entry->meta.is_in || (entry->meta.is_out && entry->meta.is_caller_allocates)) {
            if (opt_flag && entry->meta.is_nullable)
                entry->presence = GIG_ARG_PRESENCE_OPTIONAL;
            else {
                entry->presence = GIG_ARG_PRESENCE_REQUIRED;
                opt_flag = false;
            }
        }
        else {
            entry->presence = GIG_ARG_PRESENCE_IMPLICIT;
        }
    }

    // In C, if there is an array defined as a pointer and a
    // length parameter, it becomes a single S parameter.
    for (i = 0; i < n; i++) {
        entry = amap->pdata + i;
        GIArgInfo *a = g_callable_info_get_arg(info, i);
        GITypeInfo *t = g_arg_info_get_type(a);
        arg_map_determine_array_length_index(amap, entry, t);
        g_base_info_unref(t);
        g_base_info_unref(a);
    }

    amap->return_val.tuple = GIG_ARG_TUPLE_SINGLETON;
    GITypeInfo *return_type = g_callable_info_get_return_type(info);
    arg_map_determine_array_length_index(amap, &amap->return_val, return_type);
    g_base_info_unref(return_type);
}

static char *
base_info_get_qualified_name(GIRegisteredTypeInfo *info)
{
    const char *_name = g_base_info_get_attribute(info, "c:type");
    if (_name != NULL)
        return xstrdup(_name);

    const char *_namespace = g_base_info_get_namespace(info);
    const char *prefix = g_irepository_get_c_prefix(NULL, _namespace);

    // add initial % to ensure that the name is private
    return concatenate3("%", prefix, g_base_info_get_name(info));
}

static bool
callable_info_is_destructive(GICallableInfo *info)
{
    bool destructive = false;
    int n_args = g_callable_info_get_n_args(info);

    for (int i = 0; i < n_args; i++) {
        GIArgInfo *ai = g_callable_info_get_arg(info, i);
        GITypeInfo *ti = g_arg_info_get_type(ai);
        bool is_trivial;

        switch (g_type_info_get_tag(ti)) {
        case GI_TYPE_TAG_BOOLEAN:
        case GI_TYPE_TAG_DOUBLE:
        case GI_TYPE_TAG_FLOAT:
        case GI_TYPE_TAG_GTYPE:
        case GI_TYPE_TAG_INT8:
        case GI_TYPE_TAG_INT16:
        case GI_TYPE_TAG_INT32:
        case GI_TYPE_TAG_INT64:
        case GI_TYPE_TAG_UINT8:
        case GI_TYPE_TAG_UINT16:
        case GI_TYPE_TAG_UINT32:
        case GI_TYPE_TAG_UINT64:
        case GI_TYPE_TAG_UNICHAR:
            is_trivial = true;
            break;
        default:
            is_trivial = false;
        }
        g_base_info_unref(ti);

        if (!is_trivial) {
            destructive |= g_arg_info_is_caller_allocates(ai);
            destructive |= (g_arg_info_get_direction(ai) == GI_DIRECTION_INOUT);
        }
        g_base_info_unref(ai);
    }

    return destructive;
}

// Returns TRUE if this function returns a single boolean.
static bool
callable_info_is_predicate(GICallableInfo *info)
{
    bool predicate = false;
    GITypeInfo *return_type;

    if (GI_IS_SIGNAL_INFO(info))
        return false;

    return_type = g_callable_info_get_return_type(info);

    if (g_type_info_get_tag(return_type) == GI_TYPE_TAG_BOOLEAN
        && !g_type_info_is_pointer(return_type)) {
        int in, out;

        count_args(info, &in, &out);
        if (out == 0)
            predicate = true;
    }
    g_base_info_unref(return_type);
    return predicate;
}

// Gather information on how to map Scheme arguments to C arguments.
GigArgMap *
callable_info_make_amap(GICallableInfo *function_info, const char *name)
{
    GigArgMap *amap;
    size_t n;

    n = g_callable_info_get_n_args(function_info);
    amap = gig_amap_allocate(n);
    free(amap->name);
    amap->name = xstrdup(g_base_info_get_name(function_info));
    arg_map_apply_function_info(amap, function_info);
    if (amap->is_invalid) {
        //gig_amap_free(amap);
        return NULL;
    }
    arg_map_determine_argument_presence(amap, function_info);
    arg_map_compute_c_invoke_positions(amap);
    arg_map_compute_s_call_positions(amap);
    return amap;
}

// For function and method names, we want a lowercase string of the
// form 'func-name-with-hyphens'
char *
callable_info_make_name(GICallableInfo *info, const char *prefix)
{
    char *name, *str1 = NULL, *str2 = NULL;
    bool predicate, destructive;

    predicate = callable_info_is_predicate(info);
    destructive = callable_info_is_destructive(info);
    if (prefix)
        str1 = make_scm_name(prefix);
    str2 = make_scm_name(g_base_info_get_name(info));
    if (!prefix) {
        if (destructive)
            name = concatenate(str2, "!");
        else if (predicate)
            name = concatenate(str2, "?");
        else
            return str2;
    }
    else {
        if (destructive)
            name = concatenate4(str1, ":", str2, "!");
        else if (predicate)
            name = concatenate4(str1, ":", str2, "?");
        else
            name = concatenate3(str1, ":", str2);
    }
    free(str1);
    free(str2);
    return name;
}

static GigTransfer
convert_transfer(GITransfer x)
{
    if (x == GI_TRANSFER_NOTHING)
        return GIG_TRANSFER_NOTHING;
    if (x == GI_TRANSFER_CONTAINER)
        return GIG_TRANSFER_CONTAINER;
    return GIG_TRANSFER_EVERYTHING;
}

static void
type_meta_add_child_params(GigTypeMeta *meta, GITypeInfo *type_info, int n)
{
    GITypeInfo *param_type;
    gig_meta_add_params(meta, n);

    for (int i = 0; i < n; i++) {
        param_type = g_type_info_get_param_type((GITypeInfo *)type_info, i);
        gig_type_meta_init_from_type_info(&meta->params[i], param_type);
        g_base_info_unref(param_type);

        if (meta->transfer == GIG_TRANSFER_EVERYTHING)
            meta->params[i].transfer = GIG_TRANSFER_EVERYTHING;
        else
            meta->params[i].transfer = GIG_TRANSFER_NOTHING;

        meta->is_invalid |= meta->params[i].is_invalid;
    }
}

static void
type_meta_init_from_basic_type_tag(GigTypeMeta *meta, GITypeTag tag)
{
#define T(TYPETAG,ATYPE,CTYPE)                  \
    do {                                        \
        if (tag == TYPETAG) {                   \
            meta->arg_type = ATYPE;             \
            meta->item_size = sizeof (CTYPE);   \
            return;                             \
        }                                       \
    } while(false)

    T(GI_TYPE_TAG_BOOLEAN, GIG_ARG_TYPE_GBOOLEAN, gboolean);
    T(GI_TYPE_TAG_DOUBLE, GIG_ARG_TYPE_DOUBLE, double);
    T(GI_TYPE_TAG_FLOAT, GIG_ARG_TYPE_FLOAT, float);
    T(GI_TYPE_TAG_GTYPE, GIG_ARG_TYPE_GTYPE, GType);
    T(GI_TYPE_TAG_INT8, GIG_ARG_TYPE_INT8, int8_t);
    T(GI_TYPE_TAG_INT16, GIG_ARG_TYPE_INT16, int16_t);
    T(GI_TYPE_TAG_INT32, GIG_ARG_TYPE_INT32, int32_t);
    T(GI_TYPE_TAG_INT64, GIG_ARG_TYPE_INT64, int64_t);
    T(GI_TYPE_TAG_UINT8, GIG_ARG_TYPE_UINT8, uint8_t);
    T(GI_TYPE_TAG_UINT16, GIG_ARG_TYPE_UINT16, uint16_t);
    T(GI_TYPE_TAG_UINT32, GIG_ARG_TYPE_UINT32, uint32_t);
    T(GI_TYPE_TAG_UINT64, GIG_ARG_TYPE_UINT64, uint64_t);
    T(GI_TYPE_TAG_UNICHAR, GIG_ARG_TYPE_UNICHAR, uint32_t);
    T(GI_TYPE_TAG_UTF8, GIG_ARG_TYPE_UTF8_STRING, char *);
    T(GI_TYPE_TAG_FILENAME, GIG_ARG_TYPE_LOCALE_STRING, char *);
    T(GI_TYPE_TAG_ERROR, GIG_ARG_TYPE_GERROR, GError);
    gig_error("unhandled type '%s' %s %d", g_type_tag_to_string(tag), __FILE__, __LINE__);
#undef T
}

void
type_meta_init_from_callable_info(GigTypeMeta *meta, GICallableInfo *ci)
{
    GITypeInfo *type_info = g_callable_info_get_return_type(ci);
    GITransfer transfer = g_callable_info_get_caller_owns(ci);

    gig_type_meta_init_from_type_info(meta, type_info);

    meta->is_in = false;
    if (meta->arg_type != GIG_ARG_TYPE_UNKNOWN && meta->arg_type != GIG_ARG_TYPE_VOID)
        meta->is_out = true;
    else
        meta->is_out = false;
    meta->is_skip = g_callable_info_skip_return(ci);

    meta->is_caller_allocates = false;
    meta->is_optional = false;
    meta->is_nullable = g_callable_info_may_return_null(ci);

    meta->transfer = convert_transfer(transfer);
    g_base_info_unref(type_info);
}

static SCM
type_meta_to_il(GigTypeMeta *meta)
{
    SCM il = SCM_EOL;
    SCM flags = SCM_EOL;
#define D(k,v)                                              \
    il = scm_cons(scm_cons(scm_from_utf8_symbol(k),(v)),il)
#define F(x)                                            \
    flags = scm_cons(scm_from_utf8_symbol(x), flags);

    D("arg-type", scm_from_utf8_symbol(gig_arg_type_name(meta->arg_type)));

    if (meta->gtype)
        D("gtype-name", scm_from_utf8_string(g_type_name(meta->gtype)));
    if (meta->qname)
        D("qualified-name", scm_from_utf8_string(meta->qname));
    if (meta->is_ptr)
        F("ptr");
    if (meta->is_in)
        F("in");
    if (meta->is_out)
        F("out")
            if (meta->is_skip)
                F("skip");
    if (meta->is_caller_allocates)
        F("preallocated");
    if (meta->is_optional)
        F("optional");
    if (meta->is_nullable)
        F("nullable");
    if (meta->is_invalid)
        F("invalid");
    if (meta->is_zero_terminated)
        F("zero-terminated");
    if (meta->has_length_arg)
        F("sized");
    if (meta->has_fixed_size)
        F("fixed-size");
    if (!scm_is_null(flags))
        D("flags", scm_reverse(flags));
    if (meta->length_arg != 0)
        D("length-arg", scm_from_int(meta->length_arg));
    if (meta->fixed_size != 0)
        D("fixed-size", scm_from_int(meta->fixed_size));
    if (meta->item_size != 0)
        D("item-size", scm_from_size_t(meta->item_size));
    if (meta->transfer == GIG_TRANSFER_NOTHING)
        D("transfer", scm_from_utf8_symbol("nothing"));
    else if (meta->transfer == GIG_TRANSFER_CONTAINER)
        D("transfer", scm_from_utf8_symbol("container"));
    else if (meta->transfer == GIG_TRANSFER_EVERYTHING)
        D("transfer", scm_from_utf8_symbol("everything"));

    if (meta->n_params > 0) {
        SCM param_il = SCM_EOL;
        for (int i = 0; i < meta->n_params; i++) {
            param_il =
                scm_append(scm_list_2
                           (param_il, scm_list_1(type_meta_to_il(&(meta->params[i])))));
        }
        D("params", param_il);
    }
    else if (meta->arg_type == GIG_ARG_TYPE_CALLBACK)
        D("callable-info", gig_amap_to_il(meta->callable_arg_map));
    return scm_reverse(il);
}
