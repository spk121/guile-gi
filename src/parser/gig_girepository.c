// Copyright (C) 2023 Michael L. Gran

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

#include <stddef.h>
#include <libguile.h>
#include <girepository.h>

#ifdef GIR_DLL

#ifdef BUILDING_GIG
#define GIG_API __declspec(dllexport)
#else
#define GIG_API __declspec(dllexport)
#endif
#define GIG_LOCAL

#else

#define GIG_API __attribute__ ((visibility("default")))
#define GIG_LOCAL __attribute__ ((visibility("hidden")))

#endif

#if __STDC_VERSION__ < 202311L
#define nullptr (NULL)
#endif

////////////////////////////////////////////////////////////////
// GIBaseInfo Type
////////////////////////////////////////////////////////////////

static SCM gig_gibaseinfo_equalp (SCM x1, SCM x2);
static void gc_free_gibaseinfo (SCM x);

SCM gibaseinfo_fo_type = SCM_BOOL_F;

SCM
_scm_from_gibaseinfo (GIBaseInfo *c_info)
{
    return scm_make_foreign_object_1 (gibaseinfo_fo_type, c_info);
}


int
_scm_is_gibaseinfo (SCM x)
{
    if (SCM_IS_A_P (x, gibaseinfo_fo_type))
    {
        if (scm_foreign_object_ref (x, 0) != nullptr)
            return 1;
        else
            return 0;
    }
    else
        return 0;
}

GIBaseInfo *
_scm_to_gibaseinfo (SCM x)
{
    return scm_foreign_object_ref (x, 0);
}

SCM
gig_is_gibaseinfo_p (SCM x)
{
    return scm_from_bool (_scm_is_gibaseinfo (x));
}

SCM
gig_gibaseinfo_equalp (SCM x, SCM y)
{
    return scm_from_bool (_scm_to_gibaseinfo (x) == _scm_to_gibaseinfo (y));
}

static void
gc_free_gibaseinfo (SCM x)
{
    GIBaseInfo *info = scm_foreign_object_ref (x, 0);
    if (info != nullptr)
    {
        g_base_info_unref(info);
        scm_foreign_object_set_x (x, 0, nullptr);
    }
}

////////////////////////////////////////////////////////////////
// Utilities
////////////////////////////////////////////////////////////////

#define MAX_STRING_LIST_LEN 1000

static bool
is_ascii_alnum_dash_dot(char *str)
{
    size_t len = strlen(str);
    bool valid = true;
    for (size_t i = 0; i < len; i ++) {
        char c = str[i];
        if (!((c >= u8'0' && c <= u8'9')
              || (c >= u8'a' && c <= u8'z')
              || (c >= u8'A' && c <= u8'Z')
              || c == u8'-'
              || c == u8'.')) {
            valid = false;
            break;
        }
    }
    return valid;
}

static bool
is_valid_namespace_char(char c)
{
    return ((c >= u8'0' && c <= u8'9')
            || (c >= u8'a' && c <= u8'z')
            || (c >= u8'A' && c <= u8'Z')
            || c == u8'-'
            || c == u8'.');
}


static size_t
get_null_terminated_array_len(void **arr)
{
    size_t i = 0;
    while(arr[i] != nullptr) {
        i++;
        if (i == SIZE_MAX - 1)
            abort();
    }
    return i;
}

static size_t
strvlen(char **arr)
{
    size_t i = 0;
    while(arr[i] != nullptr) {
        i++;
        if (i == SIZE_MAX - 1)
            abort();
    }
    return i;
}

static void
free_arrayn(void **arr, size_t n)
{
    for (size_t i = 0; i < n; i ++)
        free(arr[i]);
    free(arr);
}

#define freev(x) _Generic((x),                 \
                          char: char8freev)(x)
static void
char8freev(char **arr)
{
    size_t i = 0;
    if (arr == nullptr)
        return;
    while(arr[i] != nullptr) {
        free(arr[i]);
        arr[i] = nullptr;
        i++;
    }
    free(arr);
    arr = nullptr;
}


static SCM
utf8_arrayn_to_scheme_list (char **c_strs, size_t n)
{
    SCM ret = SCM_EOL;

    for (size_t i = n; i > 0; i --) {
        ret = scm_cons(scm_from_utf8_string(c_strs[i - 1]),ret);
    }
}

typedef void(*scm_unwind_handler_t)(void *);

////////////////////////////////////////////////////////////////
// GIRepository functions

SCM
gig_irepository_get_dependencies(SCM s_namespace)
{
    char *c_namespace;
    gchar **c_dependencies;
    size_t len;
    SCM ret = SCM_EOL;

    scm_dynwind_begin(0);
    c_namespace = scm_to_utf8_string(s_namespace);
    scm_dynwind_free(c_namespace);

    c_dependencies = g_irepository_get_dependencies(NULL, c_namespace);
    scm_dynwind_unwind_handler((scm_unwind_handler_t) char8freev, c_dependencies, SCM_F_WIND_EXPLICITLY);
    len = strvlen(c_dependencies);
    if (len > 0)
        ret = utf8_arrayn_to_scheme_list(c_dependencies, len);
    scm_dynwind_end();

    return ret;
}

SCM
gig_irepository_get_immediate_dependencies(SCM s_namespace)
{
    char *c_namespace;
    gchar **c_dependencies;
    size_t len;
    SCM ret = SCM_EOL;

    scm_dynwind_begin(0);
    c_namespace = scm_to_utf8_string(s_namespace);
    scm_dynwind_free(c_namespace);
    c_dependencies = g_irepository_get_immediate_dependencies(NULL, c_namespace);
    scm_dynwind_unwind_handler((scm_unwind_handler_t) char8freev,
                               c_dependencies, SCM_F_WIND_EXPLICITLY);
    len = strvlen(c_dependencies);
    if (len > 0)
        ret = utf8_arrayn_to_scheme_list(c_dependencies, len);
    scm_dynwind_end();

    return ret;
}

SCM
gig_irepository_get_loaded_namespaces()
{
    gchar **c_namespaces;
    SCM ret = SCM_EOL;
    size_t len;

    scm_dynwind_begin(0);
    c_namespaces = g_irepository_get_loaded_namespaces(nullptr);
    scm_dynwind_unwind_handler((scm_unwind_handler_t) char8freev,
                               c_namespaces, SCM_F_WIND_EXPLICITLY);
    if (c_namespaces == nullptr)
        return SCM_EOL;
    len = strvlen(c_namespaces);
    if (len == 0) {
        return SCM_EOL;
    }
    ret = utf8_arrayn_to_scheme_list(c_namespaces, len);
    scm_dynwind_end();

    return ret;
}

SCM
gig_irepository_get_n_infos(SCM s_namespace)
{
    char *c_namespace = nullptr;
    int c_n;

    scm_dynwind_begin(0);
    c_namespace = scm_to_utf8_string(s_namespace);
    scm_dynwind_free(c_namespace);
    c_n = g_irepository_get_n_infos(nullptr, c_namespace);
    scm_dynwind_end();

    return scm_from_int (c_n);
}

SCM
gig_irepository_get_info(SCM s_namespace, SCM s_index)
{
    char *c_namespace;
    GIBaseInfo *c_info;
    int c_index;

    scm_dynwind_begin(0);
    c_namespace = scm_to_utf8_string(s_namespace);
    scm_dynwind_free(c_namespace);
    c_index = scm_to_int(s_index);
    c_info = g_irepository_get_info(nullptr, c_namespace, c_index);
    scm_dynwind_end();

    return _scm_from_gibaseinfo(c_info);
}

// g_irepository_get_options_group is not useful

SCM
gig_irepository_enumerate_versions(SCM s_namespace)
{
    char *c_namespace;
    GList *c_lst, *c_lst_cur;
    SCM ret = SCM_EOL;

    scm_dynwind_begin(0);
    c_namespace = scm_to_utf8_string(s_namespace);
    scm_dynwind_free(c_namespace);
    c_lst = g_irepository_enumerate_versions(nullptr, c_namespace);
    scm_dynwind_unwind_handler((scm_unwind_handler_t) g_list_free_full,
                               c_lst, SCM_F_WIND_EXPLICITLY);
    c_lst_cur = c_lst;
    while (c_lst_cur != nullptr) {
        ret = scm_cons(scm_from_utf8_string(c_lst_cur->data), ret);
        c_lst_cur = c_lst_cur->next;
    }
    scm_dynwind_end();
    return scm_reverse(ret);
}

SCM
gig_irepository_prepend_library_path(SCM s_directory)
{
    char *c_directory;

    scm_dynwind_begin(0);
    c_directory = scm_to_locale_string(s_directory);
    scm_dynwind_free(c_directory);
    // String is in locale encoding since it is a filename
    g_irepository_prepend_library_path(c_directory);
    scm_dynwind_end();

    return SCM_UNSPECIFIED;
}

SCM
gig_irepository_prepend_search_path(SCM s_directory)
{
    char *c_directory;

    scm_dynwind_begin(0);
    c_directory = scm_to_locale_string(s_directory);
    scm_dynwind_free(c_directory);
    // Using a locale string since it is a filename
    g_irepository_prepend_search_path(c_directory);
    scm_dynwind_end();

    return SCM_UNSPECIFIED;
}

SCM
gig_irepository_get_search_path()
{
    GSList *c_lst, *c_lst_cur;

    c_lst = g_irepository_get_search_path();

    SCM ret = SCM_EOL;
    c_lst_cur = c_lst;
    while (c_lst_cur != nullptr) {
        // A locale string since it is a filename
        ret = scm_cons(scm_from_locale_string(c_lst_cur->data),ret);
        c_lst_cur = c_lst_cur->next;
    }

    // Don't free c_lst: owned by libgirepository.
    return scm_reverse(ret);
}

// g_irepository_load_typelib is undocumented

SCM
gig_irepository_get_typelib_path(SCM s_namespace)
{
    char *c_namespace;
    const char *c_path;

    scm_dynwind_begin(0);
    // A locale string since it is a filename
    c_namespace = scm_to_locale_string(s_namespace);
    scm_dynwind_free(c_namespace);
    c_path = g_irepository_get_typelib_path(nullptr, c_namespace);
    scm_dynwind_end();

    if (c_path == nullptr)
        return SCM_BOOL_F;
    return scm_from_locale_string(c_path);
}

SCM
gig_irepository_is_registered(SCM s_namespace, SCM s_version)
{
    char *c_namespace, *c_version;
    gboolean c_ret;

    scm_dynwind_begin(0);
    c_namespace = scm_to_utf8_string(s_namespace);
    scm_dynwind_free(c_namespace);
    if (scm_is_true(s_version))
    {
        c_version = scm_to_utf8_string(s_version);
        c_ret = g_irepository_is_registered(nullptr,
                                            c_namespace,
                                            c_version);
        free(c_version);
    }
    else
    {
        c_ret = g_irepository_is_registered(nullptr,
                                            c_namespace,
                                            NULL);
    }
    scm_dynwind_end();

    return scm_from_bool(c_ret);
}

SCM
gig_irepository_require(SCM s_namespace, SCM s_version, SCM s_flags)
{
    char *c_namespace, *c_version = nullptr;
    int c_flags;
    GError *error = nullptr;

    scm_dynwind_begin(0);
    c_namespace = scm_to_utf8_string(s_namespace);
    scm_dynwind_free(c_namespace);

    c_flags = scm_to_int(s_flags);

    if (scm_is_false(s_version))
        g_irepository_require(nullptr, c_namespace, nullptr, c_flags, &error);
    else {
        c_version = scm_to_utf8_string(s_version);
        scm_dynwind_free(c_version);
        g_irepository_require(nullptr,
                              c_namespace, c_version, c_flags, &error);
    }

    if (error != nullptr) {
        SCM err = scm_from_utf8_string(error->message);
        g_error_free(error);
        scm_misc_error("%require", "~A", scm_list_1(err));
    }
    // Not returning GITypelib since they are useless
    scm_dynwind_end();
    return SCM_UNSPECIFIED;
}

SCM
gig_irepository_require_private(SCM s_typelib_dir,
                                SCM s_namespace, SCM s_version, SCM s_flags)
{
    char *c_typelib_dir, *c_namespace, *c_version;
    int c_flags;
    GError *error = nullptr;

    scm_dynwind_begin(0);
    c_typelib_dir = scm_to_utf8_string(s_typelib_dir);
    scm_dynwind_free(c_typelib_dir);
    c_namespace = scm_to_utf8_string(s_namespace);
    scm_dynwind_free(c_namespace);
    c_version = scm_to_utf8_string(s_namespace);
    scm_dynwind_free(c_version);
    c_flags = scm_to_int(s_flags);

    g_irepository_require_private(nullptr,
                                  c_typelib_dir,
                                  c_namespace, c_version, c_flags, &error);

    if (error != nullptr) {
        SCM err = scm_from_utf8_string(error->message);
        g_error_free(error);
        scm_misc_error("%require-private", "~A", scm_list_1(err));
    }

    scm_dynwind_end();
    // Not returning GITypelib since they are useless
    return SCM_UNSPECIFIED;
}

SCM
gig_irepository_get_c_prefix(SCM s_namespace)
{
    char *c_namespace;
    const char *c_ret;

    scm_dynwind_begin(0);
    c_namespace = scm_to_utf8_string(s_namespace);
    scm_dynwind_free(c_namespace);
    c_ret = g_irepository_get_c_prefix(nullptr, c_namespace);
    scm_dynwind_end();

    if (c_ret == nullptr)
        return SCM_BOOL_F;
    return scm_from_utf8_string(c_ret);
}

SCM
gig_irepository_get_shared_library(SCM s_namespace)
{
    char *c_namespace;
    const char *c_ret;

    scm_dynwind_begin(0);
    c_namespace = scm_to_utf8_string(s_namespace);
    scm_dynwind_free(c_namespace);
    c_ret = g_irepository_get_shared_library(nullptr, c_namespace);
    scm_dynwind_end();

    if (c_ret == nullptr)
        return SCM_BOOL_F;
    return scm_from_utf8_string(c_ret);
}

SCM
gig_irepository_get_version(SCM s_namespace)
{
    char *c_namespace;
    const char *c_ret;

    scm_dynwind_begin(0);
    c_namespace = scm_to_utf8_string(s_namespace);
    scm_dynwind_free(c_namespace);
    c_ret = g_irepository_get_version(nullptr, c_namespace);
    scm_dynwind_end();

    if (c_ret == nullptr)
        return SCM_BOOL_F;
    return scm_from_utf8_string(c_ret);
}

SCM
gig_irepository_find_by_gtype (SCM s_gtype)
{
    GType c_gtype = scm_to_size_t(s_gtype);
    GIBaseInfo *c_info;
    c_info = g_irepository_find_by_gtype(nullptr, c_gtype);
    if (c_info != nullptr)
        return _scm_from_gibaseinfo(c_info);
    return SCM_BOOL_F;
}

SCM
gig_irepository_find_by_error_domain (SCM s_quark)
{
    GQuark c_quark = scm_to_size_t(s_quark);
    GIEnumInfo *c_info;
    c_info = g_irepository_find_by_error_domain(nullptr, c_quark);
    if (c_info != nullptr)
        return _scm_from_gibaseinfo(c_info);
    return SCM_BOOL_F;
}

SCM
gig_irepository_find_by_name(SCM s_namespace, SCM s_name)
{
    char *c_namespace, *c_name;
    GIBaseInfo *c_info;

    scm_dynwind_begin(0);
    c_namespace = scm_to_utf8_string(s_namespace);
    scm_dynwind_free(c_namespace);
    c_name = scm_to_utf8_string(s_name);
    scm_dynwind_free(c_name);
    c_info = g_irepository_find_by_name(nullptr,
                                        c_namespace,
                                        c_name);
    scm_dynwind_end();
    if (c_info != nullptr)
        return _scm_from_gibaseinfo(c_info);
    return SCM_BOOL_F;
}

SCM
gig_irepository_get_object_gtype_interfaces(SCM s_gtype)
{
    GType c_gtype = scm_to_size_t(s_gtype);
    unsigned n;
    GIInterfaceInfo **infos;

    g_irepository_get_object_gtype_interfaces (nullptr, c_gtype, &n, &infos);
    SCM ret = SCM_EOL;

    for (unsigned i = 0; i < n; i ++) {
        ret = scm_cons (_scm_from_gibaseinfo(infos[i]), ret);
        i++;
    }
    return scm_reverse(ret);
}

// g_irepository_dump is not useful

// gi_cclosure_marshal_generic is not useful

SCM gig_IREPOSITORY_LOAD_FLAG_LAZY;

// G_IREPOSITORY_ERROR not useful because we do all the GError
// handling here.

// enum GIRepositoryError not useful because we do all the GError
// handling here.

////////////////////////////////////////////////////////////////
// Version info
////////////////////////////////////////////////////////////////

SCM
gig_get_major_version()
{
    return scm_from_int(gi_get_major_version());
}

SCM
gig_get_minor_version()
{
    return scm_from_int(gi_get_minor_version());
}

SCM
gig_get_micro_version()
{
    return scm_from_int(gi_get_micro_version());
}

SCM
gig_check_version(SCM s_major, SCM s_minor, SCM s_micro)
{
    int c_major = scm_to_int(s_major);
    int c_minor = scm_to_int(s_minor);
    int c_micro = scm_to_int(s_micro);
    return scm_from_bool(GI_CHECK_VERSION(c_major, c_minor, c_micro));
}

SCM gig_MAJOR_VERSION;
SCM gig_MINOR_VERSION;
SCM gig_MICRO_VERSION;

////////////////////////////////////////////////////////////////
// Common Types
////////////////////////////////////////////////////////////////

#if GI_CHECK_VERSION(1,72,0)
SCM
gig_type_tag_is_basic(SCM s_tag)
{
    if (GI_TYPE_TAG_IS_BASIC(scm_to_int(s_tag)))
        return SCM_BOOL_T;
    return SCM_BOOL_F;
}
#endif

#if GI_CHECK_VERSION(1,72,0)
SCM
gig_type_tag_is_container(SCM s_tag)
{
    if (GI_TYPE_TAG_IS_CONTAINER(scm_to_int(s_tag)))
        return SCM_BOOL_T;
    return SCM_BOOL_F;
}
#endif

#if GI_CHECK_VERSION(1,72,0)
SCM
gig_type_tag_is_numeric(SCM s_tag)
{
    if (GI_TYPE_TAG_IS_NUMERIC(scm_to_int(s_tag)))
        return SCM_BOOL_T;
    return SCM_BOOL_F;
}
#endif

SCM
gig_type_tag_to_string(SCM s_tag)
{
    return scm_from_utf8_string(g_type_tag_to_string(scm_to_int(s_tag)));
}

SCM gig_TYPE_TAG_VOID;
SCM gig_TYPE_TAG_BOOLEAN;
SCM gig_TYPE_TAG_INT8;
SCM gig_TYPE_TAG_UINT8;
SCM gig_TYPE_TAG_INT16;
SCM gig_TYPE_TAG_UINT16;
SCM gig_TYPE_TAG_INT32;
SCM gig_TYPE_TAG_UINT32;
SCM gig_TYPE_TAG_INT64;
SCM gig_TYPE_TAG_UINT64;
SCM gig_TYPE_TAG_FLOAT;
SCM gig_TYPE_TAG_DOUBLE;
SCM gig_TYPE_TAG_GTYPE;
SCM gig_TYPE_TAG_UTF8;
SCM gig_TYPE_TAG_FILENAME;
SCM gig_TYPE_TAG_ARRAY;
SCM gig_TYPE_TAG_INTERFACE;
SCM gig_TYPE_TAG_GLIST;
SCM gig_TYPE_TAG_GSLIST;
SCM gig_TYPE_TAG_GHASH;
SCM gig_TYPE_TAG_ERROR;
SCM gig_TYPE_TAG_UNICHAR;
SCM gig_ARRAY_TYPE_C;
SCM gig_ARRAY_TYPE_ARRAY;
SCM gig_ARRAY_TYPE_PTR_ARRAY;
SCM gig_ARRAY_TYPE_BYTE_ARRAY;
SCM gig_TYPE_TAG_N_TYPES;

////////////////////////////////////////////////////////////////
// GIBaseInfo
////////////////////////////////////////////////////////////////

SCM
gig_base_info_equal(SCM s_info1, SCM s_info2)
{
    GIBaseInfo *c_info1 = _scm_to_gibaseinfo(s_info1);
    GIBaseInfo *c_info2 = _scm_to_gibaseinfo(s_info2);
    return scm_from_bool(g_base_info_equal(c_info1, c_info2));
}

SCM
gig_base_info_get_type(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    GIInfoType c_infotype = g_base_info_get_type(c_info);
    return scm_from_int(c_infotype);
}

// g_base_info_get_typelib is useless

SCM
gig_base_info_get_namespace(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    const char *c_namespace = g_base_info_get_namespace(c_info);
    return scm_from_utf8_string(c_namespace);
}

SCM
gig_base_info_get_name(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    const char *c_name = g_base_info_get_name(c_info);
    return scm_from_utf8_string(c_name);
}

SCM
gig_base_info_get_attribute(SCM s_info, SCM s_name)
{
    char *c_name;
    const char *c_attribute;
    GIBaseInfo *c_info;

    c_info = _scm_to_gibaseinfo(s_info);
    scm_dynwind_begin(0);
    c_name = scm_to_utf8_string(s_name);
    scm_dynwind_free(c_name);
    c_attribute = g_base_info_get_attribute(c_info, c_name);
    scm_dynwind_end();

    if (c_attribute == nullptr)
        return SCM_BOOL_F;
    return scm_from_utf8_string(c_attribute);
}

SCM
gig_base_info_get_attributes (SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    GIAttributeIter iter = { 0, };
    char *name;
    char *value;
    SCM ret = SCM_EOL;
    while (g_base_info_iterate_attributes (c_info, &iter, &name, &value))
    {
        ret = scm_cons(scm_cons(scm_from_utf8_string(name),
                                scm_from_utf8_string(value)),
                       ret);
    }
    return scm_reverse(ret);
}

SCM
gig_base_info_get_container(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    GIBaseInfo *c_info2 = g_base_info_get_container(c_info);
    return _scm_from_gibaseinfo(g_base_info_ref(c_info2));
}

SCM
gig_base_info_is_deprecated(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(g_base_info_is_deprecated(c_info));
}

SCM
gig_info_type_to_string(SCM s_type)
{
    GIInfoType c_type = scm_to_int(s_type);
    return scm_from_utf8_string(g_info_type_to_string(c_type));
}

SCM gig_INFO_TYPE_INVALID;
SCM gig_INFO_TYPE_FUNCTION;
SCM gig_INFO_TYPE_CALLBACK;
SCM gig_INFO_TYPE_STRUCT;
SCM gig_INFO_TYPE_BOXED;
SCM gig_INFO_TYPE_ENUM;
SCM gig_INFO_TYPE_FLAGS;
SCM gig_INFO_TYPE_OBJECT;
SCM gig_INFO_TYPE_INTERFACE;
SCM gig_INFO_TYPE_CONSTANT;
SCM gig_INFO_TYPE_INVALID_0;
SCM gig_INFO_TYPE_UNION;
SCM gig_INFO_TYPE_VALUE;
SCM gig_INFO_TYPE_SIGNAL;
SCM gig_INFO_TYPE_VFUNC;
SCM gig_INFO_TYPE_PROPERTY;
SCM gig_INFO_TYPE_FIELD;
SCM gig_INFO_TYPE_ARG;
SCM gig_INFO_TYPE_TYPE;
SCM gig_INFO_TYPE_UNRESOLVED;

////////////////////////////////////////////////////////////////
// GICallableInfo
////////////////////////////////////////////////////////////////

SCM
gig_is_callable_info(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(GI_IS_CALLABLE_INFO(c_info));
}

SCM
gig_callable_info_can_throw_gerror(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(g_callable_info_can_throw_gerror(c_info));
}

SCM
gig_callable_info_get_n_args(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_int(g_callable_info_get_n_args(c_info));
}

SCM
gig_callable_info_get_arg(SCM s_info, SCM s_n)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    int c_n = scm_to_int(s_n);
    return _scm_from_gibaseinfo(g_callable_info_get_arg(c_info, c_n));
}

SCM
gig_callable_info_get_caller_owns(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_int(g_callable_info_get_caller_owns(c_info));
}

SCM
gig_callable_info_get_instance_ownership_transfer(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    int c_ret = g_callable_info_get_instance_ownership_transfer(c_info);
    return scm_from_int(c_ret);
}

SCM
gig_callable_info_get_return_attribute(SCM s_info, SCM s_name)
{
    char *c_name;
    const char *c_attr;
    GIBaseInfo *c_info;

    c_info = _scm_to_gibaseinfo(s_info);
    scm_dynwind_begin(0);
    c_name = scm_to_utf8_string(s_name);
    scm_dynwind_free(c_name);
    c_attr = g_callable_info_get_return_attribute(c_info, c_name);
    scm_dynwind_end();

    if (c_attr == nullptr)
        return SCM_BOOL_F;
    return scm_from_utf8_string(c_attr);
}

SCM
gig_callable_info_get_return_type(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return _scm_from_gibaseinfo(g_callable_info_get_return_type(c_info));
}

// g_callable_info_invoke is not necessary

SCM
gig_callable_info_is_method(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(g_callable_info_is_method(c_info));
}

SCM
gig_callable_info_get_return_attributes (SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    GIAttributeIter iter = { 0, };
    char *name;
    char *value;
    SCM ret = SCM_EOL;
    while (g_callable_info_iterate_return_attributes (c_info,
                                                      &iter, &name, &value))
    {
        ret = scm_cons(scm_cons(scm_from_utf8_string(name),
                                scm_from_utf8_string(value)),
                       ret);
    }
    return scm_reverse(ret);
}

// g_callable_info_load_arg is not good for binding
// g_callable_info_load_return_type is not good for binding

SCM
gig_callable_info_may_return_null(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(g_callable_info_may_return_null(c_info));
}

SCM
gig_callable_info_skip_return(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(g_callable_info_skip_return(c_info));
}

// g_callable_info_create_closure is unnecessary
// g_callable_info_destroy_closure is unnecessary
// g_callable_info_get_closure_native_address is unnecessary

////////////////////////////////////////////////////////////////
// GIFunctionInfo
////////////////////////////////////////////////////////////////

SCM
gig_is_function_info(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(GI_IS_FUNCTION_INFO(c_info));
}

SCM
gig_function_info_get_flags(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_int(g_function_info_get_flags(c_info));
}

SCM
gig_function_info_get_property(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    GIBaseInfo *c_ret = g_function_info_get_property(c_info);
    if (c_ret == nullptr)
        return SCM_BOOL_F;
    return _scm_from_gibaseinfo(c_info);
}

SCM
gig_function_info_get_symbol(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    const char *c_ret = g_function_info_get_symbol(c_info);
    if (c_ret == nullptr)
        return SCM_BOOL_F;
    return scm_from_utf8_string(c_ret);
}

SCM
gig_function_info_get_vfunc(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    GIBaseInfo *c_ret = g_function_info_get_vfunc(c_info);
    if (c_ret == nullptr)
        return SCM_BOOL_F;
    return _scm_from_gibaseinfo(c_info);
}

// g_function_info_invoke is not necessary
// g_invoke_error_quark is unnecessary

SCM gig_FUNCTION_IS_METHOD;
SCM gig_FUNCTION_IS_CONSTRUCTOR;
SCM gig_FUNCTION_IS_GETTER;
SCM gig_FUNCTION_IS_SETTER;
SCM gig_FUNCTION_VRAPS_VFUNC;
SCM gig_FUNCTION_THROWS;

////////////////////////////////////////////////////////////////
// GISignalInfo
////////////////////////////////////////////////////////////////

SCM
gig_is_signal_info(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(GI_IS_SIGNAL_INFO(c_info));
}

SCM
gig_signal_info_get_flags(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_int(g_signal_info_get_flags(c_info));
}

SCM
gig_signal_info_get_class_closure(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    GIBaseInfo *c_ret = g_signal_info_get_class_closure(c_info);
    if (c_ret == nullptr)
        return SCM_BOOL_F;
    return _scm_from_gibaseinfo(c_info);
}

SCM
gig_signal_info_true_stops_emit(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(g_signal_info_true_stops_emit(c_info));
}

SCM gig_SIGNAL_RUN_FIRST;
SCM gig_SIGNAL_RUN_LAST;
SCM gig_SIGNAL_RUN_CLEANUP;
SCM gig_SIGNAL_NO_RECURSE;
SCM gig_SIGNAL_DETAILED;
SCM gig_SIGNAL_ACTION;
SCM gig_SIGNAL_NO_HOOKS;
SCM gig_SIGNAL_MUST_COLLECT;
SCM gig_SIGNAL_DEPRECATED;
SCM gig_SIGNAL_ACCUMULATOR_FIRST_RUN;


////////////////////////////////////////////////////////////////
// GIVFuncInfo
////////////////////////////////////////////////////////////////

SCM
gig_is_vfunc_info(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(GI_IS_VFUNC_INFO(c_info));
}


SCM
gig_vfunc_info_get_flags(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_int(g_vfunc_info_get_flags(c_info));
}

SCM
gig_vfunc_info_get_offset(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_int(g_vfunc_info_get_flags(c_info));
}

SCM
gig_vfunc_info_get_signal(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    GIBaseInfo *c_ret = g_vfunc_info_get_signal(c_info);
    if (c_ret == nullptr)
        return SCM_BOOL_F;
    return _scm_from_gibaseinfo(c_ret);
}

SCM
gig_vfunc_info_get_invoker(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    GIBaseInfo *c_ret = g_vfunc_info_get_invoker(c_info);
    if (c_ret == nullptr)
        return SCM_BOOL_F;
    return _scm_from_gibaseinfo(c_ret);
}

// g_vfunc_info_get_address is not useful
// g_vfunc_info_invoke is not useful

SCM gig_VFUNC_MUST_CHAIN_UP;
SCM gig_VFUNC_MUST_OVERRIDE;
SCM gig_VFUNC_MUST_NOT_OVERRIDE;
SCM gig_VFUNC_THROWS;

////////////////////////////////////////////////////////////////
// GIRegisteredTypeInfo
////////////////////////////////////////////////////////////////

SCM
gig_is_registered_type_info(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(GI_IS_REGISTERED_TYPE_INFO(c_info));
}

SCM
gig_registered_type_info_get_type_name(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    const char *c_name = g_registered_type_info_get_type_name(c_info);
    if (c_name == nullptr)
        return SCM_BOOL_F;
    return scm_from_utf8_string(c_name);
}

SCM
gig_registered_type_info_get_type_init(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    const char *c_name = g_registered_type_info_get_type_init(c_info);
    if (c_name == nullptr)
        return SCM_BOOL_F;
    return scm_from_utf8_string(c_name);
}

// g_registered_type_info_get_g_type is not useful because GTypes are
// not constant. But knowing if it has a GType is useful.
SCM
gig_registered_type_info_get_g_type(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    GType type = g_registered_type_info_get_g_type(c_info);
    return scm_from_size_t(type);
}


////////////////////////////////////////////////////////////////
// GIEnumInfo
////////////////////////////////////////////////////////////////

SCM
gig_is_enum_info(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(GI_IS_ENUM_INFO(c_info));
}

SCM
gig_enum_info_get_n_values(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_int(g_enum_info_get_n_values(c_info));
}

SCM
gig_enum_info_get_value(SCM s_info, SCM s_n)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    int c_n = scm_to_int(s_n);
    GIBaseInfo *c_ret;
    c_ret = g_enum_info_get_value(c_info, c_n);
    if (c_ret == nullptr)
        return SCM_BOOL_F;
    return _scm_from_gibaseinfo(c_ret);
}

SCM
gig_enum_info_get_n_methods(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_int(g_enum_info_get_n_methods(c_info));
}

SCM
gig_enum_info_get_method(SCM s_info, SCM s_n)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    int c_n = scm_to_int(s_n);
    GIBaseInfo *c_ret;
    c_ret = g_enum_info_get_method(c_info, c_n);
    if (c_ret == nullptr)
        return SCM_BOOL_F;
    return _scm_from_gibaseinfo(c_info);
}

SCM
gig_enum_info_get_storage_type(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return _scm_from_gibaseinfo(c_info);
}

SCM
gig_enum_info_get_error_domain(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    const char *c_ret = g_enum_info_get_error_domain(c_info);
    if (c_ret == nullptr)
        return SCM_BOOL_F;
    return scm_from_utf8_string(c_ret);
}

////////////////////////////////////////////////////////////////
// GIValueInfo
////////////////////////////////////////////////////////////////

SCM
gig_is_value_info(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(GI_IS_VALUE_INFO(c_info));
}

SCM
gig_value_info_get_value(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    gint64 c_ret = g_value_info_get_value(c_info);
    return scm_from_int64(c_ret);
}



////////////////////////////////////////////////////////////////
// GIStructInfo
////////////////////////////////////////////////////////////////

SCM
gig_is_struct_info(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(GI_IS_STRUCT_INFO(c_info));
}

SCM
gig_struct_info_find_field(SCM s_info, SCM s_name)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    char *c_name = scm_to_utf8_string(s_name);
    GIBaseInfo *c_ret;
    c_ret = g_struct_info_find_field(c_info, c_name);
    free(c_name);
    if (c_ret == nullptr)
        return SCM_BOOL_F;
    return _scm_from_gibaseinfo(c_ret);
}

SCM
gig_struct_info_get_alignment(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    size_t c_ret = g_struct_info_get_alignment(c_info);
    return scm_from_size_t(c_ret);
}

SCM
gig_struct_info_get_size(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    size_t c_ret = g_struct_info_get_size(c_info);
    return scm_from_size_t(c_ret);
}

SCM
gig_struct_info_is_gtype_struct(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(g_struct_info_is_gtype_struct(c_info));
}

SCM
gig_struct_info_is_foreign(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(g_struct_info_is_foreign(c_info));
}

SCM
gig_struct_info_get_n_fields(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_int(g_struct_info_get_n_fields(c_info));
}

SCM
gig_struct_info_get_field(SCM s_info, SCM s_n)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    int c_n = scm_to_int(s_n);
    GIBaseInfo *c_ret;
    c_ret = g_struct_info_get_field(c_info, c_n);
    if (c_ret == nullptr)
        return SCM_BOOL_F;
    return _scm_from_gibaseinfo(c_ret);
}

SCM
gig_struct_info_get_n_methods(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_int(g_struct_info_get_n_methods(c_info));
}

SCM
gig_struct_info_get_method(SCM s_info, SCM s_n)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    int c_n = scm_to_int(s_n);
    GIBaseInfo *c_ret;

    // FIXME: put in range checks elsewhere?
    if (c_n >= g_struct_info_get_n_methods(c_info))
        return SCM_BOOL_F;
    c_ret = g_struct_info_get_method(c_info, c_n);
    if (c_ret == nullptr)
        return SCM_BOOL_F;
    return _scm_from_gibaseinfo(c_ret);
}

SCM
gig_struct_info_find_method(SCM s_info, SCM s_name)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    char *c_name = scm_to_utf8_string(s_name);
    GIBaseInfo *c_ret;
    c_ret = g_struct_info_find_method(c_info, c_name);
    free(c_name);
    if (c_ret == nullptr)
        return SCM_BOOL_F;
    return _scm_from_gibaseinfo(c_ret);
}

////////////////////////////////////////////////////////////////
// GIUnionInfo
////////////////////////////////////////////////////////////////

SCM
gig_is_union_info(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(GI_IS_UNION_INFO(c_info));
}

SCM
gig_union_info_get_n_fields(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_int(g_union_info_get_n_fields(c_info));
}

SCM
gig_union_info_get_field(SCM s_info, SCM s_n)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    int c_n = scm_to_int(s_n);
    GIBaseInfo *c_ret;
    c_ret = g_union_info_get_field(c_info, c_n);
    if (c_ret == nullptr)
        return SCM_BOOL_F;
    return _scm_from_gibaseinfo(c_ret);
}

SCM
gig_union_info_get_n_methods(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_int(g_union_info_get_n_methods(c_info));
}

SCM
gig_union_info_get_method(SCM s_info, SCM s_n)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    int c_n = scm_to_int(s_n);
    GIBaseInfo *c_ret;
    c_ret = g_union_info_get_method(c_info, c_n);
    if (c_ret == nullptr)
        return SCM_BOOL_F;
    return _scm_from_gibaseinfo(c_ret);
}

SCM
gig_union_info_is_discriminated(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(g_union_info_is_discriminated(c_info));
}

SCM
gig_union_info_get_discriminator_offset(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_int(g_union_info_get_discriminator_offset(c_info));
}

SCM
gig_union_info_get_discriminator_type(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return _scm_from_gibaseinfo(g_union_info_get_discriminator_type(c_info));
}

SCM
gig_union_info_get_discriminator(SCM s_info, SCM s_n)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    int c_n = scm_to_int(s_n);
    return _scm_from_gibaseinfo(g_union_info_get_discriminator(c_info, c_n));
}

SCM
gig_union_info_find_method(SCM s_info, SCM s_name)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    char *c_name = scm_to_utf8_string(s_name);
    GIBaseInfo *c_ret;
    c_ret = g_union_info_find_method(c_info, c_name);
    free(c_name);
    if (c_ret == nullptr)
        return SCM_BOOL_F;
    return _scm_from_gibaseinfo(c_ret);
}

SCM
gig_union_info_get_alignment(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    size_t c_ret = g_union_info_get_alignment(c_info);
    return scm_from_size_t(c_ret);
}

SCM
gig_union_info_get_size(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    size_t c_ret = g_union_info_get_size(c_info);
    return scm_from_size_t(c_ret);
}

////////////////////////////////////////////////////////////////
// GIObjectInfo
////////////////////////////////////////////////////////////////

SCM
gig_is_object_info(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(GI_IS_OBJECT_INFO(c_info));
}

SCM gig_object_info_get_abstract(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(g_object_info_get_abstract(c_info));
}

SCM
gig_object_info_get_fundamental(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(g_object_info_get_fundamental(c_info));
}

SCM
gig_object_info_get_final(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(g_object_info_get_final(c_info));
}

SCM
gig_object_info_get_parent(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    GIBaseInfo *c_ret = g_object_info_get_parent(c_info);
    return _scm_from_gibaseinfo(c_ret);
}

SCM
gig_object_info_get_type_name(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_utf8_string(g_object_info_get_type_name(c_info));
}

SCM
gig_object_info_get_type_init(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_utf8_string(g_object_info_get_type_init(c_info));
}

SCM
gig_object_info_get_n_constants(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);

    int c_n = g_object_info_get_n_constants(c_info);
    return scm_from_int (c_n);
}

SCM
gig_object_info_get_constant(SCM s_info, SCM s_index)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    int c_index = scm_to_int(s_index);
    GIBaseInfo *c_ret;

    c_ret = g_object_info_get_constant(c_info, c_index);
    return _scm_from_gibaseinfo(c_ret);
}

SCM
gig_object_info_get_n_fields(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);

    int c_n = g_object_info_get_n_fields(c_info);
    return scm_from_int (c_n);
}

SCM
gig_object_info_get_field(SCM s_info, SCM s_index)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    int c_index = scm_to_int(s_index);
    GIBaseInfo *c_ret;

    c_ret = g_object_info_get_field(c_info, c_index);
    return _scm_from_gibaseinfo(c_ret);
}

SCM
gig_object_info_get_n_interfaces(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);

    int c_n = g_object_info_get_n_interfaces(c_info);
    return scm_from_int (c_n);
}

SCM
gig_object_info_get_interface(SCM s_info, SCM s_index)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    int c_index = scm_to_int(s_index);
    GIBaseInfo *c_ret;

    c_ret = g_object_info_get_interface(c_info, c_index);
    return _scm_from_gibaseinfo(c_ret);
}

SCM
gig_object_info_get_n_methods(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);

    int c_n = g_object_info_get_n_methods(c_info);
    return scm_from_int (c_n);
}

SCM
gig_object_info_get_method(SCM s_info, SCM s_index)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    int c_index = scm_to_int(s_index);
    GIBaseInfo *c_ret;

    c_ret = g_object_info_get_method(c_info, c_index);
    return _scm_from_gibaseinfo(c_ret);
}

SCM
gig_object_info_find_method(SCM s_info, SCM s_name)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    char *c_name = scm_to_utf8_string(s_name);
    GIBaseInfo *c_ret;
    c_ret = g_object_info_find_method(c_info, c_name);
    free(c_name);
    if (c_ret == nullptr)
        return SCM_BOOL_F;
    return _scm_from_gibaseinfo(c_ret);
}

SCM
gig_object_info_find_method_using_interfaces(SCM s_info, SCM s_name)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    char *c_name = scm_to_utf8_string(s_name);
    GIBaseInfo *c_ret;
    GIObjectInfo *c_implementor;
    c_ret = g_object_info_find_method_using_interfaces(c_info, c_name, &c_implementor);
    free(c_name);
    if (c_ret == nullptr)
        return scm_cons(SCM_BOOL_F, SCM_BOOL_F);
    return scm_cons(_scm_from_gibaseinfo(c_ret), _scm_from_gibaseinfo(c_implementor));
}

SCM
gig_object_info_get_n_properties(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);

    int c_n = g_object_info_get_n_properties(c_info);
    return scm_from_int (c_n);
}

SCM
gig_object_info_get_property(SCM s_info, SCM s_index)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    int c_index = scm_to_int(s_index);
    GIBaseInfo *c_ret;

    c_ret = g_object_info_get_property(c_info, c_index);
    return _scm_from_gibaseinfo(c_ret);
}

SCM
gig_object_info_get_n_signals(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);

    int c_n = g_object_info_get_n_signals(c_info);
    return scm_from_int (c_n);
}

SCM
gig_object_info_get_signal(SCM s_info, SCM s_index)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    int c_index = scm_to_int(s_index);
    GIBaseInfo *c_ret;

    c_ret = g_object_info_get_signal(c_info, c_index);
    return _scm_from_gibaseinfo(c_ret);
}

SCM
gig_object_info_find_signal(SCM s_info, SCM s_name)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    char *c_name = scm_to_utf8_string(s_name);
    GIBaseInfo *c_ret;
    c_ret = g_object_info_find_signal(c_info, c_name);
    free(c_name);
    if (c_ret == nullptr)
        return SCM_BOOL_F;
    return _scm_from_gibaseinfo(c_ret);
}

SCM
gig_object_info_get_n_vfuncs(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);

    int c_n = g_object_info_get_n_vfuncs(c_info);
    return scm_from_int (c_n);
}

SCM
gig_object_info_get_vfunc(SCM s_info, SCM s_index)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    int c_index = scm_to_int(s_index);
    GIBaseInfo *c_ret;

    c_ret = g_object_info_get_vfunc(c_info, c_index);
    return _scm_from_gibaseinfo(c_ret);
}

SCM
gig_object_info_find_vfunc(SCM s_info, SCM s_name)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    char *c_name = scm_to_utf8_string(s_name);
    GIBaseInfo *c_ret;
    c_ret = g_object_info_find_vfunc(c_info, c_name);
    free(c_name);
    if (c_ret == nullptr)
        return SCM_BOOL_F;
    return _scm_from_gibaseinfo(c_ret);
}

SCM
gig_object_info_find_vfunc_using_interfaces(SCM s_info, SCM s_name)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    char *c_name = scm_to_utf8_string(s_name);
    GIBaseInfo *c_ret;
    GIObjectInfo *c_implementor;
    c_ret = g_object_info_find_vfunc_using_interfaces(c_info, c_name, &c_implementor);
    free(c_name);
    if (c_ret == nullptr)
        return scm_cons(SCM_BOOL_F, SCM_BOOL_F);
    return scm_cons(_scm_from_gibaseinfo(c_ret), _scm_from_gibaseinfo(c_implementor));
}

SCM
gig_object_info_get_class_struct(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    GIStructInfo *c_struct = g_object_info_get_class_struct(c_info);
    if (c_struct == nullptr)
        return SCM_BOOL_F;
    return _scm_from_gibaseinfo(c_struct);
}

SCM
gig_object_info_get_ref_function(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_utf8_string(g_object_info_get_ref_function(c_info));
}

// g_object_info_get_ref_function_pointer is not useful

SCM
gig_object_info_get_unref_function(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_utf8_string(g_object_info_get_unref_function(c_info));
}

// g_object_info_get_unref_function_pointer is not useful

SCM
gig_object_info_get_set_value_function(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_utf8_string(g_object_info_get_set_value_function(c_info));
}

// g_object_info_get_set_value_function_pointer is not useful

SCM
gig_object_info_get_get_value_function(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_utf8_string(g_object_info_get_get_value_function(c_info));
}

// g_object_info_get_get_value_function_pointer is not useful

////////////////////////////////////////////////////////////////
// GIInterfaceInfo
////////////////////////////////////////////////////////////////

SCM
gig_is_interface_info(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(GI_IS_INTERFACE_INFO(c_info));
}

SCM
gig_interface_info_get_n_prerequisites(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);

    int c_n = g_interface_info_get_n_prerequisites(c_info);
    return scm_from_int (c_n);
}

SCM
gig_interface_info_get_prerequisite(SCM s_info, SCM s_index)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    int c_index = scm_to_int(s_index);
    GIBaseInfo *c_ret;

    c_ret = g_interface_info_get_prerequisite(c_info, c_index);
    return _scm_from_gibaseinfo(c_ret);
}

SCM
gig_interface_info_get_n_properties(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);

    int c_n = g_interface_info_get_n_properties(c_info);
    return scm_from_int (c_n);
}

SCM
gig_interface_info_get_property(SCM s_info, SCM s_index)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    int c_index = scm_to_int(s_index);
    GIBaseInfo *c_ret;

    c_ret = g_interface_info_get_property(c_info, c_index);
    return _scm_from_gibaseinfo(c_ret);
}

SCM
gig_interface_info_get_n_methods(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_int(g_interface_info_get_n_methods(c_info));
}

SCM
gig_interface_info_get_method(SCM s_info, SCM s_n)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    int c_n = scm_to_int(s_n);
    GIBaseInfo *c_ret;
    c_ret = g_interface_info_get_method(c_info, c_n);
    if (c_info == nullptr)
        return SCM_BOOL_F;
    return _scm_from_gibaseinfo(c_ret);
}

SCM
gig_interface_info_find_method(SCM s_info, SCM s_name)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    char *c_name = scm_to_utf8_string(s_name);
    GIBaseInfo *c_ret;
    c_ret = g_interface_info_find_method(c_info, c_name);
    free(c_name);
    if (c_ret == nullptr)
        return SCM_BOOL_F;
    return _scm_from_gibaseinfo(c_ret);
}

SCM
gig_interface_info_get_n_signals(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);

    int c_n = g_interface_info_get_n_signals(c_info);
    return scm_from_int (c_n);
}

SCM
gig_interface_info_get_signal(SCM s_info, SCM s_index)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    int c_index = scm_to_int(s_index);
    GIBaseInfo *c_ret;

    c_ret = g_interface_info_get_signal(c_info, c_index);
    return _scm_from_gibaseinfo(c_ret);
}

SCM
gig_interface_info_find_signal(SCM s_info, SCM s_name)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    char *c_name = scm_to_utf8_string(s_name);
    GIBaseInfo *c_ret;
    c_ret = g_interface_info_find_signal(c_info, c_name);
    free(c_name);
    if (c_info == nullptr)
        return SCM_BOOL_F;
    return _scm_from_gibaseinfo(c_ret);
}

SCM
gig_interface_info_get_n_vfuncs(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);

    int c_n = g_interface_info_get_n_vfuncs(c_info);
    return scm_from_int (c_n);
}

SCM
gig_interface_info_get_vfunc(SCM s_info, SCM s_index)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    int c_index = scm_to_int(s_index);
    GIBaseInfo *c_ret;

    c_ret = g_interface_info_get_vfunc(c_info, c_index);
    return _scm_from_gibaseinfo(c_ret);
}

SCM
gig_interface_info_find_vfunc(SCM s_info, SCM s_name)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    char *c_name = scm_to_utf8_string(s_name);
    GIBaseInfo *c_ret;
    c_ret = g_interface_info_find_vfunc(c_info, c_name);
    free(c_name);
    if (c_ret == nullptr)
        return SCM_BOOL_F;
    return _scm_from_gibaseinfo(c_ret);
}

SCM
gig_interface_info_get_n_constants(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);

    int c_n = g_interface_info_get_n_constants(c_info);
    return scm_from_int (c_n);
}

SCM
gig_interface_info_get_constant(SCM s_info, SCM s_index)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    int c_index = scm_to_int(s_index);
    GIBaseInfo *c_ret;

    c_ret = g_interface_info_get_constant(c_info, c_index);
    return _scm_from_gibaseinfo(c_ret);
}

SCM
gig_interface_info_get_iface_struct(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    GIStructInfo *c_ret;
    c_ret = g_interface_info_get_iface_struct(c_info);
    if(c_ret == nullptr)
        return SCM_BOOL_F;
    return _scm_from_gibaseinfo(c_ret);
}

////////////////////////////////////////////////////////////////
// GIArgInfo
////////////////////////////////////////////////////////////////

SCM
gig_is_arg_info(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(GI_IS_ARG_INFO(c_info));
}

SCM
gig_arg_info_get_closure(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_int(g_arg_info_get_closure(c_info));
}

SCM
gig_arg_info_get_destroy(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_int(g_arg_info_get_destroy(c_info));
}

SCM
gig_arg_info_get_direction(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_int(g_arg_info_get_direction(c_info));
}

SCM
gig_arg_info_get_ownership_transfer(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_int(g_arg_info_get_ownership_transfer(c_info));
}

SCM
gig_arg_info_get_scope(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_int(g_arg_info_get_scope(c_info));
}

SCM
gig_arg_info_get_type(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return _scm_from_gibaseinfo(g_arg_info_get_type(c_info));
}

// g_arg_info_load_type not useful

SCM
gig_arg_info_may_be_null(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(g_arg_info_may_be_null(c_info));
}

SCM
gig_arg_info_is_caller_allocates(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(g_arg_info_is_caller_allocates(c_info));
}

SCM
gig_arg_info_is_optional(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(g_arg_info_is_optional(c_info));
}

SCM
gig_arg_info_is_return_value(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(g_arg_info_is_return_value(c_info));
}

SCM
gig_arg_info_is_skip(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(g_arg_info_is_skip(c_info));
}

SCM gig_DIRECTION_IN;
SCM gig_DIRECTION_OUT;
SCM gig_DIRECTION_INOUT;
SCM gig_SCOPE_TYPE_INVALID;
SCM gig_SCOPE_TYPE_CALL;
SCM gig_SCOPE_TYPE_ASYNC;
SCM gig_SCOPE_TYPE_NOTIFIED;
SCM gig_SCOPE_TYPE_FOREVER;
SCM gig_TRANSFER_NOTHING;
SCM gig_TRANSFER_CONTAINER;
SCM gig_TRANSFER_EVERYTHING;

////////////////////////////////////////////////////////////////
// GIConstantInfo
////////////////////////////////////////////////////////////////

SCM
gig_is_constant_info(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(GI_IS_CONSTANT_INFO(c_info));
}

SCM
gig_constant_info_get_type(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return _scm_from_gibaseinfo(g_constant_info_get_type(c_info));
}

SCM
gig_constant_info_get_value(SCM s_info)
{
    GIConstantInfo *c_info = _scm_to_gibaseinfo(s_info);
    GITypeInfo *typeinfo = g_constant_info_get_type(c_info);
    GITypeTag typetag = g_type_info_get_tag(typeinfo);
    GIArgument value;
    SCM s_value;

    g_constant_info_get_value(c_info, &value);
    switch (typetag) {
    case GI_TYPE_TAG_BOOLEAN:
        s_value = scm_from_bool(value.v_boolean);
        break;
    case GI_TYPE_TAG_FLOAT:
        s_value = scm_from_double((double)(value.v_float));
        break;
    case GI_TYPE_TAG_DOUBLE:
        s_value = scm_from_double(value.v_double);
        break;
    case GI_TYPE_TAG_INT8:
        s_value = scm_from_int8(value.v_int8);
        break;
    case GI_TYPE_TAG_INT16:
        s_value = scm_from_int16(value.v_int16);
        break;
    case GI_TYPE_TAG_INT32:
        s_value = scm_from_int32(value.v_int32);
        break;
    case GI_TYPE_TAG_INT64:
        s_value = scm_from_int64(value.v_int64);
        break;
    case GI_TYPE_TAG_UINT8:
        s_value = scm_from_uint8(value.v_uint8);
        break;
    case GI_TYPE_TAG_UINT16:
        s_value = scm_from_uint16(value.v_uint16);
        break;
    case GI_TYPE_TAG_UINT32:
        s_value = scm_from_uint32(value.v_uint32);
        break;
    case GI_TYPE_TAG_UINT64:
        s_value = scm_from_uint64(value.v_uint64);
        break;
    case GI_TYPE_TAG_UTF8:
        s_value = scm_from_utf8_string(value.v_string);
        break;
    default:
    {
        SCM tagname = scm_from_utf8_string(g_type_tag_to_string(typetag));
        scm_misc_error("constant-info-get-value",
                       "unsupported constant type '~a'",
                       scm_list_1(tagname));
        break;
    }
    }
    g_constant_info_free_value(c_info, &value);
    return s_value;
}

////////////////////////////////////////////////////////////////
// GIFieldInfo
////////////////////////////////////////////////////////////////

SCM
gig_is_field_info(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(GI_IS_FIELD_INFO(c_info));
}

// g_field_info_get_field is for runtimes
// g_field_info_set_field is for runtimes

SCM
gig_field_info_get_flags(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_int(g_field_info_get_flags(c_info));
}

SCM
gig_field_info_get_offset(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_int(g_field_info_get_offset(c_info));
}

SCM
gig_field_info_get_size(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_int(g_field_info_get_size(c_info));
}

SCM
gig_field_info_get_type(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return _scm_from_gibaseinfo(g_field_info_get_type(c_info));
}

SCM gig_FIELD_IS_WRITABLE;
SCM gig_FIELD_IS_READABLE;

////////////////////////////////////////////////////////////////
// GIPropertyInfo
////////////////////////////////////////////////////////////////

SCM
gig_is_property_info(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(GI_IS_PROPERTY_INFO(c_info));
}

SCM
gig_property_info_get_flags(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_int(g_property_info_get_flags(c_info));
}

SCM
gig_property_info_get_ownership_transfer(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_int(g_property_info_get_ownership_transfer(c_info));
}

SCM
gig_property_info_get_type(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return _scm_from_gibaseinfo(g_property_info_get_type(c_info));
}

#if GI_CHECK_VERSION(1,70,0)
SCM
gig_property_info_get_getter(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return _scm_from_gibaseinfo(g_property_info_get_getter(c_info));
}
#endif

#if GI_CHECK_VERSION(1,70,0)
SCM
gig_property_info_get_setter(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return _scm_from_gibaseinfo(g_property_info_get_setter(c_info));
}
#endif

SCM gig_PARAM_READABLE;
SCM gig_PARAM_WRITABLE;
SCM gig_PARAM_READWRITE;
SCM gig_PARAM_CONSTRUCT;
SCM gig_PARAM_CONSTRUCT_ONLY;
SCM gig_PARAM_LAX_VALIDATION;
SCM gig_PARAM_STATIC_NAME;
SCM gig_PARAM_STATIC_NICK;
SCM gig_PARAM_STATIC_BLURB;
SCM gig_PARAM_EXPLICIT_NOTIFY;
SCM gig_PARAM_DEPRECATED;

////////////////////////////////////////////////////////////////
// GITypeInfo
////////////////////////////////////////////////////////////////

SCM
gig_is_type_info(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(GI_IS_TYPE_INFO(c_info));
}

SCM
gig_type_info_is_pointer(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(g_type_info_is_pointer(c_info));
}

SCM
gig_type_info_get_tag(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_int(g_type_info_get_tag(c_info));
}

SCM
gig_type_info_get_param_type(SCM s_info, SCM s_n)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    int c_n = scm_to_int(s_n);
    return _scm_from_gibaseinfo(g_type_info_get_param_type(c_info, c_n));
}

SCM
gig_type_info_get_interface(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return _scm_from_gibaseinfo(g_type_info_get_interface(c_info));
}

SCM
gig_type_info_get_array_length(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_int(g_type_info_get_array_length(c_info));
}

SCM
gig_type_info_get_array_fixed_size(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_int(g_type_info_get_array_fixed_size(c_info));
}

SCM
gig_type_info_is_zero_terminated(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_bool(g_type_info_is_zero_terminated(c_info));
}

SCM
gig_type_info_get_array_type(SCM s_info)
{
    GIBaseInfo *c_info = _scm_to_gibaseinfo(s_info);
    return scm_from_int(g_type_info_get_array_type(c_info));
}


#define u8sym(x) scm_from_utf8_symbol(x)
#define makeFO(a,b,c) scm_make_foreign_object_type((a),(b),(c))
SCM flag(const char *nam, int val)
{
    return scm_permanent_object(scm_c_define(nam,scm_from_int(val)));
}

GIG_API void
gig_init_girepository ()
{
    static int first = 1;

    if (first)
    {
        gibaseinfo_fo_type = makeFO (u8sym ("<gibaseinfo>"),
                                     scm_list_1 (u8sym ("ptr")),
                                     gc_free_gibaseinfo);
        scm_c_define_gsubr ("gibaseinfo?", 1, 0, 0, gig_is_gibaseinfo_p);
        scm_c_define_gsubr ("gibaseinfo=?", 2, 0, 0, gig_gibaseinfo_equalp);

        scm_c_define_gsubr ("%irepository-get-dependencies", 1, 0, 0,
                            gig_irepository_get_dependencies);
        scm_c_define_gsubr ("%irepository-get-immediate-dependencies", 1, 0, 0,
                            gig_irepository_get_immediate_dependencies);
        scm_c_define_gsubr ("%irepository-get-loaded-namespaces", 0, 0, 0,
                            gig_irepository_get_loaded_namespaces);
        scm_c_define_gsubr ("%irepository-get-n-infos", 1, 0, 0,
                            gig_irepository_get_n_infos);
        scm_c_define_gsubr ("%irepository-get-info", 2, 0, 0,
                            gig_irepository_get_info);
        scm_c_define_gsubr ("%irepository-enumerate-versions", 1, 0, 0,
                            gig_irepository_enumerate_versions);
        scm_c_define_gsubr ("%irepository-prepend-library-path", 1, 0, 0,
                            gig_irepository_prepend_library_path);
        scm_c_define_gsubr ("%irepository-prepend-search-path", 1, 0, 0,
                            gig_irepository_prepend_search_path);
        scm_c_define_gsubr ("%irepository-get-search-path", 0, 0, 0,
                            gig_irepository_get_search_path);
        scm_c_define_gsubr ("%irepository-get-typelib-path", 1, 0, 0,
                            gig_irepository_get_typelib_path);
        scm_c_define_gsubr ("%irepository-is-registered", 2, 0, 0,
                            gig_irepository_is_registered);
        scm_c_define_gsubr ("%irepository-require", 3, 0, 0,
                            gig_irepository_require);
        scm_c_define_gsubr ("%irepository-require-private", 4, 0, 0,
                            gig_irepository_require_private);
        scm_c_define_gsubr ("%irepository-get-c-prefix", 1, 0, 0,
                            gig_irepository_get_c_prefix);
        scm_c_define_gsubr ("%irepository-get-shared-library", 1, 0, 0,
                            gig_irepository_get_shared_library);
        scm_c_define_gsubr ("%irepository-get-version", 1, 0, 0,
                            gig_irepository_get_version);
        scm_c_define_gsubr ("%irepository-find-by-gtype", 1, 0, 0,
                            gig_irepository_find_by_gtype);
        scm_c_define_gsubr ("%irepository-find-by-error-domain", 1, 0, 0,
                            gig_irepository_find_by_error_domain);
        scm_c_define_gsubr ("%irepository-find-by-name", 2, 0, 0,
                            gig_irepository_find_by_name);
        scm_c_define_gsubr ("%irepository-get-object-gtype-interfaces", 1, 0, 0,
                            gig_irepository_get_object_gtype_interfaces);

        gig_IREPOSITORY_LOAD_FLAG_LAZY = flag("%LOAD_FLAG_LAZY",
                                              G_IREPOSITORY_LOAD_FLAG_LAZY);

#if GI_CHECK_VERSION(1,72,0)
        scm_c_define_gsubr ("%type-tag-is-basic", 1, 0, 0,
                            gig_type_tag_is_basic);
        scm_c_define_gsubr ("%type-tag-is-container", 1, 0, 0,
                            gig_type_tag_is_container);
        scm_c_define_gsubr ("%type-tag-is-numeric", 1, 0, 0,
                            gig_type_tag_is_numeric);
#endif
        scm_c_define_gsubr ("%type-tag-to-string", 1, 0, 0,
                            gig_type_tag_to_string);

        gig_TYPE_TAG_VOID = flag("TYPE_TAG_VOID", GI_TYPE_TAG_VOID);
        gig_TYPE_TAG_BOOLEAN = flag("TYPE_TAG_BOOLEAN", GI_TYPE_TAG_BOOLEAN);
        gig_TYPE_TAG_INT8 = flag("TYPE_TAG_INT8", GI_TYPE_TAG_INT8);
        gig_TYPE_TAG_UINT8 = flag("TYPE_TAG_UINT8", GI_TYPE_TAG_UINT8);
        gig_TYPE_TAG_INT16 = flag("TYPE_TAG_INT16", GI_TYPE_TAG_INT16);
        gig_TYPE_TAG_UINT16 = flag("TYPE_TAG_UINT16", GI_TYPE_TAG_UINT16);
        gig_TYPE_TAG_INT32 = flag("TYPE_TAG_INT32", GI_TYPE_TAG_INT32);
        gig_TYPE_TAG_UINT32 = flag("TYPE_TAG_UINT32", GI_TYPE_TAG_UINT32);
        gig_TYPE_TAG_INT64 = flag("TYPE_TAG_INT64", GI_TYPE_TAG_INT64);
        gig_TYPE_TAG_UINT64 = flag("TYPE_TAG_UINT64", GI_TYPE_TAG_UINT64);
        gig_TYPE_TAG_FLOAT = flag("TYPE_TAG_FLOAT", GI_TYPE_TAG_FLOAT);
        gig_TYPE_TAG_DOUBLE = flag("TYPE_TAG_DOUBLE", GI_TYPE_TAG_DOUBLE);
        gig_TYPE_TAG_GTYPE = flag("TYPE_TAG_GTYPE", GI_TYPE_TAG_GTYPE);
        gig_TYPE_TAG_UTF8 = flag("TYPE_TAG_UTF8", GI_TYPE_TAG_UTF8);
        gig_TYPE_TAG_FILENAME = flag("TYPE_TAG_FILENAME", GI_TYPE_TAG_FILENAME);
        gig_TYPE_TAG_ARRAY = flag("TYPE_TAG_ARRAY", GI_TYPE_TAG_ARRAY);
        gig_TYPE_TAG_INTERFACE = flag("TYPE_TAG_INTERFACE",
                                      GI_TYPE_TAG_INTERFACE);
        gig_TYPE_TAG_GLIST = flag("TYPE_TAG_GLIST", GI_TYPE_TAG_GLIST);
        gig_TYPE_TAG_GSLIST = flag("TYPE_TAG_GSLIST", GI_TYPE_TAG_GSLIST);
        gig_TYPE_TAG_GHASH = flag("TYPE_TAG_GHASH", GI_TYPE_TAG_GHASH);
        gig_TYPE_TAG_ERROR = flag("TYPE_TAG_ERROR", GI_TYPE_TAG_ERROR);
        gig_TYPE_TAG_UNICHAR = flag("TYPE_TAG_UNICHAR", GI_TYPE_TAG_UNICHAR);
        gig_ARRAY_TYPE_C = flag("%ARRAY_TYPE_C", GI_ARRAY_TYPE_C);
        gig_ARRAY_TYPE_ARRAY = flag("%ARRAY_TYPE_ARRAY", GI_ARRAY_TYPE_ARRAY);
        gig_ARRAY_TYPE_PTR_ARRAY = flag("%ARRAY_TYPE_PTR_ARRAY",
                                        GI_ARRAY_TYPE_PTR_ARRAY);
        gig_ARRAY_TYPE_BYTE_ARRAY = flag("%ARRAY_TYPE_BYTE_ARRAY",
                                         GI_ARRAY_TYPE_BYTE_ARRAY);
        gig_TYPE_TAG_N_TYPES = flag("TYPE_TAG_N_TYPES", GI_TYPE_TAG_N_TYPES);

        scm_c_define_gsubr ("%get-major-version", 0, 0, 0,
                            gig_get_major_version);
        scm_c_define_gsubr ("%get-minor-version", 0, 0, 0,
                            gig_get_minor_version);
        scm_c_define_gsubr ("%get-micro-version", 0, 0, 0,
                            gig_get_micro_version);
        scm_c_define_gsubr ("%check-version", 3, 0, 0,
                            gig_check_version);
        gig_MAJOR_VERSION = flag("MAJOR_VERSION", GI_MAJOR_VERSION);
        gig_MINOR_VERSION = flag("MINOR_VERSION", GI_MINOR_VERSION);
        gig_MICRO_VERSION = flag("MICRO_VERSION", GI_MICRO_VERSION);

        scm_c_define_gsubr ("base-info-equal", 2, 0, 0,
                            gig_base_info_equal);
        scm_c_define_gsubr ("%base-info-get-type", 1, 0, 0,
                            gig_base_info_get_type);
        scm_c_define_gsubr ("%base-info-get-namespace", 1, 0, 0,
                            gig_base_info_get_namespace);
        scm_c_define_gsubr ("%base-info-get-name", 1, 0, 0,
                            gig_base_info_get_name);
        scm_c_define_gsubr ("%base-info-get-attribute", 2, 0, 0,
                            gig_base_info_get_attribute);
        scm_c_define_gsubr ("%base-info-get-attributes", 1, 0, 0,
                            gig_base_info_get_attributes);
        scm_c_define_gsubr ("%base-info-get-container", 1, 0, 0,
                            gig_base_info_get_container);
        scm_c_define_gsubr ("%base-info-is-deprecated", 1, 0, 0,
                            gig_base_info_is_deprecated);
        scm_c_define_gsubr ("%info-type-to-string", 1, 0, 0,
                            gig_info_type_to_string);
        gig_INFO_TYPE_INVALID = flag("%INFO_TYPE_INVALID", GI_INFO_TYPE_INVALID);
        gig_INFO_TYPE_FUNCTION = flag("%INFO_TYPE_FUNCTION",
                                      GI_INFO_TYPE_FUNCTION);
        gig_INFO_TYPE_CALLBACK = flag("%INFO_TYPE_CALLBACK",
                                      GI_INFO_TYPE_CALLBACK);
        gig_INFO_TYPE_STRUCT = flag("%INFO_TYPE_STRUCT", GI_INFO_TYPE_STRUCT);
        gig_INFO_TYPE_BOXED = flag("%INFO_TYPE_BOXED", GI_INFO_TYPE_BOXED);
        gig_INFO_TYPE_ENUM = flag("%INFO_TYPE_ENUM", GI_INFO_TYPE_ENUM);
        gig_INFO_TYPE_FLAGS = flag("%INFO_TYPE_FLAGS", GI_INFO_TYPE_FLAGS);
        gig_INFO_TYPE_OBJECT = flag("%INFO_TYPE_OBJECT", GI_INFO_TYPE_OBJECT);
        gig_INFO_TYPE_INTERFACE = flag("%INFO_TYPE_INTERFACE",
                                       GI_INFO_TYPE_INTERFACE);
        gig_INFO_TYPE_CONSTANT = flag("%INFO_TYPE_CONSTANT",
                                      GI_INFO_TYPE_CONSTANT);
        gig_INFO_TYPE_INVALID_0 = flag("%INFO_TYPE_INVALID_0",
                                       GI_INFO_TYPE_INVALID_0);
        gig_INFO_TYPE_UNION = flag("%INFO_TYPE_UNION", GI_INFO_TYPE_UNION);
        gig_INFO_TYPE_VALUE = flag("%INFO_TYPE_VALUE", GI_INFO_TYPE_VALUE);
        gig_INFO_TYPE_SIGNAL = flag("%INFO_TYPE_SIGNAL", GI_INFO_TYPE_SIGNAL);
        gig_INFO_TYPE_VFUNC = flag("%INFO_TYPE_VFUNC", GI_INFO_TYPE_VFUNC);
        gig_INFO_TYPE_PROPERTY = flag("%INFO_TYPE_PROPERTY",
                                      GI_INFO_TYPE_PROPERTY);
        gig_INFO_TYPE_FIELD = flag("%INFO_TYPE_FIELD", GI_INFO_TYPE_FIELD);
        gig_INFO_TYPE_ARG = flag("%INFO_TYPE_ARG", GI_INFO_TYPE_ARG);
        gig_INFO_TYPE_TYPE = flag("%INFO_TYPE_TYPE", GI_INFO_TYPE_TYPE);
        gig_INFO_TYPE_UNRESOLVED = flag("%INFO_TYPE_UNRESOLVED",
                                        GI_INFO_TYPE_UNRESOLVED);

        scm_c_define_gsubr ("%is-callable-info", 1, 0, 0,
                            gig_is_callable_info);
        scm_c_define_gsubr ("%callable-info-can-throw-gerror", 1, 0, 0,
                            gig_callable_info_can_throw_gerror);
        scm_c_define_gsubr ("%callable-info-get-n-args", 1, 0, 0,
                            gig_callable_info_get_n_args);
        scm_c_define_gsubr ("%callable-info-get-arg", 2, 0, 0,
                            gig_callable_info_get_n_args);
        scm_c_define_gsubr ("%callable-info-get-caller-owns", 1, 0, 0,
                            gig_callable_info_get_caller_owns);
        scm_c_define_gsubr ("%callable-info-get-instance-ownership-transfer",
                            1, 0, 0,
                            gig_callable_info_get_instance_ownership_transfer);
        scm_c_define_gsubr ("%callable-info-get-return-attribute", 2, 0, 0,
                            gig_callable_info_get_return_attribute);
        scm_c_define_gsubr ("%callable-info-get-return-type", 1, 0, 0,
                            gig_callable_info_get_return_type);
        scm_c_define_gsubr ("%callable-info-is-method", 1, 0, 0,
                            gig_callable_info_is_method);
        scm_c_define_gsubr ("%callable-info-get-return-attributes", 1, 0, 0,
                            gig_callable_info_get_return_attributes);
        scm_c_define_gsubr ("%callable-info-may-return-null", 1, 0, 0,
                            gig_callable_info_may_return_null);
        scm_c_define_gsubr ("%callable-info-skip-return", 1, 0, 0,
                            gig_callable_info_skip_return);

        scm_c_define_gsubr ("%is-function-info", 1, 0, 0,
                            gig_is_function_info);
        scm_c_define_gsubr ("%function-info-get-flags", 1, 0, 0,
                            gig_function_info_get_flags);
        scm_c_define_gsubr ("%function-info-get-property", 1, 0, 0,
                            gig_function_info_get_property);
        scm_c_define_gsubr ("%function-info-get-symbol", 1, 0, 0,
                            gig_function_info_get_symbol);
        scm_c_define_gsubr ("%function-info-get-vfunc", 1, 0, 0,
                            gig_function_info_get_vfunc);

        gig_FUNCTION_IS_METHOD = flag("%FUNCTION_IS_METHOD",
                                      GI_FUNCTION_IS_METHOD);
        gig_FUNCTION_IS_CONSTRUCTOR = flag("%FUNCTION_IS_CONSTRUCTOR",
                                           GI_FUNCTION_IS_CONSTRUCTOR);
        gig_FUNCTION_IS_GETTER = flag("%FUNCTION_IS_GETTER",
                                      GI_FUNCTION_IS_GETTER);
        gig_FUNCTION_IS_SETTER = flag("%FUNCTION_IS_SETTER", GI_FUNCTION_IS_SETTER);
        gig_FUNCTION_VRAPS_VFUNC = flag("%FUNCTION_WRAPS_VFUNC", GI_FUNCTION_WRAPS_VFUNC);
        gig_FUNCTION_THROWS = flag("%FUNCTION_THROWS", GI_FUNCTION_THROWS);

        scm_c_define_gsubr ("%is-signal-info", 1, 0, 0,
                            gig_is_signal_info);
        scm_c_define_gsubr ("%signal-info-get-flags", 1, 0, 0,
                            gig_signal_info_get_flags);
        scm_c_define_gsubr ("%signal-info-get-class-closure", 1, 0, 0,
                            gig_signal_info_get_class_closure);
        scm_c_define_gsubr ("%signal-info-true-stops-emit", 1, 0, 0,
                            gig_signal_info_true_stops_emit);

        gig_SIGNAL_RUN_FIRST = flag("%SIGNAL_RUN_FIRST", G_SIGNAL_RUN_FIRST);
        gig_SIGNAL_RUN_LAST = flag("%SIGNAL_RUN_LAST", G_SIGNAL_RUN_LAST);
        gig_SIGNAL_RUN_CLEANUP = flag("%SIGNAL_RUN_CLEANUP", G_SIGNAL_RUN_CLEANUP);
        gig_SIGNAL_NO_RECURSE = flag("%SIGNAL_NO_RECURSE", G_SIGNAL_NO_RECURSE);
        gig_SIGNAL_DETAILED = flag("%SIGNAL_DETAILED", G_SIGNAL_DETAILED);
        gig_SIGNAL_ACTION = flag("%SIGNAL_ACTION", G_SIGNAL_ACTION);
        gig_SIGNAL_NO_HOOKS = flag("%SIGNAL_NO_HOOKS", G_SIGNAL_NO_HOOKS);
        gig_SIGNAL_MUST_COLLECT = flag("%SIGNAL_MUST_COLLECT", G_SIGNAL_MUST_COLLECT);
        gig_SIGNAL_DEPRECATED = flag("%SIGNAL_DEPRECATED", G_SIGNAL_DEPRECATED);
        gig_SIGNAL_ACCUMULATOR_FIRST_RUN = flag("%SIGNAL_ACCUMULATOR_FIRST_RUN", G_SIGNAL_ACCUMULATOR_FIRST_RUN);

        scm_c_define_gsubr ("%is-vfunc-info", 1, 0, 0,
                            gig_is_vfunc_info);
        scm_c_define_gsubr ("%vfunc-info-get-flags", 1, 0, 0,
                            gig_vfunc_info_get_flags);
        scm_c_define_gsubr ("%vfunc-info-get-offset", 1, 0, 0,
                            gig_vfunc_info_get_offset);
        scm_c_define_gsubr ("%vfunc-info-get-signal", 1, 0, 0,
                            gig_vfunc_info_get_signal);
        scm_c_define_gsubr ("%vfunc-info-get-invoker", 1, 0, 0,
                            gig_vfunc_info_get_invoker);

        gig_VFUNC_MUST_CHAIN_UP = flag("%VFUNC_MUST_CHAIN_UP",
                                       GI_VFUNC_MUST_CHAIN_UP);
        gig_VFUNC_MUST_OVERRIDE = flag("%VFUNC_MUST_OVERRIDE",
                                       GI_VFUNC_MUST_OVERRIDE);
        gig_VFUNC_MUST_NOT_OVERRIDE = flag("%VFUNC_MUST_NOT_OVERRIDE",
                                           GI_VFUNC_MUST_NOT_OVERRIDE);
        gig_VFUNC_THROWS = flag("%VFUNC_THROWS", GI_VFUNC_THROWS);

        scm_c_define_gsubr ("%is-registered-type-info", 1, 0, 0,
                            gig_is_registered_type_info);
        scm_c_define_gsubr ("%registered-type-info-get-type-name", 1, 0, 0,
                            gig_registered_type_info_get_type_name);
        scm_c_define_gsubr ("%registered-type-info-get-type-init", 1, 0, 0,
                            gig_registered_type_info_get_type_init);
        scm_c_define_gsubr ("%registered-type-info-get-g-type", 1, 0, 0,
                            gig_registered_type_info_get_g_type);

        scm_c_define_gsubr ("%is-enum-info", 1, 0, 0,
                            gig_is_enum_info);
        scm_c_define_gsubr ("%enum-info-get-n-values", 1, 0, 0,
                            gig_enum_info_get_n_values);
        scm_c_define_gsubr ("%enum-info-get-value", 2, 0, 0,
                            gig_enum_info_get_value);
        scm_c_define_gsubr ("%enum-info-get-n-methods", 1, 0, 0,
                            gig_enum_info_get_n_methods);
        scm_c_define_gsubr ("%enum-info-get-method", 2, 0, 0,
                            gig_enum_info_get_method);
        scm_c_define_gsubr ("%enum-info-get-storage-type", 1, 0, 0,
                            gig_enum_info_get_storage_type);
        scm_c_define_gsubr ("%enum-info-get-error-domain", 1, 0, 0,
                            gig_enum_info_get_error_domain);
        scm_c_define_gsubr ("%is-value-info", 1, 0, 0,
                            gig_is_value_info);
        scm_c_define_gsubr ("%value-info-get-value", 1, 0, 0,
                            gig_value_info_get_value);

        scm_c_define_gsubr ("%is-struct-info", 1, 0, 0,
                            gig_is_struct_info);
        scm_c_define_gsubr ("%struct-info-find-field", 2, 0, 0,
                            gig_struct_info_find_field);
        scm_c_define_gsubr ("%struct-info-get-alignment", 1, 0, 0,
                            gig_struct_info_get_alignment);
        scm_c_define_gsubr ("%struct-info-get-size", 1, 0, 0,
                            gig_struct_info_get_size);
        scm_c_define_gsubr ("%struct-info-is-gtype-struct", 1, 0, 0,
                            gig_struct_info_is_gtype_struct);
        scm_c_define_gsubr ("%struct-info-is-foreign", 1, 0, 0,
                            gig_struct_info_is_foreign);
        scm_c_define_gsubr ("%struct-info-get-n-fields", 1, 0, 0,
                            gig_struct_info_get_n_fields);
        scm_c_define_gsubr ("%struct-info-get-field", 2, 0, 0,
                            gig_struct_info_get_field);
        scm_c_define_gsubr ("%struct-info-get-n-methods", 1, 0, 0,
                            gig_struct_info_get_n_methods);
        scm_c_define_gsubr ("%struct-info-get-method", 2, 0, 0,
                            gig_struct_info_get_method);
        scm_c_define_gsubr ("%struct-info-find-method", 2, 0, 0,
                            gig_struct_info_find_method);

        scm_c_define_gsubr ("%is-union-info", 1, 0, 0,
                            gig_is_union_info);
        scm_c_define_gsubr ("%union-info-get-n-fields", 1, 0, 0,
                            gig_union_info_get_n_fields);
        scm_c_define_gsubr ("%union-info-get-field", 2, 0, 0,
                            gig_union_info_get_field);
        scm_c_define_gsubr ("%union-info-get-n-methods", 1, 0, 0,
                            gig_union_info_get_n_methods);
        scm_c_define_gsubr ("%union-info-get-method", 2, 0, 0,
                            gig_union_info_get_method);
        scm_c_define_gsubr ("%union-info-is-discriminated", 1, 0, 0,
                            gig_union_info_is_discriminated);
        scm_c_define_gsubr ("%union-info-get-discriminator-offset", 1, 0, 0,
                            gig_union_info_get_discriminator_offset);
        scm_c_define_gsubr ("%union-info-get-discriminator-type", 1, 0, 0,
                            gig_union_info_get_discriminator_type);
        scm_c_define_gsubr ("%union-info-get-discriminator", 2, 0, 0,
                            gig_union_info_get_discriminator);
        scm_c_define_gsubr ("%union-info-find-method", 2, 0, 0,
                            gig_union_info_find_method);
        scm_c_define_gsubr ("%union-info-get-size", 1, 0, 0,
                            gig_union_info_get_size);
        scm_c_define_gsubr ("%union-info-get-alignment", 1, 0, 0,
                            gig_union_info_get_alignment);

        scm_c_define_gsubr ("%is-object-info", 1, 0, 0,
                            gig_is_object_info);
        scm_c_define_gsubr ("%object-info-get-abstract", 1, 0, 0,
                            gig_object_info_get_abstract);
        scm_c_define_gsubr ("%object-info-get-fundamental", 1, 0, 0,
                            gig_object_info_get_fundamental);
        scm_c_define_gsubr ("%object-info-get-final", 1, 0, 0,
                            gig_object_info_get_final);
        scm_c_define_gsubr ("%object-info-get-parent", 1, 0, 0,
                            gig_object_info_get_parent);
        scm_c_define_gsubr ("%object-info-get-type-name", 1, 0, 0,
                            gig_object_info_get_type_name);
        scm_c_define_gsubr ("%object-info-get-type-init", 1, 0, 0,
                            gig_object_info_get_type_init);
        scm_c_define_gsubr ("%object-info-get-n-constants", 1, 0, 0,
                            gig_object_info_get_n_constants);
        scm_c_define_gsubr ("%object-info-get-constant", 2, 0, 0,
                            gig_object_info_get_constant);
        scm_c_define_gsubr ("%object-info-get-n-fields", 1, 0, 0,
                            gig_object_info_get_n_fields);
        scm_c_define_gsubr ("%object-info-get-field", 2, 0, 0,
                            gig_object_info_get_field);
        scm_c_define_gsubr ("%object-info-get-n-interfaces", 1, 0, 0,
                            gig_object_info_get_n_interfaces);
        scm_c_define_gsubr ("%object-info-get-interface", 2, 0, 0,
                            gig_object_info_get_interface);
        scm_c_define_gsubr ("%object-info-get-n-methods", 1, 0, 0,
                            gig_object_info_get_n_methods);
        scm_c_define_gsubr ("%object-info-get-method", 2, 0, 0,
                            gig_object_info_get_method);
        scm_c_define_gsubr ("%object-info-find-method", 2, 0, 0,
                            gig_object_info_find_method);
        scm_c_define_gsubr ("%object-info-find-method-using-interfaces", 2, 0, 0,
                            gig_object_info_find_method_using_interfaces);
        scm_c_define_gsubr ("%object-info-get-n-properties", 1, 0, 0,
                            gig_object_info_get_n_properties);
        scm_c_define_gsubr ("%object-info-get-property", 2, 0, 0,
                            gig_object_info_get_property);
        scm_c_define_gsubr ("%object-info-get-n-signals", 1, 0, 0,
                            gig_object_info_get_n_signals);
        scm_c_define_gsubr ("%object-info-get-signal", 2, 0, 0,
                            gig_object_info_get_signal);
        scm_c_define_gsubr ("%object-info-find-signal", 2, 0, 0,
                            gig_object_info_find_signal);
        scm_c_define_gsubr ("%object-info-get-n-vfuncs", 1, 0, 0,
                            gig_object_info_get_n_vfuncs);
        scm_c_define_gsubr ("%object-info-get-vfunc", 2, 0, 0,
                            gig_object_info_get_vfunc);
        scm_c_define_gsubr ("%object-info-find-vfunc", 2, 0, 0,
                            gig_object_info_find_vfunc);
        scm_c_define_gsubr ("%object-info-find-vfunc-using-interfaces", 2, 0, 0,
                            gig_object_info_find_vfunc_using_interfaces);
        scm_c_define_gsubr ("%object-info-get-class-struct", 1, 0, 0,
                            gig_object_info_get_class_struct);
        scm_c_define_gsubr ("%object-info-get-ref-function", 1, 0, 0,
                            gig_object_info_get_ref_function);
        scm_c_define_gsubr ("%object-info-get-unref-function", 1, 0, 0,
                            gig_object_info_get_unref_function);
        scm_c_define_gsubr ("%object-info-get-set-value-function", 1, 0, 0,
                            gig_object_info_get_set_value_function);
        scm_c_define_gsubr ("%object-info-get-get-value-function", 1, 0, 0,
                            gig_object_info_get_get_value_function);

        scm_c_define_gsubr ("%is-interface-info", 1, 0, 0,
                            gig_is_interface_info);
        scm_c_define_gsubr ("%interface-info-get-n-prerequisites", 1, 0, 0,
                            gig_interface_info_get_n_prerequisites);
        scm_c_define_gsubr ("%interface-info-get-prerequisite", 2, 0, 0,
                            gig_interface_info_get_prerequisite);
        scm_c_define_gsubr ("%interface-info-get-n-properties", 1, 0, 0,
                            gig_interface_info_get_n_properties);
        scm_c_define_gsubr ("%interface-info-get-property", 2, 0, 0,
                            gig_interface_info_get_property);
        scm_c_define_gsubr ("%interface-info-get-n-methods", 1, 0, 0,
                            gig_interface_info_get_n_methods);
        scm_c_define_gsubr ("%interface-info-get-method", 2, 0, 0,
                            gig_interface_info_get_method);
        scm_c_define_gsubr ("%interface-info-find-method", 2, 0, 0,
                            gig_interface_info_find_method);
        scm_c_define_gsubr ("%interface-info-get-n-signals", 1, 0, 0,
                            gig_interface_info_get_n_signals);
        scm_c_define_gsubr ("%interface-info-get-signal", 2, 0, 0,
                            gig_interface_info_get_signal);
        scm_c_define_gsubr ("%interface-info-find-signal", 2, 0, 0,
                            gig_interface_info_find_signal);
        scm_c_define_gsubr ("%interface-info-get-n-vfuncs", 1, 0, 0,
                            gig_interface_info_get_n_vfuncs);
        scm_c_define_gsubr ("%interface-info-get-vfunc", 2, 0, 0,
                            gig_interface_info_get_vfunc);
        scm_c_define_gsubr ("%interface-info-find-vfunc", 2, 0, 0,
                            gig_interface_info_find_vfunc);
        scm_c_define_gsubr ("%interface-info-get-n-constants", 1, 0, 0,
                            gig_interface_info_get_n_constants);
        scm_c_define_gsubr ("%interface-info-get-constant", 2, 0, 0,
                            gig_interface_info_get_constant);
        scm_c_define_gsubr ("%interface-info-get-iface-struct", 1, 0, 0,
                            gig_interface_info_get_iface_struct);

        scm_c_define_gsubr ("%is-arg-info", 1, 0, 0,
                            gig_is_arg_info);
        scm_c_define_gsubr ("%arg-info-get-closure", 1, 0, 0,
                            gig_arg_info_get_closure);
        scm_c_define_gsubr ("%arg-info-get-destroy", 1, 0, 0,
                            gig_arg_info_get_destroy);
        scm_c_define_gsubr ("%arg-info-get-direction", 1, 0, 0,
                            gig_arg_info_get_direction);
        scm_c_define_gsubr ("%arg-info-get-ownership-transfer", 1, 0, 0,
                            gig_arg_info_get_ownership_transfer);
        scm_c_define_gsubr ("%arg-info-get-scope", 1, 0, 0,
                            gig_arg_info_get_scope);
        scm_c_define_gsubr ("%arg-info-get-type", 1, 0, 0,
                            gig_arg_info_get_type);
        scm_c_define_gsubr ("%arg-info-may-be-null", 1, 0, 0,
                            gig_arg_info_may_be_null);
        scm_c_define_gsubr ("%arg-info-is-caller-allocates", 1, 0, 0,
                            gig_arg_info_is_caller_allocates);
        scm_c_define_gsubr ("%arg-info-is-optional", 1, 0, 0,
                            gig_arg_info_is_optional);
        scm_c_define_gsubr ("%arg-info-is-return-value", 1, 0, 0,
                            gig_arg_info_is_return_value);
        scm_c_define_gsubr ("%arg-info-is-skip", 1, 0, 0,
                            gig_arg_info_is_skip);

        gig_DIRECTION_IN = flag("%DIRECTION_IN", GI_DIRECTION_IN);
        gig_DIRECTION_OUT = flag("%DIRECTION_OUT", GI_DIRECTION_OUT);
        gig_DIRECTION_INOUT = flag("%DIRECTION_INOUT", GI_DIRECTION_INOUT);

        gig_SCOPE_TYPE_INVALID = flag("%SCOPE_TYPE_INVALID",
                                      GI_SCOPE_TYPE_INVALID);
        gig_SCOPE_TYPE_CALL = flag("%SCOPE_TYPE_CALL", GI_SCOPE_TYPE_CALL);
        gig_SCOPE_TYPE_ASYNC = flag("%SCOPE_TYPE_ASYNC", GI_SCOPE_TYPE_ASYNC);
        gig_SCOPE_TYPE_NOTIFIED = flag("%SCOPE_TYPE_NOTIFIED",
                                       GI_SCOPE_TYPE_NOTIFIED);
        gig_SCOPE_TYPE_FOREVER = flag("%SCOPE_TYPE_FOREVER",
                                       GI_SCOPE_TYPE_FOREVER);

        gig_TRANSFER_NOTHING = flag("%TRANSFER_NOTHING", GI_TRANSFER_NOTHING);
        gig_TRANSFER_CONTAINER = flag("%TRANSFER_CONTAINER",
                                      GI_TRANSFER_CONTAINER);
        gig_TRANSFER_EVERYTHING = flag("%TRANSFER_EVERYTHING",
                                       GI_TRANSFER_EVERYTHING);

        scm_c_define_gsubr ("%is-constant-info", 1, 0, 0,
                            gig_is_constant_info);
        scm_c_define_gsubr ("%constant-info-get-type", 1, 0, 0,
                            gig_constant_info_get_type);
        scm_c_define_gsubr ("%constant-info-get-value", 1, 0, 0,
                            gig_constant_info_get_value);

        scm_c_define_gsubr ("%is-field-info", 1, 0, 0,
                            gig_is_field_info);
        scm_c_define_gsubr ("%field-info-get-flags", 1, 0, 0,
                            gig_field_info_get_flags);
        scm_c_define_gsubr ("%field-info-get-offset", 1, 0, 0,
                            gig_field_info_get_offset);
        scm_c_define_gsubr ("%field-info-get-size", 1, 0, 0,
                            gig_field_info_get_size);
        scm_c_define_gsubr ("%field-info-get-type", 1, 0, 0,
                            gig_field_info_get_type);
        gig_FIELD_IS_WRITABLE = flag("%FIELD_IS_WRITABLE", GI_FIELD_IS_WRITABLE);
        gig_FIELD_IS_READABLE = flag("%FIELD_IS_READABLE", GI_FIELD_IS_READABLE);
        scm_c_define_gsubr ("%is-property-info", 1, 0, 0,
                            gig_is_property_info);
        scm_c_define_gsubr ("%property-info-get-flags", 1, 0, 0,
                            gig_property_info_get_flags);
        scm_c_define_gsubr ("%property-info-get-ownership-transfer", 1, 0, 0,
                            gig_property_info_get_ownership_transfer);
        scm_c_define_gsubr ("%property-info-get-type", 1, 0, 0,
                            gig_property_info_get_type);
#if GI_CHECK_VERSION(1,70,0)
        scm_c_define_gsubr ("%property-info-get-getter", 1, 0, 0,
                            gig_property_info_get_getter);
        scm_c_define_gsubr ("%property-info-get-setter", 1, 0, 0,
                            gig_property_info_get_setter);
#endif

        gig_PARAM_READABLE = flag("%PARAM_READABLE", G_PARAM_READABLE);
        gig_PARAM_WRITABLE = flag("%PARAM_WRITABLE", G_PARAM_WRITABLE);
        gig_PARAM_READWRITE = flag("%PARAM_READWRITE", G_PARAM_READWRITE);
        gig_PARAM_CONSTRUCT = flag("%PARAM_CONSTRUCT", G_PARAM_CONSTRUCT);
        gig_PARAM_CONSTRUCT_ONLY = flag("%PARAM_CONSTRUCT_ONLY", G_PARAM_CONSTRUCT_ONLY);
        gig_PARAM_LAX_VALIDATION = flag("%PARAM_LAX_VALIDATION", G_PARAM_LAX_VALIDATION);
        gig_PARAM_STATIC_NAME = flag("%PARAM_STATIC_NAME", G_PARAM_STATIC_NAME);
        gig_PARAM_STATIC_NICK = flag("%PARAM_STATIC_NICK", G_PARAM_STATIC_NICK);
        gig_PARAM_STATIC_BLURB = flag("%PARAM_STATIC_BLURB", G_PARAM_STATIC_BLURB);
        gig_PARAM_EXPLICIT_NOTIFY = flag("%PARAM_EXPLICIT_NOTIFY", G_PARAM_EXPLICIT_NOTIFY);
        gig_PARAM_DEPRECATED = flag("%PARAM_DEPRECATED", G_PARAM_DEPRECATED);

        scm_c_define_gsubr ("%is-type-info", 1, 0, 0,
                            gig_is_type_info);
        scm_c_define_gsubr ("%type-info-is-pointer", 1, 0, 0,
                            gig_type_info_is_pointer);
        scm_c_define_gsubr ("%type-info-get-tag", 1, 0, 0,
                            gig_type_info_get_tag);
        scm_c_define_gsubr ("%type-info-get-param-type", 2, 0, 0,
                            gig_type_info_get_param_type);
        scm_c_define_gsubr ("%type-info-get-interface", 1, 0, 0,
                            gig_type_info_get_interface);
        scm_c_define_gsubr ("%type-info-get-array-length", 1, 0, 0,
                            gig_type_info_get_array_length);
        scm_c_define_gsubr ("%type-info-get-array-fixed-size", 1, 0, 0,
                            gig_type_info_get_array_fixed_size);
        scm_c_define_gsubr ("%type-info-is-zero-terminated", 1, 0, 0,
                            gig_type_info_is_zero_terminated);
        scm_c_define_gsubr ("%type-info-get-array-type", 1, 0, 0,
                            gig_type_info_get_array_type);
        first = 0;
    }
}
