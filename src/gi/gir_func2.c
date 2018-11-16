#include <libguile.h>
#include <girepository.h>
#include "gi_giargument.h"
#include "gir_type.h"
#include "gir_func2.h"
#include "gi_gobject.h"
#include "gi_gtype.h"
#include "gi_gstruct.h"

#define GIR_FUNC2_INIT_HASH_TABLE_SIZE 10

static SCM get_hash_table(const char *name);
static void hash_table_insert(const char *table_name, const char *namespace_,
    const char *parent,
    GIBaseInfo *info);
static void method_table_insert(GType type,
    GIFunctionInfo *info);
static gchar *flag_public_name(const char *parent, GIBaseInfo *info);
static gchar *callable_public_name(const char *namespace_, const char *parent, GICallableInfo *info);

static void export_callback_info(GString **export, const char *namespace_, const char *parent, GICallableInfo *info);
static void export_callable_info(GString **export, const char *namespace_, const char *parent, GICallableInfo *info);
static void export_type_info(GString **export, const char *namespace_, const char *parent, GIRegisteredTypeInfo *info);
static void export_constant_info(GString **export, const char *namespace_, const char *parent, GIConstantInfo *info);
static void export_method_info(GString **export, const char *namespace_, const char *parent, GICallableInfo *info);
static void export_enum_info(GString **export, const char *namespace_, const char *parent, GIEnumInfo *info);
static void export_flag_info(GString **export, const char *namespace_, const char *parent, GIEnumInfo *info);
static char *gname_to_scm_name(const char *gname);
static char *gname_to_scm_constant_name(const char *gname);

#define MAX_GERROR_MSG 100
static char gerror_msg[MAX_GERROR_MSG];

static void
store_gerror_message(const char *msg)
{
    memset(gerror_msg, 0, MAX_GERROR_MSG);
    strncpy(gerror_msg, msg, MAX_GERROR_MSG - 1);
    if (strlen(msg) >= MAX_GERROR_MSG - 1)
    {
        gerror_msg[MAX_GERROR_MSG - 2] = '.';
        gerror_msg[MAX_GERROR_MSG - 3] = '.';
        gerror_msg[MAX_GERROR_MSG - 4] = '.';
    }
}

// Returns, as a list of strings, the directories that GIRepository
// searches for *.typelib files.
static SCM
scm_get_typelib_search_path(void)
{
    GSList *slist = g_irepository_get_search_path();
    SCM entry;
    SCM output = SCM_EOL;

    if (slist == NULL)
        return SCM_EOL;
    do
    {
        entry = scm_from_utf8_string(slist->data);
        output = scm_append(scm_list_2(output, scm_list_1(entry)));
    } while ((slist = g_slist_next(slist)));
    return output;
}

// Adds a directory to the search path that GIRepository will use to
// search for *.typelib files
static SCM
scm_prepend_typelib_search_path(SCM s_dir)
{
    char *dir;
    SCM_ASSERT_TYPE(scm_is_string(s_dir), s_dir, SCM_ARG1, "prepend-typelib-search-path", "string");
    dir = scm_to_utf8_string(s_dir);
    g_irepository_prepend_search_path(dir);
    return SCM_UNSPECIFIED;
}

static SCM
scm_load_typelib(SCM s_namespace, SCM s_version)
{
    gchar *namespace_;
    gchar *version;
    GITypelib *tl;
    GError *error = NULL;

    SCM_ASSERT_TYPE(scm_is_string(s_namespace), s_namespace, SCM_ARG1, "load-typelib", "string");
    SCM_ASSERT_TYPE(scm_is_string(s_version), s_version, SCM_ARG2, "load-typelib", "string");

    namespace_ = scm_to_utf8_string(s_namespace);
    version = scm_to_utf8_string(s_version);

    tl = g_irepository_require(NULL, namespace_, version, 0, &error);
    if (tl == NULL)
    {
        free(version);
        free(namespace_);
        store_gerror_message(error->message);
        g_error_free(error);
        scm_misc_error("load-typelib", gerror_msg, SCM_EOL);
        return SCM_UNSPECIFIED;
    }

    g_debug("Loading irepository %s %s", namespace_, version);
    int n = g_irepository_get_n_infos(NULL, namespace_);
    for (int i = 0; i < n; i++)
    {
        GIBaseInfo *info;
        GIInfoType type;
        info = g_irepository_get_info(NULL, namespace_, i);
        if (g_base_info_is_deprecated(info))
        {
            g_base_info_unref(info);
            continue;
        }
        type = g_base_info_get_type(info);
        switch (type)
        {
        case GI_INFO_TYPE_CALLBACK:
            hash_table_insert("%gi-callbacks", namespace_, NULL, info);
            break;
        case GI_INFO_TYPE_FUNCTION:
            hash_table_insert("%gi-functions", namespace_, NULL, info);
            break;
        case GI_INFO_TYPE_STRUCT:
        {
            GType gtype = g_registered_type_info_get_g_type(info);
            if (gtype == G_TYPE_NONE)
            {
                g_debug("Not loading struct type '%s' because is has no GType",
                    g_base_info_get_name(info));
                g_base_info_unref(info);
                break;
            }
            g_base_info_ref(info);
            g_type_set_qdata(gtype, gtype_base_info_key, info);
            hash_table_insert("%gi-structs", namespace_, NULL, info);
            gint n_methods = g_struct_info_get_n_methods(info);
            for (gint m = 0; m < n_methods; m++)
            {
                GIFunctionInfo *func_info = g_struct_info_get_method(info, m);
                if (g_function_info_get_flags(func_info) & GI_FUNCTION_IS_METHOD)
                    method_table_insert(gtype, func_info);
                else
                    hash_table_insert("%gi-functions", namespace_, g_base_info_get_name(info), func_info);
            }
        }
        break;
        case GI_INFO_TYPE_ENUM:
            hash_table_insert("%gi-enums", namespace_, NULL, info);
            break;
        case GI_INFO_TYPE_FLAGS:
            hash_table_insert("%gi-flags", namespace_, NULL, info);
            break;
        case GI_INFO_TYPE_OBJECT:
        {
            GType gtype = g_registered_type_info_get_g_type(info);
            if (gtype == G_TYPE_NONE)
            {
                g_debug("Not loading object type '%s' because is has no GType",
                    g_base_info_get_name(info));
                g_base_info_unref(info);
                break;
            }
            g_base_info_ref(info);
            g_type_set_qdata(gtype, gtype_base_info_key, info);
            hash_table_insert("%gi-objects", namespace_, NULL, info);
            gint n_methods = g_object_info_get_n_methods(info);
            for (gint m = 0; m < n_methods; m++)
            {
                GIFunctionInfo *func_info = g_object_info_get_method(info, m);
                if (g_function_info_get_flags(func_info) & GI_FUNCTION_IS_METHOD)
                    method_table_insert(gtype, func_info);
                else
                    hash_table_insert("%gi-functions", namespace_, g_base_info_get_name(info), func_info);
            }
#if 0
            gint n_signals = g_object_info_get_n_signals(info);
            for (gint m = 0; m < n_signals; m++) {
                GISignalInfo *sig_info = g_object_info_get_signal(info, m);
                if (!(g_signal_info_get_flags(sig_info) & G_SIGNAL_DEPRECATED)) {
                    if (!insert_into_signal_table(gtype, sig_info, &is_new_method))
                        g_base_info_unref(sig_info);
                    else
                        export_signal_info(&export, g_base_info_get_name(info), sig_info, is_new_method);
                }
            }
#endif
        }
        break;
        case GI_INFO_TYPE_INTERFACE:
            hash_table_insert("%gi-interfaces", namespace_, NULL, info);
            break;
        case GI_INFO_TYPE_CONSTANT:
            hash_table_insert("%gi-constants", namespace_, NULL, info);
            break;
        case GI_INFO_TYPE_UNION:
        {
            GType gtype = g_registered_type_info_get_g_type(info);
            if (gtype == G_TYPE_NONE)
            {
                g_debug("Not loading union type '%s' because is has no GType",
                    g_base_info_get_name(info));
                g_base_info_unref(info);
                break;
            }
            hash_table_insert("%gi-unions", namespace_, NULL, info);
            gint n_methods = g_union_info_get_n_methods(info);
            for (gint m = 0; m < n_methods; m++)
            {
                GIFunctionInfo *func_info = g_object_info_get_method(info, m);
                if (g_function_info_get_flags(func_info) & GI_FUNCTION_IS_METHOD)
                    method_table_insert(gtype, func_info);
                else
                    hash_table_insert("%gi-functions", namespace_, g_base_info_get_name(info), func_info);
            }
        }
        break;
        case GI_INFO_TYPE_VALUE:
            g_critical("Unsupported irepository type 'VALUE'");
            break;
        case GI_INFO_TYPE_SIGNAL:
            g_critical("Unsupported irepository type 'SIGNAL'");
            break;
        case GI_INFO_TYPE_VFUNC:
            g_critical("Unsupported irepository type 'VFUNC'");
            break;
        case GI_INFO_TYPE_PROPERTY:
            g_critical("Unsupported irepository type 'PROPERTY'");
            break;
        case GI_INFO_TYPE_FIELD:
            g_critical("Unsupported irepository type 'FIELD'");
            break;
        case GI_INFO_TYPE_ARG:
            g_critical("Unsupported irepository type 'ARG'");
            break;
        case GI_INFO_TYPE_TYPE:
            g_critical("Unsupported irepository type 'TYPE'");
            break;
        case GI_INFO_TYPE_INVALID:
        case GI_INFO_TYPE_INVALID_0:
        default:
            g_critical("Unsupported irepository type %d", type);
            break;
        }
    }
    free(version);
    free(namespace_);

    return SCM_UNSPECIFIED;
}

// This returns a Guile hash table with the variable name NAME,
// creating it if necessary.
static SCM
get_hash_table(const char *name)
{
    SCM hashtable;

    g_assert(name != NULL);

    hashtable = scm_module_variable(scm_current_module(), scm_from_utf8_symbol(name));

    if (scm_is_false(hashtable))
    {
        g_debug("Creating hash table %s", name);
        scm_permanent_object(scm_c_define(name, scm_c_make_hash_table(GIR_FUNC2_INIT_HASH_TABLE_SIZE)));
        hashtable = scm_module_variable(scm_current_module(), scm_from_utf8_symbol(name));
    }

    g_assert(scm_is_true(scm_hash_table_p(scm_variable_ref(hashtable))));

    return scm_variable_ref(hashtable);
}

static void
hash_table_insert(const char *table_name, const char *namespace_,
    const char *parent,
    GIBaseInfo *info)
{
    SCM h;
    gchar *full_name;
    SCM s_info;

    g_assert(table_name != NULL);
    g_assert(info != NULL);

    h = get_hash_table(table_name);

    if (parent)
        full_name = g_strdup_printf("%s-%s-%s", namespace_, parent, g_base_info_get_name(info));
    else
        full_name = g_strdup_printf("%s-%s", namespace_, g_base_info_get_name(info));

    s_info = scm_from_pointer(info, (scm_t_pointer_finalizer)g_base_info_unref);
    g_debug("%s[%s] = %p", table_name, full_name, (char *)info);
    scm_hash_set_x(h, scm_from_utf8_string(full_name), s_info);
    free(full_name);
}

/* The method table is different from other hash tables in that
 * its VALUE is itself a hashtable mapping a GTYPE to a
 * FUNC_INFO.
 * This is because many methods have the same name but operate
 * on different GTypes */
static void
method_table_insert(GType type, GIFunctionInfo *info)
{
    SCM h, subhash;
    SCM s_name = scm_from_utf8_string(g_base_info_get_name(info));

    g_assert(type != 0);
    g_assert(info != NULL);

    h = get_hash_table("%gi-methods");

    subhash = scm_hash_ref(h,
        s_name,
        SCM_BOOL_F);
    if (scm_is_false(subhash))
    {
        scm_hash_set_x(h, s_name, scm_c_make_hash_table(1));
        subhash = scm_hash_ref(h, s_name, SCM_BOOL_F);
        g_assert(scm_is_true(scm_hash_table_p(subhash)));
    }
    scm_hash_set_x(subhash,
        scm_from_size_t(type),
        scm_from_pointer(info, (scm_t_pointer_finalizer)g_base_info_unref));
}

static SCM
scm_import_typelib(SCM s_namespace, SCM s_version)
{
    gchar *namespace_;
    gchar *version;
    GITypelib *tl;
    GError *error = NULL;
    GString *export;

    SCM_ASSERT(scm_is_string(s_namespace), s_namespace, SCM_ARG1, "%import-typelib");
    SCM_ASSERT(scm_is_string(s_version), s_version, SCM_ARG2, "%import-typelib");

    namespace_ = scm_to_utf8_string(s_namespace);
    version = scm_to_utf8_string(s_version);

    tl = g_irepository_require(NULL, namespace_, version, 0, &error);
    if (tl == NULL)
    {
        free(version);
        free(namespace_);
        store_gerror_message(error->message);
        g_error_free(error);
        scm_misc_error("%import-typelib", gerror_msg, SCM_EOL);
        return SCM_UNSPECIFIED;
    }

    export = g_string_new_len(NULL, 128 * 1024);
    g_string_append_printf(export, ";; Declaration for %s %s\n", namespace_, version);

    int n = g_irepository_get_n_infos(NULL, namespace_);
    for (int i = 0; i < n; i++)
    {
        GIBaseInfo *info;
        GIInfoType type;
        info = g_irepository_get_info(NULL, namespace_, i);
        if (g_base_info_is_deprecated(info))
        {
            g_base_info_unref(info);
            continue;
        }
        type = g_base_info_get_type(info);
        switch (type)
        {
        case GI_INFO_TYPE_CALLBACK:
            export_callback_info(&export, namespace_, NULL, info);
            break;
        case GI_INFO_TYPE_FUNCTION:
            export_callable_info(&export, namespace_, NULL, info);
            break;
        case GI_INFO_TYPE_STRUCT:
        {
            GType gtype = g_registered_type_info_get_g_type(info);
            if (gtype == G_TYPE_NONE)
            {
                g_base_info_unref(info);
                break;
            }
            g_base_info_ref(info);
            export_type_info(&export, namespace_, NULL, info);
            gint n_methods = g_struct_info_get_n_methods(info);
            for (gint m = 0; m < n_methods; m++)
            {
                GIFunctionInfo *func_info = g_struct_info_get_method(info, m);
                if (g_function_info_get_flags(func_info) & GI_FUNCTION_IS_METHOD)
                    export_method_info(&export, namespace_, g_base_info_get_name(info), func_info);
                else
                    export_callable_info(&export, namespace_, g_base_info_get_name(info), func_info);
            }
        }
        break;
        case GI_INFO_TYPE_ENUM:
            export_enum_info(&export, namespace_, NULL, info);
            break;
        case GI_INFO_TYPE_FLAGS:
            export_flag_info(&export, namespace_, NULL, info);
            break;
        case GI_INFO_TYPE_OBJECT:
        {
            GType gtype = g_registered_type_info_get_g_type(info);
            if (gtype == G_TYPE_NONE)
            {
                g_base_info_unref(info);
                break;
            }
            export_type_info(&export, namespace_, NULL, info);
            gint n_methods = g_object_info_get_n_methods(info);
            for (gint m = 0; m < n_methods; m++)
            {
                GIFunctionInfo *func_info = g_object_info_get_method(info, m);
                if (g_function_info_get_flags(func_info) & GI_FUNCTION_IS_METHOD)
                    export_method_info(&export, namespace_, g_base_info_get_name(info), func_info);
                else
                    export_callable_info(&export, namespace_, g_base_info_get_name(info), func_info);
            }
#if 0
            gint n_signals = g_object_info_get_n_signals(info);
            for (gint m = 0; m < n_signals; m++) {
                GISignalInfo *sig_info = g_object_info_get_signal(info, m);
                if (!(g_signal_info_get_flags(sig_info) & G_SIGNAL_DEPRECATED)) {
                    if (!insert_into_signal_table(gtype, sig_info, &is_new_method))
                        g_base_info_unref(sig_info);
                    else
                        export_signal_info(&export, g_base_info_get_name(info), sig_info, is_new_method);
                }
            }
#endif
        }
        break;
        case GI_INFO_TYPE_INTERFACE:
            // export_interface_info(&export, g_base_info_get_name(info), info);
            break;
        case GI_INFO_TYPE_CONSTANT:
            export_constant_info(&export, namespace_, NULL, info);
            break;
        case GI_INFO_TYPE_UNION:
        {
            GType gtype = g_registered_type_info_get_g_type(info);
            if (gtype == G_TYPE_NONE)
            {
                g_base_info_unref(info);
                break;
            }
            export_type_info(&export, namespace_, NULL, info);
            gint n_methods = g_union_info_get_n_methods(info);
            for (gint m = 0; m < n_methods; m++)
            {
                GIFunctionInfo *func_info = g_object_info_get_method(info, m);
                if (g_function_info_get_flags(func_info) & GI_FUNCTION_IS_METHOD)
                    export_method_info(&export, namespace_, g_base_info_get_name(info), func_info);
                else
                    export_callable_info(&export, namespace_, g_base_info_get_name(info), func_info);
            }
        }
        break;
        case GI_INFO_TYPE_VALUE:
            g_critical("Unsupported irepository type 'VALUE'");
            break;
        case GI_INFO_TYPE_SIGNAL:
            g_critical("Unsupported irepository type 'SIGNAL'");
            break;
        case GI_INFO_TYPE_VFUNC:
            g_critical("Unsupported irepository type 'VFUNC'");
            break;
        case GI_INFO_TYPE_PROPERTY:
            g_critical("Unsupported irepository type 'PROPERTY'");
            break;
        case GI_INFO_TYPE_FIELD:
            g_critical("Unsupported irepository type 'FIELD'");
            break;
        case GI_INFO_TYPE_ARG:
            g_critical("Unsupported irepository type 'ARG'");
            break;
        case GI_INFO_TYPE_TYPE:
            g_critical("Unsupported irepository type 'TYPE'");
            break;
        case GI_INFO_TYPE_INVALID:
        case GI_INFO_TYPE_INVALID_0:
        default:
            g_critical("Unsupported irepository type %d", type);
            break;
        }
    }
    free(version);
    free(namespace_);

    return scm_take_locale_string(g_string_free(export, FALSE));
}

static SCM
scm_export_typelib(SCM s_namespace, SCM s_version)
{
    gchar *namespace_;
    gchar *version;
    GITypelib *tl;
    GError *error = NULL;
    GString *export;
    GSList *consts = NULL;
    GSList *funcs = NULL;
    GSList *types = NULL;

    SCM_ASSERT(scm_is_string(s_namespace), s_namespace, SCM_ARG1, "%export-typelib");
    SCM_ASSERT(scm_is_string(s_version), s_version, SCM_ARG2, "%export-typelib");

    namespace_ = scm_to_utf8_string(s_namespace);
    version = scm_to_utf8_string(s_version);

    tl = g_irepository_require(NULL, namespace_, version, 0, &error);
    if (tl == NULL)
    {
        free(version);
        free(namespace_);
        store_gerror_message(error->message);
        g_error_free(error);
        scm_misc_error("%export-typelib", gerror_msg, SCM_EOL);
        return SCM_UNSPECIFIED;
    }

    int n = g_irepository_get_n_infos(NULL, namespace_);
    for (int i = 0; i < n; i++)
    {
        GIBaseInfo *info;
        GIInfoType type;
        info = g_irepository_get_info(NULL, namespace_, i);

        if (g_base_info_is_deprecated(info))
        {
            g_base_info_unref(info);
            continue;
        }
        type = g_base_info_get_type(info);
        switch (type)
        {
        case GI_INFO_TYPE_CALLBACK:
            break;
        case GI_INFO_TYPE_FUNCTION:
        {
            char *public_name = callable_public_name(namespace_, NULL, info);
            funcs = g_slist_insert_sorted(funcs, public_name, (GCompareFunc)strcmp);
            break;
        }
        case GI_INFO_TYPE_STRUCT:
        case GI_INFO_TYPE_UNION:
        {
            GType gtype = g_registered_type_info_get_g_type(info);
            if (gtype == G_TYPE_NONE)
            {
                g_base_info_unref(info);
                break;
            }
            types = g_slist_insert_sorted(types,
                g_strdup_printf("<%s%s>", namespace_, g_base_info_get_name(info)),
                (GCompareFunc)strcmp);
            gint n_methods = g_struct_info_get_n_methods(info);
            for (gint m = 0; m < n_methods; m++)
            {
                GIFunctionInfo *func_info = g_struct_info_get_method(info, m);
                char *public_name = callable_public_name(namespace_, g_base_info_get_name(info), func_info);
                funcs = g_slist_insert_sorted(funcs, public_name, (GCompareFunc)strcmp);
                g_base_info_unref(func_info);
            }
            break;
        }

        case GI_INFO_TYPE_ENUM:
        case GI_INFO_TYPE_FLAGS:
        case GI_INFO_TYPE_CONSTANT:
        {
            char *public_name = flag_public_name(NULL, info);
            consts = g_slist_insert_sorted(consts, public_name, (GCompareFunc)strcmp);
        }
        break;
        case GI_INFO_TYPE_OBJECT:
        {
            GType gtype = g_registered_type_info_get_g_type(info);
            if (gtype == G_TYPE_NONE)
            {
                g_base_info_unref(info);
                break;
            }
            types = g_slist_insert_sorted(types,
                g_strdup_printf("<%s%s>", namespace_, g_base_info_get_name(info)),
                (GCompareFunc)strcmp);
            gint n_methods = g_object_info_get_n_methods(info);
            for (gint m = 0; m < n_methods; m++)
            {
                GIFunctionInfo *func_info = g_object_info_get_method(info, m);
                char *public_name = callable_public_name(namespace_, g_base_info_get_name(info), func_info);
                funcs = g_slist_insert_sorted(funcs, public_name, (GCompareFunc)strcmp);
                g_base_info_unref(func_info);
            }
#if 0
            gint n_signals = g_object_info_get_n_signals(info);
            for (gint m = 0; m < n_signals; m++) {
                GISignalInfo *sig_info = g_object_info_get_signal(info, m);
                if (!(g_signal_info_get_flags(sig_info) & G_SIGNAL_DEPRECATED)) {
                    if (!insert_into_signal_table(gtype, sig_info, &is_new_method))
                        g_base_info_unref(sig_info);
                    else
                        export_signal_info(&export, g_base_info_get_name(info), sig_info, is_new_method);
                }
            }
#endif
        }
        break;
        case GI_INFO_TYPE_INTERFACE:
            // export_interface_info(&export, g_base_info_get_name(info), info);
            break;
        case GI_INFO_TYPE_VALUE:
            g_critical("Unsupported irepository type 'VALUE'");
            break;
        case GI_INFO_TYPE_SIGNAL:
            g_critical("Unsupported irepository type 'SIGNAL'");
            break;
        case GI_INFO_TYPE_VFUNC:
            g_critical("Unsupported irepository type 'VFUNC'");
            break;
        case GI_INFO_TYPE_PROPERTY:
            g_critical("Unsupported irepository type 'PROPERTY'");
            break;
        case GI_INFO_TYPE_FIELD:
            g_critical("Unsupported irepository type 'FIELD'");
            break;
        case GI_INFO_TYPE_ARG:
            g_critical("Unsupported irepository type 'ARG'");
            break;
        case GI_INFO_TYPE_TYPE:
            g_critical("Unsupported irepository type 'TYPE'");
            break;
        case GI_INFO_TYPE_INVALID:
        case GI_INFO_TYPE_INVALID_0:
        default:
            g_critical("Unsupported irepository type %d", type);
            break;
        }
    }

    export = g_string_new_len(NULL, 128 * 1024);
    g_string_append_printf(export, "(define-module (%s)\n", namespace_);
    g_string_append_printf(export, "  #:use-module (gi)\n");
    g_string_append_printf(export, "  #:export(\n");

    if (funcs)
        g_string_append_printf(export, "           ;; Functions\n");
    GSList *x = funcs;
    while (x)
    {
        g_string_append_printf(export, "           %s\n", (char *)x->data);
        x = x->next;
    }

    if (consts)
        g_string_append_printf(export, "\n           ;; Constants\n");
    x = consts;
    while (x)
    {
        g_string_append_printf(export, "           %s\n", (char *)x->data);
        x = x->next;
    }

    if (types)
        g_string_append_printf(export, "\n           ;; Types\n");
    x = types;
    while (x)
    {
        g_string_append_printf(export, "           %s\n", (char *)x->data);
        x = x->next;
    }

    g_slist_free_full(funcs, g_free);
    g_slist_free_full(consts, g_free);
    g_slist_free_full(types, g_free);
    g_string_append_printf(export, "  ))\n\n");
    g_string_append(export, "(eval-when (expand load compile)\n");
    g_string_append_printf(export, "  (load-typelib \"%s\" \"%s\"))\n\n", namespace_, version);
    g_string_append(export, scm_to_utf8_string(scm_import_typelib(s_namespace, s_version)));

    free(version);
    free(namespace_);

    return scm_take_locale_string(g_string_free(export, FALSE));
}

// #define FIGURE_OUT_ALL_ARG_TYPES
#ifdef FIGURE_OUT_ALL_ARG_TYPES
struct _arg_info_func_name
{
    GIArgInfo *ai;
    char *name;
};
static GPtrArray *gi_arg_infos = NULL;
#endif

#if 0
static guint
hash_key_func(gconstpointer v)
{
    guint hash = 5381;
    int i = 0;
    unsigned char *p = v;
    while (*p) {
        hash = hash * 33 + *p;
        p++;
        if (i++ > 100)
            break;
    }
    g_debug("string hash of %s is %u", (char *)v, hash);
    return hash;
}

static gboolean
hash_equal_func(gconstpointer a, gconstpointer b)
{
    return a == b;
}
#endif

/* Convert the type of names that GTK uses into Guile-like names */
static char *
gname_to_scm_name(const char *gname)
{
    size_t len = strlen(gname);
    GString *str = g_string_new(NULL);
    gboolean was_lower = FALSE;

    for (size_t i = 0; i < len; i++)
    {
        if (g_ascii_islower(gname[i]))
        {
            g_string_append_c(str, gname[i]);
            was_lower = TRUE;
        }
        else if (gname[i] == '_' || gname[i] == '-')
        {
            g_string_append_c(str, '-');
            was_lower = FALSE;
        }
        else if (g_ascii_isdigit(gname[i]))
        {
            g_string_append_c(str, gname[i]);
            was_lower = FALSE;
        }
        else if (g_ascii_isupper(gname[i]))
        {
            if (was_lower)
                g_string_append_c(str, '-');
            g_string_append_c(str, g_ascii_tolower(gname[i]));
            was_lower = FALSE;
        }
    }
    return g_string_free(str, FALSE);
}

static char *
gname_to_scm_constant_name(const char *gname)
{
    size_t len = strlen(gname);
    GString *str = g_string_new(NULL);
    gboolean was_lower = FALSE;

    for (size_t i = 0; i < len; i++)
    {
        if (g_ascii_islower(gname[i]))
        {
            g_string_append_c(str, g_ascii_toupper(gname[i]));
            was_lower = TRUE;
        }
        else if (gname[i] == '_' || gname[i] == '-')
        {
            g_string_append_c(str, '_');
            was_lower = FALSE;
        }
        else if (g_ascii_isdigit(gname[i]))
        {
            g_string_append_c(str, gname[i]);
            was_lower = FALSE;
        }
        else if (g_ascii_isupper(gname[i]))
        {
            if (was_lower)
                g_string_append_c(str, '_');
            g_string_append_c(str, gname[i]);
            was_lower = FALSE;
        }
    }

    char *fptr = strstr(str->str, "_FLAGS");
    if (fptr)
    {
        memcpy(fptr, fptr + 6, str->len - (fptr - str->str) - 6);
        memset(str->str + str->len - 6, 0, 6);
        str->len -= 6;
    }

    return g_string_free(str, FALSE);
}

static void
export_type_info(GString **export, const char *namespace_, const char *parent, GIRegisteredTypeInfo *info)
{
    g_assert(parent == NULL);

    g_string_append_printf(*export, "(define <%s%s>\n  (gi-lookup-type \"%s-%s\"))\n\n",
        namespace_, g_base_info_get_name(info), namespace_, g_base_info_get_name(info));
}

static void
export_constant_info(GString **export, const char *namespace_, const char *parent, GIConstantInfo *info)
{
    char *public_name = flag_public_name(namespace_, info);
    g_assert(parent == NULL);
    g_string_append_printf(*export, "(define %s\n  (gi-constant-value \"%s-%s\"))\n\n", public_name, namespace_, g_base_info_get_name(info));
    free(public_name);
}

static gchar *
flag_public_name(const char *parent, GIBaseInfo *info)
{
    char *tmp_str, *public_name;
    if (parent)
    {
        tmp_str = g_strdup_printf("%s-%s", parent, g_base_info_get_name(info));
        public_name = gname_to_scm_constant_name(tmp_str);
    }
    else
    {
        tmp_str = g_strdup_printf("%s", g_base_info_get_name(info));
        public_name = gname_to_scm_constant_name(tmp_str);
    }
    g_free(tmp_str);
    return public_name;
}

static void
export_enum_info(GString **export, const char *namespace_, const char *parent, GIEnumInfo *info)
{
    gint n_values = g_enum_info_get_n_values(info);
    gint i = 0;
    GIValueInfo *vi = NULL;

    g_assert(parent == NULL);

    while (i < n_values)
    {
        vi = g_enum_info_get_value(info, i);
        char *public_name, *lookup_name;
        public_name = flag_public_name(g_base_info_get_name(info), vi);
        g_string_append_printf(*export, "(define %s\n  (gi-enum-value \"%s-%s\" \"%s\"))\n\n",
            public_name, namespace_, g_base_info_get_name(info), g_base_info_get_name(vi));
        g_base_info_unref(vi);
        vi = NULL;
        free(public_name);
        i++;
    }
}



static void
export_flag_info(GString **export, const char *namespace_, const char *parent, GIEnumInfo *info)
{
    gint n_values = g_enum_info_get_n_values(info);
    gint i = 0;
    GIValueInfo *vi = NULL;
    char *public_name;

    while (i < n_values)
    {
        vi = g_enum_info_get_value(info, i);
        public_name = flag_public_name(g_base_info_get_name(info), vi);

        g_string_append_printf(*export, "(define %s\n  (gi-flag-value \"%s-%s\" \"%s\"))\n\n",
            public_name, namespace_, g_base_info_get_name(info), g_base_info_get_name(vi));
        g_base_info_unref(vi);
        free(public_name);
        i++;
    }
}

static void
export_callable_argument_description(GString **export, GICallableInfo *info, gboolean style)
{
    gint n_args;
    GIArgInfo *arg;
    GIDirection dir;
    GITypeInfo *type_info;
    char *name;

    n_args = g_callable_info_get_n_args(info);

    if (style)
        g_string_append(*export, ";; ARGS: \n");
    else
        g_string_append(*export, "\"  ARGS: \n");

    for (int i = 0; i < n_args; i++)
    {
        arg = g_callable_info_get_arg(info, i);
        dir = g_arg_info_get_direction(arg);
        type_info = g_arg_info_get_type(arg);
        if (!(dir == GI_DIRECTION_OUT && g_arg_info_is_caller_allocates(arg)))
        {
            if (style)
                g_string_append(*export, ";;   ");
            else
                g_string_append(*export, "     ");

            name = gname_to_scm_name(g_base_info_get_name(arg));
            g_string_append(*export, name);
            g_string_append_c(*export, ' ');
            char *desc = gi_giargument_describe_arg_in(arg);
            g_string_append_printf(*export, " - %s", desc);
            g_free(desc);
            if (dir == GI_DIRECTION_INOUT)
                g_string_append(*export, "[INOUT] ");
            else if (dir == GI_DIRECTION_OUT)
                g_string_append(*export, "[OUT]");
            free(name);
            if (i + 1 < n_args)
                g_string_append(*export, ", ");

            g_string_append_c(*export, '\n');
        }
        g_base_info_unref(type_info);
        g_base_info_unref(arg);
    }

    type_info = g_callable_info_get_return_type(info);
    if (style)
        g_string_append_printf(*export, ";; RETURN: %s%s\n",
            g_type_tag_to_string(g_type_info_get_tag(type_info)),
            g_type_info_is_pointer(type_info) ? "*" : "");
    else
        g_string_append_printf(*export, "   RETURN: %s%s\n",
            g_type_tag_to_string(g_type_info_get_tag(type_info)),
            g_type_info_is_pointer(type_info) ? "*" : "");
    g_base_info_unref(type_info);

    for (int i = 0; i < n_args; i++)
    {
        arg = g_callable_info_get_arg(info, i);
        dir = g_arg_info_get_direction(arg);
        type_info = g_arg_info_get_type(arg);
        if (dir == GI_DIRECTION_OUT && g_arg_info_is_caller_allocates(arg))
        {
            if (style)
                g_string_append(*export, ";;   ");
            else
                g_string_append(*export, "     ");

            name = gname_to_scm_name(g_base_info_get_name(arg));
            g_string_append(*export, name);
            g_string_append_c(*export, ' ');
            char *desc = gi_giargument_describe_arg_in(arg);
            g_string_append_printf(*export, " - %s", desc);
            g_free(desc);
            free(name);
            if (i + 1 < n_args)
                g_string_append(*export, ", ");

            g_string_append_c(*export, '\n');
        }
        g_base_info_unref(type_info);
        g_base_info_unref(arg);
    }

    if (!style)
        g_string_append(*export, "\"\n");

}

static gchar *
callback_public_name(const char *namespace_, const char *parent, GICallableInfo *info)
{
    char *public_name;
    char *tmp_str;

    if (parent)
    {
        tmp_str = g_strdup_printf("%s-%s", parent, g_base_info_get_name(info));
        public_name = gname_to_scm_name(tmp_str);
        g_free(tmp_str);
    }
    else
    {
        tmp_str = g_strdup_printf("%s", g_base_info_get_name(info));
        public_name = gname_to_scm_name(tmp_str);
        g_free(tmp_str);
    }
    return public_name;
}
static void
export_callback_info(GString **export, const char *namespace_, const char *parent, GICallableInfo *info)
{
    gint n_args;
    char *lookup_name;
    char *public_name;

    n_args = g_callable_info_get_n_args(info);

    if (parent)
        lookup_name = g_strdup_printf("%s-%s-%s", namespace_, parent, g_base_info_get_name(info));
    else
        lookup_name = g_strdup_printf("%s-%s", namespace_, g_base_info_get_name(info));

    public_name = callback_public_name(namespace_, parent, info);

    g_string_append_printf(*export, ";; CALLBACK\n");
    g_string_append_printf(*export, "(define %s", public_name);

    GITypeInfo *return_type = g_callable_info_get_return_type(info);
    g_assert(return_type);
    //if (g_type_info_get_tag(return_type) == GI_TYPE_TAG_BOOLEAN && !g_type_info_is_pointer(return_type))
    //    g_string_append_c(*export, '?');
    g_base_info_unref(return_type);

    g_string_append_c(*export, '\n');

    g_string_append_printf(*export, "  (gi-lookup-callback-info \"%s\"))\n", lookup_name);

    export_callable_argument_description(export, info, TRUE);
    g_free(lookup_name);
    g_free(public_name);
}

static gchar*
callable_public_name(const char *namespace_, const char *parent, GICallableInfo *info)
{
    char *public_name, *tmp_str, *tmp_str2;
    GITypeInfo *return_type;

    return_type = g_callable_info_get_return_type(info);
    g_assert(return_type);

    if (parent)
    {
        tmp_str = g_strdup_printf("%s%s", namespace_, parent);
        tmp_str2 = gname_to_scm_name(g_base_info_get_name(info));
        if (g_type_info_get_tag(return_type) == GI_TYPE_TAG_BOOLEAN && !g_type_info_is_pointer(return_type))
            public_name = g_strdup_printf("%s-%s?", tmp_str, tmp_str2);
        else
            public_name = g_strdup_printf("%s-%s", tmp_str, tmp_str2);
        g_free(tmp_str);
        g_free(tmp_str2);
    }
    else
    {
        if (g_type_info_get_tag(return_type) == GI_TYPE_TAG_BOOLEAN && !g_type_info_is_pointer(return_type))
            tmp_str = g_strdup_printf("%s-%s?", namespace_, g_base_info_get_name(info));
        else
            tmp_str = g_strdup_printf("%s-%s", namespace_, g_base_info_get_name(info));
        public_name = gname_to_scm_name(tmp_str);
        g_free(tmp_str);
    }

    g_base_info_unref(return_type);
    return public_name;
}


static void
export_callable_info(GString **export, const char *namespace_, const char *parent, GICallableInfo *info)
{
    gint n_args;
    GIArgInfo *arg;
    char *public_name, *lookup_name;
    GITypeInfo *return_type;

    n_args = g_callable_info_get_n_args(info);
    return_type = g_callable_info_get_return_type(info);
    g_assert(return_type);

    if (parent)
        lookup_name = g_strdup_printf("%s-%s-%s", namespace_, parent, g_base_info_get_name(info));
    else
        lookup_name = g_strdup_printf("%s-%s", namespace_, g_base_info_get_name(info));
    public_name = callable_public_name(namespace_, parent, info);
    g_string_append_printf(*export, "(define (%s", public_name);

    for (int i = 0; i < n_args; i++)
    {
        arg = g_callable_info_get_arg(info, i);
        GIDirection dir = g_arg_info_get_direction(arg);
        if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT
            || (dir == GI_DIRECTION_OUT && g_arg_info_is_caller_allocates(arg)))
        {
            char *arg_name;
            g_string_append_c(*export, ' ');
            arg_name = gname_to_scm_name(g_base_info_get_name(arg));
            if (dir == GI_DIRECTION_OUT)
                g_string_append(*export, "out-");
            g_string_append(*export, arg_name);
            free(arg_name);
        }
#ifdef FIGURE_OUT_ALL_ARG_TYPES
        struct _arg_info_func_name *aifn = g_new(struct _arg_info_func_name, 1);
        aifn->ai = arg;
        aifn->name = g_strdup(c_function_name);
        g_ptr_array_add(gi_arg_infos, aifn);
#else
        g_base_info_unref(arg);
#endif
    }

    g_string_append_c(*export, ')');
    g_string_append_c(*export, '\n');

    export_callable_argument_description(export, info, FALSE);

    g_string_append_printf(*export, "  (gi-function-invoke \"%s\"", lookup_name);

    for (int i = 0; i < n_args; i++)
    {
        arg = g_callable_info_get_arg(info, i);
        GIDirection dir = g_arg_info_get_direction(arg);
        if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT
            || (dir == GI_DIRECTION_OUT && g_arg_info_is_caller_allocates(arg)))
        {
            g_string_append_c(*export, ' ');
            char *arg_name = gname_to_scm_name(g_base_info_get_name(arg));
            if (dir == GI_DIRECTION_OUT)
                g_string_append(*export, "out-");
            g_string_append(*export, arg_name);
            free(arg_name);
        }
        g_base_info_unref(arg);
    }

    g_string_append(*export, "))\n\n");

    g_base_info_unref(return_type);
    g_free(lookup_name);
    g_free(public_name);
}

static void
export_method_info(GString **export, const char *namespace_, const char *parent, GICallableInfo *info)
{
    gint n_args;
    GIArgInfo *arg;
    char *lookup_name, *public_name, *tmp_str, *tmp_str2;
    GITypeInfo *return_type;

    n_args = g_callable_info_get_n_args(info);
    return_type = g_callable_info_get_return_type(info);
    g_assert(return_type);
    g_assert(parent != NULL);

    lookup_name = g_strdup_printf("%s", g_base_info_get_name(info));
    tmp_str = g_strdup_printf("%s%s", namespace_, parent);
    tmp_str2 = gname_to_scm_name(g_base_info_get_name(info));
    if (g_type_info_get_tag(return_type) == GI_TYPE_TAG_BOOLEAN && !g_type_info_is_pointer(return_type))
        public_name = g_strdup_printf("%s-%s?", tmp_str, tmp_str2);
    else
        public_name = g_strdup_printf("%s-%s", tmp_str, tmp_str2);
    g_free(tmp_str);
    g_free(tmp_str2);

    g_string_append_printf(*export, "(define (%s self", public_name);

    for (int i = 0; i < n_args; i++)
    {
        arg = g_callable_info_get_arg(info, i);
        GIDirection dir = g_arg_info_get_direction(arg);
        if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT || (dir == GI_DIRECTION_OUT && g_arg_info_is_caller_allocates(arg)))
        {
            g_string_append_c(*export, ' ');
            tmp_str = gname_to_scm_name(g_base_info_get_name(arg));
            if (dir == GI_DIRECTION_OUT)
                g_string_append(*export, "out-");
            g_string_append(*export, tmp_str);
            free(tmp_str);
        }
#ifdef FIGURE_OUT_ALL_ARG_TYPES
        struct _arg_info_func_name *aifn = g_new(struct _arg_info_func_name, 1);
        aifn->ai = arg;
        aifn->name = g_strdup(c_function_name);
        g_ptr_array_add(gi_arg_infos, aifn);
#else
        g_base_info_unref(arg);
#endif
    }

    g_string_append_c(*export, ')');
    g_string_append_c(*export, '\n');

    export_callable_argument_description(export, info, FALSE);

    g_string_append_printf(*export, "  (gi-method-send self \n");
    g_string_append_printf(*export, "     (gi-method-prepare \"%s\"", lookup_name);

    for (int i = 0; i < n_args; i++)
    {
        arg = g_callable_info_get_arg(info, i);
        GIDirection dir = g_arg_info_get_direction(arg);
        if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT || (dir == GI_DIRECTION_OUT && g_arg_info_is_caller_allocates(arg)))
        {
            g_string_append_c(*export, ' ');
            tmp_str = gname_to_scm_name(g_base_info_get_name(arg));
            if (dir == GI_DIRECTION_OUT)
                g_string_append(*export, "out-");
            g_string_append(*export, tmp_str);
            free(tmp_str);
        }
        g_base_info_unref(arg);
    }

    g_string_append(*export, ")))\n\n");
    g_free(lookup_name);
    g_free(public_name);
}

/* FIXME: this is a very sigmal way to export signal info */
static void
export_signal_info(GString **export, char *parent, GISignalInfo *info)
{
    gint n_args;
    GIArgInfo *arg;

    n_args = g_callable_info_get_n_args(info);

    char *c_function_name;
    if (parent)
        c_function_name = g_strdup_printf("%s-%s-signal", parent, g_base_info_get_name(info));
    else
        c_function_name = g_strdup_printf("%s-signal", g_base_info_get_name(info));

    char *name = gname_to_scm_constant_name(c_function_name);

    g_string_append_printf(*export, "(define (%s", name);
    free(name);

    GITypeInfo *return_type = g_callable_info_get_return_type(info);
    g_assert(return_type);
    if (g_type_info_get_tag(return_type) == GI_TYPE_TAG_BOOLEAN && !g_type_info_is_pointer(return_type))
        g_string_append_c(*export, '?');
    g_base_info_unref(return_type);

    g_string_append_printf(*export, " self");

    for (int i = 0; i < n_args; i++)
    {
        arg = g_callable_info_get_arg(info, i);
        GIDirection dir = g_arg_info_get_direction(arg);
        if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT)
        {
            g_string_append_c(*export, ' ');
            name = gname_to_scm_name(g_base_info_get_name(arg));
            g_string_append(*export, name);
            free(name);
        }
        g_base_info_unref(arg);
    }

    g_string_append_c(*export, ')');
    g_string_append_c(*export, '\n');

    g_string_append_printf(*export, "  (gi-method-send self \n");
    g_string_append_printf(*export, "     (gi-method-prepare \"%s\"", g_base_info_get_name(info));

    for (int i = 0; i < n_args; i++)
    {
        arg = g_callable_info_get_arg(info, i);
        GIDirection dir = g_arg_info_get_direction(arg);
        if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT)
        {
            g_string_append_c(*export, ' ');
            name = gname_to_scm_name(g_base_info_get_name(arg));
            g_string_append(*export, name);
            free(name);
        }
        g_base_info_unref(arg);
    }

    g_string_append(*export, ")))\n\n");
}

static SCM
scm_gi_constant_value(SCM s_name)
{
    char *name;

    SCM_ASSERT(scm_is_string(s_name), s_name, SCM_ARG1, "gi-constant-value");
    name = scm_to_utf8_string(s_name);

    SCM h = get_hash_table("%gi-constants");
    SCM val = scm_hash_ref(h, s_name, SCM_BOOL_F);
    if (scm_is_false(val))
    {
        free(name);
        scm_misc_error("gi-constant-value",
            "unknown constant ~a",
            scm_list_1(s_name));
        return SCM_UNSPECIFIED;
    }

    g_assert(SCM_POINTER_P(val));

    GIConstantInfo *info = scm_to_pointer(val);
    GITypeInfo *typeinfo;
    typeinfo = g_constant_info_get_type(info);
    GITypeTag typetag;
    typetag = g_type_info_get_tag(typeinfo);

    GIArgument value;
    SCM ret;

    g_constant_info_get_value(info, &value);

    switch (typetag)
    {
    case GI_TYPE_TAG_BOOLEAN:
        ret = scm_from_bool(value.v_boolean);
        break;
    case GI_TYPE_TAG_DOUBLE:
        ret = scm_from_double(value.v_double);
        break;
    case GI_TYPE_TAG_INT8:
        ret = scm_from_int8(value.v_int8);
        break;
    case GI_TYPE_TAG_INT16:
        ret = scm_from_int16(value.v_int16);
        break;
    case GI_TYPE_TAG_INT32:
        ret = scm_from_int32(value.v_int32);
        break;
    case GI_TYPE_TAG_INT64:
        ret = scm_from_int64(value.v_int64);
        break;
    case GI_TYPE_TAG_UINT8:
        ret = scm_from_uint8(value.v_uint8);
        break;
    case GI_TYPE_TAG_UINT16:
        ret = scm_from_uint16(value.v_uint16);
        break;
    case GI_TYPE_TAG_UINT32:
        ret = scm_from_uint32(value.v_uint32);
        break;
    case GI_TYPE_TAG_UINT64:
        ret = scm_from_uint64(value.v_uint64);
        break;
    case GI_TYPE_TAG_UTF8:
        ret = scm_from_utf8_string(value.v_string);
        break;
    default:
        g_critical("Constant %s has unsupported type %d",
            name, typetag);
        ret = SCM_BOOL_F;
    }
    g_constant_info_free_value(info, &value);
    g_base_info_unref(typeinfo);
    free(name);

    return ret;
}

static SCM
scm_gi_flag_or_enum_value(SCM s_category, SCM s_name, gboolean is_enum)
{
    char *name;
    SCM h, val;

    SCM_ASSERT(scm_is_string(s_category), s_category, SCM_ARG1,
        is_enum ? "gi-enum-value" : "gi-flag-value");
    SCM_ASSERT(scm_is_string(s_name), s_name, SCM_ARG2,
        is_enum ? "gi-enum-value" : "gi-flag-value");
    name = scm_to_utf8_string(s_name);

    if (is_enum)
        h = get_hash_table("%gi-enums");
    else
        h = get_hash_table("%gi-flags");
    val = scm_hash_ref(h, s_category, SCM_BOOL_F);

    if (scm_is_false(val))
    {
        free(name);
        scm_misc_error(is_enum ? "gi-enum-value" : "gi-flag-value",
            is_enum ? "unknown enum type '~a'" : "unknown flag type '~a'",
            scm_list_1(s_category));
        return SCM_UNSPECIFIED;
    }

    g_assert(SCM_POINTER_P(val));

    GIEnumInfo *info = scm_to_pointer(val);
    gint n_values = g_enum_info_get_n_values(info);
    gint i = 0;
    GIValueInfo *vi = NULL;

    while (i < n_values)
    {
        vi = g_enum_info_get_value(info, i);
        g_assert(vi != NULL);

#ifdef VERBOSE_DEBUG
        g_debug("flag name search: %s == %s ?",
            name,
            g_base_info_get_name(vi));
#endif
        if (strcmp(g_base_info_get_name(vi), name) == 0)
        {
            break;
        }
        g_base_info_unref(vi);
        vi = NULL;
        i++;
    }

    free(name);
    name = NULL;

    if (i >= n_values)
    {
        scm_misc_error(is_enum ? "gi-enum-value" : "gi-flag-value",
            is_enum ? "unknown enum '~a' of type '~a'" : "unknown flag '~a' of type '~a'",
            scm_list_2(s_name, s_category));
        return SCM_BOOL_F;
    }
    else
    {
        SCM ret = scm_from_int64(g_value_info_get_value(vi));
        g_base_info_unref(vi);
        vi = NULL;
        return ret;
    }

    /* never reached */
    return SCM_BOOL_F;
}

static SCM
scm_gi_flag_value(SCM s_category, SCM s_name)
{
    return scm_gi_flag_or_enum_value(s_category, s_name, FALSE);
}

static SCM
scm_gi_enum_value(SCM s_category, SCM s_name)
{
    return scm_gi_flag_or_enum_value(s_category, s_name, TRUE);
}

static SCM
scm_gi_struct_ref(SCM s_ptr, SCM s_type_name, SCM s_field_name)
{
#if 0
    char *full_type_name = NULL;

    SCM_ASSERT(SCM_POINTER_P(s_ptr), s_ptr, SCM_ARG1,
        "gi-struct-ref");
    SCM_ASSERT(scm_is_string(s_namespace), s_namespace, SCM_ARG2,
        "gi-struct-ref");
    SCM_ASSERT(scm_is_string(s_type_name), s_type_name, SCM_ARG3,
        "gi-struct-ref");
    SCM_ASSERT(scm_is_string(s_field_name), s_field_name, SCM_ARG4,
        "gi-struct-ref");

#ifdef PREFIX_NAME_IN_HASH
    {
        char *namespace_ = scm_to_utf8_string(s_namespace);
        char *type_name = scm_to_utf8_string(s_type_name);
        full_type_name = g_strdup_printf("%s-%s", namespace_, type_name);
        free(type_name);
        free(namespace_);
    }
#else
    full_type_name = scm_to_utf8_string(s_type_name);
#endif

    GIStructInfo *si = g_hash_table_lookup(gi_structs, full_type_name);
    g_free(full_type_name);
    full_type_name = NULL;

    if (!si) {
        scm_misc_error("gi-struct-ref",
            "unknown struct type '~a' in ~a",
            scm_list_2(s_type_name, s_namespace));
        return SCM_BOOL_F;
    }

    gint n_fields = g_struct_info_get_n_fields(si);
    gint i = 0;
    GIFieldInfo *fi = NULL;
    char *field_name = scm_to_utf8_string(s_field_name);

    while (i < n_fields) {
        fi = g_struct_info_get_field(si, i);
        g_assert(fi != NULL);

        g_debug("field name search: %s == %s ?",
            field_name,
            g_base_info_get_name(fi));
        if (strcmp(g_base_info_get_name(fi), field_name) == 0) {
            break;
        }
        g_base_info_unref(fi);
        fi = NULL;
        i++;
    }

    free(field_name);
    if (i >= n_fields) {
        scm_misc_error("gi-struct-ref",
            "unknown field '~a' in struct '~a' in ~a",
            scm_list_3(s_field_name, s_type_name, s_namespace));
        return SCM_BOOL_F;
    }
    else {
        gboolean ok;
        GIArgument arg;
        void *ptr = scm_to_pointer(s_ptr);
        ok = g_field_info_get_field(fi, ptr, &arg);
        g_base_info_unref(fi);
        fi = NULL;

        if (!ok) {
            scm_misc_error("gi-struct-ref",
                "cannot unpack field '~a' in struct '~a'",
                scm_list_2(s_field_name, s_type_name));
            return SCM_BOOL_F;
        }
        else {
            GITypeInfo *ti = g_field_info_get_type(fi);
            SCM output = gi_giargument_to_object(&arg, ti, FALSE);
            g_base_info_unref(ti);
            return output;
        }
    }
#endif
    /* never get here */
    g_return_val_if_reached(SCM_BOOL_F);
}

static SCM
scm_gi_struct_set(SCM s_ptr, SCM s_namespace, SCM s_type_name, SCM s_field_name, SCM s_value)
{
#if 0
    char *full_type_name = NULL;

    // SCM_VALIDATE_POINTER (1, s_ptr);
    SCM_ASSERT(scm_is_string(s_namespace), s_namespace, SCM_ARG2,
        "gi-struct-set");
    SCM_ASSERT(scm_is_string(s_type_name), s_type_name, SCM_ARG3,
        "gi-struct-set");
    SCM_ASSERT(scm_is_string(s_field_name), s_field_name, SCM_ARG4,
        "gi-struct-set");

#ifdef PREFIX_NAME_IN_HASH
    {
        char *namespace_ = scm_to_utf8_string(s_namespace);
        char *type_name = scm_to_utf8_string(s_type_name);
        full_type_name = g_strdup_printf("%s-%s", namespace_, type_name);
        free(type_name);
        free(namespace_);
    }
#else
    full_type_name = scm_to_utf8_string(s_type_name);
#endif

    GIStructInfo *si = g_hash_table_lookup(gi_structs, full_type_name);
    g_free(full_type_name);
    full_type_name = NULL;

    if (!si) {
        scm_misc_error("gi-struct-ref",
            "unknown struct type '~a' in ~a",
            scm_list_2(s_type_name, s_namespace));
        return SCM_BOOL_F;
    }

    gint n_fields = g_struct_info_get_n_fields(si);
    gint i = 0;
    GIFieldInfo *fi = NULL;
    char *field_name = scm_to_utf8_string(s_field_name);

    while (i < n_fields) {
        fi = g_struct_info_get_field(si, i);
        g_assert(fi != NULL);

        g_debug("field name search: %s == %s ?",
            field_name,
            g_base_info_get_name(fi));
        if (strcmp(g_base_info_get_name(fi), field_name) == 0) {
            break;
        }
        g_base_info_unref(fi);
        fi = NULL;
        i++;
    }

    free(field_name);
    if (i >= n_fields) {
        scm_misc_error("gi-struct-set",
            "unknown field '~a' in struct '~a' in ~a",
            scm_list_3(s_field_name, s_type_name, s_namespace));
        return SCM_BOOL_F;
    }
    else {
        gboolean ok;
        GIArgument arg;
        GITypeInfo *ti = g_field_info_get_type(fi);
        arg = gi_argument_from_object("gi-struct-set",
            s_value,
            ti,
            GI_TRANSFER_NOTHING);
        g_base_info_unref(ti);
        ti = NULL;
        void *ptr = scm_to_pointer(s_ptr);
        ok = g_field_info_set_field(fi, ptr, &arg);
        g_base_info_unref(fi);
        fi = NULL;

        if (!ok) {
            scm_misc_error("gi-struct-set",
                "cannot set field '~a' in struct '~a' to '~a'",
                scm_list_3(s_field_name, s_type_name, s_value));
            return SCM_BOOL_F;
        }
        else {
            return SCM_BOOL_T;
        }
    }
#endif
    /* never get here */
    g_return_val_if_reached(SCM_BOOL_F);
}

static void
function_info_count_args(GIFunctionInfo *info, int *in, int *out)
{
    /* Count the number of required input arguments, and store
   * the arg info in a newly allocate array. */
    int n_args = g_callable_info_get_n_args((GICallableInfo *)info);
    int n_input_args = 0;
    int n_output_args = 0;

    for (int i = 0; i < n_args; i++)
    {
        GIArgInfo *ai = g_callable_info_get_arg((GICallableInfo *)info, i);
        g_assert(ai != NULL);

        GIDirection dir = g_arg_info_get_direction(ai);
        g_base_info_unref(ai);

        if (dir == GI_DIRECTION_IN)
            n_input_args++;
        else if (dir == GI_DIRECTION_OUT)
            n_output_args++;
        else if (dir == GI_DIRECTION_INOUT)
        {
            n_input_args++;
            n_output_args++;
        }
    }
    // if (g_function_info_get_flags (info) & GI_FUNCTION_IS_METHOD)
    //  n_input_args ++;
    *in = n_input_args;
    *out = n_output_args;
}

static gboolean
function_info_typecheck_args(GIFunctionInfo *func_info, SCM s_args, char **errstr)
{
#if 0
    GIDirection dir;
    GIArgInfo *arg_info;
    gboolean type_ok = FALSE;

    int n_args = g_callable_info_get_n_args((GICallableInfo *)func_info);
    int i_input = 0;

    for (int i = 0; i < n_args; i++)
    {
        arg_info = g_callable_info_get_arg((GICallableInfo *)func_info, i);
        g_assert(arg_info != NULL);

        dir = g_arg_info_get_direction(arg_info);

        if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT)
        {
            SCM arg = scm_list_ref(s_args, scm_from_int(i_input));
            type_ok = gi_giargument_check_scm_type(arg, arg_info, errstr);
            i_input++;
        }
        g_base_info_unref(arg_info);
        if (!type_ok)
        {
            char *tmp = *errstr;
            *errstr = g_strdup_printf("In arg %d %s", i + 1, tmp);
            g_free(tmp);
        }
        break;
    }
    return type_ok;
#else
    return FALSE;
#endif
}

static void
function_info_convert_args(const char *func_name, GIFunctionInfo *func_info, SCM s_args, int *n_input_args, GIArgument **in_args, unsigned **in_args_free, int *n_output_args, GIArgument **out_args)
{
    int n_args_received;
    int n_args;
    int i_input_arg, i_output_arg, i_received_arg;
    GIArgInfo *arg_info;
    GIDirection dir;
    GIArgumentStatus status = GI_GIARGUMENT_ERROR;
    SCM obj;

    // Count the number of required input arguments, and store
    // the arg info in a newly allocate array.
    if (SCM_UNBNDP(s_args))
        n_args_received = 0;
    else
        n_args_received = scm_to_int(scm_length(s_args));
    n_args = g_callable_info_get_n_args((GICallableInfo *)func_info);
    function_info_count_args(func_info, n_input_args, n_output_args);
    g_debug("%s: %d arguments received", func_name, n_args_received);
    g_debug("%s: %d args expected (%d input, %d output)", func_name, n_args, *n_input_args, *n_output_args);

    *in_args = g_new0(GIArgument, *n_input_args);
    *in_args_free = g_new0(unsigned, *n_input_args);
    *out_args = g_new0(GIArgument, *n_output_args);

    // Step through the scheme arguments, trying to convert them
    // to C
    i_input_arg = 0;    // index into in_args
    i_output_arg = 0;   // index into out_args
    i_received_arg = 0; // index into s_args
    for (int i_required_arg = 0; i_required_arg < n_args; i_required_arg++)
    {
        arg_info = g_callable_info_get_arg((GICallableInfo *)func_info, i_required_arg);
        g_assert(arg_info != NULL);

        dir = g_arg_info_get_direction(arg_info);

        if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT)
        {
            // If a C function requires an input argument, we match the next passed-in
            // argument to it.  If we've run out of passed-in arguments but the C
            // argument is optional, we handle that case.
            if (i_received_arg >= n_args_received)
            {
                if (g_arg_info_may_be_null(arg_info))
                {
                    in_args[i_input_arg++]->v_pointer = NULL;
                    if (dir == GI_DIRECTION_INOUT)
                        out_args[i_output_arg++]->v_pointer = NULL;
                }
                else
                {
                    status = GI_GIARGUMENT_TOO_FEW_ARGUMENTS;
                    g_base_info_unref(arg_info);
                    goto arg_err_cleanup;
                }
            }
            else // i_received_arg < n_args_received
            {
                obj = scm_list_ref(s_args, scm_from_int(i_received_arg++));
                // Attempt to convert the SCM object to a GIArgument
                status = gi_giargument_convert_object_to_arg(obj, arg_info, &((*in_args_free)[i_input_arg]), &((*in_args)[i_input_arg]));
                if (dir == GI_DIRECTION_INOUT)
                {
                    out_args[i_output_arg]->v_pointer = in_args[i_input_arg]->v_pointer;
                    i_output_arg++;
                }
                i_input_arg++;
                if (status != GI_GIARGUMENT_OK)
                {
                    g_base_info_unref(arg_info);
                    goto arg_err_cleanup;
                }
            }
        }
        else if (dir == GI_DIRECTION_OUT)
        {
            // Only those output arguments that require pre-allocation, e.g.
            // that require more than a simple GIArgument to store them
            // required passed-in scheme arguments.  For simple output
            // arguments, no input scheme argument is used.
            if (g_arg_info_is_caller_allocates(arg_info))
            {
                if (i_received_arg >= n_args_received)
                {
                    if (g_arg_info_may_be_null(arg_info))
                    {
                        out_args[i_output_arg]->v_pointer = NULL;
                        i_output_arg++;
                    }
                    else
                    {
                        status = GI_GIARGUMENT_TOO_FEW_ARGUMENTS;
                        g_base_info_unref(arg_info);
                        goto arg_err_cleanup;
                    }
                }
                else
                {
#if 0
                    SCM entry = scm_list_ref(s_args, scm_from_int(i_received_arg++));

                    status = gi_giargument_preallocate_output_arg_and_object(arg_info, out_args[i_output_arg], &entry);
                    i_output_arg++;
                    if (status != GI_GIARGUMENT_OK)
                    {
                        g_base_info_unref(arg_info);
                        goto arg_err_cleanup;
                    }
#endif
                }
            }
            else
            {
                // An output argument that doesn't require pre-allocation.
                i_output_arg++;
            }
        }
        g_base_info_unref(arg_info);
    }

    if (i_received_arg != *n_input_args)
    {
        scm_misc_error("function-invoke",
            "wrong number of input arguments for function '~a', received ~a, used ~a",
            scm_list_3(scm_from_utf8_string(func_name),
                scm_from_int(*n_input_args),
                scm_from_int(i_received_arg)));
    }
    scm_remember_upto_here_1(obj);
    return;

arg_err_cleanup:
    gi_giargument_free_args(*n_input_args, *in_args_free, *in_args);
    g_free(*in_args);
    g_free(*out_args);
    g_free(*in_args_free);
    *in_args = NULL;
    *out_args = NULL;
    *in_args_free = NULL;
    if (status == GI_GIARGUMENT_OUT_OF_RANGE)
        scm_out_of_range_pos(func_name, obj, scm_from_int(i_input_arg));
    else
        scm_misc_error(func_name,
            "input argument conversion error for argument #~a ~s: ~a",
            scm_list_3(scm_from_int(i_input_arg),
                obj,
                scm_from_utf8_string(gi_giargument_error_messages[status])));

    g_return_if_reached();
#if 0
    GIDirection dir;
    GIArgInfo *arg_info;
    GIArgument *in_args = g_new0(GIArgument, scm_to_int(scm_length(s_args)));

    int n_args = g_callable_info_get_n_args((GICallableInfo *)func_info);
    int i_input = 0;

    for (int i = 0; i < n_args; i++)
    {
        arg_info = g_callable_info_get_arg((GICallableInfo *)func_info, i);
        g_assert(arg_info != NULL);

        dir = g_arg_info_get_direction(arg_info);

        if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT)
        {
            SCM arg = scm_list_ref(s_args, scm_from_int(i_input));
            in_args[i_input] = gi_argument_from_object("gi-function-invoke",
                arg,
                g_arg_info_get_type(arg_info),
                g_arg_info_get_ownership_transfer(arg_info));
            i_input++;
        }
        g_base_info_unref(arg_info);
    }

    return in_args;
#endif
}

static SCM
function_info_convert_output_args(const char *func_name, const GIFunctionInfo *func_info, int n_output_args, GIArgument *out_args)
{
    SCM output = SCM_EOL;
    int n_args = g_callable_info_get_n_args((GICallableInfo *)func_info);
    for (int i = 0; i < n_args; i++)
    {
        GIArgInfo *arg_info = g_callable_info_get_arg((GICallableInfo *)func_info, i);
        g_assert(arg_info != NULL);

        GIDirection dir = g_arg_info_get_direction(arg_info);

        if (dir == GI_DIRECTION_OUT || dir == GI_DIRECTION_INOUT)
        {
            // Non-caller-allocated arguments get returned as output.
            // Caller-allocated arguments were modified in place.
            if (!g_arg_info_is_caller_allocates(arg_info))
            {
                GITypeInfo *arg_typeinfo = g_arg_info_get_type(arg_info);
                SCM obj = SCM_BOOL_F;
                gi_giargument_convert_arg_to_object(&out_args[i], arg_info, &obj);
                output = scm_append(scm_list_2(output, scm_list_1(obj)));
                g_base_info_unref(arg_typeinfo);
            }
        }
        g_base_info_unref(arg_info);
    }
    return output;
}

static GIArgument *
method_info_convert_args(GIFunctionInfo *func_info, SCM s_object, SCM s_args)
{
#if 0
    GIDirection dir;
    GIArgInfo *arg_info;
    GIArgument *in_args = g_new0(GIArgument, scm_to_int(scm_length(s_args)) + 1);

    int n_args = g_callable_info_get_n_args((GICallableInfo *)func_info);
    int i_input = 0;

    in_args[0].v_pointer = gi_gobject_get_obj(s_object);
    for (int i = 0; i < n_args; i++) {
        arg_info = g_callable_info_get_arg((GICallableInfo *)func_info, i);
        g_assert(arg_info != NULL);

        dir = g_arg_info_get_direction(arg_info);

        if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT) {
            SCM arg = scm_list_ref(s_args, scm_from_int(i_input));
            in_args[i_input + 1] = gi_argument_from_object("gi-function-invoke",
                arg,
                g_arg_info_get_type(arg_info),
                g_arg_info_get_ownership_transfer(arg_info));
            i_input++;
        }
        g_base_info_unref(arg_info);
    }

    return in_args;
#else
    g_return_val_if_reached(NULL);
#endif
}

static void
function_info_release_args(GIFunctionInfo *func_info, GIArgument *args)
{
#if 0
    GIDirection dir;
    GIArgInfo *arg_info;
    GITypeInfo *type_info;

    int n_args = g_callable_info_get_n_args((GICallableInfo *)func_info);
    int i_input = 0;

    for (int i = 0; i < n_args; i++) {
        arg_info = g_callable_info_get_arg((GICallableInfo *)func_info, i);
        type_info = g_arg_info_get_type(arg_info);

        dir = g_arg_info_get_direction(arg_info);

        if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT) {
            gi_giargument_release(&(args[i]),
                type_info,
                g_arg_info_get_ownership_transfer(arg_info),
                dir);
            i_input++;
        }
        g_base_info_unref(arg_info);
        g_base_info_unref(type_info);
    }
#endif
}

/* The inner part of a method call.  It just looks up a method
 * from the list and appends the method and its arguments together.
 * A full method call is
 * scm_gi_method_send (object, scm_gi_method_prepare ("methodname", list_of_args))
 */
static SCM
scm_gi_method_prepare(SCM s_method_name, SCM s_list_of_args)
{
    SCM_ASSERT(scm_is_string(s_method_name), s_method_name, SCM_ARG1, "gi-method-prepare");
    SCM_ASSERT(scm_is_true(scm_list_p(s_list_of_args)), s_list_of_args, SCM_ARG2, "gi-method-prepare");
    /* Look-up the method by name. */
    SCM h, subhash;
    h = get_hash_table("%gi-methods");
    subhash = scm_hash_ref(h, s_method_name, SCM_BOOL_F);
    if (scm_is_false(subhash))
        scm_misc_error("gi-method-prepare",
            "Unknown method ~a",
            scm_list_1(s_method_name));

    /* Make a list of the method and the args, for dispatch. */
    return scm_append(scm_list_2(scm_list_2(subhash, s_method_name), s_list_of_args));
}

/* Given a wrapped GObject, struct, or union, call the attached method
 * with the given args, if the method is applicable to the object. */
static SCM
scm_gi_method_send(SCM s_object, SCM s_method_args_list)
{
    // Find out if this type of argument or of any of this argument's
    // parent types map to this method.
    GType type, original_type;
    GIFunctionInfo *info;

    SCM subhash = scm_car(s_method_args_list);
    SCM s_name = scm_cadr(s_method_args_list);
    SCM s_args = scm_cddr(s_method_args_list);
    SCM val;

    if (SCM_IS_A_P(s_object, gi_gobject_type))
        type = gi_gobject_get_ob_type(s_object);
    else if (SCM_IS_A_P(s_object, gir_gbox_type))
        type = gi_gbox_get_type(s_object);
    else
        scm_misc_error("gi-method-send",
            "Cannot invoke ::~S~S for object ~S",
            scm_list_3(s_name, s_args, s_object));

    char *method_name = scm_to_utf8_string(s_name);

    original_type = type;
    while (scm_is_false((val = scm_hash_ref(subhash, scm_from_size_t(type), SCM_BOOL_F))))
    {
        if (!(type = g_type_parent(type)))
        {
            free(method_name);
            scm_misc_error("gi-method-send",
                "Cannot find a method '~a' for ~s",
                scm_list_2(scm_cadr(s_method_args_list),
                    s_object));
        }
    }
    info = scm_to_pointer(val);

    SCM s_args_str = scm_simple_format(SCM_BOOL_F, scm_from_locale_string("~s"), scm_list_1(s_args));
    char *args_str = scm_to_utf8_string(s_args_str);
    g_debug("Invoking %s::%s%s for object of type %s",
        g_type_name(type),
        method_name,
        args_str,
        g_type_name(original_type));
    free(args_str);

    int n_input_args, n_output_args;
    GIArgument *in_args, *out_args;
    unsigned *in_args_free;

    // This converts the SCM arguments to C arguments. This will throw on conversion error.
    // scm_write(s_method_args_list, scm_current_output_port());
    function_info_convert_args(method_name, info, s_args, &n_input_args, &in_args, &in_args_free, &n_output_args,
        &out_args);
    scm_remember_upto_here_1(s_args);

    // Need to prepend 'self' to the input arguments on a method call
    in_args = g_realloc_n(in_args, n_input_args + 1, sizeof(GIArgument));
    memmove(in_args + 1, in_args, sizeof(GIArgument) * n_input_args);

    if (SCM_IS_A_P(s_object, gi_gobject_type))
        in_args[0].v_pointer = gi_gobject_get_obj(s_object);
    else if (SCM_IS_A_P(s_object, gir_gbox_type))
        in_args[0].v_pointer = gi_gbox_peek_pointer(s_object);
    else
        g_abort();

    GIArgument return_arg;

    /* Make the call. */
    GError *err = NULL;
    gboolean ret = g_function_info_invoke(info, in_args, n_input_args + 1,
        out_args, n_output_args,
        &return_arg, &err);
    if (ret)
        g_debug("Invoked method %s", method_name);
    else
        g_debug("Failed to invoke method %s", method_name);

    /* Free any allocated input */
    function_info_release_args(info, in_args + 1);
    g_free(in_args);
    g_free(in_args_free);
    in_args = NULL;
    in_args_free = NULL;

    /* If there is a GError, write an error, free, and exit. */
    if (!ret)
    {
        char str[256];
        memset(str, 0, 256);
        strncpy(str, err->message, 255);
        g_error_free(err);
        free(method_name);

        scm_misc_error("gi-method-send",
            "error invoking method '~a': ~a",
            scm_list_2(s_name, scm_from_utf8_string(str)));
        return SCM_BOOL_F;
    }

    // We've actually made a successful call.  Hooray! Convert the output
    // arguments and return values into Scheme objects.  Free the
    // C objects if necessary.  Return the output either as
    // a single return value or as aa plain list.  (maybe values list instead?). */
    GITypeInfo *return_typeinfo = g_callable_info_get_return_type(info);
    SCM s_return = gi_giargument_convert_return_val_to_object(&return_arg,
        return_typeinfo,
        g_callable_info_get_caller_owns(info),
        g_callable_info_may_return_null(info),
        g_callable_info_skip_return(info));
    g_base_info_unref(return_typeinfo);
    SCM output;
    if (scm_is_eq(s_return, SCM_UNSPECIFIED))
        output = SCM_EOL;
    else
        output = scm_list_1(s_return);

    SCM output2 = function_info_convert_output_args(method_name, info, n_output_args, out_args);
    output = scm_append(scm_list_2(output, output2));
    g_free(out_args);
    g_free(method_name);
    int outlen = scm_to_int(scm_length(output));

    scm_remember_upto_here_1(s_name);
    scm_remember_upto_here_1(s_args);
    scm_remember_upto_here_1(s_return);
    scm_remember_upto_here_1(output);
    scm_remember_upto_here_1(output2);
    scm_remember_upto_here_1(s_method_args_list);

    if (outlen == 0)
        return SCM_UNSPECIFIED;
    if (outlen == 1)
        return scm_car(output);
    return output;
}

static SCM
scm_gi_function_invoke(SCM s_name, SCM s_args)
{
    GError *err = NULL;
    int n_input_args, n_output_args;
    char *name_str;
    char *args_str;
    GIFunctionInfo *func_info;
    GIArgument *in_args, *out_args, return_arg;
    unsigned *in_args_free;

    SCM_ASSERT(scm_is_string(s_name), s_name, SCM_ARG1,
        "gi-function-invoke");

    // Look up the procedure by name
    name_str = scm_to_utf8_string(s_name);
    args_str = scm_to_utf8_string(scm_simple_format(SCM_BOOL_F,
        scm_from_utf8_string("args ~S"),
        scm_list_1(s_args)));
    g_debug("in gi-function-invoke for %s(%s)", name_str, args_str);

    SCM h, val;
    h = get_hash_table("%gi-functions");
    val = scm_hash_ref(h, s_name, SCM_BOOL_F);
    if (scm_is_false(val))
    {
        scm_misc_error("gi-function-invoke",
            "unknown procedure '~a'",
            scm_list_1(s_name));
        g_return_val_if_reached(SCM_BOOL_F);
    }

    func_info = scm_to_pointer(val);

    // This converts the SCM arguments to C arguments. This will throw on conversion error.
    function_info_convert_args(name_str, func_info, s_args, &n_input_args, &in_args, &in_args_free, &n_output_args, &out_args);

    // Use GObject's ffi to call the C function.
    gboolean ret = g_function_info_invoke(func_info, in_args, n_input_args,
        out_args, n_output_args,
        &return_arg, &err);

    // Free any allocated input that won't be used later.
    gi_giargument_free_args(n_input_args, in_args_free, in_args);
    g_free(in_args);
    g_free(in_args_free);

    // If there is a GError, write an error, free, and exit.
    if (!ret)
    {
        char str[256];
        memset(str, 0, 256);
        strncpy(str, err->message, 255);
        g_error_free(err);
        g_base_info_unref(func_info);
        g_free(out_args);

        scm_misc_error("gi-function-invoke",
            "error invoking function '~a': ~a",
            scm_list_2(s_name, scm_from_utf8_string(str)));
        g_return_val_if_reached(SCM_BOOL_F);
    }

    // We've actually made a successful call.  Hooray! Convert the output
    // arguments and return values into Scheme objects.  Free the
    // C objects if necessary.  Return the output either as
    // a single return value or as aa plain list.  (maybe values list instead?). */
    GITypeInfo *return_typeinfo = g_callable_info_get_return_type(func_info);
    SCM s_return = gi_giargument_convert_return_val_to_object(&return_arg,
        return_typeinfo,
        g_callable_info_get_caller_owns(func_info),
        g_callable_info_may_return_null(func_info),
        g_callable_info_skip_return(func_info));
    g_base_info_unref(return_typeinfo);
    SCM output;
    if (scm_is_eq(s_return, SCM_UNSPECIFIED))
        output = SCM_EOL;
    else
        output = scm_list_1(s_return);

    SCM output2 = function_info_convert_output_args(name_str, func_info, n_output_args, out_args);
    output = scm_append(scm_list_2(output, output2));
    g_free(out_args);
    free(name_str);
    free(args_str);
    int outlen = scm_to_int(scm_length(output));

    scm_remember_upto_here_1(s_return);
    scm_remember_upto_here_1(output);
    scm_remember_upto_here_1(output2);

    if (outlen == 0)
        return SCM_UNSPECIFIED;
    if (outlen == 1)
        return scm_car(output);
    return output;
}

static void
unload_repository(const char *category, GHashTable **p_hash_table)
{
    if (*p_hash_table)
    {
        g_debug("destroying %s hash table", category);
        g_hash_table_destroy(*p_hash_table);
        *p_hash_table = NULL;
    }
}

static SCM
scm_gi_unload_repositories(void)
{
    return SCM_UNSPECIFIED;
}

GType gir_lookup_type(const char *name)
{
    gpointer ptr = NULL;
    SCM h, val;
    SCM s_type_name = scm_from_utf8_string(name);
    h = get_hash_table("%gi-objects");
    val = scm_hash_ref(h, s_type_name, SCM_BOOL_F);
    if (scm_is_false(val))
    {
        h = get_hash_table("%gi-structs");
        val = scm_hash_ref(h, s_type_name, SCM_BOOL_F);
    }
    if (scm_is_false(val))
    {
        h = get_hash_table("%gi-unions");
        val = scm_hash_ref(h, s_type_name, SCM_BOOL_F);
    }
    if (scm_is_false(val))
        scm_misc_error("gi-lookup-type",
            "Cannot find an object, struct, or union type named '~a'",
            scm_list_1(s_type_name));
    ptr = scm_to_pointer(val);
    return g_registered_type_info_get_g_type(ptr);
}

static SCM
scm_gi_lookup_type(SCM s_type_name)
{
    SCM_ASSERT(scm_is_string(s_type_name), s_type_name, SCM_ARG1, "gi-lookup-type");
    gpointer ptr = NULL;
    SCM h, val;

    h = get_hash_table("%gi-objects");
    val = scm_hash_ref(h, s_type_name, SCM_BOOL_F);
    if (scm_is_false(val))
    {
        h = get_hash_table("%gi-structs");
        val = scm_hash_ref(h, s_type_name, SCM_BOOL_F);
    }
    if (scm_is_false(val))
    {
        h = get_hash_table("%gi-unions");
        val = scm_hash_ref(h, s_type_name, SCM_BOOL_F);
    }
    if (scm_is_false(val))
        scm_misc_error("gi-lookup-type",
            "Cannot find an object, struct, or union type named '~a'",
            scm_list_1(s_type_name));
    ptr = scm_to_pointer(val);
    return gi_gtype_c2g(g_registered_type_info_get_g_type(ptr));
}

/* re pygi_type_import_by_gi_info */
SCM gi_type_import_by_gi_info(GIBaseInfo *info)
{
    const gchar *namespace_ = g_base_info_get_namespace(info);
    const gchar *name = g_base_info_get_name(info);
    GIInfoType info_type = g_base_info_get_type(info);
    g_debug("gi_type_import_by_gi_info: namespace '%s' name '%s'",
        namespace_, name);

    switch (info_type)
    {
    case GI_INFO_TYPE_STRUCT:
    {
        GType g_type;
        SCM h, val;
        h = get_hash_table("%gi-structs");
        val = scm_hash_ref(h, scm_from_utf8_string(name), SCM_BOOL_F);
        if (scm_is_true(val))
        {
            g_debug("type name '%s' is found in structs", name);
            /* Have we made a Guile type for this struct? */
            g_type = g_registered_type_info_get_g_type((GIRegisteredTypeInfo *)scm_to_pointer(val));
            return gi_gtype_c2g(g_type);
        }
    }
    break;
    default:
        g_critical("unimplemented");
    }
    return SCM_UNSPECIFIED;
}

static SCM
scm_gi_lookup_callback_info(SCM s_type_name)
{
    SCM_ASSERT(scm_is_string(s_type_name), s_type_name, SCM_ARG1, "gi-lookup-callback-info");
    SCM h, val;

    h = get_hash_table("%gi-callbacks");
    val = scm_hash_ref(h, s_type_name, SCM_BOOL_F);
    if (scm_is_false(val))
        scm_misc_error("gi-lookup-callback-info",
            "Cannot find a callback type named '~a'",
            scm_list_1(s_type_name));
    return val;
}

#ifdef FIGURE_OUT_ALL_ARG_TYPES
static SCM
scm_dump_all_arg_types(void)
{
    guint len = gi_arg_infos->len;
    if (len == 0)
        return SCM_UNSPECIFIED;

    FILE *fp = fopen("arg_infos.txt", "wt");

    for (guint i = 0; i < len; i++)
    {
        struct _arg_info_func_name *aifn = gi_arg_infos->pdata[i];
        GIArgInfo *ai = aifn->ai;
        GIDirection dir = g_arg_info_get_direction(ai);
        GITypeInfo *ti = g_arg_info_get_type(ai);
        gboolean skip = g_arg_info_is_skip(ai);
        if (skip)
            continue;

        fprintf(fp, "%-11s", g_type_tag_to_string(g_type_info_get_tag(ti)));
        if (g_type_info_is_pointer(ti))
            fprintf(fp, "* ");
        else
            fprintf(fp, "  ");

        if (dir == GI_DIRECTION_IN)
            fprintf(fp, "IN    ");
        else if (dir == GI_DIRECTION_INOUT)
            fprintf(fp, "INOUT ");
        else if (dir == GI_DIRECTION_OUT)
            fprintf(fp, "OUT   ");

        if (g_type_info_get_tag(ti) == GI_TYPE_TAG_ARRAY)
        {
            fprintf(fp, "LEN %3d SIZE %3d ", g_type_info_get_array_length(ti), g_type_info_get_array_fixed_size(ti));
            if (g_type_info_is_zero_terminated(ti))
                fprintf(fp, "ZERO_TERM ");
            else
                fprintf(fp, "          ");
            GIArrayType arrt = g_type_info_get_array_type(ti);
            if (arrt == GI_ARRAY_TYPE_C)
                fprintf(fp, "C      ");
            else if (arrt == GI_ARRAY_TYPE_BYTE_ARRAY)
                fprintf(fp, "BYTE   ");
            else if (arrt == GI_ARRAY_TYPE_ARRAY)
                fprintf(fp, "GArray ");
            else if (arrt == GI_ARRAY_TYPE_PTR_ARRAY)
                fprintf(fp, "PTR    ");

            GITypeInfo *pti = g_type_info_get_param_type(ti, 0);
            fprintf(fp, "%-11s", g_type_tag_to_string(g_type_info_get_tag(pti)));
            if (g_type_info_is_pointer(pti))
                fprintf(fp, "* ");
            else
                fprintf(fp, "  ");
            GIBaseInfo *pbi = g_type_info_get_interface(pti);
            if (pbi)
            {
                GIInfoType pit = g_base_info_get_type(pbi);
                if (pit == GI_INFO_TYPE_INVALID)
                    fprintf(fp, "INVALID   ");
                else if (pit == GI_INFO_TYPE_FUNCTION)
                    fprintf(fp, "FUNCTION  ");
                else if (pit == GI_INFO_TYPE_CALLBACK)
                    fprintf(fp, "CALLBACK  ");
                else if (pit == GI_INFO_TYPE_STRUCT)
                    fprintf(fp, "STRUCT    ");
                else if (pit == GI_INFO_TYPE_STRUCT)
                    fprintf(fp, "BOXED     ");
                else if (pit == GI_INFO_TYPE_ENUM)
                    fprintf(fp, "ENUM      ");
                else if (pit == GI_INFO_TYPE_FLAGS)
                    fprintf(fp, "FLAGS     ");
                else if (pit == GI_INFO_TYPE_OBJECT)
                    fprintf(fp, "OBJECT    ");
                else if (pit == GI_INFO_TYPE_INTERFACE)
                    fprintf(fp, "INTERFACE ");
                else if (pit == GI_INFO_TYPE_CONSTANT)
                    fprintf(fp, "CONSTANT  ");
                else if (pit == GI_INFO_TYPE_UNION)
                    fprintf(fp, "UNION     ");
                else if (pit == GI_INFO_TYPE_VALUE)
                    fprintf(fp, "VALUE     ");
                else if (pit == GI_INFO_TYPE_SIGNAL)
                    fprintf(fp, "SIGNAL    ");
                else if (pit == GI_INFO_TYPE_VFUNC)
                    fprintf(fp, "VFUNC     ");
                else if (pit == GI_INFO_TYPE_PROPERTY)
                    fprintf(fp, "PROPERTY  ");
                else if (pit == GI_INFO_TYPE_FIELD)
                    fprintf(fp, "FIELD     ");
                else if (pit == GI_INFO_TYPE_ARG)
                    fprintf(fp, "ARG       ");
                else if (pit == GI_INFO_TYPE_TYPE)
                    fprintf(fp, "TYPE      ");
                fprintf(fp, "%-11s ", g_base_info_get_name(pbi));
            }
        }

        GIBaseInfo *bi = g_type_info_get_interface(ti);
        if (bi)
        {
            GIInfoType it = g_base_info_get_type(bi);
            if (it == GI_INFO_TYPE_INVALID)
                fprintf(fp, "INVALID   ");
            else if (it == GI_INFO_TYPE_FUNCTION)
                fprintf(fp, "FUNCTION  ");
            else if (it == GI_INFO_TYPE_CALLBACK)
                fprintf(fp, "CALLBACK  ");
            else if (it == GI_INFO_TYPE_STRUCT)
                fprintf(fp, "STRUCT    ");
            else if (it == GI_INFO_TYPE_STRUCT)
                fprintf(fp, "BOXED     ");
            else if (it == GI_INFO_TYPE_ENUM)
                fprintf(fp, "ENUM      ");
            else if (it == GI_INFO_TYPE_FLAGS)
                fprintf(fp, "FLAGS     ");
            else if (it == GI_INFO_TYPE_OBJECT)
                fprintf(fp, "OBJECT    ");
            else if (it == GI_INFO_TYPE_INTERFACE)
                fprintf(fp, "INTERFACE ");
            else if (it == GI_INFO_TYPE_CONSTANT)
                fprintf(fp, "CONSTANT  ");
            else if (it == GI_INFO_TYPE_UNION)
                fprintf(fp, "UNION     ");
            else if (it == GI_INFO_TYPE_VALUE)
                fprintf(fp, "VALUE     ");
            else if (it == GI_INFO_TYPE_SIGNAL)
                fprintf(fp, "SIGNAL    ");
            else if (it == GI_INFO_TYPE_VFUNC)
                fprintf(fp, "VFUNC     ");
            else if (it == GI_INFO_TYPE_PROPERTY)
                fprintf(fp, "PROPERTY  ");
            else if (it == GI_INFO_TYPE_FIELD)
                fprintf(fp, "FIELD     ");
            else if (it == GI_INFO_TYPE_ARG)
                fprintf(fp, "ARG       ");
            else if (it == GI_INFO_TYPE_TYPE)
                fprintf(fp, "TYPE      ");
            fprintf(fp, "%-11s ", g_base_info_get_name(bi));
        }

        gboolean null = g_arg_info_may_be_null(ai);
        if (null)
            fprintf(fp, "NULL_OK ");
        else
            fprintf(fp, "        ");
        gboolean caller_allocate = g_arg_info_is_caller_allocates(ai);
        if (caller_allocate)
            fprintf(fp, "ALLOC ");
        else
            fprintf(fp, "      ");
        gboolean optional = g_arg_info_is_optional(ai);
        if (optional)
            fprintf(fp, "OPT ");
        else
            fprintf(fp, "    ");
        GITransfer transfer = g_arg_info_get_ownership_transfer(ai);
        if (transfer == GI_TRANSFER_NOTHING)
            fprintf(fp, "CONST   ");
        else if (transfer == GI_TRANSFER_CONTAINER)
            fprintf(fp, "SHALLOW ");
        else if (transfer == GI_TRANSFER_EVERYTHING)
            fprintf(fp, "DEEP    ");
        fprintf(fp, "%s %s ", g_base_info_get_name(ai), aifn->name);
        fprintf(fp, "\n");
    }
    fclose(fp);
    return SCM_UNSPECIFIED;
}
#endif


void gir_init_func2(void)
{
#ifdef FIGURE_OUT_ALL_ARG_TYPES
    gi_arg_infos = g_ptr_array_new();
#endif
    scm_c_define_gsubr("get-typelib-search-path", 0, 0, 0, scm_get_typelib_search_path);
    scm_c_define_gsubr("prepend-typelib-search-path", 1, 0, 0, scm_prepend_typelib_search_path);
    scm_c_define_gsubr("load-typelib", 2, 0, 0, scm_load_typelib);
    scm_c_define_gsubr("%import-typelib", 2, 0, 0, scm_import_typelib);
    scm_c_define_gsubr("export-typelib", 2, 0, 0, scm_export_typelib);
    scm_c_define_gsubr("gi-constant-value", 1, 0, 0, scm_gi_constant_value);
    scm_c_define_gsubr("gi-flag-value", 2, 0, 0, scm_gi_flag_value);
    scm_c_define_gsubr("gi-enum-value", 2, 0, 0, scm_gi_enum_value);
    scm_c_define_gsubr("gi-function-invoke", 1, 0, 1, scm_gi_function_invoke);
    scm_c_define_gsubr("gi-lookup-callback-info", 1, 0, 0, scm_gi_lookup_callback_info);
    scm_c_define_gsubr("gi-lookup-type", 1, 0, 0, scm_gi_lookup_type);

#if 0
    scm_permanent_object(scm_c_define("%gi-functions", scm_c_make_hash_table(GIR_FUNC2_INIT_HASH_TABLE_SIZE)));
    scm_permanent_object(scm_c_define("%gi-callbacks", scm_c_make_hash_table(GIR_FUNC2_INIT_HASH_TABLE_SIZE)));
    scm_permanent_object(scm_c_define("%gi-structs", scm_c_make_hash_table(GIR_FUNC2_INIT_HASH_TABLE_SIZE)));
    scm_permanent_object(scm_c_define("%gi-enums", scm_c_make_hash_table(GIR_FUNC2_INIT_HASH_TABLE_SIZE)));
    scm_permanent_object(scm_c_define("%gi-flags", scm_c_make_hash_table(GIR_FUNC2_INIT_HASH_TABLE_SIZE)));
    scm_permanent_object(scm_c_define("%gi-objects", scm_c_make_hash_table(GIR_FUNC2_INIT_HASH_TABLE_SIZE)));
    scm_permanent_object(scm_c_define("%gi-methods", scm_c_make_hash_table(GIR_FUNC2_INIT_HASH_TABLE_SIZE)));
    scm_permanent_object(scm_c_define("%gi-unions", scm_c_make_hash_table(GIR_FUNC2_INIT_HASH_TABLE_SIZE)));
    scm_permanent_object(scm_c_define("%gi-interfaces", scm_c_make_hash_table(GIR_FUNC2_INIT_HASH_TABLE_SIZE)));
    scm_permanent_object(scm_c_define("%gi-consts", scm_c_make_hash_table(GIR_FUNC2_INIT_HASH_TABLE_SIZE)));
#endif

    //scm_c_define_gsubr("gi-load-repository", 2, 0, 0,
    //                    scm_gi_load_repository);
    scm_c_define_gsubr("gi-unload-repositories", 0, 0, 0,
        scm_gi_unload_repositories);
    scm_c_define_gsubr("gi-struct-ref", 4, 0, 0, scm_gi_struct_ref);
    scm_c_define_gsubr("gi-struct-set", 5, 0, 0, scm_gi_struct_set);
    scm_c_define_gsubr("gi-method-prepare", 1, 0, 1, scm_gi_method_prepare);
    scm_c_define_gsubr("gi-method-send", 2, 0, 0, scm_gi_method_send);
#ifdef FIGURE_OUT_ALL_ARG_TYPES
    scm_c_define_gsubr("gi-dump-arg-types", 0, 0, 0, scm_dump_all_arg_types);
#endif
}
