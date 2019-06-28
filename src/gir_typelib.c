// gir_typelib.c - introspection of typelib files
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
#include <girepository.h>
#include "gi_giargument.h"
#include "gir_type.h"
#include "gir_typelib.h"
#include "gir_function.h"
#include "gi_gobject.h"
#include "gir_method.h"
#include "gir_constant.h"
#include "gir_flag.h"
#include "gi_util.h"

static void gir_typelib_document_callback_info(GString **export, const char *namespace_,
                                               const char *parent, GICallableInfo *info);
static void gir_typelib_document_function_info(GString **export, const char *parent,
                                               GIFunctionInfo *info, gboolean method);
static void gir_typelib_document_type(GString **export, char *parent, GITypeInfo *info);
static void scm_i_typelib_load(const char *subr, const char *namespace, const char *version);
static void scm_i_typelib_load_check_args(const char *subr, SCM s_lib, SCM s_version, char **lib,
                                          char **version);

#define MAX_GERROR_MSG 100
static char gerror_msg[MAX_GERROR_MSG];

static void
store_gerror_message(const char *msg)
{
    memset(gerror_msg, 0, MAX_GERROR_MSG);
    strncpy(gerror_msg, msg, MAX_GERROR_MSG - 1);
    if (strlen(msg) >= MAX_GERROR_MSG - 1) {
        gerror_msg[MAX_GERROR_MSG - 2] = '.';
        gerror_msg[MAX_GERROR_MSG - 3] = '.';
        gerror_msg[MAX_GERROR_MSG - 4] = '.';
    }
}

// Returns, as a list of strings, the directories that GIRepository
// searches for *.typelib files.
static SCM
scm_typelib_get_search_path(void)
{
    GSList *slist = g_irepository_get_search_path();
    SCM entry;
    SCM output = SCM_EOL;

    if (slist == NULL)
        return SCM_EOL;
    do {
        entry = scm_from_utf8_string(slist->data);
        output = scm_append(scm_list_2(output, scm_list_1(entry)));
    } while ((slist = g_slist_next(slist)));
    return output;
}

// Adds a directory to the search path that GIRepository will use to
// search for *.typelib files
static SCM
scm_typelib_prepend_search_path(SCM s_dir)
{
    char *dir;
    SCM_ASSERT_TYPE(scm_is_string(s_dir), s_dir, SCM_ARG1,
                    "typelib-prepend-search-path", "string");

    dir = scm_to_utf8_string(s_dir);
    g_irepository_prepend_search_path(dir);
    return SCM_UNSPECIFIED;
}

static void
scm_i_typelib_load_check_args(const char *subr,
                              SCM s_namespace, SCM s_version, char **lib, char **version)
{
    SCM_ASSERT_TYPE(scm_is_string(s_namespace), s_namespace, SCM_ARG1, subr, "string");
    SCM_ASSERT_TYPE(scm_is_string(s_version), s_version, SCM_ARG2, subr, "string");

    *lib = scm_dynwind_or_bust(subr, scm_to_utf8_string(s_namespace));
    *version = scm_dynwind_or_bust(subr, scm_to_utf8_string(s_version));

    SCM_ASSERT(!strchr(*lib, ' '), s_namespace, SCM_ARG1, subr);
    SCM_ASSERT(!strchr(*version, ' '), s_version, SCM_ARG2, subr);
}

static SCM
scm_typelib_load(SCM s_namespace, SCM s_version)
{
    gchar *namespace_;
    gchar *version;

    scm_dynwind_begin(0);
    scm_i_typelib_load_check_args("typelib-load", s_namespace, s_version, &namespace_, &version);
    scm_i_typelib_load("typelib-load", namespace_, version);
    scm_dynwind_end();

    return SCM_UNSPECIFIED;
}

static void
scm_i_typelib_load(const char *subr, const char *namespace_, const char *version)
{
    GITypelib *tl;
    GError *error = NULL;

    tl = g_irepository_require(NULL, namespace_, version, 0, &error);
    if (tl == NULL) {
        store_gerror_message(error->message);
        g_error_free(error);
        scm_misc_error(subr, gerror_msg, SCM_EOL);
        return;
    }

    g_debug("Loading irepository %s %s", namespace_, version);
    int n = g_irepository_get_n_infos(NULL, namespace_);
    for (int i = 0; i < n; i++) {
        GIBaseInfo *info;
        GIInfoType type;
        info = g_irepository_get_info(NULL, namespace_, i);
        if (g_base_info_is_deprecated(info)) {
            g_base_info_unref(info);
            continue;
        }
        type = g_base_info_get_type(info);
        switch (type) {
        case GI_INFO_TYPE_CALLBACK:
            g_debug("Unsupported irepository type 'CALLBACK'");
            break;
        case GI_INFO_TYPE_FUNCTION:
            gir_function_define_gsubr(NULL, info);
            break;
        case GI_INFO_TYPE_STRUCT:
        {
            GType gtype = g_registered_type_info_get_g_type(info);
            if (gtype == G_TYPE_NONE) {
                g_debug("Not loading struct type '%s' because is has no GType",
                        g_base_info_get_name(info));
                break;
            }
            gir_type_define(gtype);
            if (g_struct_info_get_size(info) > 0) {
                GQuark size_quark = g_quark_from_string("size");
                g_type_set_qdata(gtype, size_quark,
                                 GSIZE_TO_POINTER(g_struct_info_get_size(info)));
            }

            gint n_methods = g_struct_info_get_n_methods(info);
            for (gint m = 0; m < n_methods; m++) {
                GIFunctionInfo *func_info = g_struct_info_get_method(info, m);
                if (g_function_info_get_flags(func_info) & GI_FUNCTION_IS_METHOD)
                    gir_method_table_insert(gtype, func_info);
                else
                    gir_function_define_gsubr(g_base_info_get_name(info), func_info);
            }
        }
            break;
        case GI_INFO_TYPE_ENUM:
        case GI_INFO_TYPE_FLAGS:
            gir_flag_define(info);
            break;
        case GI_INFO_TYPE_OBJECT:
        {
            GType gtype = g_registered_type_info_get_g_type(info);
            if (gtype == G_TYPE_NONE) {
                g_debug("Not loading object type '%s' because is has no GType",
                        g_base_info_get_name(info));
                break;
            }
            gir_type_define(gtype);

            gint n_methods = g_object_info_get_n_methods(info);
            for (gint m = 0; m < n_methods; m++) {
                GIFunctionInfo *func_info = g_object_info_get_method(info, m);
                if (g_function_info_get_flags(func_info) & GI_FUNCTION_IS_METHOD)
                    gir_method_table_insert(gtype, func_info);
                else
                    gir_function_define_gsubr(g_base_info_get_name(info), func_info);
            }
#if 0
            gint n_signals = g_object_info_get_n_signals(info);
            for (gint m = 0; m < n_signals; m++) {
                GISignalInfo *sig_info = g_object_info_get_signal(info, m);
                if (!(g_signal_info_get_flags(sig_info) & G_SIGNAL_DEPRECATED)) {
                    if (!insert_into_signal_table(gtype, sig_info, &is_new_method))
                        g_base_info_unref(sig_info);
                    else
                        export_signal_info(&export,
                                           g_base_info_get_name(info), sig_info, is_new_method);
                }
            }
#endif
        }
            break;
        case GI_INFO_TYPE_INTERFACE:
        {
            GType gtype = g_registered_type_info_get_g_type(info);
            if (gtype == G_TYPE_NONE) {
                g_debug("Not loading interface type '%s' because is has no GType",
                        g_base_info_get_name(info));
                break;
            }
            gir_type_define(gtype);

            gint n_methods = g_interface_info_get_n_methods(info);
            for (gint m = 0; m < n_methods; m++) {
                GIFunctionInfo *func_info = g_interface_info_get_method(info, m);
                if (g_function_info_get_flags(func_info) & GI_FUNCTION_IS_METHOD)
                    gir_method_table_insert(gtype, func_info);
                else
                    gir_function_define_gsubr(g_base_info_get_name(info), func_info);
            }
        }
            break;
        case GI_INFO_TYPE_CONSTANT:
            gir_constant_define(info);
            break;
        case GI_INFO_TYPE_UNION:
        {
            GType gtype = g_registered_type_info_get_g_type(info);
            if (gtype == G_TYPE_NONE) {
                g_debug("Not loading union type '%s' because is has no GType",
                        g_base_info_get_name(info));
                break;
            }
            gir_type_define(gtype);
            if (g_union_info_get_size(info) > 0) {
                GQuark size_quark = g_quark_from_string("size");
                g_type_set_qdata(gtype, size_quark, GSIZE_TO_POINTER(g_union_info_get_size(info)));
            }

            gint n_methods = g_union_info_get_n_methods(info);
            for (gint m = 0; m < n_methods; m++) {
                GIFunctionInfo *func_info = g_union_info_get_method(info, m);
                if (g_function_info_get_flags(func_info) & GI_FUNCTION_IS_METHOD)
                    gir_method_table_insert(gtype, func_info);
                else
                    gir_function_define_gsubr(g_base_info_get_name(info), func_info);
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
        g_base_info_unref(info);
    }
}

static SCM
scm_typelib_document(SCM s_namespace, SCM s_version)
{
    gchar *namespace_;
    gchar *version;
    GITypelib *tl;
    GError *error = NULL;
    GString *export;

    SCM_ASSERT(scm_is_string(s_namespace), s_namespace, SCM_ARG1, "typelib-document");
    SCM_ASSERT(scm_is_string(s_version), s_version, SCM_ARG2, "typelib-document");
    namespace_ = scm_to_utf8_string(s_namespace);
    version = scm_to_utf8_string(s_version);

    tl = g_irepository_require(NULL, namespace_, version, 0, &error);
    if (tl == NULL) {
        free(version);
        free(namespace_);
        store_gerror_message(error->message);
        g_error_free(error);
        scm_misc_error("typelib-document", gerror_msg, SCM_EOL);
        return SCM_UNSPECIFIED;
    }

    export = g_string_new_len(NULL, 128 * 1024);
    g_string_append_printf(export, "%s %s\n\n", namespace_, version);

    int n = g_irepository_get_n_infos(NULL, namespace_);
    for (int i = 0; i < n; i++) {
        GIBaseInfo *info;
        GIInfoType type;
        info = g_irepository_get_info(NULL, namespace_, i);
        if (g_base_info_is_deprecated(info)) {
            g_string_append_printf(export,
                                   "Not importing '%s' because it is deprecated.\n\n",
                                   g_base_info_get_name(info));
            g_base_info_unref(info);
            continue;
        }
        type = g_base_info_get_type(info);
        switch (type) {
        case GI_INFO_TYPE_CALLBACK:
            gir_typelib_document_callback_info(&export, namespace_, NULL, info);
            break;
        case GI_INFO_TYPE_FUNCTION:
            gir_typelib_document_function_info(&export, NULL, info, 0);
            break;
        case GI_INFO_TYPE_STRUCT:
        {
            GType gtype = g_registered_type_info_get_g_type(info);
            if (gtype == G_TYPE_NONE) {
                g_debug("Not importing struct type '%s' because is has no GType",
                        g_base_info_get_name(info));
                g_base_info_unref(info);
                break;
            }
            g_base_info_ref(info);
            gir_typelib_document_type(&export, NULL, info);
            gint n_methods = g_struct_info_get_n_methods(info);
            for (gint m = 0; m < n_methods; m++) {
                GIFunctionInfo *func_info = g_struct_info_get_method(info, m);
                if (g_function_info_get_flags(func_info)
                    & GI_FUNCTION_IS_METHOD)
                    gir_typelib_document_function_info(&export,
                                                       g_base_info_get_name(info), func_info, 1);
                else
                    gir_typelib_document_function_info(&export,
                                                       g_base_info_get_name(info), func_info, 0);
            }
        }
            break;
        case GI_INFO_TYPE_ENUM:
        case GI_INFO_TYPE_FLAGS:
            gir_flag_document(&export, info);
            break;
        case GI_INFO_TYPE_OBJECT:
        {
            GType gtype = g_registered_type_info_get_g_type(info);
            if (gtype == G_TYPE_NONE) {
                g_debug("Not importing object type '%s' because is has no GType",
                        g_base_info_get_name(info));
                g_base_info_unref(info);
                break;
            }
            gir_typelib_document_type(&export, NULL, info);
            gint n_methods = g_object_info_get_n_methods(info);
            for (gint m = 0; m < n_methods; m++) {
                GIFunctionInfo *func_info = g_object_info_get_method(info, m);
                if (g_function_info_get_flags(func_info) & GI_FUNCTION_IS_METHOD)
                    gir_typelib_document_function_info(&export,
                                                       g_base_info_get_name(info), func_info, 1);
                else
                    gir_typelib_document_function_info(&export,
                                                       g_base_info_get_name(info), func_info, 0);
            }
#if 0
            gint n_signals = g_object_info_get_n_signals(info);
            for (gint m = 0; m < n_signals; m++) {
                GISignalInfo *sig_info = g_object_info_get_signal(info, m);
                if (!(g_signal_info_get_flags(sig_info) & G_SIGNAL_DEPRECATED)) {
                    if (!insert_into_signal_table(gtype, sig_info, &is_new_method))
                        g_base_info_unref(sig_info);
                    else
                        export_signal_info(&export,
                                           g_base_info_get_name(info), sig_info, is_new_method);
                }
            }
#endif
        }
            break;
        case GI_INFO_TYPE_INTERFACE:
            // export_interface_info(&export, g_base_info_get_name(info), info);
            break;
        case GI_INFO_TYPE_CONSTANT:
            gir_constant_document(&export, namespace_, NULL, info);
            break;
        case GI_INFO_TYPE_UNION:
        {
            GType gtype = g_registered_type_info_get_g_type(info);
            if (gtype == G_TYPE_NONE) {
                g_debug("Not importing union type '%s' because is has no GType",
                        g_base_info_get_name(info));
                g_base_info_unref(info);
                break;
            }
            gir_typelib_document_type(&export, NULL, info);
            gint n_methods = g_union_info_get_n_methods(info);
            for (gint m = 0; m < n_methods; m++) {
                GIFunctionInfo *func_info = g_union_info_get_method(info, m);
                if (g_function_info_get_flags(func_info) & GI_FUNCTION_IS_METHOD)
                    gir_typelib_document_function_info(&export,
                                                       g_base_info_get_name(info), func_info, 1);
                else
                    gir_typelib_document_function_info(&export,
                                                       g_base_info_get_name(info), func_info, 0);
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

#ifdef FIGURE_OUT_ALL_ARG_TYPES
struct _arg_info_func_name
{
    GIArgInfo *ai;
    char *name;
};

static GPtrArray *gi_arg_infos = NULL;
#endif

static void
gir_typelib_document_callable_arguments(GString **export, GICallableInfo *info)
{
    gint n_args;
    GIArgInfo *arg;
    GIDirection dir;
    GITypeInfo *type_info;
    char *name;

    n_args = g_callable_info_get_n_args(info);

    g_string_append(*export, "   ARGS: \n");

    for (int i = 0; i < n_args; i++) {
        arg = g_callable_info_get_arg(info, i);
        dir = g_arg_info_get_direction(arg);
        type_info = g_arg_info_get_type(arg);
        if (!(dir == GI_DIRECTION_OUT)
            || (dir == GI_DIRECTION_OUT && g_arg_info_is_caller_allocates(arg))) {
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
    g_string_append_printf(*export, "   RETURN: %s%s\n",
                           g_type_tag_to_string(g_type_info_get_tag(type_info)),
                           g_type_info_is_pointer(type_info) ? "*" : "");
    g_base_info_unref(type_info);

    for (int i = 0; i < n_args; i++) {
        arg = g_callable_info_get_arg(info, i);
        dir = g_arg_info_get_direction(arg);
        type_info = g_arg_info_get_type(arg);
        if (dir == GI_DIRECTION_OUT && !g_arg_info_is_caller_allocates(arg)) {
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
    g_string_append(*export, "\n");
}

static gchar *
callback_public_name(const char *namespace_, const char *parent, GICallableInfo *info)
{
    char *public_name;
    char *tmp_str;

    if (parent) {
        tmp_str = g_strdup_printf("%s-%s", parent, g_base_info_get_name(info));
        public_name = gname_to_scm_name(tmp_str);
        g_free(tmp_str);
    }
    else {
        tmp_str = g_strdup_printf("%s", g_base_info_get_name(info));
        public_name = gname_to_scm_name(tmp_str);
        g_free(tmp_str);
    }
    return public_name;
}

static void
gir_typelib_document_callback_info(GString **export, const char *namespace_, const char *parent,
                                   GICallableInfo *info)
{
    char *lookup_name;
    char *public_name;

    if (parent)
        lookup_name = g_strdup_printf("%s-%s-%s", namespace_, parent, g_base_info_get_name(info));
    else
        lookup_name = g_strdup_printf("%s-%s", namespace_, g_base_info_get_name(info));

    public_name = callback_public_name(namespace_, parent, info);

    g_string_append_printf(*export, "CALLBACK %s\n", public_name);

    GITypeInfo *return_type = g_callable_info_get_return_type(info);
    g_assert(return_type);
    g_base_info_unref(return_type);

    gir_typelib_document_callable_arguments(export, info);
    g_free(lookup_name);
    g_free(public_name);
    g_string_append_printf(*export, "\n");
}

static void
gir_typelib_document_function_info(GString **export,
                                   const char *parent, GIFunctionInfo *info, gboolean method)
{
    gint n_args;
    GIArgInfo *arg;
    char *public_name;
    GITypeInfo *return_type;

    n_args = g_callable_info_get_n_args(info);
    return_type = g_callable_info_get_return_type(info);
    g_assert(return_type);

    if (method)
        public_name = gir_method_public_name(info);
    else
        public_name = gir_function_make_name(parent, info);
    if (method)
        g_string_append_printf(*export, "%s's METHOD %s", parent, public_name);
    else
        g_string_append_printf(*export, "PROCEDURE %s", public_name);

    for (int i = 0; i < n_args; i++) {
        arg = g_callable_info_get_arg(info, i);
        GIDirection dir = g_arg_info_get_direction(arg);
        if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT
            || (dir == GI_DIRECTION_OUT && g_arg_info_is_caller_allocates(arg))) {
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

    g_string_append_c(*export, '\n');

    gir_typelib_document_callable_arguments(export, info);

    g_string_append(*export, "\n\n");

    g_base_info_unref(return_type);
    g_free(public_name);
}

static void
gir_typelib_document_type(GString **export, char *parent, GITypeInfo *info)
{
    GType gtype = g_registered_type_info_get_g_type(info);
    gchar *doc = gir_type_document_type_from_gtype(gtype);
    g_string_append(*export, doc);
    g_free(doc);
}

/* FIXME: this is a very sigmal way to export signal info */
#if 0
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
    if (g_type_info_get_tag(return_type) == GI_TYPE_TAG_BOOLEAN &&
        !g_type_info_is_pointer(return_type))
        g_string_append_c(*export, '?');
    g_base_info_unref(return_type);

    g_string_append_printf(*export, " self");

    for (int i = 0; i < n_args; i++) {
        arg = g_callable_info_get_arg(info, i);
        GIDirection dir = g_arg_info_get_direction(arg);
        if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT) {
            g_string_append_c(*export, ' ');
            name = gname_to_scm_name(g_base_info_get_name(arg));
            g_string_append(*export, name);
            free(name);
        }
        g_base_info_unref(arg);
    }

    g_string_append_c(*export, ')');
    g_string_append_c(*export, '\n');

    g_string_append_printf(*export, "  (gi-signal-send self \n");
    g_string_append_printf(*export, "     (gi-signal-prepare \"%s\"", g_base_info_get_name(info));

    for (int i = 0; i < n_args; i++) {
        arg = g_callable_info_get_arg(info, i);
        GIDirection dir = g_arg_info_get_direction(arg);
        if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT) {
            g_string_append_c(*export, ' ');
            name = gname_to_scm_name(g_base_info_get_name(arg));
            g_string_append(*export, name);
            free(name);
        }
        g_base_info_unref(arg);
    }

    g_string_append(*export, ")))\n\n");
}
#endif

#ifdef FIGURE_OUT_ALL_ARG_TYPES
static SCM
scm_dump_all_arg_types(void)
{
    guint len = gi_arg_infos->len;
    if (len == 0)
        return SCM_UNSPECIFIED;

    FILE *fp = fopen("arg_infos.txt", "wt");

    for (guint i = 0; i < len; i++) {
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

        if (g_type_info_get_tag(ti) == GI_TYPE_TAG_ARRAY) {
            fprintf(fp, "LEN %3d SIZE %3d ", g_type_info_get_array_length(ti),
                    g_type_info_get_array_fixed_size(ti));
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
            if (pbi) {
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
        if (bi) {
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

static void
scm_typelib_do_define_module(void *data)
{
    const char **real_data = data;
    scm_i_typelib_load("%typelib-define-module", real_data[0], real_data[1]);
}

static SCM
scm_typelib_define_module(SCM s_lib, SCM s_version)
{
    gchar *name, *data[2];

    scm_dynwind_begin(0);
    scm_i_typelib_load_check_args("%typelib-define-module", s_lib, s_version, data, data + 1);
    name = scm_dynwind_or_bust("%typelib-define-module",
                               g_strdup_printf("%%gi %s-%s", data[0], data[1]));
    scm_c_define_module(name, scm_typelib_do_define_module, data);

    scm_dynwind_end();
    return SCM_UNSPECIFIED;
}

static SCM
scm_typelib_module_name(SCM s_lib, SCM s_version)
{
    gchar *name, *lib, *version;
    SCM ret;

    scm_dynwind_begin(0);
    scm_i_typelib_load_check_args("%typelib-module-name", s_lib, s_version, &lib, &version);
    name = g_strdup_printf("%s-%s", lib, version);

    ret = scm_list_2(scm_from_utf8_symbol("%gi"), scm_from_utf8_symbol(name));

    scm_dynwind_end();
    return ret;
}

void
gir_init_typelib_private(void)
{
    scm_c_define_gsubr("%typelib-module-name", 2, 0, 0, scm_typelib_module_name);
    scm_c_define_gsubr("%typelib-define-module", 2, 0, 0, scm_typelib_define_module);
}


void
gir_init_typelib(void)
{
#ifdef FIGURE_OUT_ALL_ARG_TYPES
    gi_arg_infos = g_ptr_array_new();
#endif
    scm_c_define_gsubr("typelib-get-search-path", 0, 0, 0, scm_typelib_get_search_path);
    scm_c_define_gsubr("typelib-prepend-search-path", 1, 0, 0, scm_typelib_prepend_search_path);
    scm_c_define_gsubr("typelib-load", 2, 0, 0, scm_typelib_load);
    scm_c_define_gsubr("typelib-document", 2, 0, 0, scm_typelib_document);

    scm_c_export("typelib-get-search-path",
                 "typelib-prepend-search-path", "typelib-load", "typelib-document", NULL);
}
