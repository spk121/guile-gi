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

#include <glib.h>
#include <libguile.h>
#include "gi_function_info.h"
#include "gi_giargument.h"
#include "gi_gobject.h"
#include "gi_util.h"
#include "gir_function.h"
#include "gir_method.h"
#include "gir_type.h"

// This structure is a hash table tree.
// On the first level we have
//  KEY: method name string
//  VALUE: hash tables
// On the second level we have
//  KEY: GType
//  VALUE: GIMethodInfo *
GHashTable *gir_method_hash_table = NULL;

static void gir_fini_method(void);


// In the method table its VALUE is itself a hashtable mapping a GTYPE
// to a FUNC_INFO.  This is because many methods have the same name
// but operate on different GTypes
void
gir_method_table_insert(GType type, GIFunctionInfo *info)
{
    g_assert(type != 0);
    g_assert(info != NULL);

    gchar *public_name = gi_function_info_make_name(info, NULL);

    GHashTable *subhash = g_hash_table_lookup(gir_method_hash_table,
                                              public_name);
    g_debug("Creating method %s for type %s", public_name, g_type_name(type));
    if (!subhash) {
        subhash = g_hash_table_new(g_direct_hash, g_direct_equal);
        g_hash_table_insert(gir_method_hash_table, public_name, subhash);
    }
    else
        g_free(public_name);

    g_hash_table_insert(subhash, GSIZE_TO_POINTER(type), info);
}


static GICallableInfo *
gir_method_lookup(SCM obj, const char *method_name)
{
    // Look up method by name
    GHashTable *subhash = g_hash_table_lookup(gir_method_hash_table, method_name);
    if (!subhash) {
        g_debug("Could not find a method '%s'", method_name);
        return NULL;
    }

    GType type;
    GICallableInfo *info;
    GHashTableIter iter;
    GType original_type = gir_type_get_gtype_from_obj(obj);

    while (original_type >= 80) {
        g_hash_table_iter_init(&iter, subhash);
        while (g_hash_table_iter_next(&iter, (gpointer *) (&type), (gpointer *) & info)) {
            //g_debug("checking if %s should call %s:%s", g_type_name(original_type),
            //        g_type_name(type), method_name);
            //if (g_type_is_a (original_type, type))
            //if (g_type_is_a (type, original_type))
            if (original_type == type) {
                g_debug("Matched method %s:%s to object of type %s",
                        g_type_name(type), method_name, g_type_name(original_type));

                return info;
            }
        }
        original_type = g_type_parent(original_type);
    }
    g_debug("Could not match any method ::%s to object of type %s",
            method_name, g_type_name(original_type));
    return NULL;
}

static GICallableInfo *
gir_method_explicit_lookup(GType type, const char *method_name)
{
    // Look up method by name
    GHashTable *subhash = g_hash_table_lookup(gir_method_hash_table, method_name);
    if (!subhash) {
        g_debug("Could not find a method '%s'", method_name);
        return NULL;
    }

    GICallableInfo *info;
    info = g_hash_table_lookup(subhash, GSIZE_TO_POINTER(type));
    if (info) {
        g_debug("Found method %s::%s", g_type_name(type), method_name);
        return info;
    }
    g_debug("Could not find method ::%s of type %s", method_name, g_type_name(type));
    return NULL;
}

// Look up method by name.  If the method is of the form TYPE:METHOD,
// it will try to look up that explicit type's method.  If the method
// is just of the form METHOD, it will try to find the correct type by
// using the object's type.
static GICallableInfo *
gir_method_lookup_full(SCM s_object, SCM s_method_name)
{
    char *method_name = scm_to_utf8_string(s_method_name);
    char *name1, *name2;
    char *token = ":";
    GICallableInfo *info;

    name1 = strtok(method_name, token);
    name2 = strtok(NULL, token);
    if (name2 == NULL)
        info = gir_method_lookup(s_object, name1);
    else {
        GType type = g_type_from_name(name1);
        info = gir_method_explicit_lookup(type, name2);
    }
    free(method_name);
    return info;
}


// Call the named method on the object and list of args.  If the
// method has the form TYPE:METHOD, an explicit method is called.  If
// the method has the form METHOD, we try to guess the TYPE by
// inspecting S_OBJECT.
static SCM
scm_call_method(SCM s_object, SCM s_method_name, SCM s_list_of_args)
{
    SCM_ASSERT(scm_is_string(s_method_name), s_method_name, SCM_ARG2, "call-method");
    SCM_ASSERT(scm_is_true(scm_list_p(s_list_of_args)), s_list_of_args, SCM_ARG3, "call-method");

    GICallableInfo *info;
    char *method_name = scm_to_utf8_string(s_method_name);
    info = gir_method_lookup_full(s_object, s_method_name);
    if (info == NULL) {
        free(method_name);
        scm_misc_error("call-method",
                       "Cannot find a method '~a' for ~s", scm_list_2(s_method_name, s_object));
    }

    SCM s_args_str = scm_simple_format(SCM_BOOL_F,
                                       scm_from_locale_string("~s"),
                                       scm_list_1(s_list_of_args));
    char *args_str = scm_to_utf8_string(s_args_str);
    g_debug("Invoking %s%s for object of type %s",
            method_name, args_str, g_type_name(gir_type_get_gtype_from_obj(s_object)));
    free(args_str);

    GObject *object = scm_foreign_object_ref(s_object, OBJ_SLOT);

    GError *err = NULL;
    SCM output = gir_function_invoke(method_name, info, object, s_list_of_args, &err);

    /* If there is a GError, write an error, free, and exit. */
    if (err) {
        g_debug("Failed to invoke method %s", method_name);

        char str[256];
        memset(str, 0, 256);
        strncpy(str, err->message, 255);
        g_error_free(err);
        free(method_name);

        scm_misc_error("call-method",
                       "error invoking method '~a': ~a",
                       scm_list_2(s_method_name, scm_from_utf8_string(str)));
        return SCM_BOOL_F;
    }

    g_debug("Invoked method %s", method_name);

    return output;
}

void
gir_method_document(GString **export, const char *namespace_,
                    const char *parent, GICallableInfo *info)
{
#if 0
    gint n_args;
    GIArgInfo *arg;
    char *lookup_name, *public_name, *tmp_str;

    n_args = g_callable_info_get_n_args(info);
    g_assert(parent != NULL);

    public_name = gir_method_public_name(info);
    lookup_name = g_strdup_printf("%s", g_base_info_get_name(info));

    g_string_append_printf(*export, "(define (%s self", public_name);

    // Write the docstring
    for (int i = 0; i < n_args; i++) {
        arg = g_callable_info_get_arg(info, i);
        GIDirection dir = g_arg_info_get_direction(arg);
        if (dir == GI_DIRECTION_IN
            || dir == GI_DIRECTION_INOUT
            || (dir == GI_DIRECTION_OUT && g_arg_info_is_caller_allocates(arg))) {
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

    // Write the SCM procedure
    g_string_append_printf(*export, "  (gi-method-send self \n");
    g_string_append_printf(*export, "     (gi-method-prepare \"%s\"", lookup_name);

    for (int i = 0; i < n_args; i++) {
        arg = g_callable_info_get_arg(info, i);
        GIDirection dir = g_arg_info_get_direction(arg);
        if (dir == GI_DIRECTION_IN
            || dir == GI_DIRECTION_INOUT
            || (dir == GI_DIRECTION_OUT && g_arg_info_is_caller_allocates(arg))) {
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
#endif
}

void
gir_init_method(void)
{
    gir_method_hash_table = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, NULL);
    scm_c_define_gsubr("call-method", 2, 0, 1, scm_call_method);
    scm_c_export("call-method", NULL);
    atexit(gir_fini_method);
}

static void
gir_fini_method(void)
{
    // Free the hash table.
    GHashTableIter iter;
    gpointer key, value;

    g_hash_table_iter_init(&iter, gir_method_hash_table);
    while (g_hash_table_iter_next(&iter, &key, &value)) {
        GHashTable *value_hash = value;

        GHashTableIter iter2;
        gpointer key2, value2;
        g_hash_table_iter_init(&iter2, value_hash);
        while (g_hash_table_iter_next(&iter2, &key2, &value2))
            g_base_info_unref((GIBaseInfo *)value2);
        g_hash_table_destroy(value_hash);
    }
    g_hash_table_destroy(gir_method_hash_table);
    g_debug("Freed method table");
}
