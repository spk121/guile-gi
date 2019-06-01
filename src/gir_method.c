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
#include "gir_method.h"
#include "gir_function.h"
#include "gi_gobject.h"
#include "gi_gstruct.h"
#include "gi_giargument.h"
#include "gir_type.h"

// This structure is a hash table tree.
// On the first level we have
//  KEY: method name string
//  VALUE: hash tables
// On the second level we have
//  KEY: GType
//  VALUE: GIMethodInfo *
GHashTable *gir_method_hash_table = NULL;

// Convert the type of names that GTK uses into Guile-like names
static char *
gir_method_gname_to_scm_name(const char *gname)
{
    g_assert (gname != NULL);
    g_assert (strlen(gname) > 0);

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

gchar*
gir_method_public_name(GICallableInfo *info)
{
    char *public_name, *tmp_str;
    GITypeInfo *return_type;

    // For the method names, we want a CamelCase type followed by a
    // lowercase string with hyphens such as 'TypeName-method-name'
    return_type = g_callable_info_get_return_type(info);
    g_assert(return_type);
    tmp_str = gir_method_gname_to_scm_name(g_base_info_get_name(info));
    if (g_type_info_get_tag(return_type) == GI_TYPE_TAG_BOOLEAN
        && !g_type_info_is_pointer(return_type))
        public_name = g_strdup_printf("%s?", tmp_str);
    else
        public_name = g_strdup(tmp_str);

    g_base_info_unref(return_type);
    g_free(tmp_str);
    return public_name;
}

// In the method table its VALUE is itself a hashtable mapping a GTYPE
// to a FUNC_INFO.  This is because many methods have the same name
// but operate on different GTypes
void
gir_method_table_insert(GType type, GIFunctionInfo *info)
{
    g_assert(type != 0);
    g_assert(info != NULL);

    gchar *public_name = gir_method_public_name(info);

    GHashTable *subhash = g_hash_table_lookup(gir_method_hash_table,
                                              public_name);
    if (!subhash)
    {
        subhash = g_hash_table_new (g_direct_hash, g_direct_equal);
        g_hash_table_insert(gir_method_hash_table, public_name, subhash);
    }
    g_hash_table_insert(subhash, GSIZE_TO_POINTER(type), info);
    g_debug("Creating method %s for type %s", public_name, g_type_name(type));
}

static GICallableInfo *
gir_method_lookup(SCM obj, const char *method_name)
{
    // Look up method by name
    GHashTable *subhash = g_hash_table_lookup(gir_method_hash_table, method_name);
    if (!subhash)
    {
        g_debug("Could not find a method '%s'", method_name);
        return NULL;
    }

    GType type;
    GICallableInfo *info;
    GHashTableIter iter;
    GType original_type = gir_type_get_gtype_from_obj(obj);

    while (original_type >= 80)
    {
        g_hash_table_iter_init (&iter, subhash);
        while (g_hash_table_iter_next (&iter,
                                       (gpointer *) (&type),
                                       (gpointer *) &info))
        {
            //g_debug("checking if %s should call %s:%s", g_type_name(original_type),
            //        g_type_name(type), method_name);
            //if (g_type_is_a (original_type, type))
            //if (g_type_is_a (type, original_type))
            if (original_type == type)
            {
                g_debug("Matched method %s:%s to object of type %s",
                        g_type_name(type),
                        method_name,
                        g_type_name(original_type));

                return info;
            }
        }
        original_type = g_type_parent(original_type);
    }
    g_debug("Could not match any method ::%s to object of type %s",
            method_name,
            g_type_name(original_type));
    return NULL;
}

static GICallableInfo *
gir_method_explicit_lookup(GType type, const char *method_name)
{
    // Look up method by name
    GHashTable *subhash = g_hash_table_lookup(gir_method_hash_table, method_name);
    if (!subhash)
    {
        g_debug("Could not find a method '%s'", method_name);
        return NULL;
    }

    GICallableInfo *info;
    info = g_hash_table_lookup(subhash, GSIZE_TO_POINTER(type));
    if (info)
    {
        g_debug("Found method %s::%s",
                g_type_name(type),
                method_name);
        return info;
    }
    g_debug("Could not find method ::%s of type %s",
            method_name,
            g_type_name(type));
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
        info = gir_method_lookup (s_object, name1);
    else
    {
        GType type = g_type_from_name(name1);
        info = gir_method_explicit_lookup(type, name2);
    }
    free (method_name);
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
    if (info == NULL)
    {
        free(method_name);
        scm_misc_error("call-method",
                       "Cannot find a method '~a' for ~s",
                       scm_list_2(s_method_name,
                                  s_object));
    }

    SCM s_args_str = scm_simple_format(SCM_BOOL_F,
                                       scm_from_locale_string("~s"),
                                       scm_list_1(s_list_of_args));
    char *args_str = scm_to_utf8_string(s_args_str);
    g_debug("Invoking %s%s for object of type %s",
            method_name,
            args_str,
            g_type_name(gir_type_get_gtype_from_obj(s_object)));
    free(args_str);

    int n_input_args, n_output_args;
    GIArgument *in_args, *out_args;
    unsigned *in_args_free;

    gir_function_info_convert_args(info,
                                   s_list_of_args,
                                   &n_input_args,
                                   &in_args,
                                   &in_args_free,
                                   &n_output_args,
                                   &out_args);
    scm_remember_upto_here_1(s_list_of_args);

    // Need to prepend 'self' to the input arguments on a method call
    in_args = g_realloc_n(in_args, n_input_args + 1, sizeof(GIArgument));
    memmove(in_args + 1, in_args, sizeof(GIArgument) * n_input_args);

    in_args[0].v_pointer = scm_foreign_object_ref(s_object, OBJ_SLOT);

    GIArgument return_arg;

    // Make the call.
    GError *err = NULL;
    gboolean ret = g_function_info_invoke(info, in_args, n_input_args + 1,
                                          out_args, n_output_args,
                                          &return_arg, &err);
    if (ret)
        g_debug("Invoked method %s", method_name);
    else
        g_debug("Failed to invoke method %s", method_name);

    // FIXME: Free any allocated input
    // gir_function_info_release_args(info, in_args + 1);
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

        scm_misc_error("call-method",
                       "error invoking method '~a': ~a",
                       scm_list_2(s_method_name, scm_from_utf8_string(str)));
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

    SCM output2 = gir_function_info_convert_output_args(method_name, info, n_output_args, out_args);
    output = scm_append(scm_list_2(output, output2));
    g_free(out_args);
    g_free(method_name);
    int outlen = scm_to_int(scm_length(output));

    if (outlen == 0)
        return SCM_UNSPECIFIED;
    if (outlen == 1)
        return scm_car(output);
    return output;
}



void
gir_method_unref_object(SCM s_object)
{
    GType type, original_type;
    GIFunctionInfo *info;

#if 0
    if (SCM_IS_A_P(s_object, gi_gobject_type))
        type = gi_gobject_get_ob_type(s_object);
    else if (SCM_IS_A_P(s_object, gir_gbox_type))
        type = gi_gbox_get_type(s_object);
    else
#endif
        scm_misc_error("gir_method_unref_object",
            "Cannot invoke \'unref\' for object ~S",
            scm_list_1(s_object));

#if 0
    original_type = type;
    SCM val;
    SCM s_name = scm_from_utf8_string("unref");
    SCM subhash = scm_hash_ref(gir_method_hash_table, s_name,
        SCM_BOOL_F);

    while (scm_is_false((val = scm_hash_ref(subhash, scm_from_size_t(type), SCM_BOOL_F))))
    {
        if (!(type = g_type_parent(type)))
        {
            // Should be impossible.
            scm_misc_error("gir_method_unref_object", "Unknown object type ~s",
                           scm_list_1(s_object));
        }
    }
    info = scm_to_pointer(val);

    g_debug("Invoking %s::unref for object of type %s",
        g_type_name(type),
        g_type_name(original_type));

    GIArgument in_arg;
#if 0
    if (SCM_IS_A_P(s_object, gi_gobject_type))
        in_arg.v_pointer = gi_gobject_get_obj(s_object);
    else if (SCM_IS_A_P(s_object, gir_gbox_type))
        in_arg.v_pointer = gi_gbox_peek_pointer(s_object);
    else
#endif
    g_assert_not_reached();
        g_abort();

    GIArgument return_arg;

    /* Make the call. */
    GError *err = NULL;
    gboolean ret = g_function_info_invoke(info, &in_arg, 1, NULL, 0, &return_arg, &err);
    if (ret)
        g_debug("Invoked unref");
    else
        g_debug("Failed to invoke unref");

    /* If there is a GError, write an error, free, and exit. */
    if (!ret)
    {
        char str[256];
        memset(str, 0, 256);
        strncpy(str, err->message, 255);
        g_error_free(err);

        scm_misc_error("gir_method_unref_object",
            "error invoking method 'unref': ~a",
            scm_list_2(s_name, scm_from_utf8_string(str)));
    }
#endif
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

    public_name = gir_method_public_name (info);
    lookup_name = g_strdup_printf("%s", g_base_info_get_name(info));

    g_string_append_printf(*export, "(define (%s self", public_name);

    // Write the docstring
    for (int i = 0; i < n_args; i++)
    {
        arg = g_callable_info_get_arg(info, i);
        GIDirection dir = g_arg_info_get_direction(arg);
        if (dir == GI_DIRECTION_IN
            || dir == GI_DIRECTION_INOUT
            || (dir == GI_DIRECTION_OUT && g_arg_info_is_caller_allocates(arg)))
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

    // Write the SCM procedure
    g_string_append_printf(*export, "  (gi-method-send self \n");
    g_string_append_printf(*export, "     (gi-method-prepare \"%s\"", lookup_name);

    for (int i = 0; i < n_args; i++)
    {
        arg = g_callable_info_get_arg(info, i);
        GIDirection dir = g_arg_info_get_direction(arg);
        if (dir == GI_DIRECTION_IN
            || dir == GI_DIRECTION_INOUT
            || (dir == GI_DIRECTION_OUT && g_arg_info_is_caller_allocates(arg)))
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
#endif
}

void gir_init_method(void)
{
    gir_method_hash_table = g_hash_table_new_full (g_str_hash,
                                                   g_str_equal,
                                                   NULL,
                                                   g_hash_table_remove_all);
    scm_c_define_gsubr("call-method", 2, 0, 1, scm_call_method);
    scm_c_export("call-method", NULL);
}