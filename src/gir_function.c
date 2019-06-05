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

#include "gir_function.h"
#include "gi_giargument.h"

GSList *function_list = NULL;

static gir_gsubr_t *gir_function_create_gsubr(GIFunctionInfo *function_info, const char *name, int *n_required, int *n_optional);
static char *gir_function_name_to_scm_name(const char *gname);

static void gir_function_info_count_args(GIFunctionInfo *info, int *in, int *out);
static void gir_function_count_input_args(GIFunctionInfo *info, int *required, int *optional);
static void gir_function_binding(ffi_cif *cif, void *ret, void **ffi_args,
    void *user_data);

static SCM gir_function_info_convert_output_args(const char *func_name, const GIFunctionInfo *func_info, int n_output_args, GIArgument *out_args);
static void gir_function_info_convert_args(GIFunctionInfo *func_info, SCM s_args, int n_input_args, GIArgument *in_args, unsigned *in_args_free, int n_output_args, GIArgument *out_args);

// Given some function introspection information from a typelib file, this procedure
// creates a SCM wrapper for that procedure in the current module.
void
gir_function_define_gsubr(const char *namespace_, const char *parent, GIFunctionInfo *info)
{
    gir_gsubr_t *func_gsubr;
    char *name;
    int n_required, n_optional;

    name = gir_function_make_name(parent, info);
    func_gsubr = gir_function_create_gsubr(info, name, &n_required, &n_optional);
    scm_c_define_gsubr(name, n_required, n_optional, 0, func_gsubr);
    scm_c_export(name, NULL);

    g_debug("dynamically bound %s to %s with %d required and %d optional arguments", name, g_base_info_get_name(info), n_required, n_optional);
    g_free (name);
}

static gir_gsubr_t *
gir_function_create_gsubr(GIFunctionInfo *function_info, const char *name, int *n_required, int *n_optional)
{
    // Check the cache to see if this function has already been created.
    GSList *x = function_list;
    GirFunction *gfn;

    while (x != NULL)
    {
        gfn = x->data;
        if (gfn->function_info == function_info)
        {
            *n_required = gfn->n_required;
            *n_optional = gfn->n_optional;
            return gfn->function_ptr;
        }
        x = x->next;
    }

    // Note that the scheme binding argument count doesn't include
    // any output arguments that don't require pre-allocation.
    gir_function_count_input_args(function_info, n_required, n_optional);

    gfn = g_new0(GirFunction, 1);
    ffi_type **ffi_args = NULL;
    ffi_type *ffi_ret_type;

    gfn->n_required = *n_required;
    gfn->n_optional = *n_optional;
    gfn->function_info = function_info;
    gfn->name = g_strdup(name);
    g_base_info_ref(function_info);

    // STEP 1
    // Allocate the block of memory that FFI uses to hold a closure object,
    // and set a pointer to the corresponding executable address.
    gfn->closure = ffi_closure_alloc(sizeof(ffi_closure),
        &(gfn->function_ptr));

    g_return_val_if_fail(gfn->closure != NULL, NULL);
    g_return_val_if_fail(gfn->function_ptr != NULL, NULL);

    // STEP 2
    // Next, we begin to construct an FFI_CIF to describe the function call.

    // Initialize the argument info vectors.
    if (*n_required + *n_optional > 0)
        ffi_args = g_new0(ffi_type *, *n_required + *n_optional);

    // All of our arguments will be SCM, so we use pointer storage.
    for (int i = 0; i < *n_required + *n_optional; i++)
        ffi_args[i] = &ffi_type_pointer;
    // The return type is also SCM, for which we use a pointer.
    ffi_ret_type = &ffi_type_pointer;

    // Initialize the CIF Call Interface Struct.
    ffi_status prep_ok;
    prep_ok = ffi_prep_cif(&(gfn->cif),
        FFI_DEFAULT_ABI,
        *n_required + *n_optional,
        ffi_ret_type,
        ffi_args);

    if (prep_ok != FFI_OK)
        scm_misc_error("gir-function-create-gsubr",
            "closure call interface preparation error #~A",
            scm_list_1(scm_from_int(prep_ok)));

    // STEP 3
    // Initialize the closure
    ffi_status closure_ok;
    closure_ok = ffi_prep_closure_loc(gfn->closure,
        &(gfn->cif),
        gir_function_binding,
        gfn,                 // The 'user-data' passed to the function
        gfn->function_ptr);

    if (closure_ok != FFI_OK)
        scm_misc_error("gir-function-create-gsubr",
            "closure location preparation error #~A",
            scm_list_1(scm_from_int(closure_ok)));

    // We add the allocated structs to a list so we
    // can deallocate nicely later.
    function_list = g_slist_prepend(function_list, gfn);

    return gfn->function_ptr;
}

static void
gir_function_cleanup (void)
{
    GSList *x = function_list;
    g_debug ("In gir_function_cleanup");

    while (x != NULL)
    {
        GirFunction *gfn = x->data;
        g_free(gfn->name);
        g_base_info_unref(gfn->function_info);
        g_free(x->data);
        x = x->next;
    }
    g_slist_free(function_list);
    function_list = NULL;
}

gchar*
gir_function_make_name(const char *parent, GICallableInfo *info)
{
    char *public_name, *tmp_str, *tmp_str2;
    GITypeInfo *return_type;

    // For the callable names, we want a lowercase string of the form
    // 'func-name-with-hyphens'
    return_type = g_callable_info_get_return_type(info);
    g_assert(return_type);

    if (parent)
    {
        tmp_str = g_strdup(parent);
        tmp_str2 = gir_function_name_to_scm_name(g_base_info_get_name(info));
        // tmp_str2 = gir_function_name_to_scm_name(g_type_name(g_base_info_get_type(info)));
        if (g_type_info_get_tag(return_type) == GI_TYPE_TAG_BOOLEAN
            && !g_type_info_is_pointer(return_type))
            public_name = g_strdup_printf("%s-%s?", tmp_str, tmp_str2);
        else
            public_name = g_strdup_printf("%s-%s", tmp_str, tmp_str2);
        g_free(tmp_str);
        g_free(tmp_str2);
    }
    else
    {
        if (g_type_info_get_tag(return_type) == GI_TYPE_TAG_BOOLEAN
            && !g_type_info_is_pointer(return_type))
            tmp_str = g_strdup_printf("%s?", g_base_info_get_name(info));
        else
            tmp_str = g_strdup_printf("%s",
                                      g_base_info_get_name(info));
        public_name = gir_function_name_to_scm_name(tmp_str);
        g_free(tmp_str);
    }

    g_base_info_unref(return_type);
    return public_name;
}

/* Convert the type of names that GTK uses into Guile-like names */
static char *
gir_function_name_to_scm_name(const char *gname)
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
        else if (gname[i] == '?')
        {
            g_string_append_c(str, '?');
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

SCM
gir_function_invoke (char *name, GIFunctionInfo *info, GObject *object, SCM args, GError **error)
{
    int n_input_args, n_input_args0, n_output_args;
    GIArgument *in_args, *in_args0, *out_args, return_arg;
    unsigned *in_args_free, *in_args_free0;

    gir_function_info_count_args(info, &n_input_args0, &n_output_args);
    if (object)
        n_input_args = n_input_args0 + 1;
    else
        n_input_args = n_input_args0;

    in_args = g_new0 (GIArgument, n_input_args);
    in_args_free = g_new0 (unsigned, n_input_args);
    out_args = g_new0 (GIArgument, n_output_args);

    if (object) {
        in_args[0].v_pointer = object;
        in_args_free[0] = GIR_FREE_NONE;
        in_args0 = in_args + 1;
        in_args_free0 = in_args_free + 1;
    }
    else {
        in_args0 = in_args;
        in_args_free0 = in_args_free;
    }

    gir_function_info_convert_args(info, args, n_input_args0, in_args0, in_args_free0, n_output_args, out_args);

    // Make the actual call.
    // Use GObject's ffi to call the C function.
    gboolean ok = g_function_info_invoke(info, in_args, n_input_args,
                                         out_args, n_output_args,
                                         &return_arg, error);

    gi_giargument_free_args(n_input_args, in_args_free, in_args);
    g_free(in_args);
    g_free(in_args_free);
    in_args = NULL;
    in_args_free = NULL;

    if (!ok) {
        g_free (out_args);
        return SCM_UNDEFINED;
    }

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

    SCM output2 = gir_function_info_convert_output_args(name, info, n_output_args, out_args);
    output = scm_append(scm_list_2(output, output2));
    g_free(out_args);

    scm_remember_upto_here_1 (s_return);
    scm_remember_upto_here_1 (output);
    scm_remember_upto_here_1 (output2);

    switch (scm_to_int (scm_length (output)))
    {
    case 0:
        return SCM_UNSPECIFIED;
    case 1:
        return scm_car (output);
    default:
        return output;
    }
}

// This is the core of a dynamically generated GICallable function wrapper.
// It converts FFI arguments to SCM arguments, converts those
// SCM arguments into GIArguments, calls the C function,
// and returns the results as an SCM packed into an FFI argument.
// Also, it converts GErrors into SCM misc-errors.
static void gir_function_binding(ffi_cif *cif, void *ret, void **ffi_args,
                                 void *user_data)
{
    GirFunction *gfn = user_data;
    SCM s_args = SCM_EOL;

    g_assert (cif != NULL);
    g_assert (ret != NULL);
    g_assert (ffi_args != NULL);
    g_assert (user_data != NULL);

    g_debug("in gir function binding for %s as %s", g_base_info_get_name(gfn->function_info), gfn->name);
    unsigned int n_args = cif->nargs;

    g_assert (n_args >= 0);

    // First, convert pointers to SCM
    for (unsigned int i = 0; i < n_args; i++)
    {
        SCM s_entry = SCM_BOOL_F;

        s_entry = SCM_PACK(*(scm_t_bits *)(ffi_args[i]));
        // I convert unspecified arguments to false because,
        // once you get here, unspecified arguments are shorthand for null
        // pointers
        if (SCM_UNBNDP(s_entry))
            s_args = scm_append(scm_list_2(s_args, scm_list_1(SCM_BOOL_F)));
        else
            s_args = scm_append(scm_list_2(s_args, scm_list_1(s_entry)));
    }

    // Then invoke the actual function
    GError *err = NULL;
    SCM output = gir_function_invoke (gfn->name, gfn->function_info, NULL, s_args, &err);

    // If there is a GError, write an error and exit.
    if (err)
    {
        char str[256];
        memset(str, 0, 256);
        strncpy(str, err->message, 255);
        g_error_free(err);

        scm_misc_error(gfn->name, str, SCM_EOL);
        g_return_if_reached();
    }

    *(ffi_arg *)ret = SCM_UNPACK(output);
}

// This procedure counts the number of input arguments
// that the SCM binding is expecting
static void
gir_function_count_input_args(GIFunctionInfo *info, int *required, int *optional)
{
    /* Count the number of required input arguments, and store
   * the arg info in a newly allocate array. */
    int n_args = g_callable_info_get_n_args((GICallableInfo *)info);
    int opt_flag = TRUE;

    *required = 0;
    *optional = 0;
    for (int i = n_args - 1; i >= 0; i--)
    {
        GIArgInfo *ai = g_callable_info_get_arg((GICallableInfo *)info, i);
        g_assert(ai != NULL);

        GIDirection dir = g_arg_info_get_direction(ai);
        if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT || (dir == GI_DIRECTION_OUT && g_arg_info_is_caller_allocates(ai)))
        {
            if (opt_flag && g_arg_info_may_be_null(ai))
                *optional = *optional + 1;
            else
            {
                *required = *required + 1;
                opt_flag = FALSE;
            }
        }
        g_base_info_unref (ai);
    }
}

// This procedure counts the number of arguments that the
// GObject Introspection FFI call is expecting.
static void
gir_function_info_count_args(GIFunctionInfo *info, int *in, int *out)
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

static void
gir_function_info_convert_args(GIFunctionInfo *func_info, SCM s_args, int n_input_args, GIArgument *in_args, unsigned *in_args_free, int n_output_args, GIArgument *out_args)
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
    g_debug("%s: %d arguments received", g_base_info_get_name(func_info), n_args_received);
    g_debug("%s: %d args expected (%d input, %d output)", g_base_info_get_name(func_info), n_args, n_input_args, n_output_args);

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
                    in_args[i_input_arg++].v_pointer = NULL;
                    if (dir == GI_DIRECTION_INOUT)
                        out_args[i_output_arg++].v_pointer = NULL;
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
                status = gi_giargument_convert_object_to_arg(obj, arg_info, &(in_args_free[i_input_arg]), &(in_args[i_input_arg]));
                if (dir == GI_DIRECTION_INOUT)
                {
                    out_args[i_output_arg].v_pointer = in_args[i_input_arg].v_pointer;
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
                // If we've run out of arguments, but this argument is
                // optional, we can handle that.
                if (i_received_arg >= n_args_received)
                {
                    if (g_arg_info_may_be_null(arg_info))
                    {
                        out_args[i_output_arg].v_pointer = NULL;
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
                    // We have an argument for an output argument that requires
                    // preallocation.
                }
            }
            else
            {
                // An output argument that doesn't require pre-allocation.
                // We don't require SCMs for those.
                i_output_arg++;
            }
        }
        g_base_info_unref(arg_info);
    }

    if (i_received_arg != n_input_args)
    {
        scm_misc_error("function-invoke",
            "wrong number of input arguments for function '~a', received ~a, used ~a",
            scm_list_3(scm_from_utf8_string(g_base_info_get_name(func_info)),
                scm_from_int(n_input_args),
                scm_from_int(i_received_arg)));
    }
    scm_remember_upto_here_1(obj);
    return;

arg_err_cleanup:
    gi_giargument_free_args(n_input_args, in_args_free, in_args);
    g_free(in_args);
    g_free(out_args);
    g_free(in_args_free);
    if (status == GI_GIARGUMENT_OUT_OF_RANGE)
        scm_out_of_range_pos(g_base_info_get_name(func_info), obj, scm_from_int(i_input_arg));
    else
        scm_misc_error(g_base_info_get_name(func_info),
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
gir_function_info_convert_output_args(const char *func_name, const GIFunctionInfo *func_info, int n_output_args, GIArgument *out_args)
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

void
gir_init_function(void)
{
    atexit(gir_function_cleanup);
}
