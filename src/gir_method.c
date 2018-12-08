#include "gir_method.h"

static SCM
scm_call_method (SCM s_self, SCM s_method_name, SCM s_list_of_args)
{
  SCM_ASSERT(scm_is_string(s_method_name), s_method_name, SCM_ARG2, "call-method");
  SCM_ASSERT(scm_is_true(scm_list_p(s_list_of_args)), s_list_of_args, SCM_ARG3, "call-method");

  // Look up method by name
  SCM h, subhash;
  h = get_hash_table("%gi-methods");
  subhash = scm_hash_ref(h, s_method_name, SCM_BOOL_F);
  if (scm_is_false(subhash))
    scm_misc_error("call-method",
		   "Unknown method ~a",
		   scm_list_1(s_method_name));


    if (SCM_IS_A_P(s_object, gi_gobject_type))
        type = gi_gobject_get_ob_type(s_object);
    else if (SCM_IS_A_P(s_object, gir_gbox_type))
        type = gi_gbox_get_type(s_object);
    else
        scm_misc_error("call-method",
            "Cannot invoke ::~S~S for object ~S",
            scm_list_3(s_name, s_args, s_object));

    char *method_name = scm_to_utf8_string(s_name);

    original_type = type;
    while (scm_is_false((val = scm_hash_ref(subhash, scm_from_size_t(type), SCM_BOOL_F))))
    {
        if (!(type = g_type_parent(type)))
        {
            free(method_name);
            scm_misc_error("call-method",
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

    // This converts the SCM arguments to C arguments. This will throw
    // on conversion error.
    
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

void
gir_init_method(void)
{
  scm_c_define_gsubr("call-method", 2, 0, 1, scm_call_method);
}
