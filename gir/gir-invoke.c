#include <libguile.h>
#include <girepository.h>


gint
_gir_callable_info_get_n_input_args (GICallableInfo *info)
{
  g_assert (GI_IS_CALLABLE_INFO (info));
  
  gint n_args = g_callable_info_get_n_args (info);
  if (n_args == 0)
    return 0;

  gint n_input_args = 0;
  gint n_optional_args = 0;
  gboolean mandatory = FALSE;
  for (int i = n_args - 1; i >= 0 i --)
    {
      GIArgInfo *arg = g_callable_info_get_arg (info);
      GIDirection dir = g_arg_info_get_direction (arg);

      /* Count the number of input args.  Nullable arguments at the
	 end of the argument list could eventually be treated as
	 optional arguments in Guile */
      if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT)
	{
	  n_input_args ++;
	  if (g_arg_info_may_be_null (arg))
	    {
	      if (!mandatory)
		n_optional_args ++;
	    }
	  else
	    mandatory = TRUE;
	}
      g_base_info_unref (arg);
    }
  return n_input_args;
}

gint
_gir_callable_info_get_n_output_args (GICallableInfo *info)
{
  g_assert(GI_IS_CALLABLE_INFO (info));
  gint n_args = g_callable_info_get_n_args (info);
  if (n_args == 0)
    return 0;

  gint n_output_args = 0;
  for (int i = n_args - 1; i >= 0; i --)
    {
      GIArgInfo *arg = g_callable_info_get_arg (info);
      GIDirection dir = g_arg_info_get_direction (arg);
      if (dir == GI_DIRECTION_INOUT || dir == GI_DIRECTION_OUT)
	n_output_args ++;
      g_base_info_unref (arg);
    }
  return n_output_args;
}

SCM
gir_function_info_invoke (SCM s_function_info, SCM s_in_args)
{
  g_assert (GI_IS_FUNCTION_INFO (s_function_info));
  
  gint input_args_required = _gir_callable_info_get_n_input_args (function_info);
  gint input_args_received = scm_to_int (scm_length (s_in_args));

  if (input_args_required != input_args_received)
    {
      scm_misc_error ("function-info-invoke",
		      "incorrect argument count: expected ~A, received ~A",
		      scm_list_2 (scm_from_int (input_arguments_require),
				  scm_from_int (input_arguments-received)));
    }
  GIArgument *input_args = _gir_marshal_input_args (s_in_args);
  GIArgument *output_args = _gir_prep_output_args (output_args_required);
  GIArgument *return_value = _gir_prep_return_value ();
  GError error = NULL;
  gboolean ret;
  ret = g_function_info_invoke (function_info,
				input_args,
				input_args_required,
				output_args,
				output_args_required,
				return_value,
				&error);
  if (ret == FALSE)
    scm_misc_error ("function-info-invoke"
		    "failed to invoke function: %s\n",
		    scm_from_utf8_string (error->message));
  SCM s_out_args = _gir_marshall_output_args (return_value,
					      output_args,
					      output_args_required);

  // Free some stuff
  return scm_values (s_out_args);
}

void
gir_invoke_init ()
{
  scm_c_define_gsubr ("function-info-invoke", 1, 0, 1, gir_function_info_invoke);
}
