/* -*- Mode: C; c-basic-offset: 4 -*- */
#include <ctype.h>
#include "pycompat.h"

SCM scm_none_type;
SCM scm_none;

int
scm_is_none (SCM arg)
{
  return scm_is_true (scm_eq_p (scm_none, arg));
}


/* Adds an integer constant to the given Guile module */
void
GuModule_AddIntConstant (SCM module, const char *name, long value)
{
  scm_c_module_define (module, name, scm_from_long (value));
}

char ParseTupleError[256];

/* This is a serious simplification to the Python function.  It
   only works for format strings that are simple types, and most
   special parsing is ignored.  */
int
GuArg_ParseTuple(SCM args, const char *format, ...)
{
  va_list va;
  size_t n_args_provided, n_args_required;
  size_t i_arg, i_fmt;
  size_t len;
  const char *bar, *colon, *name;
  int opt;
  static const char *arg = "arguments";
  SCM prev = SCM_BOOL_F;

  memset (ParseTupleError, 0, 256);
  n_args_provided = scm_to_size_t (scm_length (args));
  len = strlen (format);
  colon = strchr(format, ':');
  if (colon)
    len = colon - format;
  
  bar = strchr(format, '|');
  if (bar)
    n_args_required = bar - format;
  else if (colon)
    n_args_required = colon - format;
  else
    n_args_required = strlen(format);

  if (!colon)
    name = arg;
  else
    name = colon + 1;

  if (n_args_provided < n_args_required) {
    sprintf(ParseTupleError, "%s: expected %zu arguments, but, received %zu",
	    name, n_args_required, n_args_provided);
    return FALSE;
  }

  va_start(va, format);

  /* Typechecking */
  i_arg = 0;
  i_fmt = 0;
  opt = FALSE;
  for (i_fmt = 0; i_fmt < len; i_fmt ++) {
    if (i_arg >= n_args_provided)
      break;
    if (format[i_fmt] == '|')
      opt = TRUE;
    if (isalpha (format[i_fmt]) || format[i_fmt] == '!') {
      void *ptr = va_arg(va, void *);
      SCM entry = scm_list_ref (args, scm_from_int (i_arg));
      if (scm_is_none (entry) && opt)
	continue;
      switch (format[i_fmt]) {
      case 'c':
	if (!scm_is_true (scm_char_p (entry)) && !scm_is_exact_integer (entry)) {
	  sprintf(ParseTupleError, "%s: arg %zu, expected a char", name, i_fmt);
	  return FALSE;
	}
	break;
      case 's':
	if (!scm_is_string (entry)) {
	  sprintf(ParseTupleError, "%s: arg %zu, expected a string", name, i_fmt);
	  return FALSE;
	}
	break;
      case 'z':
	if (!scm_is_string (entry) && !scm_is_none (entry)) {
	  sprintf(ParseTupleError, "%s: arg %zu, expected a string or NONE", name, i_fmt);
	  return FALSE;
	}
	break;
      case 'i':
      case 'I':
      case 'k':
      case 'K':
      case 'l':
      case 'L':
	if (!scm_is_exact_integer (entry)) {
	  sprintf(ParseTupleError, "%s: arg %zu, expected an exact integer", name, i_fmt);
	  return FALSE;
	}
	break;
      case 'f':
      case 'd':
	if (!scm_is_real (entry)) {
	  sprintf(ParseTupleError, "%s: arg %zu, expected a real number", name, i_fmt);
	  return FALSE;
	}
	break;
      case 'O':
	if (format[i_fmt + 1] == '!') {
	  if (!SCM_IS_A_P(entry, (SCM) ptr)) {
	    sprintf(ParseTupleError, "%s: arg %zu, expected a matching type", name, i_fmt);
	    return FALSE;
	  }
	}
	break;
      case '!':
	/* We don't consume an argument here. */
	continue;

      default:
	g_return_val_if_reached (FALSE);
	break;
      }

      i_arg ++;
    }
  }

  /* Marshaling */
  i_arg = 0;
  i_fmt = 0;
  for (i_fmt = 0; i_fmt < len; i_fmt ++) {
    if (isalpha (format[i_fmt]) || format[i_fmt] == '!') {
      void *ptr = va_arg(va, void *);
      SCM entry;
      if (i_arg < n_args_provided)
	entry = scm_list_ref (args, scm_from_size_t (i_arg));
      else
	entry = scm_none;
      switch (format[i_fmt]) {
      case 'c':
	if (scm_is_none (entry))
	  *(char *)ptr = '\0';
	else if (scm_is_true (scm_char_p (entry)))
	  *(char *)ptr = SCM_CHAR (entry);
	else if (scm_is_exact_integer (entry))
	  *(char *)ptr = scm_to_char (entry);
	break;
      case 's':
      case 'z':
	if (scm_is_none (entry))
	  ptr = NULL;
	else
	  ptr = scm_to_utf8_string (entry);
	break;
      case 'i':
	if (scm_is_none (entry))
	  *(int *) ptr = 0;
	else
	  *(int *) ptr = scm_to_int (entry);
	break;
      case 'I':
	if (scm_is_none (entry))
	  *(unsigned int *)ptr = 0;
	else
	  *(unsigned int *)ptr = scm_to_uint (entry);
	break;
      case 'k':
	if (scm_is_none (entry))
	  *(unsigned long *)ptr = 0;
	else
	  *(unsigned long *)ptr = scm_to_ulong (entry);
	break;
      case 'K':
	if (scm_is_none (entry))
	  *(unsigned long long *)ptr = 0;
	else
	  *(unsigned long long *)ptr = scm_to_ulong_long (entry);
	break;
      case 'l':
	if (scm_is_none (entry))
	  *(long *)ptr = 0;
	else
	  *(long *)ptr = scm_to_long (entry);
	break;
      case 'L':
	if (scm_is_none (entry))
	  *(long long *)ptr = 0;
	else 
	  *(long long *)ptr = scm_to_long_long (entry);
	break;
      case 'f':
	if (scm_is_none (entry))
	  *(float *)ptr = 0.0f;
	else
	  *(float *)ptr = scm_to_double (entry);
	break;
      case 'd':
	if (scm_is_none (entry))
	  *(double *)ptr = 0.0;
	else
	  *(double *)ptr = scm_to_double (entry);
	break;
      case 'O':
	if (format[i_fmt + 1] != '!')
	  *(scm_t_bits *)ptr = SCM_UNPACK (entry);
	break;
      case '!':
	*(scm_t_bits *)ptr = SCM_UNPACK (prev);
	/* We don't consume an argument here. */
	continue;
	
      default:
	g_return_val_if_reached (FALSE);
	break;
      }
      prev = entry;
      i_arg ++;
    }
  }
  va_end(va);
  return TRUE;
}


void
init_pycompat (void)
{
  scm_none_type = scm_make_foreign_object_type (scm_from_utf8_string ("$NONE"),
						SCM_EOL, NULL);
  scm_none = scm_permanent_object (scm_make_foreign_object_0 (scm_none_type));
}
