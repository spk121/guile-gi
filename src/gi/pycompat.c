/* -*- Mode: C; c-basic-offset: 4 -*- */
#include <ctype.h>
#include "pycompat.h"

SCM Gu_None_Type;
SCM Gu_None;

SCM GuExc_RuntimeError = SCM_BOOL_F;
SCM GuExc_TypeError = SCM_BOOL_F;
SCM GuExc_ImportError = SCM_BOOL_F;
SCM GuExc_RuntimeWarning = SCM_BOOL_F;
SCM GuExc_Warning = SCM_BOOL_F;


int
scm_is_none (SCM arg)
{
  return scm_is_true (scm_eq_p (Gu_None, arg));
}

/* Iterate over all key-value pairs in the hash table P. The ssize_t
   referred to by POS must be initialized to 0 prior to the first call
   to this function to start the iteration; the function returns TRUE
   for each pair in the hash table, and FALSE once all pairs have been
   reported. The value is POS will be incremented. The parameters KEY
   and VALUE will be filled in with each key and value,
   respectively. */
int GuDict_Next(SCM p, ssize_t *pos, SCM key, SCM value)
{
    SCM buckets = SCM_HASHTABLE_VECTOR (p);
    long n = scm_c_vector_length (buckets);
    if (*pos >= n)
	return FALSE;

    SCM ls = scm_c_vector_ref (buckets, *pos);
    key = scm_car (ls);
    value = scm_cdr (ls);
    *pos = *pos + 1;
    return TRUE;
}

/* Adds an integer constant to the given Guile module */
void
GuModule_AddIntConstant (SCM module, const char *name, long value)
{
  scm_c_module_define (module, name, scm_from_long (value));
}

SCM GuSequence_GetSlice (SCM obj, Gu_ssize_t i1, Gu_ssize_t i2)
{
    SCM ret = SCM_EOL;
    for (ssize_t i = i1; i < i2; i ++) {
	SCM entry = scm_list_ref (obj, scm_from_ssize_t (i));
	ret = scm_append (scm_list_2 (ret, scm_list_1 (entry)));
    }
    return ret;
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
	entry = Gu_None;
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


static char *GuErr_LastMessage;
static SCM GuErr_LastType;

void
GuErr_SetString(SCM type, const char *message)
{
    GuErr_LastType = type;
    free (GuErr_LastMessage);
    GuErr_LastMessage = NULL;
    if (message)
	GuErr_LastMessage = g_strdup(message);
}

void GuErr_SetObject(SCM type, SCM value)
{
    g_debug("In stub version of GuErr_SetObject");
}

void GuErr_Restore(SCM type, SCM value, SCM traceback)
{
    g_debug("In stub version of GuErr_Restore");
}

void Gu_INCREF(SCM x)
{
    g_debug("In stub versionof Gu_INCREF");
}

void GuErr_PrintEx(int set_sys_last_vars)
{
    g_debug("In stub version of GuErr_PrintEx");
}

void GuGILState_Release(GuGILState_STATE x)
{
    g_debug("In stub version of GuGILState_Release");
}

void GuErr_Clear(void)
{
    g_debug("In stub version of GuErr_Clear");
}

int GuCallable_Check(SCM o)
{
    g_debug("In stub version of GuCallable_Check");
    return 0;
}

void Gu_XINCREF(SCM x)
{
    g_debug("In stub versionof Gu_XINCREF");
}

int GuDict_SetItemString(SCM p, const char *key, SCM val)
{
    g_debug("In stub version of GuDict_SetItemString");
    return FALSE;
}

SCM GuObject_CallMethod(SCM obj, const char *name, const char *format, ...)
{
    g_debug("In stub version of GuObject_CallMethod")
	;
    return SCM_UNSPECIFIED;
}

SCM GuDict_GetItemString(SCM p, const char *key)
{
    g_debug("In stub version of GuDict_GetItemString()");
    return SCM_UNSPECIFIED;
}

int GuDict_Check(SCM p)
{
    g_debug("In stub version of GuDict_Check");
    return FALSE;
}

int GuDict_DelItemString(SCM p, const char *key)
{
    g_debug("In stub version of GuDict_DelItemString");
    return  -1;
}

int GuObject_HasAttrString(SCM obj, const char *attr_name)
{
    g_debug("In stub version of GuObject_HasAttrString");
    return FALSE;
}

ssize_t GuTuple_GET_SIZE(SCM p)
{
    g_debug("In stub version of GuTuple_GET_SIZE");
    return 0;
}

SCM GuTuple_GET_ITEM(SCM p, ssize_t pos)
{
    g_debug("In stub version of GuTuple_GET_ITEM");
    return SCM_UNSPECIFIED;
}

int GuType_Check(SCM o)
{
    g_debug ("In stub version of GType_Check");
    return FALSE;
}

int GuType_IsSubtype(SCM a, SCM b)
{
    g_debug("In stub version of GuType_IsSubtype");
    return FALSE;
}

int GuErr_WarnEx(SCM category, const char* message, ssize_t stack_level)
{
    g_debug("In stub version of GuErr_WarnEx");
    return 0;
}
    
int GuObject_SetAttrString(SCM o, const char *attr_name, SCM v)
{
    g_debug ("In the stub version of GuObject_SetAttrString");
    return 0;
}

int Gu_IsInitialized(void)
{
    g_debug("In the stub version of Gu_IsInitialized");
    return 1;
}

SCM GuErr_NewException(const char *name, SCM base, SCM dict)
{
    g_debug("In the stub version of GuErr_NewException");
    return SCM_UNSPECIFIED;
}

int GuArg_ParseTupleAndKeywords(SCM args, SCM kb, const char *format, char *keywords[], ...)
{
    g_debug("In the stub version of GuArg_ParseTupleAndKeywords");
}

int GuObject_TypeCheck(SCM o, SCM type)
{
    g_debug("In the stub version of GuObject_TypeCheck");
    return FALSE;
}


void
init_pycompat (void)
{
  Gu_None_Type = scm_make_foreign_object_type (scm_from_utf8_string ("$NONE"),
						SCM_EOL, NULL);
  Gu_None = scm_permanent_object (scm_make_foreign_object_0 (Gu_None_Type));
}


