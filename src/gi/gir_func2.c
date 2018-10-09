#include <libguile.h>
#include <girepository.h>
#include "gi_giargument.h"
#include "gir_type.h"
#include "gir_func2.h"


#define MAX_GERROR_MSG 100
static char gerror_msg[MAX_GERROR_MSG];

static void
store_gerror_message (const char *msg)
{
  memset (gerror_msg, 0, MAX_GERROR_MSG);
  strncpy (gerror_msg, msg, MAX_GERROR_MSG - 1);
  if (strlen (msg) >= MAX_GERROR_MSG - 1) {
    gerror_msg[MAX_GERROR_MSG-2] = '.';
    gerror_msg[MAX_GERROR_MSG-3] = '.';
    gerror_msg[MAX_GERROR_MSG-4] = '.';
  }
}

static GHashTable *gi_constants = NULL;
static GHashTable *gi_enums = NULL;
static GHashTable *gi_flags = NULL;
static GHashTable *gi_functions = NULL;
static GHashTable *gi_callbacks = NULL;
static GHashTable *gi_structs = NULL;
static GHashTable *gi_unions = NULL;
static GHashTable *gi_objects = NULL;
static GHashTable *gi_interfaces = NULL;

#if 0
static guint
hash_key_func (gconstpointer v)
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

static gboolean
insert_into_hash_table (const char *category,
			const char *namespace_,
			const char *parent,
			GHashTable **p_hash_table,
			GIBaseInfo *info)
{
  if (!*p_hash_table) {
    g_debug ("Creating %s hash table", category);
    *p_hash_table = g_hash_table_new_full (g_str_hash,
					   g_str_equal,
					   g_free,
					   (GDestroyNotify) g_base_info_unref);
  }
  gchar *full_name;
#ifdef PREFIX_NAME_IN_HASH
  if (parent)
    full_name = g_strdup_printf ("%s-%s-%s", namespace_, parent, g_base_info_get_name (info));
  else
    full_name = g_strdup_printf ("%s-%s", namespace_, g_base_info_get_name (info));
#else
  if (parent)
    full_name = g_strdup_printf ("%s-%s", parent, g_base_info_get_name (info));
  else
    full_name = g_strdup_printf ("%s", g_base_info_get_name (info));
#endif
  if (g_hash_table_contains (*p_hash_table, full_name)) {
    g_critical ("Did not overwrite %s in %s hash table.", full_name, category);
    g_free (full_name);
    return FALSE;
  }
  
  /* The hash table keeps the key string. */
  g_hash_table_replace (*p_hash_table,
			full_name,
			info);
  g_debug ("Inserted %s into %s hash table. %u entries.",
	   full_name,
	   category,
	   g_hash_table_size (*p_hash_table));
  return TRUE;
}

static SCM
scm_gi_load_repository (SCM s_namespace, SCM s_version)
{
  gchar *namespace_; 
  gchar *version;
  GITypelib *tl;
  GError *error = NULL;

  SCM_ASSERT (scm_is_string (s_namespace), s_namespace, SCM_ARG1, "irepository-load");
  SCM_ASSERT (scm_is_string (s_version), s_version, SCM_ARG2, "irepository-load");

  namespace_ = scm_to_utf8_string (s_namespace);
  version = scm_to_utf8_string (s_version);

  tl = g_irepository_require (NULL, namespace_, version, 0, &error);
  if (tl == NULL) {
    free (version);
    free (namespace_);
    store_gerror_message (error->message);
    g_error_free (error);
    scm_misc_error ("%irepository-require", gerror_msg, SCM_EOL);
    return SCM_UNSPECIFIED;
  }

  g_debug ("Parsing irepository %s %s", namespace_, version);
  int n = g_irepository_get_n_infos (NULL, namespace_);
  for (int i = 0; i < n; i ++) {
      GIBaseInfo *info;
      GIInfoType type;
      info = g_irepository_get_info (NULL, namespace_, i);
      type = g_base_info_get_type (info);
      switch (type) {
      case GI_INFO_TYPE_CALLBACK:
	insert_into_hash_table ("callbacks", namespace_, NULL, &gi_callbacks, info);
	break;
      case GI_INFO_TYPE_FUNCTION:
	insert_into_hash_table ("functions", namespace_, NULL, &gi_functions, info);
	break;
      case GI_INFO_TYPE_STRUCT:
	{
	  insert_into_hash_table ("structs", namespace_, NULL, &gi_structs, info);
	  gint n_methods = g_struct_info_get_n_methods (info);
	  for (gint m = 0; m < n_methods; m ++) {
	    GIFunctionInfo *func_info = g_struct_info_get_method (info, m);
	    if (!insert_into_hash_table ("functions",
					 namespace_,
					 g_base_info_get_name (info),
					 &gi_functions,
					 func_info))
	      g_base_info_unref (func_info);
	  }
	}
	break;
      case GI_INFO_TYPE_ENUM:
	insert_into_hash_table ("enums", namespace_, NULL, &gi_enums, info);
	  break;
	case GI_INFO_TYPE_FLAGS:
	  insert_into_hash_table ("flags", namespace_, NULL, &gi_flags, info);
	  break;
	case GI_INFO_TYPE_OBJECT:
	  {
	    insert_into_hash_table ("objects", namespace_, NULL, &gi_objects, info);
	    gint n_methods = g_object_info_get_n_methods (info);
	    for (gint m = 0; m < n_methods; m ++) {
	      GIFunctionInfo *func_info = g_object_info_get_method (info, m);
	      if (!insert_into_hash_table ("functions", namespace_,
					   g_base_info_get_name (info),
					   &gi_functions, func_info))
		g_base_info_unref (func_info);
	    }
	  }
	  break;
	case GI_INFO_TYPE_INTERFACE:
	  insert_into_hash_table ("interfaces", namespace_, NULL, &gi_interfaces, info);
	  break;
	case GI_INFO_TYPE_CONSTANT:
	  insert_into_hash_table ("constants", namespace_, NULL, &gi_constants, info);
	  break;
	case GI_INFO_TYPE_UNION:
	  {
	    insert_into_hash_table ("unions", namespace_, NULL, &gi_unions, info);
	    gint n_methods = g_union_info_get_n_methods (info);
	    for (gint m = 0; m < n_methods; m ++) {
	      GIFunctionInfo *func_info = g_union_info_get_method (info, m);
	      if (!insert_into_hash_table ("functions", namespace_,
					   g_base_info_get_name (info), &gi_functions, func_info))
		g_base_info_unref (func_info);
	    }
	  }
	  break;
	case GI_INFO_TYPE_VALUE:
	  g_critical ("Unsupported irepository type 'VALUE'");
	  break;
	case GI_INFO_TYPE_SIGNAL:
	  g_critical ("Unsupported irepository type 'SIGNAL'");
	  break;
	case GI_INFO_TYPE_VFUNC:
	  g_critical ("Unsupported irepository type 'VFUNC'");
	  break;
	case GI_INFO_TYPE_PROPERTY:
	  g_critical ("Unsupported irepository type 'PROPERTY'");
	  break;
	case GI_INFO_TYPE_FIELD:
	  g_critical ("Unsupported irepository type 'FIELD'");
	  break;
	case GI_INFO_TYPE_ARG:
	  g_critical ("Unsupported irepository type 'ARG'");
	  break;
	case GI_INFO_TYPE_TYPE:
	  g_critical ("Unsupported irepository type 'TYPE'");
	  break;
	case GI_INFO_TYPE_INVALID:
	case GI_INFO_TYPE_INVALID_0:
	default:
	  g_critical ("Unsupported irepository type %d", type);
	  break;
      }
  }
  free (version);
  free (namespace_);
  
  return SCM_UNSPECIFIED;
}

static SCM
scm_gi_constant_value (SCM s_namespace, SCM s_name)
{
  char *namespace_;
  char *name;
  char *full_name;

  SCM_ASSERT (scm_is_string (s_namespace), s_namespace, SCM_ARG1, "gi-constant-value");
  SCM_ASSERT (scm_is_string (s_name), s_name, SCM_ARG2, "gi-constant-value");
  namespace_ = scm_to_utf8_string (s_namespace);
  name = scm_to_utf8_string (s_name);

#ifdef PREFIX_NAME_IN_HASH
  full_name = g_strdup_printf ("%s-%s", namespace_, name);
#else
  full_name = g_strdup_printf ("%s", name);
#endif
  gpointer val = g_hash_table_lookup (gi_constants, full_name);
  if (!val) {
    free (namespace_);
    free (name);
    g_free (full_name);
    scm_misc_error ("gi-constant-value",
		    "unknown constant ~a in ~a",
		    scm_list_2 (s_name, s_namespace));
    return SCM_UNSPECIFIED;
  }

  GIConstantInfo *info = val;
  GITypeInfo *typeinfo;
  typeinfo = g_constant_info_get_type (info);
  GITypeTag typetag;
  typetag = g_type_info_get_tag (typeinfo);

  gint siz;
  GIArgument value;
  siz = g_constant_info_get_value(info, &value);
  SCM ret;

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
      g_critical ("Constant %s in %s has unsupported type %d",
		  name, namespace_, typetag);
      ret = SCM_BOOL_F;
    }
  g_constant_info_free_value (info, &value);
  free (namespace_);
  free (name);
  g_free (full_name);

  return ret;
}

static SCM
scm_gi_flag_or_enum_value (SCM s_namespace, SCM s_category, SCM s_name, gboolean is_enum)
{
  char *name;
  char *full_category;
  
  SCM_ASSERT (scm_is_string (s_namespace), s_namespace, SCM_ARG1,
	      is_enum ? "gi-enum-value" : "gi-flag-value");
  SCM_ASSERT (scm_is_string (s_category), s_category, SCM_ARG2,
	      is_enum ? "gi-enum-value" : "gi-flag-value");
  SCM_ASSERT (scm_is_string (s_name), s_name, SCM_ARG3,
	      is_enum ? "gi-enum-value" : "gi-flag-value");
  
  name = scm_to_utf8_string (s_name);

#ifdef PREFIX_NAME_IN_HASH
  {
    char *namespace_ = NULL;
    char *category = NULL;
    namespace_ = scm_to_utf8_string (s_namespace);
    category = scm_to_utf8_string (s_category);
    full_category = g_strdup_printf ("%s-%s", namespace_, category);
    free (category);
    free (namespace_);
  }
#else
  full_category = scm_to_utf8_string (s_category);
#endif

  gpointer val;
  if (is_enum)
    val = g_hash_table_lookup (gi_enums, full_category);
  else
    val = g_hash_table_lookup (gi_flags, full_category);
    
  g_free (full_category);
  full_category = NULL;
  
  if (!val) {
    free (name);
    name = NULL;

    scm_misc_error (is_enum ? "gi-enum-value" : "gi-flag-value",
		    is_enum ? "unknown enum type '~a' in ~a" : "unknown flag type '~a' in ~a",
		    scm_list_2 (s_category, s_namespace));
    return SCM_BOOL_F;
  }

  GIEnumInfo *info = val;
  gint n_values = g_enum_info_get_n_values (info);
  gint i = 0;
  GIValueInfo *vi = NULL;

  while (i < n_values) {
    vi = g_enum_info_get_value (info, i);
    g_assert (vi != NULL);
    
    g_debug ("flag name search: %s == %s ?",
	     name,
	     g_base_info_get_name (vi));
    if (strcmp (g_base_info_get_name (vi), name) == 0) {
      break;
    }
    g_base_info_unref (vi);
    vi = NULL;
    i++;
  }

  free (name);
  name = NULL;

  if (i >= n_values) {
    scm_misc_error (is_enum ? "gi-enum-value" : "gi-flag-value",
		    is_enum ? "unknown enum '~a' of type '~a' in ~a": "unknown flag '~a' of type '~a' in ~a",
		    scm_list_3 (s_name, s_category, s_namespace));
    return SCM_BOOL_F;
  } else {
    SCM ret = scm_from_int64 (g_value_info_get_value (vi));
    g_base_info_unref (vi);
    vi = NULL;
    return ret;
  }

  /* never reached */
  return SCM_BOOL_F;
}

static SCM
scm_gi_flag_value (SCM s_namespace, SCM s_category, SCM s_name)
{
  return scm_gi_flag_or_enum_value (s_namespace, s_category, s_name, FALSE);
}

static SCM
scm_gi_enum_value (SCM s_namespace, SCM s_category, SCM s_name)
{
  return scm_gi_flag_or_enum_value (s_namespace, s_category, s_name, TRUE);
}

static SCM
scm_gi_struct_ref (SCM s_ptr, SCM s_namespace, SCM s_type_name, SCM s_field_name)
{
  char *full_type_name = NULL;
  
  SCM_ASSERT (SCM_POINTER_P (s_ptr), s_ptr, SCM_ARG1,
	      "gi-struct-ref");
  SCM_ASSERT (scm_is_string (s_namespace), s_namespace, SCM_ARG2,
	      "gi-struct-ref");
  SCM_ASSERT (scm_is_string (s_type_name), s_type_name, SCM_ARG3,
	      "gi-struct-ref");
  SCM_ASSERT (scm_is_string (s_field_name), s_field_name, SCM_ARG4,
	      "gi-struct-ref");

#ifdef PREFIX_NAME_IN_HASH
  {
    char *namespace_ = scm_to_utf8_string (s_namespace);
    char *type_name = scm_to_utf8_string (s_type_name);
    full_type_name = g_strdup_printf ("%s-%s", namespace_, type_name);
    free (type_name);
    free (namespace_);
  }
#else
  full_type_name = scm_to_utf8_string (s_type_name);
#endif

  GIStructInfo *si = g_hash_table_lookup (gi_structs, full_type_name);
  g_free (full_type_name);
  full_type_name = NULL;
  
  if (!si) {
    scm_misc_error ("gi-struct-ref",
		    "unknown struct type '~a' in ~a",
		    scm_list_2 (s_type_name, s_namespace));
    return SCM_BOOL_F;
  }
  
  gint n_fields = g_struct_info_get_n_fields (si);
  gint i = 0;
  GIFieldInfo *fi = NULL;
  char *field_name = scm_to_utf8_string (s_field_name);

  while (i < n_fields) {
    fi = g_struct_info_get_field (si, i);
    g_assert (fi != NULL);
    
    g_debug ("field name search: %s == %s ?",
	     field_name,
	     g_base_info_get_name (fi));
    if (strcmp (g_base_info_get_name (fi), field_name) == 0) {
      break;
    }
    g_base_info_unref (fi);
    fi = NULL;
    i++;
  }

  free (field_name);
  if (i >= n_fields) {
    scm_misc_error ("gi-struct-ref",
		    "unknown field '~a' in struct '~a' in ~a",
		    scm_list_3 (s_field_name, s_type_name, s_namespace));
    return SCM_BOOL_F;
  } else {
    gboolean ok;
    GIArgument arg;
    void *ptr = scm_to_pointer (s_ptr);
    ok = g_field_info_get_field (fi, ptr, &arg);
    g_base_info_unref (fi);
    fi = NULL;

    if (!ok) {
      scm_misc_error ("gi-struct-ref",
		      "cannot unpack field '~a' in struct '~a'",
		      scm_list_2 (s_field_name, s_type_name));
      return SCM_BOOL_F;
    } else {
      GITypeInfo *ti = g_field_info_get_type (fi);
      SCM output = gi_giargument_to_object (&arg, ti, FALSE);
      g_base_info_unref (ti);
      return output;
    }
  }
  
  /* never get here */
  return SCM_BOOL_F;
}

static SCM
scm_gi_struct_set (SCM s_ptr, SCM s_namespace, SCM s_type_name, SCM s_field_name, SCM s_value)
{
  char *full_type_name = NULL;
  
  // SCM_VALIDATE_POINTER (1, s_ptr);
  SCM_ASSERT (scm_is_string (s_namespace), s_namespace, SCM_ARG2,
	      "gi-struct-set");
  SCM_ASSERT (scm_is_string (s_type_name), s_type_name, SCM_ARG3,
	      "gi-struct-set");
  SCM_ASSERT (scm_is_string (s_field_name), s_field_name, SCM_ARG4,
	      "gi-struct-set");

#ifdef PREFIX_NAME_IN_HASH
  {
    char *namespace_ = scm_to_utf8_string (s_namespace);
    char *type_name = scm_to_utf8_string (s_type_name);
    full_type_name = g_strdup_printf ("%s-%s", namespace_, type_name);
    free (type_name);
    free (namespace_);
  }
#else
  full_type_name = scm_to_utf8_string (s_type_name);
#endif

  GIStructInfo *si = g_hash_table_lookup (gi_structs, full_type_name);
  g_free (full_type_name);
  full_type_name = NULL;
  
  if (!si) {
    scm_misc_error ("gi-struct-ref",
		    "unknown struct type '~a' in ~a",
		    scm_list_2 (s_type_name, s_namespace));
    return SCM_BOOL_F;
  }
  
  gint n_fields = g_struct_info_get_n_fields (si);
  gint i = 0;
  GIFieldInfo *fi = NULL;
  char *field_name = scm_to_utf8_string (s_field_name);

  while (i < n_fields) {
    fi = g_struct_info_get_field (si, i);
    g_assert (fi != NULL);
    
    g_debug ("field name search: %s == %s ?",
	     field_name,
	     g_base_info_get_name (fi));
    if (strcmp (g_base_info_get_name (fi), field_name) == 0) {
      break;
    }
    g_base_info_unref (fi);
    fi = NULL;
    i++;
  }

  free (field_name);
  if (i >= n_fields) {
    scm_misc_error ("gi-struct-set",
		    "unknown field '~a' in struct '~a' in ~a",
		    scm_list_3 (s_field_name, s_type_name, s_namespace));
    return SCM_BOOL_F;
  } else {
    gboolean ok;
    GIArgument arg;
    GITypeInfo *ti = g_field_info_get_type (fi);
    arg = gi_argument_from_object ("gi-struct-set",
				   s_value,
				   ti,
				   GI_TRANSFER_NOTHING);
    g_base_info_unref (ti);
    ti = NULL;
    void *ptr = scm_to_pointer (s_ptr);
    ok = g_field_info_set_field (fi, ptr, &arg);
    g_base_info_unref (fi);
    fi = NULL;

    if (!ok) {
      scm_misc_error ("gi-struct-set",
		      "cannot set field '~a' in struct '~a' to '~a'",
		      scm_list_3 (s_field_name, s_type_name, s_value));
      return SCM_BOOL_F;
    } else {
      return SCM_BOOL_T;
    }
  }
  
  /* never get here */
  return SCM_BOOL_F;
}

static void
function_info_count_args (GIFunctionInfo *info, int *in, int *out)
{
  /* Count the number of required input arguments, and store
   * the arg info in a newly allocate array. */
  int n_args = g_callable_info_get_n_args ((GICallableInfo *) info);
  int n_input_args = 0;
  int n_output_args = 0;
  
  for (int i = 0; i < n_args; i ++) {
    GIArgInfo *ai = g_callable_info_get_arg ((GICallableInfo *) info, i);
    g_assert (ai != NULL);
    
    GIDirection dir = g_arg_info_get_direction (ai);
    g_base_info_unref (ai);
    
    if (dir == GI_DIRECTION_IN)
      n_input_args ++;
    else if (dir == GI_DIRECTION_OUT)
      n_output_args ++;
    else if (dir == GI_DIRECTION_INOUT) {
      n_input_args ++;
      n_output_args ++;
    }
  }
  *in = n_input_args;
  *out = n_output_args;
}

static gboolean
function_info_typecheck_args (GIFunctionInfo *func_info, SCM s_args, char **errstr)
{
  GIDirection dir;
  GIArgInfo *arg_info;
  gboolean type_ok;
  
  int n_args = g_callable_info_get_n_args ((GICallableInfo *) func_info);
  int i_input = 0;
  
  for (int i = 0; i < n_args; i ++) {
    arg_info = g_callable_info_get_arg ((GICallableInfo *) func_info, i);
    g_assert (arg_info != NULL);
    
    dir = g_arg_info_get_direction (arg_info);
    
    if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT) {
      SCM arg = scm_list_ref (s_args, scm_from_int (i_input));
      type_ok = gi_giargument_check_scm_type(arg, arg_info, errstr);
      i_input ++;
    }
    g_base_info_unref (arg_info);
    if (!type_ok)
      break;
  }
  return type_ok;
}

static GIArgument *
function_info_convert_args (GIFunctionInfo *func_info, SCM s_args)
{
  GIDirection dir;
  GIArgInfo *arg_info;
  GIArgument *in_args = g_new0 (GIArgument, scm_to_int (scm_length (s_args)));
  
  int n_args = g_callable_info_get_n_args ((GICallableInfo *) func_info);
  int i_input = 0;
  
  for (int i = 0; i < n_args; i ++) {
    arg_info = g_callable_info_get_arg ((GICallableInfo *) func_info, i);
    g_assert (arg_info != NULL);
    
    dir = g_arg_info_get_direction (arg_info);
    
    if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT) {
      SCM arg = scm_list_ref (s_args, scm_from_int (i_input));
      in_args[i_input] = gi_argument_from_object ("gi-function-invoke",
						  arg,
						  g_arg_info_get_type (arg_info),
						  g_arg_info_get_ownership_transfer (arg_info));
      i_input ++;
    }
    g_base_info_unref (arg_info);
  }

  return in_args;
}


static SCM
scm_gi_function_invoke (SCM s_namespace, SCM s_name, SCM s_args)
{
  GError *err = NULL;

  SCM_ASSERT (scm_is_string (s_namespace), s_namespace, SCM_ARG1,
	      "gi-function-invoke");
  SCM_ASSERT (scm_is_string (s_name), s_name, SCM_ARG2,
	      "gi-function-invoke");

  GIFunctionInfo *info = NULL;

  {
    char *name = scm_to_utf8_string (s_name);
    g_debug ("in gi-function-invoke for %s", name);
    info = g_hash_table_lookup (gi_functions, name);
    free (name);
    name = NULL;
  }
  
  if (!info) {
    scm_misc_error ("gi-function-invoke",
		    "unknown procedure '~a' in ~a",
		    scm_list_2 (s_name, s_namespace));
    return SCM_BOOL_F;
  }
  
  int n_input_args_received;
  if (SCM_UNBNDP (s_args))
      n_input_args_received = 0;
  else
    n_input_args_received = scm_to_int (scm_length (s_args));
  g_debug ("\t%d input arguments received", n_input_args_received);
  
  /* Count the number of required input arguments, and store
   * the arg info in a newly allocate array. */
  int n_input_args, n_output_args;
  function_info_count_args (info, &n_input_args, &n_output_args);
  g_debug ("\t%d input arguments expected", n_input_args);
  g_debug ("\t%d output arguments expected", n_output_args);
  
  if (n_input_args_received != n_input_args) {
    scm_misc_error ("function-invoke",
		    "wrong number of input arguments for funtion '~a', expected ~S, received ~s",
		    scm_list_3 (s_name,
			       scm_from_int (n_input_args),
			       scm_from_int (n_input_args_received)));
    return SCM_BOOL_F;
  }

  char *errstr = NULL;
  if (!function_info_typecheck_args (info, s_args, &errstr)) {
    g_assert (errstr != NULL);
    
    SCM s_errstr = scm_from_utf8_string (errstr);
    g_free (errstr);
    scm_misc_error ("gi-function-invoke",
		    "wrong type argument for function '~a' - ~s",
		    scm_list_2 (s_name, s_errstr));
    return SCM_BOOL_F;
  }


  GIArgument *in_args = function_info_convert_args (info, s_args);

  /* Allocate a GIArgument list of the output and return values. */
  GIArgument *out_args = NULL;
  if (n_output_args > 0)
    out_args = g_new0(GIArgument, n_output_args);
  GIArgument return_arg;

  /* Make the call. */
  gboolean ret = g_function_info_invoke(info, in_args, n_input_args,
					out_args, n_output_args,
					&return_arg, &err);

  /* Free any allocated input */
  for (int i = 0; i < n_input_args_received; i ++) {
    // free (in_args_free);
    // FIXME: do this
  }
  g_free (in_args);
  in_args = NULL;
  
  /* If there is a GError, write an error, free, and exit. */
  if (!ret) {
    char str[256];
    memset (str, 0, 256);
    strncpy (str, err->message, 255);
    g_error_free (err);
    
    scm_misc_error ("gi-function-invoke",
		    "error invoking function '~a': ~a",
		    scm_list_2 (s_name, scm_from_utf8_string (str)));
    return SCM_BOOL_F;
  }

  /* We've actually made a call.  Hooray! Convert the output
   * arguments and return values into Scheme objects.  Free the
   * C objects if necessary.  Return the output either as
   * a plain list or as a values list. */
  GITypeInfo *return_typeinfo = g_callable_info_get_return_type (info);
  SCM s_return = gi_giargument_to_object (&return_arg,
					  return_typeinfo,
					  g_callable_info_get_caller_owns (info));
  g_base_info_unref (return_typeinfo);
  SCM output;
  if (s_return == SCM_UNSPECIFIED)
    output = SCM_EOL;
  else
    output = scm_list_1 (s_return);

  int n_args = g_callable_info_get_n_args ((GICallableInfo *) info);
  for (int i = 0; i < n_args; i ++) {
    GIArgInfo *arg_info = g_callable_info_get_arg ((GICallableInfo *) info, i);
    g_assert (arg_info != NULL);
    
    GIDirection dir = g_arg_info_get_direction (arg_info);
    
    if (dir == GI_DIRECTION_OUT || dir == GI_DIRECTION_INOUT) {
      GITypeInfo *arg_typeinfo = g_arg_info_get_type (arg_info);
      SCM entry = gi_giargument_to_object (&out_args[i],
					   arg_typeinfo,
					   g_arg_info_get_ownership_transfer (arg_info));
      output = scm_append(scm_list_2(output, scm_list_1 (entry)));
      g_base_info_unref (arg_typeinfo);
    }
    g_base_info_unref (arg_info);
  }

  return output;
}

static void
unload_repository (const char *category, GHashTable **p_hash_table)
{
  if (*p_hash_table) {
    g_debug ("destroying %s hash table", category);
    g_hash_table_destroy (*p_hash_table);
    *p_hash_table = NULL;
  }
}

static SCM
scm_gi_unload_repositories (void)
{
  unload_repository ("constants", &gi_constants);
  unload_repository ("enums", &gi_enums);
  unload_repository ("flags", &gi_flags);
  unload_repository ("functions", &gi_functions);
  unload_repository ("callbacks", &gi_callbacks);
  unload_repository ("structs", &gi_structs);
  unload_repository ("unions", &gi_unions);
  unload_repository ("objects", &gi_objects);
  unload_repository ("interfaces", &gi_interfaces);
  return SCM_UNSPECIFIED;
}

void
gir_init_func2(void)
{
  scm_c_define_gsubr ("gi-load-repository", 2, 0, 0,
		      scm_gi_load_repository);
  scm_c_define_gsubr ("gi-unload-repositories", 0, 0, 0,
		      scm_gi_unload_repositories);
  scm_c_define_gsubr ("gi-constant-value", 2, 0, 0, scm_gi_constant_value);
  scm_c_define_gsubr ("gi-flag-value", 3, 0, 0, scm_gi_flag_value);
  scm_c_define_gsubr ("gi-enum-value", 3, 0, 0, scm_gi_enum_value);
  scm_c_define_gsubr ("gi-struct-ref", 4, 0, 0, scm_gi_struct_ref);
  scm_c_define_gsubr ("gi-struct-set", 5, 0, 0, scm_gi_struct_set);
  scm_c_define_gsubr ("gi-function-invoke", 2, 0, 1, scm_gi_function_invoke);
}
