#include <libguile.h>
#include <girepository.h>
#include "gi_giargument.h"
#include "gir_type.h"
#include "gir_func.h"

#define MAX_GERROR_MSG 100
static char gerror_msg[MAX_GERROR_MSG];

#define SCONST(NAME) SCM s_ ## NAME;
#define SCONSTX(NAME) s_ ## NAME = scm_permanent_object (scm_c_define (#NAME, scm_from_int (NAME)))


/****************************************************************/
/* BASE                                                         */

#define GET_NAME(TYPE)					\
  static SCM									\
  gir_ ## TYPE ## _get_name(SCM s_info)					\
  { \
    TYPE *info;								\
    const char *name; \
    scm_assert_foreign_object_type(s_ ## TYPE ## _type, s_info);	\
    info = scm_foreign_object_ref (s_info, 0);			\
    if (info == NULL) \
      return SCM_BOOL_F; \
    name = g_base_info_get_name((GIBaseInfo *)info);	\
    return scm_from_utf8_string (name);					\
  }
#define GET_NAMEX(NAME,TYPE)				\
  scm_c_define_gsubr(NAME, 1, 0, 0, gir_ ## TYPE ## _get_name);

GET_NAME(GIArgInfo);
GET_NAME(GICallbackInfo);
GET_NAME(GIConstantInfo);
GET_NAME(GIEnumInfo);
GET_NAME(GIFieldInfo);
GET_NAME(GIFunctionInfo);
GET_NAME(GIInterfaceInfo);
GET_NAME(GIObjectInfo);
GET_NAME(GIPropertyInfo);
GET_NAME(GISignalInfo);
GET_NAME(GIStructInfo);
GET_NAME(GITypeInfo);
GET_NAME(GIUnionInfo);
GET_NAME(GIVFuncInfo);

/****************************************************************/
/* ARG                                                          */
SCONST(GI_DIRECTION_IN);
SCONST(GI_DIRECTION_OUT);
SCONST(GI_DIRECTION_INOUT);

/****************************************************************/
/* CALLABLE                                                     */

#define MAY_RETURN_NULL(TYPE) \
  static SCM \
  gir_ ## TYPE ## _may_return_null (SCM s_info) \
  { \
    TYPE *info; \
    gboolean ret; \
    scm_assert_foreign_object_type(s_ ## TYPE ## _type, s_info);	\
    info = scm_foreign_object_ref (s_info, 0);			\
    ret = g_callable_info_may_return_null((GICallableInfo *)info);	\
    return scm_from_bool (ret);					\
  }

#define MAY_RETURN_NULLX(NAME,TYPE)				\
  scm_c_define_gsubr(NAME, 1, 0, 0, gir_ ## TYPE ## _may_return_null);

MAY_RETURN_NULL(GIFunctionInfo);
MAY_RETURN_NULL(GICallbackInfo);
MAY_RETURN_NULL(GIVFuncInfo);

#define GET_RETURN_TYPE(TYPE)				\
  static SCM						\
  gir_ ## TYPE ## _get_return_type(SCM s_info)								\
  {									\
  TYPE *info; \
  GITypeInfo *typeinfo; \
  GITransfer transfer; \
  GITypeTag typetag; \
  GIInfoType itype; \
  const gchar *ifacename; \
  SCM entry; \
    scm_assert_foreign_object_type(s_ ## TYPE ## _type, s_info);		\
    info = scm_foreign_object_ref (s_info, 0);			\
    typeinfo = g_callable_info_get_return_type ((GICallableInfo *) info); \
    transfer = g_callable_info_get_caller_owns ((GICallableInfo *) info); \
    typetag = g_type_info_get_tag (typeinfo);			\
    if (typetag == GI_TYPE_TAG_INTERFACE)				\
      {									\
	GIBaseInfo *iface = g_type_info_get_interface(typeinfo);	\
	itype = g_base_info_get_type (iface);				\
	ifacename = g_base_info_get_name (iface);			\
	g_base_info_unref(iface);					\
      }									\
    entry = scm_list_n(scm_cons(scm_from_utf8_symbol("type1"),		\
				scm_from_utf8_string(g_type_tag_to_string(typetag))), \
		       scm_cons(scm_from_utf8_symbol("type2"),		\
				typetag!=GI_TYPE_TAG_INTERFACE?scm_from_utf8_string (g_type_tag_to_string (typetag)):scm_from_utf8_string(g_info_type_to_string(itype))), \
		       scm_cons(scm_from_utf8_symbol("type3"),		\
				typetag!=GI_TYPE_TAG_INTERFACE?scm_from_utf8_string (g_type_tag_to_string (typetag)):scm_from_utf8_string(ifacename)), \
		       scm_cons(scm_from_utf8_symbol("pointer?"),	\
				scm_from_bool(g_type_info_is_pointer(typeinfo))), \
		       scm_cons(scm_from_utf8_symbol("transfer"),	\
				transfer==GI_TRANSFER_NOTHING?scm_from_utf8_symbol("nothing"): \
				(transfer==GI_TRANSFER_CONTAINER?scm_from_utf8_symbol("container"): \
				 scm_from_utf8_symbol("everyting"))),	\
		       SCM_UNDEFINED);					\
    g_base_info_unref(typeinfo);					\
    return entry;							\
  }

#define GET_RETURN_TYPEX(NAME,TYPE) \
  scm_c_define_gsubr(NAME, 1, 0, 0, gir_ ## TYPE ## _get_return_type);

GET_RETURN_TYPE(GIFunctionInfo);
GET_RETURN_TYPE(GICallbackInfo);
GET_RETURN_TYPE(GIVFuncInfo);

#define GET_ARGS(TYPE)				\
  static SCM						\
  gir_ ## TYPE ## _get_args (SCM s_info)				\
  {									\
    TYPE *info;								\
    gint n;								\
    SCM output;								\
    scm_assert_foreign_object_type(s_ ## TYPE ## _type, s_info);		\
    info = scm_foreign_object_ref (s_info, 0);			\
    n = g_callable_info_get_n_args((GICallableInfo *)info);	\
    if (n == 0) return SCM_EOL;						\
    output = SCM_EOL;						\
    for (int i = 0; i < n; i ++)					\
      {									\
	GIArgInfo *ai;							\
	ai = g_callable_info_get_arg((GICallableInfo *)info, i);	\
	const gchar *name = g_base_info_get_name((GIBaseInfo *)ai);	\
	gint closure = g_arg_info_get_closure(ai);			\
	gint destroy = g_arg_info_get_destroy(ai);			\
	GIDirection direction = g_arg_info_get_direction(ai);		\
	GITransfer transfer = g_arg_info_get_ownership_transfer (ai);	\
	GIScopeType scopetype = g_arg_info_get_scope (ai);		\
	GITypeInfo *typeinfo = g_arg_info_get_type (ai);			\
	gboolean nullp = g_arg_info_may_be_null (ai);			\
	gboolean preallocated = g_arg_info_is_caller_allocates (ai);	\
	gboolean optional = g_arg_info_is_optional (ai);		\
	gboolean rtn = g_arg_info_is_return_value (ai);			\
	gboolean skip = g_arg_info_is_skip (ai);			\
	GITypeTag typetag = g_type_info_get_tag (typeinfo); \
	GIInfoType itype; \
	const gchar *ifacename; \
	if (typetag == GI_TYPE_TAG_INTERFACE) \
	  { \
	    GIBaseInfo *iface = g_type_info_get_interface(typeinfo); \
	    itype = g_base_info_get_type (iface); \
	    ifacename = g_base_info_get_name (iface); \
	    g_base_info_unref(iface); \
	  } \
	SCM entry;							\
	entry = scm_list_n(scm_cons(scm_from_utf8_symbol("name"), scm_from_utf8_string(name)), \
			   scm_cons(scm_from_utf8_symbol("type1"), scm_from_utf8_string(g_type_tag_to_string(typetag))), \
			   scm_cons(scm_from_utf8_symbol("type2"), \
				    typetag!=GI_TYPE_TAG_INTERFACE?scm_from_utf8_string (g_type_tag_to_string (typetag)):scm_from_utf8_string(g_info_type_to_string(itype))), \
			   scm_cons(scm_from_utf8_symbol("type3"), \
				    typetag!=GI_TYPE_TAG_INTERFACE?scm_from_utf8_string (g_type_tag_to_string (typetag)):scm_from_utf8_string(ifacename)), \
			     scm_cons(scm_from_utf8_symbol("direction"),\
				      direction==GI_DIRECTION_IN?scm_from_utf8_symbol("in"):\
				      (direction==GI_DIRECTION_OUT?scm_from_utf8_symbol("out"):scm_from_utf8_symbol("inout"))), \
			   scm_cons(scm_from_utf8_symbol("pointer?"),	\
				    scm_from_bool(g_type_info_is_pointer(typeinfo))), \
			     scm_cons(scm_from_utf8_symbol("transfer"), \
				      transfer==GI_TRANSFER_NOTHING?scm_from_utf8_symbol("nothing"): \
				      (transfer==GI_TRANSFER_CONTAINER?scm_from_utf8_symbol("container"):\
				       scm_from_utf8_symbol("everyting"))), \
			     scm_cons(scm_from_utf8_symbol("scope"),\
				      scopetype==GI_SCOPE_TYPE_INVALID?scm_from_utf8_symbol("invalid"):\
				      (scopetype==GI_SCOPE_TYPE_CALL?scm_from_utf8_symbol("call"):\
				       (scopetype==GI_SCOPE_TYPE_ASYNC?scm_from_utf8_symbol("async"):\
					scm_from_utf8_symbol("notified")))), \
			   scm_cons(scm_from_utf8_symbol("skip?"), scm_from_bool(skip)), \
			   scm_cons(scm_from_utf8_symbol("return?"), scm_from_bool(rtn)), \
			   scm_cons(scm_from_utf8_symbol("optional?"), scm_from_bool(optional)), \
			   scm_cons(scm_from_utf8_symbol("caller-allocated?"), scm_from_bool(preallocated)), \
			   scm_cons(scm_from_utf8_symbol("maybe-null?"), scm_from_bool(nullp)), \
			   scm_cons(scm_from_utf8_symbol("closure"), scm_from_int(closure)), \
			   scm_cons(scm_from_utf8_symbol("destroy"), scm_from_int(destroy)), \
			   SCM_UNDEFINED);				\
	output = scm_append(scm_list_2(output,scm_list_1(entry)));	\
	g_base_info_unref(typeinfo);					\
      }									\
    return output;							\
  }

#define GET_ARGSX(NAME,TYPE)				\
  scm_c_define_gsubr(NAME, 1, 0, 0, gir_ ## TYPE ## _get_args);

GET_ARGS(GIFunctionInfo);
GET_ARGS(GICallbackInfo);
GET_ARGS(GIVFuncInfo);

/****************************************************************/
/* CONSTANT                                                     */

static SCM
gir_constant_info_get_type(SCM s_info)
{
  scm_assert_foreign_object_type(s_GIConstantInfo_type, s_info);

  GIConstantInfo *info = scm_foreign_object_ref (s_info, 0);
  GITypeInfo *typeinfo;
  typeinfo = g_constant_info_get_type (info);
  GITypeTag typetag;
  typetag = g_type_info_get_tag (typeinfo);
  return scm_from_utf8_string (g_type_tag_to_string (typetag));
}

static SCM
gir_constant_info_get_value(SCM s_info)
{
  scm_assert_foreign_object_type(s_GIConstantInfo_type, s_info);

  GIConstantInfo *info = scm_foreign_object_ref (s_info, 0);
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
      ret = scm_from_int(-1);
    }
  g_constant_info_free_value (info, &value);

  return ret;
}

/****************************************************************/
/* ENUM                                                         */

static SCM
gir_enum_info_get_values(SCM s_info)
{
  scm_assert_foreign_object_type(s_GIEnumInfo_type, s_info);

  GIEnumInfo *info = scm_foreign_object_ref (s_info, 0);
  gint n = g_enum_info_get_n_values (info);
  if (n == 0)
    return SCM_EOL;

  SCM output = SCM_EOL;
  for (int i = 0; i < n; i ++)
    {
      GIValueInfo *vi;
      SCM key, value;
      vi = g_enum_info_get_value (info, i);
      key = scm_from_utf8_symbol (g_base_info_get_name (vi));
      value = scm_from_int64 (g_value_info_get_value (vi));

      output = scm_append (scm_list_2 (output, scm_list_1 (scm_cons (key, value))));
      g_base_info_unref (vi);
    }
  return output;
}

/****************************************************************/
/* FUNCTION                                                     */

SCONST(GI_FUNCTION_IS_METHOD);
SCONST(GI_FUNCTION_IS_CONSTRUCTOR);
SCONST(GI_FUNCTION_IS_GETTER);
SCONST(GI_FUNCTION_IS_SETTER);
SCONST(GI_FUNCTION_WRAPS_VFUNC);
SCONST(GI_FUNCTION_THROWS);

static SCM
gir_function_info_get_flags(SCM s_info)
{
  //if (!SCM_IS_A_P(s_info, s_GIFunctionInfo_type))
  //   scm_wrong_type_arg_msg("%function-info-get-flags",SCM_ARG1, s_info, "GIFunctionInfo");
  
  GIFunctionInfo *info = scm_foreign_object_ref (s_info, 0);
  return scm_from_int (g_function_info_get_flags (info));
}

static SCM
gir_function_info_get_property(SCM s_info)
{
  scm_assert_foreign_object_type(s_GIFunctionInfo_type, s_info);

  GIFunctionInfo *info = scm_foreign_object_ref (s_info, 0);
  GIPropertyInfo *prop = g_function_info_get_property (info);
  if (prop == NULL)
    return SCM_BOOL_F;
  return scm_make_foreign_object_1(s_GIPropertyInfo_type, prop);
}

static SCM
gir_function_info_get_symbol(SCM s_info)
{
  scm_assert_foreign_object_type(s_GIFunctionInfo_type, s_info);

  GIFunctionInfo *info = scm_foreign_object_ref (s_info, 0);
  const gchar *sym = g_function_info_get_symbol (info);
  if (sym == NULL)
    return SCM_BOOL_F;
  return scm_from_utf8_string (sym);
}

static SCM
gir_function_info_is_deprecated(SCM s_info)
{
  scm_assert_foreign_object_type(s_GIFunctionInfo_type, s_info);
  GIFunctionInfo *info = scm_foreign_object_ref (s_info, 0);
  gboolean ret = g_base_info_is_deprecated((GIBaseInfo *)info);
  return scm_from_bool(ret);
}

static gboolean
check_type(SCM obj, GIArgInfo *ai, char **errstr)
{
  GITypeInfo *ti = g_arg_info_get_type (ai);
  GITypeTag tt = g_type_info_get_tag (ti);
  gboolean is_ptr = g_type_info_is_pointer (ti);

  g_critical("check_type: IMPLEMENT ME");
  g_base_info_unref (ti);
  return TRUE;
}

static SCM
gir_function_invoke(SCM s_info, SCM s_args)
{
    scm_assert_foreign_object_type(s_GIFunctionInfo_type, s_info);
    GIFunctionInfo *info = scm_foreign_object_ref(s_info, 0);

    int s_n_args = scm_to_int(scm_length(s_args));

    /* Count the number of required input arguments, and store
   * the arg info in a newly allocate array. */
    int n_args = g_callable_info_get_n_args((GICallableInfo *)info);
    int n_input_args = 0;
    int n_output_args = 0;
    GIArgInfo **ai = g_new0(GIArgInfo *, 1);
    for (int i = 0; i < n_args; i++)
    {
        ai[i] = g_callable_info_get_arg((GICallableInfo *)info, i);
        GIDirection dir = g_arg_info_get_direction(ai[i]);
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

    /* If we don't have the right number of input arguments, free
   * memory and error. */
    if (s_n_args != n_input_args)
    {
        for (int i = 0; i < n_args; i++)
        {
            g_base_info_unref(ai[i]);
        }
        g_free(ai);
        scm_misc_error("function-invoke",
                       "wrong number of input arguments, expected ~S",
                       scm_list_1(scm_from_int(n_input_args)));
        return SCM_BOOL_F;
    }

    /* Typecheck the SCM input arguments to ensure that they can be
   * converted successfully. */
    gboolean type_ok = TRUE;
    char *errstr = NULL;
    for (int i = 0; i < n_input_args; i++)
    {
        if (!gi_giargument_check_scm_type(scm_list_ref(s_args, scm_from_int(i)), ai[i], &errstr))
            type_ok = FALSE;
    }

    if (!type_ok)
    {
        for (int i = 0; i < n_args; i++)
        {
            g_base_info_unref(ai[i]);
        }
        g_free(ai);
        SCM s_errstr = scm_from_utf8_string(errstr);
        scm_misc_error("function-invoke",
                       "~S",
                       scm_list_1(s_errstr));
        return SCM_BOOL_F;
    }

    /* Convert the input arguments into a GIArgument list. Since we've
   * typechecked completely, this shouldn't throw. */
    GIArgument *in_args = NULL;
    gboolean *in_args_free = NULL;
    if (n_input_args > 0)
    {
        in_args = g_new0(GIArgument, n_input_args);
        in_args_free = g_new0(gboolean, n_input_args);
    }
    for (int i = 0; i < n_input_args; i++)
    {
        in_args[i] = gi_argument_from_object("function-invoke",
                                             scm_list_ref(s_args, scm_from_int(i)), g_arg_info_get_type(ai[i]),
                                             g_arg_info_get_ownership_transfer(ai[i]));
    }

    /* Allocate a GIArgument list of the output and return values. */
    GIArgument *out_args = NULL;
    if (n_output_args > 0)
        out_args = g_new0(GIArgument, n_output_args);
    GIArgument return_arg;

    /* Make the call. */
    GError *err;
    gboolean ret = g_function_info_invoke(info, in_args, n_input_args, out_args, n_output_args, &return_arg, &err);

    /* Free any allocated input */
    for (int i = 0; i < n_input_args; i++)
    {
        free(in_args_free);
    }
    /* If there is a GError, write an error, free, and exit. */
    if (!ret)
    {
    }

    /* We've actually made a call.  Hooray! Convert the output
   * arguments and return values into Scheme objects.  Free the
   * C objects if necessary.  Return the output either as
   * a plain list or as a values list. */
    GITypeInfo *return_typeinfo = g_callable_info_get_return_type(info);
    SCM s_return = gi_giargument_to_object(&return_arg,
                                           return_typeinfo,
                                           g_callable_info_get_caller_owns(info));
    g_base_info_unref(return_typeinfo);
    SCM output = scm_list_1(s_return);
    int j = 0;
    for (int i = 0; i < n_args; i++)
    {
        ai[i] = g_callable_info_get_arg((GICallableInfo *)info, i);
        GIDirection dir = g_arg_info_get_direction(ai[i]);
        if (dir == GI_DIRECTION_OUT || dir == GI_DIRECTION_INOUT)
        {
            GITypeInfo *arg_typeinfo = g_arg_info_get_type (ai[i]);
            SCM entry = gi_giargument_to_object(&out_args[j++],
                                                arg_typeinfo,
                                                g_arg_info_get_ownership_transfer(ai[i]));
            g_base_info_unref (arg_typeinfo);
            output = scm_append(scm_list_2(output, scm_list_1(entry)));
        }
    }

    return output;
}

/****************************************************************/
/* REGISTERED TYPE */

#define GET_G_TYPE(TYPE) \
  static SCM \
  gir_ ## TYPE ## _get_g_type (SCM s_info) \
  { \
    scm_assert_foreign_object_type(s_ ## TYPE ## _type, s_info);	\
    TYPE *info = scm_foreign_object_ref (s_info, 0);			\
    size_t ret = g_registered_type_info_get_g_type((GIRegisteredTypeInfo *)info);	\
    return scm_from_size_t (ret);					\
  }

#define GET_G_TYPEX(NAME,TYPE)				\
  scm_c_define_gsubr(NAME, 1, 0, 0, gir_ ## TYPE ## _get_g_type);

#define GET_TYPE_NAME(TYPE) \
  static SCM \
  gir_ ## TYPE ## _get_type_name (SCM s_info) \
  { \
    scm_assert_foreign_object_type(s_ ## TYPE ## _type, s_info);	\
    TYPE *info = scm_foreign_object_ref (s_info, 0);			\
    const gchar *ret = g_registered_type_info_get_type_name((GIRegisteredTypeInfo *)info);	\
    if (!ret) return SCM_BOOL_F; \
    return scm_from_utf8_string (ret);					\
  }

#define GET_TYPE_NAMEX(NAME,TYPE)				\
  scm_c_define_gsubr(NAME, 1, 0, 0, gir_ ## TYPE ## _get_type_name);

GET_G_TYPE(GIEnumInfo);
GET_G_TYPE(GIInterfaceInfo);
GET_G_TYPE(GIObjectInfo);
GET_G_TYPE(GIStructInfo);
GET_G_TYPE(GIUnionInfo);

GET_TYPE_NAME(GIEnumInfo);
GET_TYPE_NAME(GIInterfaceInfo);
GET_TYPE_NAME(GIObjectInfo);
GET_TYPE_NAME(GIStructInfo);
GET_TYPE_NAME(GIUnionInfo);

#define GET_FIELDS(TYPE, LC_TYPE)		\
  static SCM						       \
  gir_ ## LC_TYPE ## _get_fields (SCM s_info)		       \
  {							       \
    scm_assert_foreign_object_type(s_ ## TYPE ## _type, s_info); \
    TYPE *info = scm_foreign_object_ref (s_info, 0);		 \
    gint n_fields = g_ ## LC_TYPE ## _get_n_fields (info);       \
    if (n_fields == 0)						 \
      return SCM_EOL;						 \
    SCM fields = SCM_EOL;					 \
    for (gint i = 0; i < n_fields; i ++) {			 \
    GIFieldInfo *fi = g_ ## LC_TYPE ## _get_field (info, i);	 \
    const gchar *name = g_base_info_get_name((GIBaseInfo *)fi);	 \
    GIInfoType itype = g_base_info_get_type((GIBaseInfo *)fi);	 \
    GIFieldInfoFlags flags = g_field_info_get_flags(fi);	 \
    gint offset = g_field_info_get_offset(fi);			 \
    gint size = g_field_info_get_size(fi);			 \
    GITypeInfo *typeinfo = g_field_info_get_type(fi);		 \
    GITypeTag typetag = g_type_info_get_tag (typeinfo);		 \
    const gchar *ifacename;						\
    if (typetag == GI_TYPE_TAG_INTERFACE)	{			\
      GIBaseInfo *iface = g_type_info_get_interface(typeinfo);		\
      itype = g_base_info_get_type (iface);				\
      ifacename = g_base_info_get_name (iface);				\
      g_base_info_unref(iface);						\
    }									\
    SCM entry;								\
    entry = scm_list_n(scm_cons(scm_from_utf8_symbol("name"), scm_from_utf8_string(name)), \
		       scm_cons(scm_from_utf8_symbol("type1"), scm_from_utf8_string(g_type_tag_to_string(typetag))), \
		       scm_cons(scm_from_utf8_symbol("type2"),		\
				typetag!=GI_TYPE_TAG_INTERFACE?scm_from_utf8_string (g_type_tag_to_string (typetag)):scm_from_utf8_string(g_info_type_to_string(itype))), \
		       scm_cons(scm_from_utf8_symbol("type3"),		\
				typetag!=GI_TYPE_TAG_INTERFACE?scm_from_utf8_string (g_type_tag_to_string (typetag)):scm_from_utf8_string(ifacename)), \
		       scm_cons(scm_from_utf8_symbol("pointer?"),	\
				scm_from_bool(g_type_info_is_pointer(typeinfo))), \
		       scm_cons(scm_from_utf8_symbol("offset"),		\
				scm_from_int(offset)),			\
		       scm_cons(scm_from_utf8_symbol("size"),		\
				scm_from_int(size)),			\
		       scm_cons(scm_from_utf8_symbol("readable"),	\
				scm_from_bool(flags & GI_FIELD_IS_READABLE)), \
		       scm_cons(scm_from_utf8_symbol("writable"),	\
				scm_from_bool(flags & GI_FIELD_IS_WRITABLE)), \
		       SCM_UNDEFINED);					\
                    fields = scm_append(scm_list_2(fields, scm_list_1(entry)));		\
                    g_base_info_unref(typeinfo);				\
		    }							\
return fields;								\
}

#define GET_METHODS(TYPE, LC_TYPE) \
static SCM \
gir_ ## LC_TYPE ## _get_methods (SCM s_info) \
{ \
  scm_assert_foreign_object_type(s_ ## TYPE ## _type, s_info); \
  TYPE *info = scm_foreign_object_ref (s_info, 0); \
  gint n_methods = g_ ## LC_TYPE ## _get_n_methods (info); \
  if (n_methods == 0) \
    return SCM_EOL; \
  SCM methods = SCM_EOL; \
  for (gint i = 0; i < n_methods; i ++) { \
    SCM entry = scm_make_foreign_object_1(s_GIFunctionInfo_type, g_ ## LC_TYPE ## _get_method(info, i)); \
    methods = scm_append(scm_list_2(methods, scm_list_1(entry))); \
  } \
  return methods; \
}

GET_FIELDS(GIObjectInfo, object_info);
GET_FIELDS(GIStructInfo, struct_info);
GET_METHODS(GIObjectInfo, object_info);
GET_METHODS(GIStructInfo, struct_info);

/****************************************************************/
/* STRUCT                                                       */

static SCM
gir_struct_info_get_alignment(SCM s_info)
{
  scm_assert_foreign_object_type(s_GIStructInfo_type, s_info);
  GIStructInfo *info = scm_foreign_object_ref (s_info, 0);
  return scm_from_size_t (g_struct_info_get_alignment (info));
}

static SCM
gir_struct_info_get_size(SCM s_info)
{
  scm_assert_foreign_object_type(s_GIStructInfo_type, s_info);
  GIStructInfo *info = scm_foreign_object_ref (s_info, 0);
  return scm_from_size_t (g_struct_info_get_size (info));
}

static SCM
gir_struct_info_is_gtype_struct(SCM s_info)
{
  scm_assert_foreign_object_type(s_GIStructInfo_type, s_info);
  GIStructInfo *info = scm_foreign_object_ref (s_info, 0);
  return scm_from_bool (g_struct_info_is_gtype_struct (info));
}

static SCM
gir_struct_info_is_foreign(SCM s_info)
{
  scm_assert_foreign_object_type(s_GIStructInfo_type, s_info);
  GIStructInfo *info = scm_foreign_object_ref (s_info, 0);
  return scm_from_bool (g_struct_info_is_foreign (info));
}

#if 0
static SCM
gir_struct_info_get_fields (SCM s_info)
{
  scm_assert_foreign_object_type(s_GIStructInfo_type, s_info);
  GIStructInfo *info = scm_foreign_object_ref (s_info, 0);
  gint n_fields = g_struct_info_get_n_fields (info);
  if (n_fields == 0)
    return SCM_EOL;
  SCM fields = SCM_EOL;
  for (gint i = 0; i < n_fields; i ++) {
    GIFieldInfo *fi = g_struct_info_get_field (info, i);
    const gchar *name = g_base_info_get_name((GIBaseInfo *)fi);
    GIInfoType itype = g_base_info_get_type((GIBaseInfo *)fi);
    GIFieldInfoFlags flags = g_field_info_get_flags(fi);
    gint offset = g_field_info_get_offset(fi);
    gint size = g_field_info_get_size(fi);
    GITypeInfo *typeinfo = g_field_info_get_type(fi);
    GITypeTag typetag = g_type_info_get_tag (typeinfo);
    const gchar *ifacename;
    if (typetag == GI_TYPE_TAG_INTERFACE)
      {
	GIBaseInfo *iface = g_type_info_get_interface(typeinfo);
	itype = g_base_info_get_type (iface);
	ifacename = g_base_info_get_name (iface);
	g_base_info_unref(iface);
      }
    SCM entry;
    entry = scm_list_n(scm_cons(scm_from_utf8_symbol("name"), scm_from_utf8_string(name)),
		       scm_cons(scm_from_utf8_symbol("type1"), scm_from_utf8_string(g_type_tag_to_string(typetag))),
		       scm_cons(scm_from_utf8_symbol("type2"),
				typetag!=GI_TYPE_TAG_INTERFACE?scm_from_utf8_string (g_type_tag_to_string (typetag)):scm_from_utf8_string(g_info_type_to_string(itype))),
		       scm_cons(scm_from_utf8_symbol("type3"),
				typetag!=GI_TYPE_TAG_INTERFACE?scm_from_utf8_string (g_type_tag_to_string (typetag)):scm_from_utf8_string(ifacename)),
		       scm_cons(scm_from_utf8_symbol("pointer?"),
				scm_from_bool(g_type_info_is_pointer(typeinfo))),
		       scm_cons(scm_from_utf8_symbol("offset"),
				scm_from_int(offset)),
		       scm_cons(scm_from_utf8_symbol("size"),
				scm_from_int(size)),
		       scm_cons(scm_from_utf8_symbol("readable"),
				scm_from_bool(flags & GI_FIELD_IS_READABLE)),
		       scm_cons(scm_from_utf8_symbol("writable"),
				scm_from_bool(flags & GI_FIELD_IS_WRITABLE)),
		       SCM_UNDEFINED);
    fields = scm_append(scm_list_2(fields, scm_list_1(entry)));
    g_base_info_unref(typeinfo);
  }
  return fields;
}
#endif

#if 0
static SCM
gir_struct_info_get_methods (SCM s_info)
{
  scm_assert_foreign_object_type(s_GIStructInfo_type, s_info);
  GIStructInfo *info = scm_foreign_object_ref (s_info, 0);
  gint n_methods = g_struct_info_get_n_methods (info);
  if (n_methods == 0)
    return SCM_EOL;
  SCM methods = SCM_EOL;
  for (gint i = 0; i < n_methods; i ++) {
    SCM entry = scm_make_foreign_object_1(s_GIFunctionInfo_type, g_struct_info_get_method(info, i));
    methods = scm_append(scm_list_2(methods, scm_list_1(entry)));
  }
  return methods;
}
#endif

/****************************************************************/
/* OBJECT                                                       */

#define OBJECT_BOOL_FUNC(NAME)			\
  static SCM						\
  gir_object_info_ ## NAME (SCM s_info)		\
  {							       \
    scm_assert_foreign_object_type(s_GIObjectInfo_type, s_info);	\
    GIObjectInfo *info = scm_foreign_object_ref (s_info, 0);		\
    return scm_from_bool (g_object_info_ ## NAME (info));		\
  }

OBJECT_BOOL_FUNC(get_abstract);
OBJECT_BOOL_FUNC(get_fundamental);

static SCM
gir_object_info_get_parent(SCM s_info)
{
  scm_assert_foreign_object_type(s_GIObjectInfo_type, s_info);
  GIObjectInfo *info = scm_foreign_object_ref (s_info, 0);
  GIObjectInfo *parent = g_object_info_get_parent (info);

  // FIXME: parent needs to g_base_info_unref on GC
  return scm_make_foreign_object_1(s_GIObjectInfo_type, parent);
}


#define MAKE(TYPE, BASE) \
  scm_make_foreign_object_1(s_ ## TYPE ## _type, BASE);


static SCM
s_xg_irepository_get_n_infos(SCM s_namespace_)
{
  char *namespace_ = scm_to_utf8_string (s_namespace_);
  int n = g_irepository_get_n_infos (NULL, namespace_);
  free (namespace_);
  return scm_from_int (n);
}

static SCM
s_xg_irepository_get_infos(SCM s_namespace_)
{
  char *namespace_ = scm_to_utf8_string (s_namespace_);
  int n = g_irepository_get_n_infos (NULL, namespace_);
  int i;

  if (n == 0)
    return SCM_EOL;

  SCM output = SCM_EOL;
  for (i = 0; i < n; i ++)
    {
      GIBaseInfo *info;
      GIInfoType type;
      SCM entry;
      info = g_irepository_get_info (NULL, namespace_, i);
      type = g_base_info_get_type (info);
      switch (type)
	{
	case GI_INFO_TYPE_CALLBACK:
	  entry = MAKE(GICallbackInfo, info);
	  break;
	case GI_INFO_TYPE_FUNCTION:
	  entry = MAKE(GIFunctionInfo, info);
	  break;
	case GI_INFO_TYPE_STRUCT:
	  entry = MAKE(GIStructInfo, info);
	  break;
	/* case GI_INFO_TYPE_BOXED: */
	/*   entry = MAKE(GIBoxedInfo, info); */
	/*   break; */
	case GI_INFO_TYPE_ENUM:
	case GI_INFO_TYPE_FLAGS:
	  entry = MAKE(GIEnumInfo, info);
	  break;
	case GI_INFO_TYPE_OBJECT:
	  entry = MAKE(GIObjectInfo, info);
	  break;
	case GI_INFO_TYPE_INTERFACE:
	  entry = MAKE(GIInterfaceInfo, info);
	  break;
	case GI_INFO_TYPE_CONSTANT:
	  entry = MAKE(GIConstantInfo, info);
	  break;
	case GI_INFO_TYPE_UNION:
	  entry = MAKE(GIUnionInfo, info);
	  break;
	case GI_INFO_TYPE_VALUE:
	  entry = MAKE(GIValueInfo, info);
	  break;
	case GI_INFO_TYPE_SIGNAL:
	  entry = MAKE(GISignalInfo, info);
	  break;
	case GI_INFO_TYPE_VFUNC:
	  entry = MAKE(GIVFuncInfo, info);
	  break;
	case GI_INFO_TYPE_PROPERTY:
	  entry = MAKE(GIPropertyInfo, info);
	  break;
	case GI_INFO_TYPE_FIELD:
	  entry = MAKE(GIFieldInfo, info);
	  break;
	case GI_INFO_TYPE_ARG:
	  entry = MAKE(GIArgInfo, info);
	  break;
	case GI_INFO_TYPE_TYPE:
	  entry = MAKE(GITypeInfo, info);
	  break;
	case GI_INFO_TYPE_INVALID:
	case GI_INFO_TYPE_INVALID_0:
	default:
	  entry = SCM_UNSPECIFIED;
	  break;
	}
      if (! SCM_UNBNDP (entry))
	output = scm_append(scm_list_2 (output, scm_list_1 (entry)));
    }
  return output;
}

static SCM
gir_irepository_require (SCM s_namespace_, SCM s_version)
{
  char *namespace_ = scm_to_utf8_string (s_namespace_);
  char *version = scm_to_utf8_string (s_version);
  GITypelib *tl;
  GError *error = NULL;

  tl = g_irepository_require (NULL, namespace_, version, 0, &error);
  free (version);
  free (namespace_);
  if (tl != NULL)
    return scm_make_foreign_object_1 (s_GITypelib_type, tl);

  strncpy (gerror_msg, error->message, MAX_GERROR_MSG - 1);
  g_error_free (error);
  scm_misc_error ("%irepository-require", gerror_msg, SCM_EOL);

  return SCM_UNSPECIFIED;
}


void
gir_init_funcs(void)
{
  /* BASE */
  GET_NAMEX("arg-info-get-name", GIArgInfo);
  GET_NAMEX("callback-info-get-name", GICallbackInfo);
  GET_NAMEX("constant-info-get-name", GIConstantInfo);
  GET_NAMEX("enum-info-get-name", GIEnumInfo);
  GET_NAMEX("field-info-get-name", GIFieldInfo);
  GET_NAMEX("function-info-get-name", GIFunctionInfo);
  GET_NAMEX("interface-info-get-name", GIInterfaceInfo);
  GET_NAMEX("object-info-get-name", GIObjectInfo);
  GET_NAMEX("property-info-get-name", GIPropertyInfo);
  GET_NAMEX("signal-info-get-name", GISignalInfo);
  GET_NAMEX("struct-info-get-name", GIStructInfo);
  GET_NAMEX("type-info-get-name", GITypeInfo);
  GET_NAMEX("union-info-get-name", GIUnionInfo);
  GET_NAMEX("vfunc-info-get-name", GIVFuncInfo);

  /* ARG */
  SCONSTX(GI_DIRECTION_IN);
  SCONSTX(GI_DIRECTION_OUT);
  SCONSTX(GI_DIRECTION_INOUT);

  /* CALLABLE */
  MAY_RETURN_NULLX("function-info-may-return-null?", GIFunctionInfo);
  MAY_RETURN_NULLX("callback-info-may-return-null?", GICallbackInfo);
  MAY_RETURN_NULLX("vfunc-info-may-return-null?", GIVFuncInfo);
  GET_RETURN_TYPEX("function-info-get-return-type", GIFunctionInfo);
  GET_RETURN_TYPEX("callback-info-get-return-type", GICallbackInfo);
  GET_RETURN_TYPEX("vfunc-info-get-return-type", GIVFuncInfo);
  GET_ARGSX("function-info-get-args", GIFunctionInfo);
  GET_ARGSX("callback-info-get-args", GICallbackInfo);
  GET_ARGSX("vfunc-inf-get-args", GIVFuncInfo);

  /* CONST */
  scm_c_define_gsubr("constant-info-get-type", 1, 0, 0, gir_constant_info_get_type);
  scm_c_define_gsubr("constant-info-get-value", 1, 0, 0, gir_constant_info_get_value);

  /* ENUM */
  scm_c_define_gsubr("enum-info-get-values", 1, 0, 0, gir_enum_info_get_values);

  /* FUNCTION */
  SCONSTX(GI_FUNCTION_IS_METHOD);
  SCONSTX(GI_FUNCTION_IS_CONSTRUCTOR);
  SCONSTX(GI_FUNCTION_IS_GETTER);
  SCONSTX(GI_FUNCTION_IS_SETTER);
  SCONSTX(GI_FUNCTION_WRAPS_VFUNC);
  SCONSTX(GI_FUNCTION_THROWS);


  scm_c_define_gsubr("function-info-is-deprecated?", 1, 0, 0,
		     gir_function_info_is_deprecated);
  scm_c_define_gsubr("%function-info-get-flags", 1, 0, 0,
		     gir_function_info_get_flags);
  scm_c_define_gsubr("function-info-get-property", 1, 0, 0,
		     gir_function_info_get_property);
  scm_c_define_gsubr("function-info-get-symbol", 1, 0, 0,
		     gir_function_info_get_symbol);
  scm_c_define_gsubr("%irepository-get-n-infos", 1, 0, 0,
		     s_xg_irepository_get_n_infos);
  scm_c_define_gsubr("%irepository-get-infos", 1, 0, 0,
		     s_xg_irepository_get_infos);
  scm_c_define_gsubr("irepository-require", 2, 0, 0,
		     gir_irepository_require);
  scm_c_define_gsubr("function-invoke", 1, 0, 1,
		     gir_function_invoke);

  /* REGISTERED TYPE */
  GET_G_TYPEX("enum-info-get-g-type", GIEnumInfo);
  GET_G_TYPEX("interface-info-get-g-type", GIInterfaceInfo);
  GET_G_TYPEX("object-info-get-g-type", GIObjectInfo);
  GET_G_TYPEX("struct-info-get-g-type", GIStructInfo);
  GET_G_TYPEX("union-info-get-g-type", GIUnionInfo);
  GET_TYPE_NAMEX("enum-info-get-type-name", GIEnumInfo);
  GET_TYPE_NAMEX("interface-info-get-type-name", GIInterfaceInfo);
  GET_TYPE_NAMEX("object-info-get-type-name", GIObjectInfo);
  GET_TYPE_NAMEX("struct-info-get-type-name", GIStructInfo);
  GET_TYPE_NAMEX("union-info-get-type-name", GIUnionInfo);

  /* STRUCT */
  scm_c_define_gsubr("struct-info-get-alignment", 1, 0, 0,
		     gir_struct_info_get_alignment);
  scm_c_define_gsubr("struct-info-get-size", 1, 0, 0,
		     gir_struct_info_get_size);
  scm_c_define_gsubr("struct-info-is-gtype-struct?", 1, 0, 0,
		     gir_struct_info_is_gtype_struct);
  scm_c_define_gsubr("struct-info-get-fields", 1, 0, 0,
		     gir_struct_info_get_fields);
  scm_c_define_gsubr("struct-info-get-methods", 1, 0, 0,
		     gir_struct_info_get_methods);
  scm_c_define_gsubr("struct-info-is-foreign?", 1, 0, 0,
		     gir_struct_info_is_foreign);

  /* OBJECT */
  scm_c_define_gsubr("object-info-get-fields", 1, 0, 0,
		     gir_object_info_get_fields);
  scm_c_define_gsubr("object-info-get-methods", 1, 0, 0,
		     gir_object_info_get_methods);
  
}

#if 0
     SCM
     make_image (SCM name, SCM s_width, SCM s_height)
     {
       struct image *image;
       int width = scm_to_int (s_width);
       int height = scm_to_int (s_height);

       /* Allocate the `struct image'.  Because we
          use scm_gc_malloc, this memory block will
          be automatically reclaimed when it becomes
          inaccessible, and its members will be traced
          by the garbage collector.  */
       image = (struct image *)
         scm_gc_malloc (sizeof (struct image), "image");

       image->width = width;
       image->height = height;

       /* Allocating the pixels with
          scm_gc_malloc_pointerless means that the
          pixels data is collectable by GC, but
          that GC shouldn't spend time tracing its
          contents for nested pointers because there
          aren't any.  */
       image->pixels =
         scm_gc_malloc_pointerless (width * height, "image pixels");

       image->name = name;
       image->update_func = SCM_BOOL_F;

       /* Now wrap the struct image* in a new foreign
          object, and return that object.  */
       return scm_make_foreign_object_1 (image_type, image);
     }
#endif
