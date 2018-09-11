/* -*- Mode: C; c-basic-offset: 4 -*- */
#include <libguile.h>
#include <glib-object.h>
#include <girepository.h>

/* A GIBaseInfo *, or subclass of it */
#define GIR_BASE_INFO_INFO_SLOT (0)
/* A Guile weak-vector */
#define GIR_BASE_INFO_INST_WEAKREFLIST_SLOT (1)
/* A ScmGICallableCache in an SCM */
#define GIR_BASE_INFO_CACHE_SLOT (2)

/* ?? */
#define GIR_CALLABLE_INFO_UNBOUND_INFO_SLOT (3)
/* An SCM, it holds a bound argument for instance, class, and vfunc
   methods. */
#define GIR_CALLABLE_INFO_BOUND_ARG_SLOT (4)

SCM scm_class_name_proc;

SCM ScmGIBaseInfo_Type;
SCM ScmGIBaseInfo_Type_Store;


/* BaseInfo */

/* We need to be careful about calling g_base_info_get_name because
 * calling it with a GI_INFO_TYPE_TYPE will crash.
 * See: https://bugzilla.gnome.org/show_bug.cgi?id=709456
 */
static const char *
_safe_base_info_get_name (GIBaseInfo *info)
{
    if (g_base_info_get_type (info) == GI_INFO_TYPE_TYPE) {
        return "type_type_instance";
    } else {
        return g_base_info_get_name (info);
    }
}

static void
_base_info_dealloc (SCM self)
{
    SCM inst_weakreflist;

    /* MLG: is it sufficient to just release a reference to the weak vector?. */
    scm_foreign_object_set_x (self, GIR_BASE_INFO_INST_WEAKREFLIST_SLOT, SCM_BOOL_F);
    

    g_base_info_unref (scm_foreign_object_ref (self, GIR_BASE_INFO_INFO_SLOT));
    scm_foreign_object_set_x (self, GIR_BASE_INFO_INFO_SLOT, SCM_BOOL_F);

    /* FIXME: Do something about the callable cache */
#if 0
    SCM cache = scm_foreign_object_ref (self, GIR_BASE_INFO_CACHE_SLOT);
    if (scm_is_true (cache))
	gir_callable_cache_free (cache);
    scm_foreign_object_set_x (self, GIR_BASE_INFO_CACHE_SLOT, SCM_BOOL_F);
#endif
}

SCM
_base_info_repr (SCM self)
{
    const char *base_info_name
	= _safe_base_info_get_name (scm_foreign_object_ref (self, GIR_BASE_INFO_INFO_SLOT));
    char *scm_type_name
	= scm_to_utf8_string (scm_call_1 (scm_class_name_proc, self));

    char *joined_names = g_strdup_printf ("%s(%s)", scm_type_name, base_info_name);
    SCM ret = scm_from_utf8_string (joined_names);
    free (joined_names);
    free (scm_type_name);
    return ret;
}

gboolean
_gir_is_r5rs_keyword (const gchar *name)
{
    /* This should be a list of Scheme syntax keywords that might be
       confused with something in an introspected library. */
    static const gchar* keywords[] = {
				      "and",
				      "begin",
				      "car",
				      "case",
				      "cdr",
				      "cond",
				      "define",
				      "delay",
				      "do",
				      "else",
				      "if",
				      "lambda",
				      "let",
				      "letrec",
				      "list",
				      "or",
				      "quasiquote",
				      "quote",
				      "unquote",
				      "unquote-splicing",
				      "vector",
				      NULL};

    const gchar **i;

    for (i = keywords; *i != NULL; ++i) {
        if (strcmp (name, *i) == 0) {
            return TRUE;
        }
    }

    return FALSE;
}

static SCM
_wrap_g_base_info_get_type (SCM s_baseinfo)
{
  return scm_from_uint (g_base_info_get_type (scm_foreign_object_ref (s_baseinfo, GIR_BASE_INFO_INFO_SLOT)));
}

SCM
_gir_info_new (GIBaseInfo *info)
{
    g_critical("_gir_info_new: this procedure is just a stub");
    return SCM_ELISP_NIL;
}

void
gir_info_register_types(void)
{
  ScmGIBaseInfo_Type =
    scm_make_foreign_object_type (scm_from_locale_symbol ("gi.BaseInfo"),
				  scm_list_3 (scm_from_locale_symbol ("info"),
					      scm_from_locale_symbol ("weakreflist"),
					      scm_from_locale_symbol ("cache")),
				  _base_info_dealloc);
  ScmGIBaseInfo_Type_Store
    = scm_permanent_object (scm_c_define ("gi.BaseInfo",
					  ScmGIBaseInfo_Type));

  scm_class_name_proc = scm_c_public_ref ("oop goops", "class-name");
  scm_c_define_gsubr ("%base-info-get-type", 1, 0, 0, _wrap_g_base_info_get_type);
  // scm_c_define_gsubr ("%base-info-get-name", 1, 0, 0, _wrap_g_base_info_get_name);
  // scm_c_define_gsubr ("%base-info-get-unescaped", 1, 0, 0, _wrap_g_base_info_get_name_unescaped);
  
}

