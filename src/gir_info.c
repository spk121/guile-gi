/* -*- Mode: C; c-basic-offset: 4 -*- */
#include <libguile.h>
#include <glib-object.h>
#include <girepository.h>

#define GIR_BASE_INFO_INFO_SLOT (0)
#define GIR_BASE_INFO_WEAKREFLIST_SLOT (1)
#define GIR_BASE_INFO_CACHE_SLOT (2)

SCM ScmGIBaseInfo_Type;
SCM ScmGIBaseInfo_Type_Store;


static void
_base_info_dealloc (SCM self)
{
  /* FIXME: Do something about a weakreflist */

  g_base_info_unref (scm_foreign_object_ref (self, GIR_BASE_INFO_INFO_SLOT));
  scm_foreign_object_set_x (self, GIR_BASE_INFO_INFO_SLOT, SCM_BOOL_F);

  /* FIXME: Do something about the callable cache */
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

  // FIXME: figure out to to mockup PyErr_NewException
  // scm_c_export ("Repository", NULL);
  
  scm_c_define_gsubr ("%base-info-get-type", 1, 0, 0, _wrap_g_base_info_get_type);
}

