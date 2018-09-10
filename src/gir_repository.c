/* -*- Mode: C; c-basic-offset: 4 -*- */
#include <libguile.h>
#include <glib-object.h>
#include <girepository.h>
#include "gir_info.h"

#define SCM_LIST_APPEND(collection, entry) \
    collection = scm_append (scm_list_2 ((collection), scm_list_1 (entry)))

/* A foreign object type that holds a GIRepository pointer */;
SCM ScmGIRepository_Type;
SCM ScmGIRepository_Type_Store;

static SCM
_wrap_g_irepository_enumerate_versions (SCM             s_self,
                                        SCM             s_namespace_)
{
    char *namespace_;
    GList *versions, *item;
    SCM ret;

    namespace_ = scm_to_utf8_string (s_namespace_);
    versions = g_irepository_enumerate_versions (scm_foreign_object_ref (s_self, 0), namespace_);
    free (namespace_);
    
    ret = SCM_EOL;
    for (item = versions; item; item = item->next) {
        char *version = item->data;
	SCM scm_version = scm_from_utf8_string (version);
	SCM_LIST_APPEND (ret, scm_version);
        g_free (version);
    }
    g_list_free(versions);

    return ret;
}

static SCM
_wrap_g_irepository_get_default ()
{
    SCM self = scm_make_foreign_object_0 (ScmGIRepository_Type);
    scm_foreign_object_set_x (self, 0, g_irepository_get_default());
    return self;
}

static SCM
_wrap_g_irepository_require (SCM             s_self,
			     SCM             s_namespace_,
			     SCM             s_version,
			     SCM             s_lazy)
{
    char *namespace_;
    char *version = NULL;
    GIRepositoryLoadFlags flags = 0;
    GError *error;

    namespace_ = scm_to_utf8_string (s_namespace_);
    if (!SCM_UNBNDP (s_version))
	version = scm_to_utf8_string (s_version);
    if (!SCM_UNBNDP (s_lazy) && scm_is_true (s_lazy)) {
	flags |= G_IREPOSITORY_LOAD_FLAG_LAZY;
    }

    error = NULL;
    g_irepository_require (scm_foreign_object_ref (s_self, 0),
			   namespace_, version, flags, &error);
    free (version);
    free (namespace_);
    if (error != NULL) {
	char msg[256];
	strncpy(msg, error->message, 256);
	g_error_free (error);
	scm_misc_error ("%irepository-require", msg, SCM_EOL);
    }

    return SCM_UNSPECIFIED;
}

static SCM
_wrap_g_irepository_is_registered (SCM           s_self,
                                   SCM           s_namespace_,
                                   SCM           s_version)
{
    char *namespace_;
    char *version = NULL;
    gboolean ret;

    namespace_ = scm_to_utf8_string (s_namespace_);
    if (!SCM_UNBNDP (s_version))
	version = scm_to_utf8_string (s_version);

    ret = g_irepository_is_registered (scm_foreign_object_ref (s_self, 0),
				       namespace_, version);
    free (version);
    free (namespace_);
    return scm_from_bool (ret);
}

static SCM
_wrap_g_irepository_find_by_name (SCM             s_self,
                                  SCM             s_namespace_,
                                  SCM             s_name)
{
    char *namespace_;
    char *name;
    GIBaseInfo *info;
    size_t len;
    char *trimmed_name = NULL;

    namespace_ = scm_to_utf8_string (s_namespace_);
    name = scm_to_utf8_string (s_name);

    /* If the given name ends with an underscore, it might be due to usage
     * as an accessible replacement for something in GI with the same name
     * as a Python keyword. Test for this and trim it out if necessary.
     */
    len = strlen (name);
    if (len > 0 && name[len-1] == '_') {
        trimmed_name = g_strndup (name, len-1);
        if (_gir_is_r5rs_keyword (trimmed_name)) {
            name = trimmed_name;
        }
    }

    info = g_irepository_find_by_name (scm_foreign_object_ref (s_self, 0), namespace_, name);
    g_free (trimmed_name);
    free (name);
    free (namespace_);

    if (info == NULL) {
        return SCM_BOOL_F;
    }

    return _gir_info_new (info);
}

static SCM
_wrap_g_irepository_get_infos (SCM             s_self,
                               SCM             s_namespace_)
{
    char *namespace_;
    gssize n_infos;
    SCM infos;
    gint i;
    GIRepository *repository;

    namespace_ = scm_to_utf8_string (s_namespace_);
    repository = scm_foreign_object_ref (s_self, 0);
    n_infos = g_irepository_get_n_infos (repository, namespace_);
    free (namespace_);
    if (n_infos < 0) {
	scm_misc_error ("%irepository-get-infos", "Namespace '~A' not loaded",
			scm_list_1 (s_namespace_));
        return SCM_BOOL_F;
    }

    infos = SCM_EOL;

    for (i = 0; i < n_infos; i++) {
        GIBaseInfo *info;
        SCM scm_info;

        info = g_irepository_get_info (repository, namespace_, i);
        g_assert (info != NULL);

        scm_info = _gir_info_new (info);

        g_base_info_unref (info);

        if (scm_info == NULL) {
	    g_critical ("_wrap_g_irepository_get_info: FIXME free memory here");
            break;
        }
	SCM_LIST_APPEND (infos, scm_info);
    }

    return infos;
}

static SCM
_wrap_g_irepository_get_typelib_path (SCM             s_self,
                                      SCM             s_namespace_)
{
    GIRepository *repository;
    char *namespace_;
    const gchar *typelib_path;

    namespace_ = scm_to_utf8_string (s_namespace_);
    repository = scm_foreign_object_ref (s_self, 0);
    typelib_path = g_irepository_get_typelib_path (repository, namespace_);
    free (namespace_);
    if (typelib_path == NULL) {
	scm_misc_error ("%irepository-get-typelib-path", "Namespace '~A' not loaded",
			scm_list_1 (s_namespace_));
        return SCM_BOOL_F;
    }
    SCM ret = scm_from_utf8_string (typelib_path);
    return ret;
}

static SCM
_wrap_g_irepository_get_version (SCM            s_self,
                                 SCM            s_namespace_)
{
    GIRepository *repository;
    char *namespace_;
    const gchar *version;

    namespace_ = scm_to_utf8_string (s_namespace_);
    repository = scm_foreign_object_ref (s_self, 0);
    version = g_irepository_get_version (repository, namespace_);
    free (namespace_);
    if (version == NULL) {
	scm_misc_error ("%irepository-get-version", "Namespace '~A' not loaded",
			scm_list_1 (s_namespace_));
	return SCM_BOOL_F;
    }

    SCM ret = scm_from_utf8_string (version);
    return ret;
}

static SCM
_wrap_g_irepository_get_loaded_namespaces (SCM s_self)
{
    char **namespaces;
    SCM scm_namespaces;
    gssize i;

    namespaces = g_irepository_get_loaded_namespaces (scm_foreign_object_ref (s_self, 0));

    scm_namespaces = SCM_EOL;
    for (i = 0; namespaces[i] != NULL; i++) {
        SCM scm_namespace = scm_from_utf8_string (namespaces[i]);
	SCM_LIST_APPEND (scm_namespaces, scm_namespace);
        g_free (namespaces[i]);
    }

    g_free (namespaces);

    return scm_namespaces;
}

static SCM
_wrap_g_irepository_get_dependencies (SCM            s_self,  
                                      SCM            s_namespace_)
{
    char *namespace_;
    char **namespaces;
    SCM scm_namespaces;
    gssize i;

    namespace_ = scm_to_utf8_string (s_namespace_);
    scm_namespaces = SCM_EOL;
    /* Returns NULL in case of no dependencies */
    namespaces = g_irepository_get_dependencies (scm_foreign_object_ref (s_self, 0), namespace_);
    free (namespace_);
    if (namespaces == NULL) {
        return scm_namespaces;
    }

    for (i = 0; namespaces[i] != NULL; i++) {
        SCM scm_namespace = scm_from_utf8_string (namespaces[i]);
	SCM_LIST_APPEND (scm_namespaces, scm_namespace);
    }

    g_strfreev (namespaces);

    return scm_namespaces;
}

static SCM
_wrap_g_irepository_get_immediate_dependencies (SCM            s_self,  
						SCM            s_namespace_)
{
    char *namespace_;
    char **namespaces;
    SCM scm_namespaces;
    gssize i;

    namespace_ = scm_to_utf8_string (s_namespace_);
    scm_namespaces = SCM_EOL;
    /* Returns NULL in case of no dependencies */
    namespaces = g_irepository_get_immediate_dependencies (scm_foreign_object_ref (s_self, 0), namespace_);
    free (namespace_);
    if (namespaces == NULL) {
        return scm_namespaces;
    }

    for (i = 0; namespaces[i] != NULL; i++) {
        SCM scm_namespace = scm_from_utf8_string (namespaces[i]);
	SCM_LIST_APPEND (scm_namespaces, scm_namespace);
    }

    g_strfreev (namespaces);

    return scm_namespaces;
}

void
gir_repository_register_types ()
{
    g_debug ("gir_repository_register_types: entered");
    ScmGIRepository_Type =
	scm_make_foreign_object_type (scm_from_locale_symbol ("gi.Repository"),
				      scm_list_1 (scm_from_locale_symbol ("data")),
				      NULL);
    ScmGIRepository_Type_Store
	= scm_permanent_object (scm_c_define ("gi.Repository",
					      ScmGIRepository_Type));
    
    scm_c_define_gsubr ("%irepository-enumerate-versions", 2, 0, 0, _wrap_g_irepository_enumerate_versions);
    scm_c_define_gsubr ("%irepository-get-default", 0, 0, 0, _wrap_g_irepository_get_default);
    scm_c_define_gsubr ("%irepository-require", 2, 2, 0, _wrap_g_irepository_require);
    scm_c_define_gsubr ("%irepository-is-registered?", 2, 1, 0, _wrap_g_irepository_is_registered);
    scm_c_define_gsubr ("%irepository-find-by-name", 3, 0, 0, _wrap_g_irepository_find_by_name);
    scm_c_define_gsubr ("%irepository-get-typelib-path", 2, 0, 0, _wrap_g_irepository_get_typelib_path);
    scm_c_define_gsubr ("%irepository-get-version", 2, 0, 0, _wrap_g_irepository_get_version);
    scm_c_define_gsubr ("%irepository-get-loaded-namespaces", 1, 0, 0, _wrap_g_irepository_get_loaded_namespaces);
    scm_c_define_gsubr ("%irepository-get-dependencies", 2, 0, 0, _wrap_g_irepository_get_dependencies);
    scm_c_define_gsubr ("%irepository-get-immediate-dependencies", 2, 0, 0, _wrap_g_irepository_get_immediate_dependencies);
    scm_c_export ("gi.Repository",
		  "%irepository-enumerate-versions",
		  "%irepository-get-default",
		  "%irepository-require",
		  "%irepository-is-registered?",
		  "%irepository-find-by-name",
		  "%irepository-get-typelib-path",
		  "%irepository-get-version",
		  "%irepository-get-loaded-namespaces",
		  "%irepository-get-dependencies",
		  "%irepository-get-immediate-dependencies",
		  NULL);
}

  
