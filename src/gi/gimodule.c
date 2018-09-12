/* -*- Mode: C; c-basic-offset: 4 -*- */

#include <libguile.h>
#include <glib-object.h>
#include "pycompat.h"
#include "gugi-error.h"
#include "gugi-foreign.h"
#include "gugi-info.h"
#include "gugi-repository.h"
#include "gugi-type.h"
#include "gugi-util.h"

/* Returns a new flag/enum type or %NULL */
static SCM
flags_enum_from_gtype (GType g_type,
		       SCM (add_func) (SCM, const char *, const char *, GType))
{
    SCM new_type;
    GIRepository *repository;
    GIBaseInfo *info;
    const gchar *type_name;
    
    repository = g_irepository_get_default ();
    info = g_irepository_find_by_gtype (repository, g_type);
    if (info != NULL) {
	type_name = g_base_info_get_name (info);
	new_type = add_func (SCM_UNSPECIFIED, type_name, NULL, g_type);
	g_base_info_unref (info);
    } else {
	type_name = g_type_name (g_type);
	new_type = add_func (SCM_UNSPECIFIED, type_name, NULL, g_type);
    }

    return new_type;
}

static void
gug_flags_add_constants (SCM module, GType flags_type,
			 const gchar *strip_prefix);

/**
 * gug_enum_add_constants:
 * @module: a Guile module
 * @enum_type: the GType of the enumeration.
 * @stip_prefix: the prefix to strip from the constant names
 *
 * Adds constants to the given Guile module for each value name of
 * the enumeration.  A prefix will be stripped from each enum name.
 */
static void
gug_enum_add_constants (SCM module, GType enum_type,
			const gchar *strip_prefix)
{
    GEnumClass *eclass;
    guint i;

    if (!G_TYPE_IS_ENUM(enum_type)) {
	if (G_TYPE_IS_FLAGS(enum_type))
	    gug_flags_add_constants(module, enum_type, strip_prefix);
	else
	    g_warning("`%s' is not an enum type", g_type_name(enum_type));
	return;
    }
    g_return_if_fail (strip_prefix != NULL);

    eclass = G_ENUM_CLASS(g_type_class_ref(enum_type));

    for (i = 0; i < eclass->n_values; i ++) {
	const gchar *name = eclass->values[i].value_name;
	gint value = eclass->values[i].value;

	GuModule_AddIntConstant(module,
				(char *) gug_constant_strip_prefix(name, strip_prefix),
				(long) value);
    }

    g_type_class_unref(eclass);
}

/**
 * gug_flags_add_constants:
 * @module: a Guile module
 * @flags_type: the GType of the flags type.
 * @strip_prefix: the prefix to strip from the constant names.
 *
 * Adds constants to the given Python module for each value name of
 * the flags set.  A prefix will be stripped from each flag name.
 */
static void
gug_flags_add_constants(SCM module, GType flags_type,
			const gchar *strip_prefix)
{
    GFlagsClass *fclass;
    guint i;

    if (!G_TYPE_IS_FLAGS(flags_type)) {
	if (G_TYPE_IS_ENUM(flags_type))
	    gug_enum_add_constants(module, flags_type, strip_prefix);
	else
	    g_warning("`%s' is not an flags type", g_type_name(flags_type));
	return;
    }
    g_return_if_fail (strip_prefix != NULL);

    fclass = G_FLAGS_CLASS(g_type_class_ref(flags_type));

    for (i = 0; i < fclass->n_values; i++) {
	const gchar *name = fclass->values[i].value_name;
	guint value = fclass->values[i].value;

	GuModule_AddIntConstant(module,
				(char*) gug_constant_strip_prefix(name, strip_prefix),
				(long) value);
    }

    g_type_class_unref(fclass);
}

void
gir_log_handler (const gchar *log_domain,
		 GLogLevelFlags log_level,
		 const gchar *message,
		 gpointer user_data)
{
  time_t timer;
  char buffer[26];
  struct tm* tm_info;
  time(&timer);
  tm_info = localtime(&timer);
  strftime(buffer, 26, "%Y-%m-%d %H:%M:%S", tm_info);
  FILE *fp = fopen("gir-log.txt", "at");
  fprintf (fp, "%s: %s %d %s\n", buffer, log_domain, log_level, message);
  fclose (fp);
}


static void
scm_add_string_constant (const char *name, const char *val)
{
  SCM var = scm_c_define (name, scm_from_utf8_string (val));
  scm_permanent_object (var);
  scm_c_export (name, NULL);
}

void gir_module_init(void)
{
  SCM api;
  SCM module_dict = scm_c_make_hash_table(30);

  g_log_set_handler (G_LOG_DOMAIN, G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL
		     | G_LOG_FLAG_RECURSION, gir_log_handler, NULL);
  g_log_set_handler ("GLib", G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL
                   | G_LOG_FLAG_RECURSION, gir_log_handler, NULL);

  g_debug ("begin initialization");
  scm_add_string_constant ("__package__", "gir._gi");
  gir_register_NONE_sigil();
  gir_foreign_init();
  gir_error_register_types();
  gir_repository_register_types();
  gir_info_register_types();
  g_debug ("end initialization");
}
