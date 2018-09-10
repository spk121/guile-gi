#include <libguile.h>
#include <glib-object.h>
#include "gir_error.h"
#include "gir_foreign.h"
#include "gir_info.h"
#include "gir_repository.h"
#include "gir_type.h"

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
