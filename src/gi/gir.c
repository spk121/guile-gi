#include <time.h>
#include "gir_gtype.h"
#include "gir_gboxed.h"

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

void
gir_init(void)
{
  g_log_set_handler (G_LOG_DOMAIN, G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL
		     | G_LOG_FLAG_RECURSION, gir_log_handler, NULL);
  g_log_set_handler ("GLib", G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL
                   | G_LOG_FLAG_RECURSION, gir_log_handler, NULL);
  g_debug ("Begin libguile-gir initialization");
  gir_init_gtype();
  gir_init_gboxed();
  g_debug ("End libguile-gir initialization");
}
