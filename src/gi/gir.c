/* -*- Mode: C; c-basic-offset: 4 -*- */
#include <time.h>
#include <glib-object.h>
#include <glib.h>
#include <girepository.h>
#include <libguile.h>
#include "gi_gobject.h"
#include "gi_gvalue.h"
#include "gi_gtype.h"
#include "gi_gsignal.h"
#include "gi_gparamspec.h"
#include "gi_ginterface.h"
#include "gi_signal_closure.h"
#include "gi_giargument.h"
#include "gir_func.h"
#include "gir_func2.h"
#include "gir_type.h"
#include "gi_gstruct.h"
#include "gir_callback.h"

#ifdef _WIN32
static const int _win32 = TRUE;
#else
static const int _win32 = FALSE;
#endif

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
    if (log_level == G_LOG_LEVEL_DEBUG && !_win32)
    {
        FILE *fp = fopen("gir-debug-log.xt", "at");
        fprintf (fp, "%s: %s %d %s\n", buffer, log_domain, log_level, message);
        fclose (fp);
    }
    else
	fprintf (stderr, "%s: %s %d %s\n", buffer, log_domain, log_level, message);
    fflush(stderr);
}

void
gir_init(void)
{
#if 1
    g_log_set_handler (G_LOG_DOMAIN, G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL
                       | G_LOG_FLAG_RECURSION, gir_log_handler, NULL);
    g_log_set_handler ("GLib", G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL
                       | G_LOG_FLAG_RECURSION, gir_log_handler, NULL);
#endif                       
    g_debug ("Begin libguile-gir initialization");
    gi_init_gtype ();
    gi_init_gvalue ();
    gi_init_gsignal ();
    gi_init_gparamspec ();
    gi_init_gbox ();

    gir_init_funcs();
    gir_init_func2();
    gi_init_giargument();
    gir_init_types();
    gi_init_gobject();
    gir_init_callback();
    g_debug ("End libguile-gir initialization");
}

int main(int argc, char **argv)
{
    scm_init_guile();

    gir_init ();
    scm_shell(argc, argv);
    return 0;
}
