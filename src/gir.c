/* -*- Mode: C; c-basic-offset: 4 -*- */
// Copyright (C) 2018, 2019 Michael L. Gran

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <time.h>
#include <glib-object.h>
#include <glib.h>
#include <girepository.h>
#include <libguile.h>
#include "gi_gobject.h"
#include "gi_gvalue.h"
#include "gi_gsignal.h"
#include "gi_gparamspec.h"
#include "gi_signal_closure.h"
#include "gi_giargument.h"
#include "gir_typelib.h"
#include "gir_type.h"
#include "gir_callback.h"
#include "gir_function.h"
#include "gir_method.h"
#include "gir_constant.h"
#include "gir_flag.h"
#include "gi_struct.h"

#ifdef _WIN32
static const int _win32 = TRUE;
#else
static const int _win32 = FALSE;
#endif

#ifdef ENABLE_GCOV
void __gcov_reset(void);
void __gcov_dump(void);
#endif

void
gir_log_handler(const gchar *log_domain,
                GLogLevelFlags log_level, const gchar *message, gpointer user_data)
{
    time_t timer;
    char buffer[26];
    struct tm *tm_info;
    time(&timer);
    tm_info = localtime(&timer);
    strftime(buffer, 26, "%Y-%m-%d %H:%M:%S", tm_info);

    // Opening and closing files as append in Win32 is noticeably slow.
    if (log_level == G_LOG_LEVEL_DEBUG && !_win32) {
        FILE *fp = fopen("gir-debug-log.xt", "at");
        fprintf(fp, "%s: %s %d %s\n", buffer, log_domain, log_level, message);
        fclose(fp);
    }
    else
        fprintf(stderr, "%s: %s %d %s\n", buffer, log_domain, log_level, message);
    fflush(stderr);
}

#ifdef ENABLE_GCOV
static SCM
scm_gcov_reset(void)
{
    __gcov_reset();
    return SCM_UNSPECIFIED;
}


static SCM
scm_gcov_dump(void)
{
    __gcov_dump();
    return SCM_UNSPECIFIED;
}
#endif

void
gir_init(void)
{
#if 0
    g_log_set_handler(G_LOG_DOMAIN, G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL
                      | G_LOG_FLAG_RECURSION, gir_log_handler, NULL);
    g_log_set_handler("GLib", G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL
                      | G_LOG_FLAG_RECURSION, gir_log_handler, NULL);
#endif
    g_debug("Begin libguile-gir initialization");
    gir_init_types();
    gir_init_typelib();
    gir_init_constant();
    gir_init_flag();

    gi_init_struct();
    gi_init_gvalue();
    gi_init_gsignal();
    gi_init_gparamspec();

    gi_init_giargument();
    gi_init_gobject();
    gir_init_callback();
    gir_init_function();
    gir_init_method();
    g_debug("End libguile-gir initialization");

#ifdef ENABLE_GCOV
    scm_c_define_gsubr("gcov-reset", 0, 0, 0, scm_gcov_reset);
    scm_c_define_gsubr("gcov-dump", 0, 0, 0, scm_gcov_dump);
#endif
}

int
main(int argc, char **argv)
{
    scm_init_guile();

    gir_init();
    scm_shell(argc, argv);
    return 0;
}
