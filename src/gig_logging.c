// Copyright (C) 2018, 2019, 2022 Michael L. Gran

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

#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "gig_logging.h"

// GIG_DEBUG:

// To make logging happen, set the environment variable GIG_DEBUG.
// If it is set at all, the error and warning messages will be printed.
// If it contains any of these words, it prints more info
// - all: print all categories debug and info messages
// - invoke: print debug messages about function invocation
// - transfer: print debug messages about object creation and transfer
// - load: print debug messages about loading typelib info
// - init: print debug messages about initialization and finalization
// - verbose: also print file names, line numbers, PIDs

// GIG_DEBUG_OUTPUT
// If this is set, the output will be printed to a file using
// this filename.  The filename will be appended.

#define N_TERMINALS 455
static char color_terminals[N_TERMINALS][24] = {
    "xterm", "xterm-256color", "xterm-new", "konsole",
    "konsole-256color", "linux", "gnome", "gnome-256color", "screen",
    "screen-16color", "screen-256color",

#if 0
    "alacritty", "ansi", "ansi80x25", "ansis", "Apple_Terminal",
    "aterm", "bterm", "cons25", "cygwin", "Eterm", "Eterm-256color",
    "Eterm-88color", "eterm-color", "Eterm-color", "hurd", "jfbterm",
    "kitty", "kon", "kon2", "mach-color", "mach-gnu-color", "mlterm",
    "mrxvt", "nsterm", "nsterm-256color", "nxterm", "pcansi", "putty",
    "putty-256color", "rxvt", "rxvt-16color", "rxvt-256color",
    "rxvt-88color", "rxvt-color", "rxvt-cygwin", "rxvt-cygwin-native",
    "rxvt-unicode", "rxvt-unicode-256color", "rxvt-xpm",
    "screen.Eterm", "screen.gnome", "screen.konsole",
    "screen.konsole-256color", "screen.linux", "screen.linux-s",
    "screen.mlterm", "screen.mlterm-256color", "screen.mrxvt",
    "screen.putty", "screen.putty-256color", "screen.rxvt",
    "screen.teraterm", "screen.vte", "screen.vte-256color",
    "screen.xterm-256color", "screen.xterm-new",
    "screen.xterm-xfree86", "st", "st-16color", "st-256color",
    "stterm", "stterm-16color", "stterm-256color", "teraterm",
    "teraterm2.3", "tmux", "tmux-256color", "tmux-direct", "vte",
    "vte-256color", "vwmterm", "wsvt25", "wsvt25m", "xfce",
    "xterm-1002", "xterm-1003", "xterm-1005", "xterm-1006",
    "xterm-16color", "xterm-88color", "xterm-8bit", "xterm-basic",
    "xterm-color", "xterm-direct", "xterm-direct16", "xterm-direct2",
    "xterm-direct256", "xterm-hp", "xterm-nic", "xterm-noapp",
    "xterm-sco", "xterm-sun", "xterm-utf8", "xterm-vt220",
    "xterm-x10mouse", "xterm-x11hilite", "xterm-x11mouse",
    "xterm-xf86-v32", "xterm-xf86-v33", "xterm-xf86-v333",
    "xterm-xf86-v40", "xterm-xf86-v43", "xterm-xf86-v44",
    "xterm-xfree86", "xterm-xi"
#endif
};

static bool
is_color_term(const char *str)
{
    for (int i = 0; i < N_TERMINALS; i++) {
        if (strcmp(str, color_terminals[i]) == 0)
            return true;
    }
    return false;
}

void
gig_log(int level, const char *file, int line, const char *func,
        const char *domain, const char *template, ...)
{
    static char msg[401];
    static int msg_len = 400;
    char *debug;

    debug = getenv("GIG_DEBUG");
    if (!debug)
        return;
    if (level > GIG_LOG_WARNING && (strstr(debug, "all") == NULL && strstr(debug, domain) == NULL))
        return;

    va_list args;
    va_start(args, template);
    memset(msg, 0, sizeof(msg));
    vsnprintf(msg, msg_len, template, args);
    va_end(args);

    char *debug_output = getenv("GIG_DEBUG_OUTPUT");
    FILE *fp = NULL;
    bool use_verbose = false;
    char *color, *reset, *prefix;

    if (debug_output)
        fp = fopen(debug_output, "a");

    if (strstr(debug, "verbose") != NULL)
        use_verbose = true;

    switch (level) {
    case GIG_LOG_ERROR:
        color = "\033[1;31m";
        reset = "\033[0m";
        prefix = "ERROR";
        break;
    case GIG_LOG_CRITICAL:
        color = "\033[1;35m";
        reset = "\033[0m";
        prefix = "CRITICAL";
        break;
    case GIG_LOG_WARNING:
        color = "\033[1;35m";
        reset = "\033[0m";
        prefix = "WARNING";
        break;
    case GIG_LOG_NOTICE:
        color = "\033[1;32m";
        reset = "\033[0m";
        prefix = "NOTICE";
        break;
    case GIG_LOG_INFO:
        color = "\033[1;32m";
        reset = "\033[0m";
        prefix = "INFO";
        break;
    case GIG_LOG_DEBUG:
        color = "\033[1;32m";
        reset = "\033[0m";
        prefix = "DEBUG";
        break;
    default:
        color = "";
        reset = "";
        prefix = "";
    }

    char *term = getenv("TERM");

    if (!term || !is_color_term(term)) {
        color = "";
        reset = "";
    }

    if (use_verbose) {
        time_t t = time(NULL);
        char timestr[80];
        strftime(timestr, 80, "%F %T", gmtime(&t));
        if (fp)
            fprintf(fp, "%s%s%s %s %s %s %s:%d %s\n",
                    color, prefix, reset, domain, timestr, func, file, line, msg);
        else
            fprintf(stderr, "%s%s%s %s %s %s %s:%d %s\n",
                    color, prefix, reset, domain, timestr, func, file, line, msg);
    }
    else {
        if (fp)
            fprintf(fp, "%s%s%s %s\n", color, prefix, reset, msg);
        else
            fprintf(stderr, "%s%s%s %s\n", color, prefix, reset, msg);
    }

    if (fp)
        fclose(fp);
}
