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

#include <libguile.h>
#include "core.h"
#include "type.h"
#include "func.h"
#include "gig_logging.h"

GIG_API void gig_init(void);

#ifdef ENABLE_GCOV
void __gcov_reset(void);
void __gcov_dump(void);
#endif

#ifdef MTRACE
#include <mcheck.h>
#endif

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

GIG_API void
gig_init(void)
{
#ifdef MTRACE
    mtrace();
#endif
    gig_debug("Begin libguile-gir initialization");
    gig_init_flag();
    gig_init_signal();
    gig_init_callback();
    gig_init_function();
    gig_lib_init();
    gig_constant_init();
    gig_init_object();
#ifdef ENABLE_GCOV
    scm_c_define_gsubr("gcov-reset", 0, 0, 0, scm_gcov_reset);
    scm_c_define_gsubr("gcov-dump", 0, 0, 0, scm_gcov_dump);
#endif
    gig_debug("End libguile-gir initialization");
}

#ifdef STANDALONE
int
main(int argc, char **argv)
{
    scm_init_guile();

    gig_init();
    scm_shell(argc, argv);
    return 0;
}
#endif
