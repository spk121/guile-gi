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

#include <libguile.h>
#include "gig_callback.h"
#include "gig_flag.h"
#include "gig_function.h"
#include "gig_object.h"
#include "gig_logging.h"
#include "gig_signal.h"
#include "gig_type.h"
#include "gig_util.h"
#include "gig_value.h"
#include "gig.h"

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

void
gig_init(void)
{
#ifdef MTRACE
    mtrace();
#endif
    gig_debug_init("Begin libguile-gir initialization");
    gig_init_flag();
    gig_init_signal();
    gig_init_callback();
    gig_init_function();
#ifdef ENABLE_GCOV
    scm_c_define_gsubr("gcov-reset", 0, 0, 0, scm_gcov_reset);
    scm_c_define_gsubr("gcov-dump", 0, 0, 0, scm_gcov_dump);
#endif
    gig_debug_init("End libguile-gir initialization");
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
