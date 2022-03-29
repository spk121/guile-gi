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
#include "../core.h"
#include "gig_constant_priv.h"

SCM gig_il_constant_func = SCM_UNDEFINED;

SCM
gig_il_constant(SCM s_name, SCM s_value)
{
#define FUNC_NAME "^constant"
    SCM_ASSERT_TYPE(scm_is_symbol(s_name), s_name, SCM_ARG1, FUNC_NAME, "symbol");
    scm_permanent_object(scm_define(s_name, s_value));
    return scm_list_1(s_name);
#undef FUNC_NAME
}

void
gig_init_constant()
{
    gig_il_constant_func = scm_c_define_gsubr("^constant", 2, 0, 0, gig_il_constant);
}
