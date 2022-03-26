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

#ifndef GIG_FUNCTION_PUB_H
#define GIG_FUNCTION_PUB_H

#include <libguile.h>

GIG_API extern SCM gig_il_function_func;
GIG_API extern SCM gig_il_signal_func;

SCM gig_function_define_full(const char *namespace_, size_t gtype, const char *long_name,
                             const char *short_name, const char *symbol, GigArgMap *amap);
SCM gig_signal_define_full(const char *namespace_, size_t gtype, const char *long_name,
                           const char *short_name, const char *symbol, GigArgMap *amap);

SCM
gig_il_signal(SCM s_namespace_, SCM s_gtype_name, SCM s_long_name,
              SCM s_short_name, SCM s_symbol_name, SCM s_amap);
SCM
gig_il_function(SCM s_namespace_, SCM s_gtype_name, SCM s_long_name,
                SCM s_short_name, SCM s_symbol_name, SCM s_amap);


void gig_init_function(void);

#endif
