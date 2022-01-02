// Copyright (C) 2019, 2020 Michael L. Gran

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

#ifndef GIG_FUNCTION_PRIVATE_H
#define GIG_FUNCTION_PRIVATE_H

extern SCM make_proc;
extern SCM add_method_proc;

extern SCM top_type;
extern SCM method_type;

extern SCM kwd_specializers;
extern SCM kwd_formals;
extern SCM kwd_procedure;

extern SCM sym_self;

SCM default_definition(SCM name);

#endif
