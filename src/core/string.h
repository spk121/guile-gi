// Copyright (C) 2022 Michael L. Gran

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

#ifndef CORE_STRING_H
#define CORE_STRING_H

char *bracketize(const char *str);
char *concatenate(const char *str1, const char *str2);
char *concatenate3(const char *str1, const char *str2, const char *str3);
char *concatenate4(const char *str1, const char *str2, const char *str3, const char *str4);
char *decorate_string(const char *fmt, const char *str);
char *make_scm_name(const char *gname);
const char *skip_prefix(const char *name, const char *prefix);
size_t strvlen(const char **x);

#endif
