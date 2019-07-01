// Copyright (C) 2018 Michael L. Gran

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

#ifndef _GIR_CONSTANT_H_
#define _GIR_CONSTANT_H_
#include <girepository.h>

void gir_constant_define(GIConstantInfo *info);
static inline void
gir_constant_document(GString **str, const char *namespace_, const char *parent,
                      GIConstantInfo *info)
{
}

void gir_init_constant(void);
#endif
