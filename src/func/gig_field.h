// Copyright (C) 2026 Michael L. Gran

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

#ifndef GIG_FIELD_H
#define GIG_FIELD_H

#include <stdbool.h>
#include <girepository.h>
#include <libguile.h>

// Whether guile-gi can read, and write, field FI of a struct, union or
// object whose fields gig_fields_are_reliable.
bool gig_field_is_readable(GIFieldInfo *fi);
bool gig_field_is_writable(GIFieldInfo *fi);

// False when the typelib's field offsets can't be trusted, as for
// bitfields, which it describes as whole fields.
bool gig_fields_are_reliable(GIRegisteredTypeInfo *container);

// Defines accessors for the fields of CONTAINER, a struct, union or
// object, in the current module, and returns their names: NAME:FIELD,
// which works with set! when the field is writable, and
// NAME:set-FIELD!.
SCM gig_fields_define(GIRegisteredTypeInfo *container);

// The Scheme name of field FI, e.g. "long" for long_.  Free it.
char *gig_field_make_name(GIFieldInfo *fi);

// Whether a method of CONTAINER has the name of field FI's getter, so
// that the getter isn't defined.
bool gig_field_is_shadowed(GIRegisteredTypeInfo *container, GIFieldInfo *fi);

void gig_init_field(void);

#endif
