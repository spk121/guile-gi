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

#ifndef GIG_FOREIGN_STRUCT_H
#define GIG_FOREIGN_STRUCT_H

#include <stdbool.h>
#include <girepository.h>
#include <libguile.h>

// Plain structs that GTK, GSK and GDK use but give no GType, such as
// GskRoundedRect, GskColorStop or GtkPageRange.  Without a GType they
// can't be boxed types, so each one is a Guile foreign object type
// with a C constructor, getters and setters.

typedef struct _GigForeignStruct GigForeignStruct;

// The pseudo-GType of argument type metadata for these structs; the
// metadata also points to the struct's GigForeignStruct.
GType gig_foreign_struct_get_type(void);
#define G_TYPE_PRIV_FOREIGN_STRUCT (gig_foreign_struct_get_type())

// The foreign struct for a struct info, or NULL if guile-gi doesn't
// wrap it, or its layout in the typelib isn't the expected one.
const GigForeignStruct *gig_foreign_struct_find(GIBaseInfo *info);

const char *gig_foreign_struct_c_name(const GigForeignStruct *fs);
size_t gig_foreign_struct_size(const GigForeignStruct *fs);
bool gig_foreign_struct_is_opaque(const GigForeignStruct *fs);
SCM gig_foreign_struct_scheme_type(const GigForeignStruct *fs);
const char *gig_foreign_struct_name(const GigForeignStruct *fs);
size_t gig_foreign_struct_n_fields(const GigForeignStruct *fs);
const char *gig_foreign_struct_field_name(const GigForeignStruct *fs, size_t i);

// The C struct of OBJ, which must be of FS's type.  Opaque structs,
// which reach Scheme as raw pointers in signal handlers, may also be
// given as pointers.  Raises a wrong-type-arg error otherwise.
void *gig_foreign_struct_peek(const GigForeignStruct *fs, SCM obj, const char *subr, int argpos);

// The C struct of OBJ if it is any foreign struct, else NULL.
void *gig_foreign_struct_peek_any(SCM obj);

// A Scheme object for the C struct PTR, which the caller still owns.
// Plain structs are copied; opaque structs are only valid while C
// lends them, so they are wrapped.
SCM gig_foreign_struct_to_scm(const GigForeignStruct *fs, const void *ptr);

// Defines FS's class, constructor and field accessors in the current
// module and returns their names.
SCM gig_foreign_struct_define(const GigForeignStruct *fs);

void gig_init_foreign_struct(void);

#endif
