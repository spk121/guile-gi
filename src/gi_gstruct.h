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

#ifndef _GI_GSTRUCT_H_
#define _GI_GSTRUCT_H_
#include <libguile.h>
#include <glib.h>
#include <glib-object.h>

#if 0
extern SCM gir_gbox_type;
extern SCM gir_gbox_type_store;

#define GI_GSTRUCT_NO_FREE_FUNC 0
#define GI_GSTRUCT_DEFAULT_FREE_FUNC 1
#define GI_GSTRUCT_CUSTOM_FREE_FUNC 2

typedef enum _GirPointerType
{
    SPTR_HOLDS_STRUCT,
    SPTR_HOLDS_UNION,
    SPTR_HOLDS_GBOXED,
    SPTR_HOLDS_POINTER
} GirPointerContents;

typedef enum _GirPointerFree
{
    SPTR_NO_FREE_FUNC,
    SPTR_DEFAULT_FREE_FUNC,
    SPTR_C_FREE_FUNC,
    SPTR_SCM_FREE_FUNC,
    SPTR_SCM_UNREF_METHOD
} GirPointerFreeAction;

typedef struct _GirSmartPtr
{
    gpointer ptr;
    GirPointerContents holds;
    GirPointerFreeAction dealloc;
    GType type;
    int count;
    union {
        void (*c_free_func)(gpointer ptr);
        SCM scm_free_func;
    };
} GirSmartPtr;

void gir_sptr_add_ref (GirSmartPtr *sptr);
gboolean gir_sptr_release (GirSmartPtr *sptr);

void *gi_gbox_peek_pointer (SCM self);
void *gi_gbox_ref_pointer (SCM self);

GType gi_gbox_get_type (SCM self);

SCM gir_new_gbox (GirPointerContents holds, GType gtype, gpointer ptr, gboolean use_default_free);
void gir_gbox_connect_c_free_func (SCM box, void (*free_func)(gpointer ptr));
void gir_gbox_connect_scm_free_func (SCM box, SCM free_func);
SCM gir_new_struct_gbox (GType type, void *ptr, gboolean free_on_dealloc);
SCM gir_new_union_gbox (GType type, void *ptr, gboolean free_on_dealloc);

SCM gi_gbox_p (SCM self);
#endif



SCM gir_struct_new(GIStructInfo *referenced_struct_info, void *obj, gboolean free);
void gi_init_gbox (void);
#endif
