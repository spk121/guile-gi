/* -*- Mode: C; c-basic-offset: 4 -*- */
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

#include <libguile.h>
#include <girepository.h>
#include "gir_type.h"

// In C, a GType is an integer.  It is an integer ID that maps to a type of GObject.

// If Guile-GI does its job well, the users will never need to concern themselves with
// these integers, and instead just use Guile types created from them.
// But, internally, we need to wrap them up.
// In Guile, we define a <GTypeID> foreign object type to hold these GType integer IDs.
// The <GTypeID> has two slots
// - type: a pointer-like integer
// - info: a GIBaseInfo pointer

// For most of the GType integer IDs, the ID is associated with a GObject struct type,
// union type, or object type.  For those IDs that indicate a GObject struct, union,
// or object type, we make a Guile foreign object type.
// For example, if, for example, there were a GType ID 0xaabbccdd that mapped to the
// C struct GtkWindow pointer type, on the Guile
// side, a <GtkWindow> foreign object type would be created.

// The Guile foreign object types that get created primarily are just boxes that
// hold C Pointers.  The <GtkWindow> foreign object type has a slot "obj".  Instances
// of the <GtkWindow> foreign object type will use the "obj" slot to hold a
// C GtkWindow pointer.

// But, these Guile foreign object types also have slots that may be used for
// bookkeeping and memory management.
// The slots in all the Guile GObject foreign object types created by this library are
// - ob_type: the GType ID (unsigned pointer-like integer)
// - ob_refcnt: an integer (unsigned integer)
// - obj: a C pointer
// - inst_dict: a Guile key/value store that gets used for custom types
// - weakreflist: unused for now, but, might be used later for memory management
// - flags: (unsigned integer)

// I've decided not to use GOOPS.  One problem that results from this is that
// there is no place in the Guile foreign object types to back-reference the GTypeID from which
// the Guile type was created.  For example, the <GtkWindow> foreign object types
// doesn't have a C++-like class static slot to hold the GTypeID.  As a workaround,
// there is a hash table that maps GTypeIDs to their associated foreign object types.



 * Behind the scenes, it is a complicated private structure,
 * hidden from the user.
 * Types are refcounted.

 * In Guile, we define a GType wrapper class that, for each GType,
 * holds information by Guile to create, manage, and GC instances of
 * that class.

 * When parsing a Typelib file, an argument type is a sort of a triple
 * - type_tag: either a simple type like "guint", else "INTERFACE"
 *             find this with g_type_info_get_tag (typeinfo)
 * - interface_type: one of struct, enum, object, flags
 *             Find this with g_base_info_get_type (g_type_info_get_interface (typeinfo))
 * - interface_name: the actual name of type of the interface, like "Window"
 *             Find this with g_base_info_get_name (g_type_info_get_interface (typeinfo))

 * The GType wrapper is for GTypes of type
 * - STRUCT
 * - ENUM
 * - OBJECT
 * - CALLBACK
 * - FLAGS (rare)
 * - INTERFACE (rare)

 * The Guile binding only creates new GTypes when it makes GObject classes that subtype
 * existing classes.
 *
 */

GSList *all_fo_types = NULL;

static void
gir_type_define(GType gtype, GIBaseInfo *info)
{
    // Make a foreign object type for instances of this GType.
    // All of our custom introspected foreign object types will
    // have the same slots.
    // ob_type,ob_refcnt,obj,inst_dict,weakreflist,flags
    gchar *type_class_name = type_class_public_name(info);
    SCM sname = scm_from_utf8_symbol(type_class_name);
    SCM slots = scm_list_n(scm_from_utf8_symbol("ob_type"),
        scm_from_utf8_symbol("ob_refcnt"),
        scm_from_utf8_symbol("obj"),
        scm_from_utf8_symbol("dealloc"),
        scm_from_utf8_symbol("free_func"),
        scm_from_utf8_symbol("inst_dict"),
        scm_from_utf8_symbol("weakreflist"),
        scm_from_utf8_symbol("flags"),
        SCM_UNDEFINED);
    SCM fo_type = scm_make_foreign_object_type(sname, slots, NULL);
    g_debug("Creating a new GType foreign object type: %p %s", SCM_UNPACK_POINTER(fo_type), type_class_name);
    scm_permanent_object(scm_c_define(type_class_name, fo_type));

    // Associate the foreign object type with the GType
    SCM s_gtype = gi_gtype_define_wrapper(gtype, info, fo_type);

    // Make a predicate for this type.
    all_fo_types = g_slist_append(all_fo_types, SCM_UNPACK_POINTER(fo_type));
    gchar *predicate_name = type_class_predicate_name(info);
    gpointer func = gir_type_create_predicate(predicate_name, fo_type);
    if (func)
        scm_c_define_gsubr(predicate_name, 1, 0, 0, func);

    scm_c_export(type_class_name, predicate_name, NULL);
    g_free(predicate_name);
    g_free(type_class_name);
}


void gir_init_types(void)
{

}