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
#include <girepository.h>
#include <ffi.h>
#include "gir_type.h"
#include "gi_util.h"
#include "gig_object.h"
#include "gig_type_private.h"
#if HAVE_CONFIG_H
#include "config.h"
#endif

// In C, a GType is an integer.  It is an integer ID that maps to a
// type of GObject.

// If Guile-GI does its job well, the users will never need to concern
// themselves with these integers, and instead just use Guile types
// created from them.

// For most of the GType integer IDs, the ID is associated with a
// GObject struct type, union type, or object type.  For those IDs
// that indicate a GObject struct, union, or object type, we make a
// Guile foreign object type.  For example, if, for example, there
// were a GType ID 0xaabbccdd that mapped to the C struct GtkWindow
// pointer type, on the Guile side, a <GtkWindow> foreign object type
// would be created.

// The Guile foreign object types that get created primarily are just
// boxes that hold C Pointers.  The <GtkWindow> foreign object type
// has a slot "obj".  Instances of the <GtkWindow> foreign object type
// will use the "obj" slot to hold a C GtkWindow pointer.

// But, these Guile foreign object types also have slots that may be
// used for bookkeeping and memory management.  The slots in all the
// Guile GObject foreign object types created by this library are
// - ob_type: the GType ID (unsigned pointer-like integer)
// - ob_refcnt: an integer (unsigned integer)
// - obj: a C pointer
// - inst_dict: a Guile key/value store that gets used for custom types
// - weakreflist: unused for now, but, might be used later for memory management
// - flags: (unsigned integer)

// I've decided not to use GOOPS.  One problem that results from this
// is that there is no place in the Guile foreign object types to
// back-reference the GType ID from which the Guile type was created.
// For example, the <GtkWindow> foreign object types doesn't have a
// C++-like class static slot to hold the GType ID.  As a workaround,
// there is a hash table that maps GTypeIDs to their associated
// foreign object types.

/*
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

// Maps GType to SCM (pointer)
GHashTable *gir_type_gtype_hash = NULL;
#if ENABLE_GIR_TYPE_SCM_HASH
// Maps SCM to GType
GHashTable *gir_type_scm_hash = NULL;
#endif

gchar *
gir_type_class_name_from_gtype(GType gtype)
{
    return g_strdup_printf("<%s>", g_type_name(gtype));
}

gchar *
gir_type_document_type_from_gtype(GType gtype)
{
    gchar *class_name = gir_type_class_name_from_gtype(gtype);
    gchar *str = g_strdup_printf("TYPE %s\n\n", class_name);
    g_free(class_name);
    return str;
}

// Given a GType integer but no introspection information, this stores
// that GType in our hash table of known types without creating an
// associated foreign object type.
void
gir_type_register(GType gtype)
{
    GType parent = g_type_parent(gtype);
    if (parent != 0)
        gir_type_register(parent);

    if (!g_hash_table_contains(gir_type_gtype_hash, GSIZE_TO_POINTER(gtype))) {
        g_hash_table_insert(gir_type_gtype_hash, GSIZE_TO_POINTER(gtype), NULL);
        g_debug("Registering a new GType: %zu -> %s", gtype, g_type_name(gtype));
    }
}

static SCM make_class_proc;
static SCM make_instance_proc;
static SCM make_fundamental_proc;
static SCM kwd_name;
static SCM kwd_ptr;
static SCM sym_ptr;
static SCM sym_ref;
static SCM sym_unref;
static SCM sym_size;

typedef gpointer(*GigTypeRefFunction) (gpointer);
typedef void (*GigTypeUnrefFunction)(gpointer);

SCM
gir_type_transfer_object(GType type, gpointer ptr, GITransfer transfer)
{
    if (G_TYPE_IS_CLASSED(type))
        type = G_OBJECT_TYPE(ptr);

#if GIG_DEBUG_TRANSFERS
    g_debug("gir_type_transfer_object(%s, %p, %d)", g_type_name(type), ptr, transfer);
#endif

    SCM scm_type = gir_type_get_scheme_type(type);
    GigTypeRefFunction ref;
    ref = (GigTypeRefFunction) scm_to_pointer(scm_class_ref(scm_type, sym_ref));
    GigTypeUnrefFunction unref;
    unref = (GigTypeUnrefFunction) scm_to_pointer(scm_class_ref(scm_type, sym_unref));

    SCM pointer;
    switch (transfer) {
    case GI_TRANSFER_NOTHING:
        pointer = scm_from_pointer(ref(ptr), unref);
        break;

    case GI_TRANSFER_CONTAINER:
    case GI_TRANSFER_EVERYTHING:
        pointer = scm_from_pointer(ptr, unref);
        break;
    }

    return scm_call_3(make_instance_proc, scm_type, kwd_ptr, pointer);
}

static SCM gig_fundamental_type;
static SCM gig_boxed_type;

gpointer
gir_type_peek_object(SCM obj)
{
    g_return_val_if_fail(SCM_IS_A_P(obj, gig_fundamental_type), NULL);
    return scm_to_pointer(scm_slot_ref(obj, sym_ptr));
}

// Given introspection info from a typelib library for a given GType,
// this makes a new Guile foreign object type and its associated predicate,
// and it stores the type in our hash table of known types.
void
gir_type_define(GType gtype)
{
    g_assert(GSIZE_TO_POINTER(gtype) != NULL);
    // Make a foreign object type for instances of this GType.
    // All of our custom introspected foreign object types will
    // have the same slots.
    // ob_type,ob_refcnt,obj,inst_dict,weakreflist,flags

    gboolean newkey;
    gpointer orig_key, value;
    newkey = g_hash_table_lookup_extended(gir_type_gtype_hash,
                                          GSIZE_TO_POINTER(gtype), &orig_key, &value);
    if (newkey == FALSE) {
        g_debug("trying to define %s", g_type_name(gtype));
        GType parent = g_type_parent(gtype);

        gchar *type_class_name = gir_type_class_name_from_gtype(gtype);

        g_return_if_fail(parent != G_TYPE_INVALID);
        gir_type_define(parent);

        SCM new_type, dsupers, slots = SCM_EOL;
        gpointer sparent = g_hash_table_lookup(gir_type_gtype_hash,
                                               GSIZE_TO_POINTER(parent));
        g_return_if_fail(sparent != NULL);


        GType fundamental = G_TYPE_FUNDAMENTAL(parent);

        switch (fundamental) {
        case G_TYPE_BOXED:
        {
            dsupers = scm_list_1(SCM_PACK_POINTER(sparent));
            new_type = scm_call_4(make_class_proc, dsupers, slots, kwd_name,
                                  scm_from_utf8_string(type_class_name));

            GigBoxedFuncs *funcs = _boxed_funcs_for_type(gtype);

            GIRepository *repository;
            GIBaseInfo *info;
            gsize size = 0;

            repository = g_irepository_get_default();
            info = g_irepository_find_by_gtype(repository, gtype);
            if (info != NULL && GI_IS_STRUCT_INFO(info))
                size = g_struct_info_get_size((GIStructInfo *) info);
            g_base_info_unref(info);

            scm_class_set_x(new_type, sym_ref, scm_from_pointer(funcs->copy, NULL));
            scm_class_set_x(new_type, sym_unref, scm_from_pointer(funcs->free, NULL));
            scm_class_set_x(new_type, sym_size, scm_from_size_t(size));
            break;
        }

        case G_TYPE_INTERFACE:
        case G_TYPE_PARAM:
        case G_TYPE_OBJECT:
            // TODO: add interfaces
            dsupers = scm_list_1(SCM_PACK_POINTER(sparent));
            new_type = scm_call_4(make_class_proc, dsupers, slots, kwd_name,
                                  scm_from_utf8_string(type_class_name));
            break;
        default:
            g_error("unhandled type %s derived from %s",
                    g_type_name(gtype), g_type_name(fundamental));
        }

        g_return_if_fail(!SCM_UNBNDP(new_type));

        scm_permanent_object(scm_c_define(type_class_name, new_type));
        scm_c_export(type_class_name, NULL);
        newkey = g_hash_table_insert(gir_type_gtype_hash,
                                     GSIZE_TO_POINTER(gtype), SCM_UNPACK_POINTER(new_type));
        g_debug("Creating a new GigType: %zu -> %s aka %s", gtype,
                type_class_name, g_type_name(gtype));
#if ENABLE_GIR_TYPE_SCM_HASH
        g_hash_table_insert(gir_type_scm_hash,
                            SCM_UNPACK_POINTER(new_type), GSIZE_TO_POINTER(gtype));
#endif

        g_free(type_class_name);
#if ENABLE_GIR_TYPE_SCM_HASH
        g_debug("Hash table sizes %d %d", g_hash_table_size(gir_type_gtype_hash),
                g_hash_table_size(gir_type_scm_hash));
#else
        g_debug("Hash table size %d", g_hash_table_size(gir_type_gtype_hash));
#endif
    }
    else
        g_debug("GType foriegn_object_type already exists for: %zu -> %s",
                gtype, g_type_name(gtype));
}

// This routine returns the integer GType ID of a scheme object, that is
// - already a GType ID encoded as size_t,
// - a foreign object for a GType
// - a foreign object for an object instance
// The last one is accidental, as internally `gir_type_get_gtype_from_obj'
// is used. This may change in future and should not be relied on.
GType
scm_to_gtype(SCM x)
{
    if (scm_is_integer(x))
        return scm_to_size_t(x);
    else
        return gir_type_get_gtype_from_obj(x);
}

// This routine returns the integer GType ID of a given
// GIR foreign object type, or an instance of a GIR foreign object type.
// It returns #f on failure.
GType
gir_type_get_gtype_from_obj(SCM x)
{
#if ENABLE_GIR_TYPE_SCM_HASH
    gpointer value;
    if ((value = g_hash_table_lookup(gir_type_scm_hash, SCM_UNPACK_POINTER(x))))
        return GPOINTER_TO_SIZE(value);
    else if (SCM_INSTANCEP(x) &&
             (value = g_hash_table_lookup(gir_type_scm_hash, SCM_UNPACK_POINTER(SCM_CLASS_OF(x)))))
        return GPOINTER_TO_SIZE(value);
#else
    SCM klass;
    if (SCM_INSTANCEP(x))
        klass = SCM_CLASS_OF(x);
    else
        klass = x;

    GHashTableIter iter;
    gpointer key, value;

    g_hash_table_iter_init(&iter, gir_type_gtype_hash);
    while (g_hash_table_iter_next(&iter, &key, &value)) {
        SCM svalue = SCM_PACK_POINTER(value);
        if (scm_is_eq(klass, svalue)
            || scm_is_eq(x, svalue))
            return GPOINTER_TO_SIZE(key);
    }
#endif

    return G_TYPE_INVALID;
}

static void
gir_type_free_types(void)
{
    g_debug("Freeing gtype hash table");
    g_hash_table_remove_all(gir_type_gtype_hash);
#if ENABLE_GIR_TYPE_SCM_HASH
    g_hash_table_remove_all(gir_type_scm_hash);
#endif
    _free_boxed_funcs();
}

SCM
gir_type_get_scheme_type(GType gtype)
{
    gpointer *scm_ptr = g_hash_table_lookup(gir_type_gtype_hash, GSIZE_TO_POINTER(gtype));
    if (scm_ptr)
        return SCM_PACK_POINTER(scm_ptr);
    return SCM_BOOL_F;
}

////////////////////////////////////////////////////////////////
// GUILE API

// This Guile API routine returns the integer GType ID of a given GIR
// foreign object type, or an instance of a GIR foreign object type.
// It returns #f on failure.
static SCM
scm_type_get_gtype(SCM x)
{
    GType type = gir_type_get_gtype_from_obj(x);
    if (type != G_TYPE_INVALID)
        return scm_from_size_t(type);
    else
        return SCM_BOOL_F;
}

// Given an integer that is a GType, this returns an associated Guile
// type, if one exists.
static SCM
scm_type_gtype_get_scheme_type(SCM s_gtype)
{
    SCM_ASSERT_TYPE(scm_is_integer(s_gtype), s_gtype, SCM_ARG1, "gtype-get-scheme-type",
                    "integer");
    GType type = scm_to_uintptr_t(s_gtype);
    return gir_type_get_scheme_type(type);
}

// Given an integer that is a GType, this returns a Guile string of
// the type's name.
static SCM
scm_type_gtype_get_name(SCM s_gtype)
{
    SCM_ASSERT_TYPE(scm_is_integer(s_gtype), s_gtype, SCM_ARG1, "gtype-get-name", "integer");
    GType type = scm_to_uintptr_t(s_gtype);
    if (g_hash_table_contains(gir_type_gtype_hash, GSIZE_TO_POINTER(type)))
        return scm_from_utf8_string(g_type_name(type));

    return scm_from_utf8_string("invalid");
}

// Given a Guile integer that is a GType, this returns a Guile integer
// that is the GType of this type's parent.
static SCM
scm_type_gtype_get_parent(SCM s_gtype)
{
    SCM_ASSERT_TYPE(scm_is_integer(s_gtype), s_gtype, SCM_ARG1, "gtype-get-parent", "integer");
    GType type = scm_to_uintptr_t(s_gtype);

    if (g_hash_table_contains(gir_type_gtype_hash, GSIZE_TO_POINTER(type)))
        return scm_from_uintptr_t(g_type_parent(type));
    return SCM_BOOL_F;
}

// Given a Guile integer that is a GType, this returns a Guile integer
// that is the GType of this type's fundamental parent.
static SCM
scm_type_gtype_get_fundamental(SCM s_gtype)
{
    SCM_ASSERT_TYPE(scm_is_integer(s_gtype), s_gtype, SCM_ARG1, "gtype-get-fundamental",
                    "integer");
    GType type = scm_to_uintptr_t(s_gtype);

    if (g_hash_table_contains(gir_type_gtype_hash, GSIZE_TO_POINTER(type)))
        return scm_from_uintptr_t(g_type_fundamental(type));
    return SCM_BOOL_F;
}

// Given a Guile integer that is a GType, this returns a list of Guile
// integers that are the children types of this GType
static SCM
scm_type_gtype_get_children(SCM s_gtype)
{
    SCM_ASSERT_TYPE(scm_is_integer(s_gtype), s_gtype, SCM_ARG1, "gtype-get-children", "integer");
    GType type = scm_to_uintptr_t(s_gtype);

    SCM ret = SCM_EOL;

    if (g_hash_table_contains(gir_type_gtype_hash, GSIZE_TO_POINTER(type))) {
        GType *children;
        guint n_children, i;
        SCM entry;

        children = g_type_children(type, &n_children);
        for (i = 0; i < n_children; i++) {
            entry = scm_from_uintptr_t(children[i]);
            ret = scm_append(scm_list_2(ret, scm_list_1(entry)));
        }
        g_free(children);
    }

    return ret;
}

// Given a Guile integer that is a GType, this returns a list of Guile
// integers that are the interface types of this GType
static SCM
scm_type_gtype_get_interfaces(SCM s_gtype)
{
    SCM_ASSERT_TYPE(scm_is_integer(s_gtype), s_gtype, SCM_ARG1, "gtype-get-interfaces", "integer");
    GType type = scm_to_uintptr_t(s_gtype);

    SCM ret = SCM_EOL;

    if (g_hash_table_contains(gir_type_gtype_hash, GSIZE_TO_POINTER(type))) {
        GType *interfaces;
        guint n_interfaces, i;
        SCM entry;

        interfaces = g_type_interfaces(type, &n_interfaces);
        for (i = 0; i < n_interfaces; i++) {
            entry = scm_from_uintptr_t(interfaces[i]);
            ret = scm_append(scm_list_2(ret, scm_list_1(entry)));
        }
        g_free(interfaces);
    }

    return ret;
}

// Given a Guile integer that is a GType, this returns an integer
// which is the depth of this GType in its GObject class structure
static SCM
scm_type_gtype_get_depth(SCM s_gtype)
{
    SCM_ASSERT_TYPE(scm_is_integer(s_gtype), s_gtype, SCM_ARG1, "gtype-get-depth", "integer");
    GType type = scm_to_uintptr_t(s_gtype);

    if (g_hash_table_contains(gir_type_gtype_hash, GSIZE_TO_POINTER(type)))
        return scm_from_uint(g_type_depth(type));
    return SCM_BOOL_F;
}

// Given a Guile integer that is a GType, this returns #t
// if the GType is a GObject interface type.
static SCM
scm_type_gtype_is_interface_p(SCM s_gtype)
{
    SCM_ASSERT_TYPE(scm_is_integer(s_gtype), s_gtype, SCM_ARG1, "gtype-is-interface?", "integer");
    GType type = scm_to_uintptr_t(s_gtype);

    if (g_hash_table_contains(gir_type_gtype_hash, GSIZE_TO_POINTER(type)))
        return scm_from_bool(G_TYPE_IS_INTERFACE(type));
    return SCM_BOOL_F;
}

// Given a Guile integer that is a GType, this returns #t if the GType
// is a GObject classed type.
static SCM
scm_type_gtype_is_classed_p(SCM s_gtype)
{
    SCM_ASSERT_TYPE(scm_is_integer(s_gtype), s_gtype, SCM_ARG1, "gtype-is-classed?", "integer");
    GType type = scm_to_uintptr_t(s_gtype);

    if (g_hash_table_contains(gir_type_gtype_hash, GSIZE_TO_POINTER(type)))
        return scm_from_bool(G_TYPE_IS_CLASSED(type));
    return SCM_BOOL_F;
}

// Given a Guile integer that is a GType, this returns #t if the GType
// is a GObject type can be used to instantiate objects.
static SCM
scm_type_gtype_is_instantiatable_p(SCM s_gtype)
{
    SCM_ASSERT_TYPE(scm_is_integer(s_gtype), s_gtype, SCM_ARG1, "gtype-is-instantiatable?",
                    "integer");
    GType type = scm_to_uintptr_t(s_gtype);

    if (g_hash_table_contains(gir_type_gtype_hash, GSIZE_TO_POINTER(type)))
        return scm_from_bool(G_TYPE_IS_INSTANTIATABLE(type));
    return SCM_BOOL_F;
}

// Given a Guile integer that is a GType, this returns #t if the GType
// is a GObject type than can be base class for another type.
static SCM
scm_type_gtype_is_derivable_p(SCM s_gtype)
{
    SCM_ASSERT_TYPE(scm_is_integer(s_gtype), s_gtype, SCM_ARG1, "gtype-is-derivable?", "integer");
    GType type = scm_to_uintptr_t(s_gtype);

    if (g_hash_table_contains(gir_type_gtype_hash, GSIZE_TO_POINTER(type)))
        return scm_from_bool(G_TYPE_IS_DERIVABLE(type));
    return SCM_BOOL_F;
}

// Given two Guile integers that are GTypes, with the first this
// returns #t if the second class is a base class to the first class
static SCM
scm_type_gtype_is_a_p(SCM gself, SCM gparent)
{
    SCM_ASSERT_TYPE(scm_is_integer(gself), gself, SCM_ARG1, "gtype-is-a?", "integer");
    SCM_ASSERT_TYPE(scm_is_integer(gparent), gparent, SCM_ARG2, "gtype-is-a?", "integer");

    GType self, parent;
    self = scm_to_uintptr_t(gself);
    parent = scm_to_uintptr_t(gparent);

    if (g_hash_table_contains(gir_type_gtype_hash, GSIZE_TO_POINTER(self))
        && g_hash_table_contains(gir_type_gtype_hash, GSIZE_TO_POINTER(parent)))
        return scm_from_bool(g_type_is_a(self, parent));
    return SCM_BOOL_F;
}

static SCM
scm_type_dump_type_table(void)
{
    GHashTableIter iter;
    gpointer key, value;
    SCM list = SCM_EOL;

    g_hash_table_iter_init(&iter, gir_type_gtype_hash);
    while (g_hash_table_iter_next(&iter, &key, &value)) {
        SCM entry;
        SCM fo_type;
        size_t skey = GPOINTER_TO_SIZE(key);

        if (value)
            fo_type = SCM_PACK_POINTER(value);
        else
            fo_type = SCM_BOOL_F;
        entry =
            scm_list_3(scm_from_size_t(skey), scm_from_utf8_string(g_type_name(skey)), fo_type);
        list = scm_append(scm_list_2(list, scm_list_1(entry)));
    }
    return list;
}

static SCM
scm_allocate_boxed(SCM boxed_type)
{
    SCM_ASSERT_TYPE(SCM_SUBCLASSP(boxed_type, gig_boxed_type), boxed_type, SCM_ARG1,
                    "%allocate-boxed", "boxed type");
    SCM s_size = scm_class_ref(boxed_type, sym_size);

    gsize size = scm_to_size_t(s_size);

    if (size == 0)
        scm_out_of_range("%allocate-boxed", s_size);

    gpointer boxed = g_malloc0(size);
    GigTypeUnrefFunction unref;
    unref = (GigTypeUnrefFunction) scm_to_pointer(scm_class_ref(boxed_type, sym_unref));
    SCM pointer = scm_from_pointer(boxed, unref);

    return scm_call_3(make_instance_proc, boxed_type, kwd_ptr, pointer);
}

void
gig_type_associate(GType gtype, SCM stype)
{
    g_hash_table_insert(gir_type_gtype_hash, GSIZE_TO_POINTER(gtype), SCM_UNPACK_POINTER(stype));
#if ENABLE_GIR_TYPE_SCM_HASH
    g_hash_table_insert(gir_type_scm_hash, SCM_UNPACK_POINTER(stype), GSIZE_TO_POINTER(gtype));
#endif
    SCM module = scm_current_module();
    SCM name = scm_class_name(stype);
    scm_module_define(module, name, stype);
    scm_module_export(module, scm_list_1(name));
}

void
gir_type_define_fundamental(GType type, SCM extra_supers,
                            GigTypeRefFunction ref, GigTypeUnrefFunction unref)
{
    scm_dynwind_begin(0);
    char *class_name = scm_dynwind_or_bust("%define-compact-type",
                                           gir_type_class_name_from_gtype(type));

    SCM new_type = scm_call_4(make_fundamental_proc,
                              scm_from_utf8_symbol(class_name),
                              extra_supers,
                              scm_from_pointer(ref, NULL),
                              scm_from_pointer(unref, NULL));

    gig_type_associate(type, new_type);
    scm_dynwind_end();
}

static SCM make_fundamental_proc;

void
gir_init_types(void)
{
    gig_fundamental_type = scm_c_private_ref("gi oop", "<GFundamental>");
    gig_boxed_type = scm_c_private_ref("gi oop", "<GBoxed>");
    make_class_proc = scm_c_public_ref("oop goops", "make-class");
    make_instance_proc = scm_c_public_ref("oop goops", "make");
    make_fundamental_proc = scm_c_private_ref("gi oop", "%make-fundamental-class");
    kwd_name = scm_from_utf8_keyword("name");
    kwd_ptr = scm_from_utf8_keyword("ptr");
    sym_ptr = scm_from_utf8_symbol("ptr");
    sym_ref = scm_from_utf8_symbol("ref");
    sym_unref = scm_from_utf8_symbol("unref");
    sym_size = scm_from_utf8_symbol("size");

    gir_type_gtype_hash = g_hash_table_new(g_direct_hash, g_direct_equal);
#if ENABLE_GIR_TYPE_SCM_HASH
    gir_type_scm_hash = g_hash_table_new(g_direct_hash, g_direct_equal);
#endif

    gir_type_define_fundamental(G_TYPE_OBJECT, SCM_EOL,
                                (GigTypeRefFunction) g_object_ref_sink,
                                (GigTypeUnrefFunction) g_object_unref);
    gir_type_define_fundamental(G_TYPE_INTERFACE, SCM_EOL, NULL, NULL);
    gig_type_associate(G_TYPE_BOXED, gig_boxed_type);
    gir_type_define_fundamental(G_TYPE_PARAM,
                                scm_list_1(scm_c_public_ref("oop goops",
                                                            "<applicable-struct-with-setter>")),
                                (GigTypeRefFunction) g_param_spec_ref_sink,
                                (GigTypeUnrefFunction) g_param_spec_unref);
    gir_type_define_fundamental(G_TYPE_VARIANT, SCM_EOL,
                                (GigTypeRefFunction) g_variant_ref_sink,
                                (GigTypeUnrefFunction) g_variant_unref);

    gig_object_type = gir_type_get_scheme_type(G_TYPE_OBJECT);
    gig_paramspec_type = gir_type_get_scheme_type(G_TYPE_PARAM);

    atexit(gir_type_free_types);

#define D(x)                                                            \
    do                                                                  \
    {                                                                   \
        gir_type_register(x);                                           \
        scm_permanent_object(scm_c_define(#x, scm_from_uintptr_t(x)));  \
        scm_c_export(#x, NULL);                                         \
    } while (0)

    D(G_TYPE_NONE);
    /* D(G_TYPE_INTERFACE); */
    D(G_TYPE_CHAR);
    D(G_TYPE_UCHAR);
    D(G_TYPE_BOOLEAN);
    D(G_TYPE_INT);
    D(G_TYPE_UINT);
    D(G_TYPE_LONG);
    D(G_TYPE_ULONG);
    D(G_TYPE_INT64);
    D(G_TYPE_UINT64);
    D(G_TYPE_ENUM);
    D(G_TYPE_FLAGS);
    D(G_TYPE_FLOAT);
    D(G_TYPE_DOUBLE);
    /* D(G_TYPE_STRING); */
    /* D(G_TYPE_GTYPE); */
    /* D(G_TYPE_VARIANT); */
    /* D(G_TYPE_CHECKSUM); */
    /* D(G_TYPE_POINTER); */
#undef D

    scm_c_define_gsubr("get-gtype", 1, 0, 0, scm_type_get_gtype);
    scm_c_define_gsubr("gtype-get-scheme-type", 1, 0, 0, scm_type_gtype_get_scheme_type);
    scm_c_define_gsubr("gtype-get-name", 1, 0, 0, scm_type_gtype_get_name);
    scm_c_define_gsubr("gtype-get-parent", 1, 0, 0, scm_type_gtype_get_parent);
    scm_c_define_gsubr("gtype-get-fundamental", 1, 0, 0, scm_type_gtype_get_fundamental);
    scm_c_define_gsubr("gtype-get-children", 1, 0, 0, scm_type_gtype_get_children);
    scm_c_define_gsubr("gtype-get-interfaces", 1, 0, 0, scm_type_gtype_get_interfaces);
    scm_c_define_gsubr("gtype-get-depth", 1, 0, 0, scm_type_gtype_get_depth);
    scm_c_define_gsubr("gtype-is-interface?", 1, 0, 0, scm_type_gtype_is_interface_p);
    scm_c_define_gsubr("gtype-is-classed?", 1, 0, 0, scm_type_gtype_is_classed_p);
    scm_c_define_gsubr("gtype-is-instantiatable?", 1, 0, 0, scm_type_gtype_is_instantiatable_p);
    scm_c_define_gsubr("gtype-is-derivable?", 1, 0, 0, scm_type_gtype_is_derivable_p);
    scm_c_define_gsubr("gtype-is-a?", 2, 0, 0, scm_type_gtype_is_a_p);
    scm_c_define_gsubr("%gtype-dump-table", 0, 0, 0, scm_type_dump_type_table);
    scm_c_define_gsubr("%allocate-boxed", 1, 0, 0, scm_allocate_boxed);
    scm_c_export("get-gtype",
                 "gtype-get-scheme-type",
                 "gtype-get-name",
                 "gtype-get-parent",
                 "gtype-get-fundamental",
                 "gtype-get-children",
                 "gtype-get-interfaces",
                 "gtype-get-depth",
                 "gtype-is-interface?",
                 "gtype-is-classed?",
                 "gtype-is-instantiatable?",
                 "gtype-is-derivable?", "gtype-is-a?", "%gtype-dump-table", NULL);
}
