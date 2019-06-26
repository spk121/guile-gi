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

// Holds for foreign function info for predicates
GSList *gir_type_predicates_list = NULL;

// Holds information about dynamically created C procedures used in
// Guile predicate procedure such as 'MyArray?'
typedef struct _GirPredicate
{
    ffi_closure *closure;
    ffi_cif cif;
    void *function_ptr;
    SCM fo_type;
} GirPredicate;


static SCM   gir_type_make_fo_type_from_name(const gchar *type_class_name);
static void *gir_type_create_predicate(const char *name, SCM fo_type);
static void  gir_type_predicate_binding(ffi_cif *cif, void *ret, void **ffi_args, void *user_data);

// Given a GType integer but no introspection information, this stores
// that GType in our hash table of known types without creating an
// associated foreign object type.
void
gir_type_register(GType gtype)
{
    g_assert(gtype != G_TYPE_NONE);

    GType parent = g_type_parent(gtype);
    if (parent != 0)
        gir_type_register (parent);

    if (!g_hash_table_contains(gir_type_gtype_hash, GSIZE_TO_POINTER(gtype)))
    {
        g_hash_table_insert(gir_type_gtype_hash, GSIZE_TO_POINTER(gtype), NULL);
        g_debug("Registering a new GType: %zu -> %s", gtype, g_type_name(gtype));
    }
}

// Given introspection info from a typelib library for a given GType,
// this makes a new Guile foreign object type and its associated predicate,
// and it stores the type in our hash table of known types.
void
gir_type_define(GType gtype)
{
    g_assert (GSIZE_TO_POINTER(gtype) != NULL);
    // Make a foreign object type for instances of this GType.
    // All of our custom introspected foreign object types will
    // have the same slots.
    // ob_type,ob_refcnt,obj,inst_dict,weakreflist,flags

    gboolean newkey;
    gpointer orig_key, value;
    newkey = g_hash_table_lookup_extended(gir_type_gtype_hash,
                                          GSIZE_TO_POINTER(gtype),
                                          &orig_key,
                                          &value);
    if (newkey == FALSE || value == NULL)
    {
        GType parent = g_type_parent(gtype);
        if (parent != 0)
            gir_type_register (parent);

        gchar *type_class_name = g_strdup_printf("<%s>", g_type_name(gtype));
        SCM fo_type = gir_type_make_fo_type_from_name(type_class_name);
        scm_permanent_object(scm_c_define(type_class_name, fo_type));
        scm_c_export (type_class_name, NULL);
        newkey = g_hash_table_insert(gir_type_gtype_hash,
                                     GSIZE_TO_POINTER(gtype),
                                     SCM_UNPACK_POINTER(fo_type));
        if (newkey)
            g_debug("Creating a new GType foreign object type: %zu -> %s aka %s", gtype, type_class_name, g_type_name(gtype));
        else
            g_debug("Updating a GType foreign object type: %zu -> %s aka %s", gtype, type_class_name, g_type_name(gtype));
#if ENABLE_GIR_TYPE_SCM_HASH
        g_hash_table_insert (gir_type_scm_hash,
                             SCM_UNPACK_POINTER (fo_type),
                             GSIZE_TO_POINTER (gtype));
#endif

        g_free(type_class_name);
#if ENABLE_GIR_TYPE_SCM_HASH
        g_debug ("Hash table sizes %d %d", g_hash_table_size(gir_type_gtype_hash),
                 g_hash_table_size (gir_type_scm_hash));
#else
        g_debug ("Hash table size %d", g_hash_table_size(gir_type_gtype_hash));
#endif

        gchar *type_name = gname_to_scm_name (g_type_name (gtype));
        gchar *predicate_name = g_strdup_printf("%s?", type_name);
        gpointer func = gir_type_create_predicate(predicate_name, fo_type);
        scm_c_define_gsubr(predicate_name, 1, 0, 0, func);
        scm_c_export(predicate_name, NULL);

        g_free (type_name);
        g_free (predicate_name);
    }
    else
        g_debug("GType foriegn_object_type already exists for: %zu -> %s",
                gtype, g_type_name(gtype));
}

// This makes an instance of a Guile foreign object type for a GObject pointer.
// FIXME: handle the TRANSFER argument.
SCM
gir_type_make_object(GType gtype, gpointer obj, GITransfer transfer)
{
    g_assert (GSIZE_TO_POINTER(gtype) != NULL);
    g_assert (obj != NULL);

    if (gtype == G_TYPE_OBJECT)
        gtype = G_OBJECT_TYPE(obj);

    gpointer scm_ptr = g_hash_table_lookup (gir_type_gtype_hash,
                                            GSIZE_TO_POINTER(gtype));

    if (scm_ptr == NULL)
        return SCM_BOOL_F;

    void *params[6] = {GSIZE_TO_POINTER(gtype),
                       GINT_TO_POINTER(1),
                       obj,
                       NULL,
                       NULL,
                       GINT_TO_POINTER(0)};

    return scm_make_foreign_object_n(SCM_PACK_POINTER(scm_ptr), 6, params);
}

// Given the Guile name for a type, this creates a Guile foreign object type.
static SCM
gir_type_make_fo_type_from_name(const gchar *type_class_name)
{
    g_assert (type_class_name != NULL);

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
    return scm_make_foreign_object_type(sname, slots, NULL);
}

// Given a Guile foreign object type, this creates a Guile type predicate
// function for that type. e.g. if fo_type is <MyArray>, this creates
// the procedure 'MyArray?'
static void *
gir_type_create_predicate(const char *name, SCM fo_type)
{
    g_assert (name != NULL);

    GirPredicate *gp = g_new0(GirPredicate, 1);

    ffi_type **ffi_args = NULL;
    ffi_type *ffi_ret_type;

    // STEP 1
    // Allocate the block of memory that FFI uses to hold a closure object,
    // and set a pointer to the corresponding executable address.
    gp->fo_type = fo_type;
    gp->closure = ffi_closure_alloc(sizeof(ffi_closure),
                                    &(gp->function_ptr));

    g_return_val_if_fail(gp->closure != NULL, NULL);
    g_return_val_if_fail(gp->function_ptr != NULL, NULL);

    // STEP 2
    // Next, we begin to construct an FFI_CIF to describe the function call.

    // Initialize the argument info vectors.
    ffi_args = g_new0(ffi_type *, 1);

    // Our argument will be SCM, so we use pointer storage.
    ffi_args[0] = &ffi_type_pointer;
    // The return type is also SCM, for which we use a pointer.
    ffi_ret_type = &ffi_type_pointer;

    // Initialize the CIF Call Interface Struct.
    ffi_status prep_ok;
    prep_ok = ffi_prep_cif(&(gp->cif),
                           FFI_DEFAULT_ABI,
                           1,
                           ffi_ret_type,
                           ffi_args);

    if (prep_ok != FFI_OK)
        scm_misc_error("gir-type-create-predicate",
                       "closure call interface preparation error #~A",
                       scm_list_1(scm_from_int(prep_ok)));

    // STEP 3
    // Initialize the closure
    ffi_status closure_ok;
    closure_ok = ffi_prep_closure_loc(gp->closure,
                                      &(gp->cif),
                                      gir_type_predicate_binding,
                                      gp,                 // The 'user-data' passed to the function
                                      gp->function_ptr);

    if (closure_ok != FFI_OK)
        scm_misc_error("gir-type-create-predicate",
                       "closure location preparation error #~A",
                       scm_list_1(scm_from_int(closure_ok)));

    g_debug ("Created predicate %s", name);

    // We stash the predicates in a list so they may be freed
    // on shutdown.
    gir_type_predicates_list = g_slist_append(gir_type_predicates_list, gp);

    // Return the function pointer.
    return gp->function_ptr;
}

// This is the core of a dynamically generated type predicate.
// It converts an FFI argument to a SCM foreign object.
// And checks if that foreign object has the type this
// predicate is testing for.
static void gir_type_predicate_binding(ffi_cif *cif, void *ret, void **ffi_args,
                                       void *user_data)
{
    GirPredicate *gp = user_data;

    g_assert (cif != NULL);
    g_assert (ret != NULL);
    g_assert (ffi_args != NULL);
    g_assert (user_data != NULL);

    unsigned int n_args = cif->nargs;

    g_assert (n_args == 1);

    SCM arg = SCM_PACK(*(scm_t_bits *)(ffi_args[0]));

    if (SCM_IS_A_P(arg, gp->fo_type))
        *(ffi_arg *)ret = SCM_UNPACK(SCM_BOOL_T);
    else
        *(ffi_arg *)ret = SCM_UNPACK(SCM_BOOL_F);
}

// This routine returns the integer GType ID of a scheme object, that is
// - already a GType ID encoded as size_t,
// - a foreign object for a GType
// - a foreign object for an object instance
// The last one is accidental, as internally `gir_type_get_gtype_from_obj'
// is used. This may change in future and should not be relied on.
GType
scm_to_gtype (SCM x)
{
    if (scm_is_integer (x))
        return scm_to_size_t (x);
    else
        return gir_type_get_gtype_from_obj (x);
}

// This routine returns the integer GType ID of a given
// GIR foreign object type, or an instance of a GIR foreign object type.
// It returns #f on failure.
GType
gir_type_get_gtype_from_obj(SCM x)
{
#if ENABLE_GIR_TYPE_SCM_HASH
    gpointer value;
    if ((value = g_hash_table_lookup (gir_type_scm_hash,
                                      SCM_UNPACK_POINTER (x))))
        return GPOINTER_TO_SIZE (value);
    else if (SCM_INSTANCEP (x) &&
             (value = g_hash_table_lookup (gir_type_scm_hash,
                                           SCM_UNPACK_POINTER (SCM_CLASS_OF (x)))))
        return GPOINTER_TO_SIZE (value);
#else
    SCM klass;
    if (SCM_INSTANCEP (x))
        klass = SCM_CLASS_OF (x);
    else
        klass = x;

    GHashTableIter iter;
    gpointer key, value;

    g_hash_table_iter_init (&iter, gir_type_gtype_hash);
    while (g_hash_table_iter_next (&iter, &key, &value))
    {
        SCM svalue = SCM_PACK_POINTER(value);
        if (scm_is_eq(klass, svalue)
            || scm_is_eq(x, svalue))
            return GPOINTER_TO_SIZE(key);
    }
#endif

    return G_TYPE_INVALID;
}

#ifdef GIR_FREE_MEMORY
// Note that since there is not such thing as undefining
// a gsubr (no scm_c_undefine_gsubr), we don't normally want
// to free any of our ffi predicates, except under Valgrind.
static void gir_type_predicate_free (GirPredicate *gp)
{
    ffi_closure_free(gp->closure);
    g_free(gp);
}

void gir_type_free_predicates (void)
{
    g_debug ("Freeing type predicates");
    g_slist_free_full (gir_type_predicates_list, gir_type_predicate_free);
}

#endif

static void gir_type_free_types (void)
{
    g_debug ("Freeing gtype hash table");
    g_hash_table_remove_all (gir_type_gtype_hash);
#if ENABLE_GIR_TYPE_SCM_HASH
    g_hash_table_remove_all (gir_type_scm_hash);
#endif
}

SCM
gir_type_get_scheme_type (GType gtype)
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
    GType type = gir_type_get_gtype_from_obj (x);
    if (type != G_TYPE_INVALID)
        return scm_from_size_t (type);
    else
        return SCM_BOOL_F;
}

// Given an integer that is a GType, this returns an associated Guile
// type, if one exists.
static SCM
scm_type_gtype_get_scheme_type(SCM s_gtype)
{
    SCM_ASSERT_TYPE(scm_is_integer(s_gtype), s_gtype, SCM_ARG1, "gtype-get-scheme-type", "integer");
    GType type = scm_to_uintptr_t(s_gtype);
    return gir_type_get_scheme_type (type);
}

// Given an integer that is a GType, this returns a Guile string of
// the type's name.
static SCM
scm_type_gtype_get_name(SCM s_gtype)
{
    SCM_ASSERT_TYPE(scm_is_integer(s_gtype), s_gtype, SCM_ARG1, "gtype-get-name", "integer");
    GType type = scm_to_uintptr_t(s_gtype);
    if (g_hash_table_contains(gir_type_gtype_hash, GSIZE_TO_POINTER(type)))
        return scm_from_utf8_string (g_type_name (type));

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
        return scm_from_uintptr_t (g_type_parent (type));
    return SCM_BOOL_F;
}

// Given a Guile integer that is a GType, this returns a Guile integer
// that is the GType of this type's fundamental parent.
static SCM
scm_type_gtype_get_fundamental(SCM s_gtype)
{
    SCM_ASSERT_TYPE(scm_is_integer(s_gtype), s_gtype, SCM_ARG1, "gtype-get-fundamental", "integer");
    GType type = scm_to_uintptr_t(s_gtype);

    if (g_hash_table_contains(gir_type_gtype_hash, GSIZE_TO_POINTER(type)))
        return scm_from_uintptr_t (g_type_fundamental (type));
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

    if (g_hash_table_contains(gir_type_gtype_hash, GSIZE_TO_POINTER(type)))
    {
        GType *children;
        guint n_children, i;
        SCM entry;

        children = g_type_children(type, &n_children);
        for (i = 0; i < n_children; i++)
        {
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

    if (g_hash_table_contains(gir_type_gtype_hash, GSIZE_TO_POINTER(type)))
    {
        GType *interfaces;
        guint n_interfaces, i;
        SCM entry;

        interfaces = g_type_interfaces(type, &n_interfaces);
        for (i = 0; i < n_interfaces; i++)
        {
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
    SCM_ASSERT_TYPE(scm_is_integer(s_gtype), s_gtype, SCM_ARG1, "gtype-is-instantiatable?", "integer");
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

    g_hash_table_iter_init (&iter, gir_type_gtype_hash);
    while (g_hash_table_iter_next (&iter, &key, &value))
    {
        SCM entry;
        SCM fo_type;

        if (value)
            fo_type = SCM_PACK_POINTER(value);
        else
            fo_type = SCM_BOOL_F;
        entry = scm_list_3 (scm_from_size_t(key),
                            scm_from_utf8_string (g_type_name (key)),
                            fo_type);
        list = scm_append(scm_list_2 (list, scm_list_1 (entry)));
    }
    return list;
}

static SCM
scm_type_cast(SCM s_obj, SCM s_fo_type)
{
    gpointer obj = scm_foreign_object_ref(s_obj, OBJ_SLOT);
    GType type = gir_type_get_gtype_from_obj (s_fo_type);
    return gir_type_make_object(type, obj, 0);
}

void gir_init_types(void)
{

    gir_type_gtype_hash = g_hash_table_new(g_direct_hash, g_direct_equal);
#if ENABLE_GIR_TYPE_SCM_HASH
    gir_type_scm_hash = g_hash_table_new(g_direct_hash, g_direct_equal);
#endif

#ifdef GIR_FREE_MEMORY
    atexit (gir_type_free_predicates);
#endif
    atexit (gir_type_free_types);

#define D(x)                                                            \
    do                                                                  \
    {                                                                   \
        gir_type_register(x);                                           \
        scm_permanent_object(scm_c_define(#x, scm_from_uintptr_t(x)));  \
        scm_c_export(#x, NULL);                                         \
    } while (0)

    // D(G_TYPE_NONE);
    D(G_TYPE_INTERFACE);
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
    D(G_TYPE_STRING);
    D(G_TYPE_BOXED);
    D(G_TYPE_PARAM);
    D(G_TYPE_OBJECT);
    D(G_TYPE_GTYPE);
    D(G_TYPE_VARIANT);
    D(G_TYPE_CHECKSUM);
    D(G_TYPE_POINTER);
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
    scm_c_define_gsubr("cast", 2, 0, 0, scm_type_cast);
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
                 "gtype-is-derivable?",
                 "gtype-is-a?",
                 "%gtype-dump-table",
                 "cast",
                 NULL);
}
