// Copyright (C) 2018, 2019, 2020, 2021, 2022 Michael L. Gran

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>

#include <assert.h>
#include <stdio.h>
#include <libguile.h>
#include <girepository.h>
#include <ffi.h>
#include "x.h"
#include "type.h"
#include "object.h"
#include "type_private.h"
#include "y/guile.h"
#include "y/util.h"

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
// pointer type, on the Guile side, a <GtkWindow> object type
// would be created.

// The Guile foreign object types that get created primarily are just
// boxes that hold C Pointers.  The <GtkWindow> foreign object type
// has a slot "value".  Instances of the <GtkWindow> object type
// will use the "value" slot to hold a C GtkWindow pointer.

// These Guile types also have a few class-allocated slots, such as
// "ref", which points to a class-specific ref-function and "unref",
// which points to the unref-function.

/*
 * When parsing a Typelib file, an argument type is a sort of a triple
 * - type_tag: either a simple type like "unsigned", else "INTERFACE"
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

// Maps GType to SCM
static keyval_t *gtype_scm_store = NULL;
// Maps string to SCM
static strval_t *name_scm_store = NULL;
// Maps SCM to GType
static keyval_t *scm_gtype_store = NULL;

SCM enum_type;
SCM flags_type;
SCM object_type;
SCM interface_type;
SCM paramspec_type;
SCM value_type;
SCM closure_type;

static SCM make_class_proc;
static SCM kwd_name;
SCM sym_obarray;

char *
class_name_from_gtype(GType gtype)
{
    size_t len = strlen("<>") + strlen(g_type_name(gtype)) + 1;
    char *str = malloc(len);
    snprintf(str, len, "<%s>", g_type_name(gtype));
    return str;
}

// Returns TRUE if TYPE is contained in the hash table of known types.
static int
is_registered(GType gtype)
{
    scm_t_bits x;
    x = keyval_find_entry(gtype_scm_store, gtype);
    return x != 0;
}

static void
register_self(GType gtype, SCM stype)
{
    GType parent = g_type_parent(gtype);
    scm_t_bits pval;
    char *stype_str = NULL, *old_stype_str = NULL;

    pval = keyval_find_entry(gtype_scm_store, gtype);

    // #<undefined> beats NULL. Anything defined beats #<undefined>.

    if (pval != 0 && scm_is_eq(stype, SCM_PACK(pval)))
        return;
    if (pval != 0 && !SCM_UNBNDP(SCM_PACK(pval)) && SCM_UNBNDP(stype))
        return;
    stype_str = scm_write_to_utf8_stringn(stype, 80);
    keyval_add_entry(gtype_scm_store, gtype, SCM_UNPACK(stype));
    if (pval == 0) {
        if (parent)
            debug_load("%s - registering a new %s type for %zx as %s", g_type_name(gtype),
                           g_type_name(parent), gtype, stype_str);
        else
            debug_load("%s - registering a new type for %zx as %s", g_type_name(gtype), gtype,
                           stype_str);
    }
    else {
        old_stype_str = scm_write_to_utf8_stringn(SCM_PACK(pval), 80);
        if (parent)
            debug_load("%s - re-registering %s type for %zx from %s to %s", g_type_name(gtype),
                           g_type_name(parent), gtype, old_stype_str, stype_str);
        else
            debug_load("%s - re-registering type for %zx from %s to %s", g_type_name(gtype),
                           gtype, old_stype_str, stype_str);
        free(old_stype_str);
    }
    free(stype_str);
}

// Given a GType integer but no introspection information, this stores
// that GType in our hash table of known types without creating an
// associated foreign object type.
void
register_type(GType gtype, SCM stype)
{
    GType parent = g_type_parent(gtype);
    if (parent != 0)
        register_self(parent, SCM_UNDEFINED);
    register_self(gtype, stype);
}

static SCM make_instance_proc;
static SCM make_fundamental_proc;
static SCM kwd_value;
static SCM sym_value;
static SCM sym_ref;
static SCM sym_unref;
static SCM sym_size;

SCM
transfer_object(GType type, void *ptr, GITransfer transfer)
{
    if (G_TYPE_IS_CLASSED(type))
        type = G_OBJECT_TYPE(ptr);

    debug_transfer("type_transfer_object(%s, %p, %d)", g_type_name(type), ptr, transfer);

    SCM scm_type = get_scheme_type(type);
    return_val_if_fail(SCM_CLASSP(scm_type), SCM_BOOL_F);
    Type_ref_function ref;
    ref = (Type_ref_function)scm_to_pointer(scm_class_ref(scm_type, sym_ref));
    Type_unref_function unref;
    unref = (Type_unref_function)scm_to_pointer(scm_class_ref(scm_type, sym_unref));

    SCM pointer;
    switch (transfer) {
    case GI_TRANSFER_NOTHING:
        pointer = scm_from_pointer(ref(ptr), unref);
        break;

    case GI_TRANSFER_CONTAINER:
    case GI_TRANSFER_EVERYTHING:
    default:
        pointer = scm_from_pointer(ptr, unref);
        break;
    }

    return scm_call_3(make_instance_proc, scm_type, kwd_value, pointer);
}

static SCM fundamental_type;
static SCM boxed_type;

int
check_object(SCM obj)
{
    return SCM_IS_A_P(obj, fundamental_type);
}

int
check_typed_object(SCM obj, SCM expected_type)
{
    return SCM_IS_A_P(obj, expected_type);
}

void *
peek_typed_object(SCM obj, SCM expected_type)
{
    return_val_if_fail(SCM_IS_A_P(obj, expected_type), NULL);
    return scm_to_pointer(scm_slot_ref(obj, sym_value));
}

void *
peek_object(SCM obj)
{
    return peek_typed_object(obj, fundamental_type);
}

static SCM type_less_p_proc;
static SCM sym_sort_key;

static SCM
type_less_p(SCM a, SCM b)
{
    SCM key_a = scm_object_property(a, sym_sort_key);
    SCM key_b = scm_object_property(b, sym_sort_key);

    // reverse order
    return scm_less_p(key_b, key_a);
}

SCM
associate_type(GType gtype, SCM stype)
{
    register_self(gtype, stype);
    scm_set_object_property_x(stype, sym_sort_key,
                              scm_from_size_t(keyval_size(gtype_scm_store)));
    keyval_add_entry(scm_gtype_store, SCM_UNPACK(stype), gtype);
    return scm_class_name(stype);
}

SCM
define_type_with_info(GIRegisteredTypeInfo *info, SCM dsupers, SCM slots)
{
    if (g_registered_type_info_get_g_type(info) != G_TYPE_NONE) {
        g_critical("type_define_with_info used when GType was available, "
                   "use type_define or type_define_full instead.");
        return SCM_UNDEFINED;
    }

    char *_name = g_registered_type_info_get_qualified_name(info);
    assert(_name != NULL);
    scm_t_bits _value = strval_find_entry(name_scm_store, _name);

    SCM cls;
    if (_value) {
        free(_name);
        cls = SCM_PACK(_value);
    }
    else {
        int len = strlen("<>") + strlen(_name) + 1;
        char *name = xmalloc(len);
        snprintf(name, len, "<%s>", _name);
        SCM class_name = scm_from_utf8_symbol(name);
        cls = scm_call_4(make_class_proc, dsupers, slots, kwd_name, class_name);
        debug_load("%s - creating new type", name);
        strval_add_entry(name_scm_store, _name, SCM_UNPACK(cls));
        free(name);
    }

    return cls;
}

// Given introspection info from a typelib library for a given GType,
// this makes a new Guile foreign object type and it stores the type
// in our hash table of known types.
SCM
define_type_full(GType gtype, SCM defs, SCM extra_supers)
{
    assert(GSIZE_TO_POINTER(gtype) != NULL);

    scm_t_bits orig_value;
    GType parent = g_type_parent(gtype);
    GType fundamental = G_TYPE_FUNDAMENTAL(gtype);
    char *_type_class_name = class_name_from_gtype(gtype);

    orig_value = keyval_find_entry(gtype_scm_store, gtype);
    if (orig_value == 0) {
        debug_load("%s - creating new %s type for %zx %s",
                       _type_class_name, g_type_name(fundamental), gtype, g_type_name(gtype));

        GType reserved = g_type_from_name(g_type_name(gtype));
        if (reserved != 0 && reserved != gtype)
            critical_load("%s - %s already has %zx as registered GType", _type_class_name,
                              g_type_name(gtype), reserved);

        SCM type_class_name = scm_from_utf8_symbol(_type_class_name);

        if (parent != G_TYPE_INVALID)
            define_type(parent, defs);

        SCM new_type, dsupers, slots = SCM_EOL;
        scm_t_bits sparent = keyval_find_entry(gtype_scm_store, parent);

        switch (fundamental) {
        case G_TYPE_ENUM:
        {
            GEnumClass *_class = g_type_class_ref(gtype);
            SCM size = scm_from_uint(_class->n_values);
            SCM obarray = scm_make_hash_table(size);

            for (unsigned i = 0; i < _class->n_values; i++) {
                SCM key = scm_from_utf8_symbol(_class->values[i].value_nick);
                SCM value = scm_from_int(_class->values[i].value);
                debug_load("%s - add enum %s %d",
                               _type_class_name, _class->values[i].value_nick,
                               _class->values[i].value);
                scm_hashq_set_x(obarray, key, value);
            }
            g_type_class_unref(_class);

            dsupers = scm_list_1(SCM_PACK_POINTER(sparent));
            new_type = scm_call_4(make_class_proc, dsupers, slots, kwd_name, type_class_name);

            scm_class_set_x(new_type, sym_obarray, obarray);
            break;
        }

        case G_TYPE_FLAGS:
        {
            GFlagsClass *_class = g_type_class_ref(gtype);
            SCM size = scm_from_uint(_class->n_values);
            SCM obarray = scm_make_hash_table(size);

            for (unsigned i = 0; i < _class->n_values; i++) {
                SCM key = scm_from_utf8_symbol(_class->values[i].value_nick);
                SCM value = scm_from_int(_class->values[i].value);
                debug_load("%s - add flag %s %u",
                               _type_class_name, _class->values[i].value_nick,
                               _class->values[i].value);
                scm_hashq_set_x(obarray, key, value);
            }
            g_type_class_unref(_class);

            dsupers = scm_list_1(SCM_PACK_POINTER(sparent));
            new_type = scm_call_4(make_class_proc, dsupers, slots, kwd_name, type_class_name);

            scm_class_set_x(new_type, sym_obarray, obarray);
            break;
        }

        case G_TYPE_BOXED:
        {
            dsupers = scm_cons(SCM_PACK_POINTER(sparent), extra_supers);
            new_type = scm_call_4(make_class_proc, dsupers, slots, kwd_name, type_class_name);

            Boxed_funcs *funcs = _boxed_funcs_for_type(gtype);

            GIRepository *repository;
            GIBaseInfo *info;
            size_t size = 0;

            repository = g_irepository_get_default();
            info = g_irepository_find_by_gtype(repository, gtype);
            if (info != NULL) {
                if (GI_IS_STRUCT_INFO(info))
                    size = g_struct_info_get_size((GIStructInfo *) info);
                g_base_info_unref(info);
            }

            scm_class_set_x(new_type, sym_ref, scm_from_pointer(funcs->copy, NULL));
            scm_class_set_x(new_type, sym_unref, scm_from_pointer(funcs->free, NULL));
            scm_class_set_x(new_type, sym_size, scm_from_size_t(size));
            break;
        }

        case G_TYPE_INTERFACE:
        case G_TYPE_PARAM:
        case G_TYPE_OBJECT:
        {
            GType *interfaces = NULL;
            unsigned n_interfaces = 0;
            if (fundamental == G_TYPE_OBJECT)
                interfaces = g_type_interfaces(gtype, &n_interfaces);
            else if (fundamental == G_TYPE_INTERFACE)
                interfaces = g_type_interface_prerequisites(gtype, &n_interfaces);

            if (parent == G_TYPE_INTERFACE)
                dsupers = extra_supers;
            else
                dsupers = scm_cons(SCM_PACK_POINTER(sparent), extra_supers);

            for (unsigned n = 0; n < n_interfaces; n++)
                dsupers = scm_cons(get_scheme_type(interfaces[n]), dsupers);
            free(interfaces);

            // dsupers need to be sorted, or else Guile will barf
            dsupers = scm_sort_x(dsupers, type_less_p_proc);
            new_type = scm_call_4(make_class_proc, dsupers, slots, kwd_name, type_class_name);
            break;

        }

        default:
        {
            GType *interfaces = NULL;
            unsigned n_interfaces = 0;
            interfaces = g_type_interfaces(gtype, &n_interfaces);
            if (sparent)
                dsupers = scm_cons(SCM_PACK_POINTER(sparent), extra_supers);
            else
                dsupers = scm_cons(fundamental_type, extra_supers);

            for (unsigned n = 0; n < n_interfaces; n++)
                dsupers = scm_cons(get_scheme_type(interfaces[n]), dsupers);
            free(interfaces);

            dsupers = scm_sort_x(dsupers, type_less_p_proc);
            new_type = scm_call_4(make_class_proc, dsupers, slots, kwd_name, type_class_name);
            break;
        }
        }

        return_val_if_fail(!SCM_UNBNDP(new_type), defs);

        SCM key = associate_type(gtype, new_type);
        if (!SCM_UNBNDP(defs)) {
            scm_define(key, new_type);
            defs = scm_cons(key, defs);
        }
        debug_load("Hash table sizes %d %d", keyval_size(gtype_scm_store),
                       keyval_size(scm_gtype_store));
    }
    else {
        debug_load("%s - type already exists for %zx %s",
                       _type_class_name, gtype, g_type_name(gtype));
        SCM val = SCM_PACK(orig_value);

        // FIXME: The warning below should be infrequent enough to not need silencing
        if (SCM_UNBNDP(val))
            return defs;
        return_val_if_fail(!SCM_UNBNDP(val), defs);
        SCM key = scm_class_name(val);
        if (!SCM_UNBNDP(defs)) {
            scm_define(key, val);
            defs = scm_cons(key, defs);
        }
    }

    free(_type_class_name);
    return defs;
}

SCM
define_type(GType gtype, SCM defs)
{
    return define_type_full(gtype, defs, SCM_EOL);
}

// This routine returns the integer GType ID of a scheme object, that is
// - already a GType ID encoded as size_t,
// - a foreign object for a GType
GType
scm_to_gtype(SCM x)
{
    return get_gtype_full(x, NULL, SCM_ARGn);
}

GType
get_gtype_full(SCM x, const char *subr, int argpos)
{
    if (scm_is_unsigned_integer(x, 0, SIZE_MAX))
        return scm_to_size_t(x);
    else if (SCM_CLASSP(x))
        return keyval_find_entry(scm_gtype_store, SCM_UNPACK(x));
    else
        scm_wrong_type_arg_msg(subr, argpos, x, "GType integer or class");
}

SCM
scm_from_gtype(GType x)
{
    // GType <-> SCM associations must go both ways
    scm_t_bits _value = keyval_find_entry(gtype_scm_store, x);
    if (_value != 0) {
        if (keyval_find_entry(scm_gtype_store, _value) != 0)
            return SCM_PACK(_value);
    }
    return scm_from_size_t(x);
}

// This routine returns the integer GType ID of a given
// GIR foreign object type, or an instance of a GIR foreign object type.
// It returns #f on failure.
GType
get_gtype_from_obj(SCM x)
{
    GType value;
    if ((value = keyval_find_entry(scm_gtype_store, SCM_UNPACK(x))))
        return value;
    else if (SCM_INSTANCEP(x) &&
             (value = keyval_find_entry(scm_gtype_store, SCM_UNPACK(SCM_CLASS_OF(x)))))
        return value;

    return G_TYPE_INVALID;
}

static void
free_types(void)
{
    debug_init("Freeing gtype hash table");
    keyval_free(gtype_scm_store, NULL, NULL);
    strval_free(name_scm_store, NULL);
    keyval_free(scm_gtype_store, NULL, NULL);
    _free_boxed_funcs();
}

static SCM
check_scheme_type(scm_t_bits _stype)
{
    return_val_if_fail(_stype != 0, SCM_UNDEFINED);
    return SCM_PACK(_stype);
}

SCM
get_scheme_type(GType gtype)
{
    scm_t_bits _value = keyval_find_entry(gtype_scm_store, gtype);

    if (_value)
        return check_scheme_type(_value);
    else {
        define_type(gtype, SCM_UNDEFINED);
        _value = keyval_find_entry(gtype_scm_store, gtype);
        return check_scheme_type(_value);
    }
}

SCM
get_scheme_type_with_info(GIRegisteredTypeInfo *info)
{
    char *_name = g_registered_type_info_get_qualified_name(info);
    scm_t_bits value = strval_find_entry(name_scm_store, _name);
    free(_name);
    if (value == 0)
        return SCM_UNDEFINED;
    return SCM_PACK(value);
}

////////////////////////////////////////////////////////////////
// GUILE API

// This Guile API routine returns the integer GType ID of a given GIR
// foreign object type, or an instance of a GIR foreign object type.
// It returns #f on failure.
static SCM
scm_type_get_gtype(SCM x)
{
    GType type = get_gtype_from_obj(x);
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
    return get_scheme_type(type);
}

// Given an integer that is a GType, this returns a Guile string of
// the type's name.
static SCM
scm_type_gtype_get_name(SCM s_gtype)
{
    SCM_ASSERT_TYPE(scm_is_integer(s_gtype), s_gtype, SCM_ARG1, "gtype-get-name", "integer");
    GType type = scm_to_uintptr_t(s_gtype);
    if (is_registered(type))
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

    if (is_registered(type))
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

    if (is_registered(type))
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

    if (is_registered(type)) {
        GType *children;
        unsigned n_children, i;
        SCM entry;

        children = g_type_children(type, &n_children);
        for (i = 0; i < n_children; i++) {
            entry = scm_from_uintptr_t(children[i]);
            ret = scm_append(scm_list_2(ret, scm_list_1(entry)));
        }
        free(children);
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

    if (is_registered(type)) {
        GType *interfaces;
        unsigned n_interfaces, i;
        SCM entry;

        interfaces = g_type_interfaces(type, &n_interfaces);
        for (i = 0; i < n_interfaces; i++) {
            entry = scm_from_uintptr_t(interfaces[i]);
            ret = scm_append(scm_list_2(ret, scm_list_1(entry)));
        }
        free(interfaces);
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

    if (is_registered(type))
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

    if (is_registered(type))
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

    if (is_registered(type))
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

    if (is_registered(type))
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

    if (is_registered(type))
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

    if (is_registered(self) && is_registered(parent))
        return scm_from_bool(g_type_is_a(self, parent));
    return SCM_BOOL_F;
}

static SCM
scm_type_dump_type_table(void)
{
    SCM list = SCM_EOL;

    for (int i = 0; i < gtype_scm_store->len; i++) {
        SCM fo_type;
        size_t skey = gtype_scm_store->entries[i].key;
        scm_t_bits value = gtype_scm_store->entries[i].key;

        if (value)
            fo_type = SCM_PACK_POINTER(value);
        else
            fo_type = SCM_BOOL_F;
        SCM entry =
            scm_list_3(scm_from_size_t(skey), scm_from_utf8_string(g_type_name(skey)), fo_type);
        list = scm_append(scm_list_2(list, scm_list_1(entry)));
    }
    return list;
}

static SCM
scm_allocate_boxed(SCM boxed_type)
{
    SCM_ASSERT_TYPE(SCM_SUBCLASSP(boxed_type, boxed_type), boxed_type, SCM_ARG1,
                    "%allocate-boxed", "boxed type");
    SCM s_size = scm_class_ref(boxed_type, sym_size);

    size_t size = scm_to_size_t(s_size);

    if (size == 0)
        scm_out_of_range("%allocate-boxed", s_size);

    void *boxed = xcalloc(1, size);
    Type_unref_function unref;
    unref = (Type_unref_function)scm_to_pointer(scm_class_ref(boxed_type, sym_unref));
    SCM pointer = scm_from_pointer(boxed, unref);

    return scm_call_3(make_instance_proc, boxed_type, kwd_value, pointer);
}

void
define_fundamental_type(GType type, SCM extra_supers,
                            Type_ref_function ref, Type_unref_function unref)
{
    GIRepository *repository;
    GIBaseInfo *info;

    if (is_registered(type)) {
        warning_load("not redefining fundamental type %s", g_type_name(type));
        return;
    }

    assert(scm_is_true(scm_module_public_interface(scm_current_module())));

    repository = g_irepository_get_default();
    info = g_irepository_find_by_gtype(repository, type);
    if (info != NULL) {
        if (g_base_info_get_type(info) == GI_INFO_TYPE_OBJECT) {
            GIObjectInfoRefFunction _ref;
            GIObjectInfoUnrefFunction _unref;
            _ref = g_object_info_get_ref_function_pointer(info);
            _unref = g_object_info_get_unref_function_pointer(info);
            if (_ref)
                ref = _ref;
            if (_unref)
                unref = _unref;
        }
        g_base_info_unref(info);
    }

    scm_dynwind_begin(0);
    char *class_name = dynfree(class_name_from_gtype(type));

    SCM new_type = scm_call_4(make_fundamental_proc,
                              scm_from_utf8_symbol(class_name),
                              extra_supers,
                              scm_from_pointer(ref, NULL),
                              scm_from_pointer(unref, NULL));

    SCM key = associate_type(type, new_type);
    scm_define(key, new_type);
    scm_module_export(scm_current_module(), scm_list_1(key));
    scm_dynwind_end();
}

void
init_types(void)
{
    fundamental_type = scm_c_private_ref("gi oop", "<GFundamental>");
    boxed_type = scm_c_private_ref("gi oop", "<GBoxed>");
    enum_type = scm_c_private_ref("gi oop", "<GEnum>");
    flags_type = scm_c_private_ref("gi oop", "<GFlags>");
    make_class_proc = scm_c_public_ref("oop goops", "make-class");
    make_instance_proc = scm_c_public_ref("oop goops", "make");
    make_fundamental_proc = scm_c_private_ref("gi oop", "%make-fundamental-class");

    kwd_name = scm_from_utf8_keyword("name");
    kwd_value = scm_from_utf8_keyword("value");

    sym_value = scm_from_utf8_symbol("value");
    sym_ref = scm_from_utf8_symbol("ref");
    sym_unref = scm_from_utf8_symbol("unref");
    sym_size = scm_from_utf8_symbol("size");
    sym_sort_key = scm_from_utf8_symbol("sort-key");
    sym_obarray = scm_from_utf8_symbol("obarray");

    SCM getter_with_setter = scm_c_public_ref("oop goops", "<applicable-struct-with-setter>");

    gtype_scm_store = keyval_new();
    name_scm_store = strval_new();
    scm_gtype_store = keyval_new();

#define A(G,S)                                  \
    do {                                        \
        SCM key = associate_type(G, S);     \
        scm_define(key, S);                     \
    } while (0)

    // fundamental types
    define_fundamental_type(G_TYPE_OBJECT, SCM_EOL,
                                (Type_ref_function)g_object_ref_sink,
                                (Type_unref_function)g_object_unref);
    define_fundamental_type(G_TYPE_INTERFACE, SCM_EOL, NULL, NULL);
    define_fundamental_type(G_TYPE_PARAM,
                                scm_list_1(getter_with_setter),
                                (Type_ref_function)g_param_spec_ref_sink,
                                (Type_unref_function)g_param_spec_unref);

    define_fundamental_type(G_TYPE_VARIANT, SCM_EOL,
                                (Type_ref_function)g_variant_ref_sink,
                                (Type_unref_function)g_variant_unref);

    A(G_TYPE_BOXED, boxed_type);
    A(G_TYPE_ENUM, enum_type);
    A(G_TYPE_FLAGS, flags_type);

    object_type = get_scheme_type(G_TYPE_OBJECT);
    paramspec_type = get_scheme_type(G_TYPE_PARAM);

    // derived types

    define_type(GI_TYPE_BASE_INFO, SCM_EOL);

    define_type_full(G_TYPE_VALUE, SCM_EOL, scm_list_1(getter_with_setter));
    value_type = get_scheme_type(G_TYPE_VALUE);
    define_type_full(G_TYPE_CLOSURE, SCM_EOL,
                         scm_list_1(scm_c_public_ref("oop goops", "<applicable-struct>")));
    closure_type = get_scheme_type(G_TYPE_CLOSURE);

    scm_class_set_x(value_type, sym_size, scm_from_size_t(sizeof(GValue)));

    // value associations, do not rely on them for anything else
    associate_type(G_TYPE_STRING, scm_c_public_ref("oop goops", "<string>"));
    SCM _scm_real = scm_c_public_ref("oop goops", "<real>");
    SCM _scm_integer = scm_c_public_ref("oop goops", "<integer>");
    SCM _scm_hashtable = scm_c_public_ref("oop goops", "<hashtable>");
    register_type(G_TYPE_INT, _scm_integer);
    register_type(G_TYPE_UINT, _scm_integer);
    register_type(G_TYPE_LONG, _scm_integer);
    register_type(G_TYPE_ULONG, _scm_integer);
    register_type(G_TYPE_INT64, _scm_integer);
    register_type(G_TYPE_UINT64, _scm_integer);

    register_type(G_TYPE_FLOAT, _scm_real);
    register_type(G_TYPE_DOUBLE, _scm_real);

    // there is no base type for string, array and all the other accepted array conversions
    register_type(G_TYPE_BYTE_ARRAY, SCM_UNDEFINED);
    register_type(G_TYPE_ARRAY, SCM_UNDEFINED);
    register_type(G_TYPE_PTR_ARRAY, SCM_UNDEFINED);

    register_type(G_TYPE_HASH_TABLE, _scm_hashtable);

    atexit(free_types);

    // G_TYPE_X constants for use where SCM classes don't apply
#define D(x)                                                            \
    do                                                                  \
    {                                                                   \
        register_type(x, SCM_UNDEFINED);                            \
        scm_permanent_object(scm_c_define(#x, scm_from_uintptr_t(x)));  \
        scm_c_export(#x, NULL);                                         \
    } while (0)

    D(G_TYPE_NONE);
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
    D(G_TYPE_GTYPE);
    D(G_TYPE_OBJECT);
    D(G_TYPE_STRING);
    D(G_TYPE_POINTER);
#undef D

    type_less_p_proc = scm_c_make_gsubr("type-<?", 2, 0, 0, type_less_p);

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