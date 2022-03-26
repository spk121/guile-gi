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
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <stdbool.h>
#include <assert.h>
#include <stdio.h>
#include <libguile.h>
#include <ffi.h>
#include "gig_type_priv.h"
#include "gig_boxed.h"

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

SCM gig_il_type_func = SCM_UNDEFINED;
SCM gig_il_untyped_flags_func = SCM_UNDEFINED;
SCM gig_il_untyped_enum_func = SCM_UNDEFINED;


// Maps GType to SCM
static SCM gtype_hash_var = SCM_UNDEFINED;
// Maps string to SCM
static SCM info_hash_var = SCM_UNDEFINED;
// Maps SCM to GType
static SCM reverse_hash_var = SCM_UNDEFINED;

static SCM enum_type;
static SCM flags_type;
static SCM fundamental_type;
static SCM boxed_type;
static SCM make_fundamental_proc;
static SCM type_less_p_proc;
static SCM sym_sort_key;

static SCM make_type_with_gtype(const char *type_class_name, GType gtype, SCM extra_supers,
                                size_t boxed_size);
static SCM gig_type_associate(GType gtype, SCM stype);
static void make_untyped_flag_enum(SCM s_class_name, SCM s_qname, SCM flags, bool is_flags);

////////////////////////////////////////////////////////////////
// Type definition
////////////////////////////////////////////////////////////////

// Given introspection info from a typelib library for a given GType,
// this makes a new Guile foreign object type and it stores the type
// in our hash table of known types.
SCM
gig_type_define_full(const char *_type_class_name, GType gtype, SCM extra_supers,
                     size_t boxed_size)
{
    assert(GSIZE_TO_POINTER(gtype) != NULL);
    SCM defs = SCM_EOL;
    SCM orig_value;
    GType parent = g_type_parent(gtype);
    GType fundamental = G_TYPE_FUNDAMENTAL(gtype);

    SCM hash = scm_variable_ref(gtype_hash_var);
    orig_value = scm_hashq_ref(hash, scm_from_size_t(gtype), SCM_BOOL_F);

    if (!scm_is_true(orig_value)) {
        gig_debug_load("%s - creating new %s type for %zx %s",
                       _type_class_name, g_type_name(fundamental), gtype, g_type_name(gtype));

        GType reserved = g_type_from_name(g_type_name(gtype));
        if (reserved != 0 && reserved != gtype)
            gig_critical_load("%s - %s already has %zx as registered GType", _type_class_name,
                              g_type_name(gtype), reserved);

        if (parent != G_TYPE_INVALID)
            defs = scm_append2(defs, gig_type_define(parent));

        SCM new_type = make_type_with_gtype(_type_class_name, gtype, extra_supers, boxed_size);

        g_return_val_if_fail(!scm_is_unknown_class(new_type), defs);

        gig_type_associate(gtype, new_type);
        SCM sym = scm_from_utf8_symbol(_type_class_name);
        scm_define(sym, new_type);
        defs = scm_cons(sym, defs);

        SCM reverse_hash = scm_variable_ref(reverse_hash_var);
        gig_debug_load("Hash table sizes %d %d", SCM_HASHTABLE_N_ITEMS(hash),
                       SCM_HASHTABLE_N_ITEMS(reverse_hash));
    }
    else {
        gig_debug_load("%s - type already exists for %zx %s",
                       _type_class_name, gtype, g_type_name(gtype));
        SCM val = orig_value;

        if (SCM_UNBNDP(val))
            gig_critical("%s - %s is undefined", _type_class_name, g_type_name(gtype));
        SCM key = scm_from_utf8_symbol(_type_class_name);
        scm_define(key, val);
        defs = scm_cons(key, defs);
    }

    return defs;
}

SCM
gig_type_define(GType gtype)
{
    char *_type_class_name = gig_type_class_name_from_gtype(gtype);
    SCM ret;
    ret = gig_type_define_full(_type_class_name, gtype, SCM_EOL, 0);
    free(_type_class_name);
    return ret;
}

SCM
gig_il_type(SCM s_name, SCM s_gtype_name, SCM s_boxed_size)
{
#define FUNC_NAME "^type"
    char *gtype_name;
    size_t gtype;
    char *type_class_name;
    size_t boxed_size = 0;

    SCM_ASSERT_TYPE(scm_is_symbol(s_name), s_name, SCM_ARG1, FUNC_NAME, "symbol");
    SCM_ASSERT_TYPE(scm_is_string(s_gtype_name), s_gtype_name, SCM_ARG2, FUNC_NAME, "string");

    if (scm_is_true(scm_string_null_p(s_gtype_name)))
        scm_wrong_type_arg(FUNC_NAME, SCM_ARG2, s_gtype_name);
    if (!SCM_UNBNDP(s_boxed_size))
        boxed_size = scm_to_size_t(s_boxed_size);
    gtype_name = scm_to_utf8_string(s_gtype_name);
    gtype = g_type_from_name(gtype_name);
    free(gtype_name);
    if (gtype == 0)
        scm_misc_error(FUNC_NAME, "cannot find GType for '~A'", scm_list_1(s_gtype_name));
    type_class_name = scm_to_utf8_symbol(s_name);
    gig_type_define_full(type_class_name, gtype, SCM_EOL, boxed_size);
    free(type_class_name);
    return scm_list_1(s_name);
#undef FUNC_NAME
}

SCM
gig_il_untyped_flags(SCM s_class_name, SCM s_qname, SCM flags)
{
#define FUNC_NAME "^untyped-flags"

    SCM_ASSERT_TYPE(scm_is_symbol(s_class_name), s_class_name, SCM_ARG1, FUNC_NAME, "symbol");
    SCM_ASSERT_TYPE(scm_is_symbol(s_qname), s_qname, SCM_ARG2, FUNC_NAME, "symbol");
    SCM_ASSERT_TYPE(scm_is_list(flags), flags, SCM_ARG3, FUNC_NAME, "alist");

    make_untyped_flag_enum(s_class_name, s_qname, flags, true);
    return scm_list_1(s_class_name);
#undef FUNC_NAME
}

SCM
gig_il_untyped_enum(SCM s_class_name, SCM s_qname, SCM flags)
{
#define FUNC_NAME "^untyped-enum"

    SCM_ASSERT_TYPE(scm_is_symbol(s_class_name), s_class_name, SCM_ARG1, FUNC_NAME, "symbol");
    SCM_ASSERT_TYPE(scm_is_symbol(s_qname), s_qname, SCM_ARG2, FUNC_NAME, "symbol");
    SCM_ASSERT_TYPE(scm_is_list(flags), flags, SCM_ARG3, FUNC_NAME, "alist");

// FIXME: typecheck flags contents

    make_untyped_flag_enum(s_class_name, s_qname, flags, false);
    return scm_list_1(s_class_name);
#undef FUNC_NAME
}

static void
make_untyped_flag_enum(SCM s_class_name, SCM s_qname, SCM flags, bool is_flags)
{
    SCM cls;

    SCM info_hash = scm_variable_ref(info_hash_var);
    cls = scm_hashq_ref(info_hash, s_qname, SCM_BOOL_F);
    char *class_name = scm_to_utf8_symbol(s_class_name);
    char *qname = scm_to_utf8_symbol(s_qname);
    if (scm_is_true(cls) && !scm_is_unknown_class(cls))
        goto def;

    if (!scm_is_true(cls))
        gig_debug_load("%s - creating new flag/enum type for %s", class_name, qname);
    else {
        gig_debug_load("%s - existing flag/enum type for %s", class_name, qname);
        goto def;
    }

    int n_values = scm_c_length(flags);
    SCM obarray;
    SCM dsupers;

    if (is_flags)
        dsupers = scm_list_1(gig_flags_type());
    else
        dsupers = scm_list_1(gig_enum_type());
    cls = scm_make_class_with_name(dsupers, SCM_EOL, s_class_name);
    obarray = scm_make_hash_table(scm_from_int(n_values));

    SCM cur;
    cur = flags;
    while (!scm_is_equal(cur, SCM_EOL)) {
        SCM key, val;
        SCM entry;

        entry = scm_car(cur);
        key = scm_car(entry);
        val = scm_cdr(entry);
        scm_hashq_set_x(obarray, key, val);

        cur = scm_cdr(cur);
    }
    scm_set_class_obarray_slot(cls, obarray);

    scm_hashq_set_x(info_hash, s_qname, cls);

  def:
    scm_c_define(class_name, cls);
    scm_c_export(class_name, NULL);
    free(class_name);
}

////////////////////////////////////////////////////////////////
// Type creation
////////////////////////////////////////////////////////////////

static SCM
make_type_with_gtype(const char *_type_class_name, GType gtype, SCM extra_supers,
                     size_t boxed_size)
{
    GType parent = g_type_parent(gtype);
    GType fundamental = g_type_fundamental(gtype);
    SCM new_type, dsupers, slots = SCM_EOL;
    SCM hash = scm_variable_ref(gtype_hash_var);
    SCM sparent = scm_hashq_ref(hash, scm_from_size_t(parent), SCM_BOOL_F);
    // g_return_val_if_fail(scm_is_true(sparent), defs);
    SCM type_class_name = scm_from_utf8_symbol(_type_class_name);

    switch (fundamental) {
    case G_TYPE_ENUM:
    {
        GEnumClass *_class = g_type_class_ref(gtype);
        SCM size = scm_from_uint(_class->n_values);
        SCM obarray = scm_make_hash_table(size);

        for (unsigned i = 0; i < _class->n_values; i++) {
            SCM key = scm_from_utf8_symbol(_class->values[i].value_nick);
            SCM value = scm_from_int(_class->values[i].value);
            gig_debug_load("%s - add enum %s %d",
                           _type_class_name, _class->values[i].value_nick,
                           _class->values[i].value);
            scm_hashq_set_x(obarray, key, value);
        }
        g_type_class_unref(_class);

        dsupers = scm_list_1(sparent);
        new_type = scm_make_class_with_name(dsupers, slots, type_class_name);

        scm_set_class_obarray_slot(new_type, obarray);
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
            gig_debug_load("%s - add flag %s %u",
                           _type_class_name, _class->values[i].value_nick,
                           _class->values[i].value);
            scm_hashq_set_x(obarray, key, value);
        }
        g_type_class_unref(_class);

        dsupers = scm_list_1(sparent);
        new_type = scm_make_class_with_name(dsupers, slots, type_class_name);

        scm_set_class_obarray_slot(new_type, obarray);
        break;
    }

    case G_TYPE_BOXED:
    {
        dsupers = scm_cons(sparent, extra_supers);
        new_type = scm_make_class_with_name(dsupers, slots, type_class_name);

        GigBoxedFuncs *funcs = _boxed_funcs_for_type(gtype);

        scm_set_class_ref_slot(new_type, scm_from_pointer(funcs->copy, NULL));
        scm_set_class_unref_slot(new_type, scm_from_pointer(funcs->free, NULL));
        if (boxed_size != 0)
            scm_set_class_size_slot(new_type, scm_from_size_t(boxed_size));
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
            dsupers = scm_cons(sparent, extra_supers);

        for (unsigned n = 0; n < n_interfaces; n++)
            dsupers = scm_cons(gig_type_get_scheme_type(interfaces[n]), dsupers);
        free(interfaces);

        // dsupers need to be sorted, or else Guile will barf
        dsupers = scm_sort_x(dsupers, type_less_p_proc);
        new_type = scm_make_class_with_name(dsupers, slots, type_class_name);
        break;

    }

    default:
    {
        GType *interfaces = NULL;
        unsigned n_interfaces = 0;
        interfaces = g_type_interfaces(gtype, &n_interfaces);
        if (scm_is_true(sparent))
            dsupers = scm_cons(sparent, extra_supers);
        else
            dsupers = scm_cons(fundamental_type, extra_supers);

        for (unsigned n = 0; n < n_interfaces; n++)
            dsupers = scm_cons(gig_type_get_scheme_type(interfaces[n]), dsupers);
        free(interfaces);

        dsupers = scm_sort_x(dsupers, type_less_p_proc);
        new_type = scm_make_class_with_name(dsupers, slots, type_class_name);
        break;
    }
    }
    return new_type;
}

////////////////////////////////////////////////////////////////
// Helpers
////////////////////////////////////////////////////////////////

// Returns TRUE if TYPE is contained in the hash table of known types.
bool
gig_type_is_registered(GType gtype)
{
    SCM hash = scm_variable_ref(gtype_hash_var);
    SCM x = scm_hashq_ref(hash, scm_from_size_t(gtype), SCM_BOOL_F);
    return scm_is_true(x);
}

bool
gig_type_check_typed_object(SCM obj, SCM expected_type)
{
    return SCM_IS_A_P(obj, expected_type);
}

void *
gig_type_peek_typed_object(SCM obj, SCM expected_type)
{
    g_return_val_if_fail(SCM_IS_A_P(obj, expected_type), NULL);
    return scm_to_pointer(scm_get_value_slot(obj));
}

void *
gig_type_peek_object(SCM obj)
{
    return gig_type_peek_typed_object(obj, fundamental_type);
}

char *
gig_type_class_name_from_gtype(GType gtype)
{
    char *class_name;
#ifdef FIX_GTYPE_CLASS_NAME_BUG
    GIBaseInfo *info = g_irepository_find_by_gtype(NULL, gtype);
    if (info != NULL) {
        class_name = bracketize(g_base_info_get_name(info));
        g_base_info_unref(info);
    }
    else
        class_name = bracketize(g_type_name(gtype));
#else
    class_name = bracketize(g_type_name(gtype));
#endif
    return class_name;
}

bool
gig_type_check_object(SCM obj)
{
    return SCM_IS_A_P(obj, fundamental_type);
}

static SCM
type_less_p(SCM a, SCM b)
{
    SCM key_a = scm_object_property(a, sym_sort_key);
    SCM key_b = scm_object_property(b, sym_sort_key);

    // reverse order
    return scm_less_p(key_b, key_a);
}

////////////////////////////////////////////////////////////////
// Type registration and lookup
////////////////////////////////////////////////////////////////

// This routine returns the integer GType ID of a scheme object, that is
// - already a GType ID encoded as size_t,
// - a foreign object for a GType
GType
scm_to_gtype(SCM x)
{
    return scm_to_gtype_full(x, NULL, SCM_ARGn);
}

GType
scm_to_gtype_full(SCM x, const char *subr, int argpos)
{
    if (scm_is_unsigned_integer(x, 0, SIZE_MAX))
        return scm_to_size_t(x);
    else if (SCM_CLASSP(x)) {
        SCM reverse_hash = scm_variable_ref(reverse_hash_var);
        return scm_to_size_t(scm_hashq_ref(reverse_hash, x, scm_from_size_t(G_TYPE_INVALID)));
    }
    else
        scm_wrong_type_arg_msg(subr, argpos, x, "GType integer or class");
}

SCM
scm_from_gtype(GType x)
{
    // GType <-> SCM associations must go both ways
    SCM hash = scm_variable_ref(gtype_hash_var);
    SCM _value = scm_hashq_ref(hash, scm_from_size_t(x), SCM_BOOL_F);
    if (scm_is_true(_value)) {
        SCM reverse_hash = scm_variable_ref(reverse_hash_var);
        SCM _type = scm_hashq_ref(reverse_hash, _value, SCM_BOOL_F);
        if (scm_is_true(_type))
            return _value;
    }
    return scm_from_size_t(x);
}

// This routine returns the integer GType ID of a given
// GIR foreign object type, or an instance of a GIR foreign object type.
// It returns #f on failure.
GType
gig_type_get_gtype_from_obj(SCM x)
{
    SCM value;
    SCM reverse_hash = scm_variable_ref(reverse_hash_var);
    value = scm_hashq_ref(reverse_hash, x, SCM_BOOL_F);
    if (scm_is_true(value))
        return scm_to_size_t(value);
    if (SCM_INSTANCEP(x)) {
        value = scm_hashq_ref(reverse_hash, SCM_CLASS_OF(x), SCM_BOOL_F);
        if (scm_is_true(value))
            return scm_to_size_t(value);
    }

    return G_TYPE_INVALID;
}

SCM
gig_type_get_scheme_type(GType gtype)
{
    SCM hash = scm_variable_ref(gtype_hash_var);
    SCM _value = scm_hashq_ref(hash, scm_from_size_t(gtype), SCM_BOOL_F);

    if (scm_is_true(_value))
        return _value;
    else {
        gig_type_define(gtype);
        _value = scm_hashq_ref(hash, scm_from_size_t(gtype), SCM_BOOL_F);
        if (scm_is_false(_value))
            return scm_get_unknown_class();
        return _value;
    }
}

SCM
gig_type_get_scheme_type_with_qname(const char *qname)
{
    SCM info_hash = scm_variable_ref(info_hash_var);
    SCM value = scm_hashq_ref(info_hash, scm_from_utf8_symbol(qname), SCM_BOOL_F);
    if (scm_is_false(value))
        return scm_get_unknown_class();
    return value;
}

SCM
gig_type_transfer_object(GType type, void *ptr, GigTransfer transfer)
{
    if (G_TYPE_IS_CLASSED(type))
        type = G_OBJECT_TYPE(ptr);

    gig_debug_transfer("gig_type_transfer_object(%s, %p, %d)", g_type_name(type), ptr, transfer);

    SCM scm_type = gig_type_get_scheme_type(type);
    g_return_val_if_fail(scm_is_class(scm_type), SCM_BOOL_F);
    g_return_val_if_fail(!scm_is_unknown_class(scm_type), SCM_BOOL_F);
    GigTypeRefFunction ref;
    ref = (GigTypeRefFunction)scm_to_pointer(scm_get_class_ref_slot(scm_type));
    GigTypeUnrefFunction unref;
    unref = (GigTypeUnrefFunction)scm_to_pointer(scm_get_class_unref_slot(scm_type));

    SCM pointer;
    switch (transfer) {
    case GIG_TRANSFER_NOTHING:
        if (ref)
            pointer = scm_from_pointer(ref(ptr), unref);
        else {
            gig_debug_transfer("%s has no ref function", g_type_name(type));
            pointer = scm_from_pointer(ptr, unref);
        }
        break;

    case GIG_TRANSFER_CONTAINER:
    case GIG_TRANSFER_EVERYTHING:
    default:
        pointer = scm_from_pointer(ptr, unref);
        break;
    }

    return scm_make_with_value(scm_type, pointer);
}

static void
gig_type_register_self(GType gtype, SCM stype)
{
    GType parent = g_type_parent(gtype);
    SCM pval;
    char *stype_str = NULL, *old_stype_str = NULL;
    SCM hash = scm_variable_ref(gtype_hash_var);
    pval = scm_hashq_ref(hash, scm_from_size_t(gtype), SCM_BOOL_F);

    if (scm_is_true(pval) && scm_is_eq(stype, pval))
        return;
    if (scm_is_true(pval) && !scm_is_unknown_class(pval) && scm_is_unknown_class(stype))
        return;
    stype_str = scm_write_to_utf8_stringn(stype, 80);
    scm_hashq_set_x(hash, scm_from_size_t(gtype), stype);
    if (!scm_is_true(pval)) {
        if (parent)
            gig_debug_load("%s - registering a new %s type for %zx as %s", g_type_name(gtype),
                           g_type_name(parent), gtype, stype_str);
        else
            gig_debug_load("%s - registering a new type for %zx as %s", g_type_name(gtype), gtype,
                           stype_str);
    }
    else {
        old_stype_str = scm_write_to_utf8_stringn(pval, 80);
        if (parent)
            gig_debug_load("%s - re-registering %s type for %zx from %s to %s", g_type_name(gtype),
                           g_type_name(parent), gtype, old_stype_str, stype_str);
        else
            gig_debug_load("%s - re-registering type for %zx from %s to %s", g_type_name(gtype),
                           gtype, old_stype_str, stype_str);
        free(old_stype_str);
    }
    free(stype_str);
}

// Given a GType integer but no introspection information, this stores
// that GType in our hash table of known types without creating an
// associated foreign object type.
void
gig_type_register(GType gtype, SCM stype)
{
    GType parent = g_type_parent(gtype);
    if (parent != 0)
        gig_type_register_self(parent, scm_get_unknown_class());
    gig_type_register_self(gtype, stype);
}


static SCM
gig_type_associate(GType gtype, SCM stype)
{
    gig_type_register_self(gtype, stype);
    SCM hash = scm_variable_ref(gtype_hash_var);
    SCM reverse_hash = scm_variable_ref(reverse_hash_var);
    scm_set_object_property_x(stype, sym_sort_key, scm_from_size_t(SCM_HASHTABLE_N_ITEMS(hash)));
    scm_hashq_set_x(reverse_hash, stype, scm_from_size_t(gtype));
    return scm_class_name(stype);
}

static void
gig_type_free_types(void)
{
    gig_debug("Freeing gtype hash table");
    _free_boxed_funcs();
}

GType
gig_type_get_c_array_type()
{
    static GType type = 0;
    if (type == 0)
        type = g_type_from_name("CArray");
    if (type == 0)
        gig_error("CArray type is used before it is initialized");

    return type;
}

SCM
gig_object_type()
{
    static SCM stype = SCM_BOOL_F;
    if (scm_is_false(stype))
        stype = gig_type_get_scheme_type(G_TYPE_OBJECT);
    return stype;
}

SCM
gig_paramspec_type()
{
    static SCM stype = SCM_BOOL_F;
    if (scm_is_false(stype))
        stype = gig_type_get_scheme_type(G_TYPE_PARAM);
    return stype;
}

SCM
gig_enum_type()
{
    return enum_type;
}

SCM
gig_flags_type()
{
    return flags_type;
}

SCM
gig_value_type()
{
    static SCM stype = SCM_BOOL_F;
    if (scm_is_false(stype))
        stype = gig_type_get_scheme_type(G_TYPE_VALUE);
    return stype;
}

SCM
gig_closure_type()
{
    static SCM stype = SCM_BOOL_F;
    if (scm_is_false(stype))
        stype = gig_type_get_scheme_type(G_TYPE_CLOSURE);
    return stype;
}

////////////////////////////////////////////////////////////////
// GUILE API

// This Guile API routine returns the integer GType ID of a given GIR
// foreign object type, or an instance of a GIR foreign object type.
// It returns #f on failure.
static SCM
scm_type_get_gtype(SCM x)
{
    GType type = gig_type_get_gtype_from_obj(x);
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
    return gig_type_get_scheme_type(type);
}

// Given an integer that is a GType, this returns a Guile string of
// the type's name.
static SCM
scm_type_gtype_get_name(SCM s_gtype)
{
    SCM_ASSERT_TYPE(scm_is_integer(s_gtype), s_gtype, SCM_ARG1, "gtype-get-name", "integer");
    GType type = scm_to_uintptr_t(s_gtype);
    if (gig_type_is_registered(type))
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

    if (gig_type_is_registered(type))
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

    if (gig_type_is_registered(type))
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

    if (gig_type_is_registered(type)) {
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

    if (gig_type_is_registered(type)) {
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

    if (gig_type_is_registered(type))
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

    if (gig_type_is_registered(type))
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

    if (gig_type_is_registered(type))
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

    if (gig_type_is_registered(type))
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

    if (gig_type_is_registered(type))
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

    if (gig_type_is_registered(self) && gig_type_is_registered(parent))
        return scm_from_bool(g_type_is_a(self, parent));
    return SCM_BOOL_F;
}

static SCM
scm_allocate_boxed(SCM type)
{
    SCM_ASSERT_TYPE(SCM_SUBCLASSP(type, boxed_type), type, SCM_ARG1,
                    "%allocate-boxed", "boxed type");
    size_t typ = gig_type_get_gtype_from_obj(type);
    printf(".... %s\n", g_type_name(typ));
    SCM s_size = scm_get_class_size_slot(type);

    size_t size = scm_to_size_t(s_size);

    if (size == 0)
        scm_out_of_range("%allocate-boxed", s_size);

    void *boxed = xcalloc(1, size);
    GigTypeUnrefFunction unref;
    unref = (GigTypeUnrefFunction)scm_to_pointer(scm_get_class_unref_slot(type));
    SCM pointer = scm_from_pointer(boxed, unref);

    return scm_make_with_value(type, pointer);
}

void
gig_type_define_fundamental(GType type, SCM extra_supers,
                            GigTypeRefFunction ref, GigTypeUnrefFunction unref)
{
    if (gig_type_is_registered(type)) {
        gig_warning("not redefining fundamental type %s", g_type_name(type));
        return;
    }

    assert(scm_is_true(scm_module_public_interface(scm_current_module())));

#if 0
    GIBaseInfo *info;

    info = g_irepository_find_by_gtype(NULL, type);
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
#endif

    scm_dynwind_begin(0);
    char *class_name = scm_dynfree(gig_type_class_name_from_gtype(type));

    SCM new_type = scm_call_4(make_fundamental_proc,
                              scm_from_utf8_symbol(class_name),
                              extra_supers,
                              scm_from_pointer(ref, NULL),
                              scm_from_pointer(unref, NULL));

    SCM key = gig_type_associate(type, new_type);
    scm_define(key, new_type);
    scm_module_export(scm_current_module(), scm_list_1(key));
    scm_dynwind_end();
}

static SCM
scm_g_type_register_static_simple_unsafe(SCM parent_type, SCM name, SCM flags)
{
    GType ret;
    char *str = scm_to_utf8_string(name);
    ret = g_type_register_static_simple(scm_to_size_t(parent_type), str,
                                        0, NULL, 0, NULL, scm_to_int(flags));
    free(str);
    return scm_from_size_t(ret);
}

static SCM
scm_g_type_name_unsafe(SCM gtype)
{
    return scm_from_utf8_string(g_type_name(scm_to_size_t(gtype)));
}

static SCM
scm_g_type_parent_unsafe(SCM gtype)
{
    return scm_from_size_t(g_type_parent(scm_to_size_t(gtype)));
}

static SCM
scm_gig_type_define_full_unsafe(SCM gtype, SCM supers, SCM boxed_size)
{
    size_t type = scm_to_size_t(gtype);
    char *type_class_name = gig_type_class_name_from_gtype(type);
    SCM ret;
    size_t size = 0;
    if (!SCM_UNBNDP(boxed_size))
        size = scm_to_size_t(boxed_size);
    ret = gig_type_define_full(type_class_name, type, supers, size);
    free(type_class_name);
    return ret;
}

static SCM
scm_make_type_with_gtype_unsafe(SCM gtype, SCM extra_supers, SCM boxed_size)
{
    size_t type = scm_to_size_t(gtype);
    char *type_class_name = gig_type_class_name_from_gtype(type);
    SCM ret;
    size_t size = 0;
    if (!SCM_UNBNDP(boxed_size))
        size = scm_to_size_t(boxed_size);

    ret = make_type_with_gtype(type_class_name, type, extra_supers, size);
    free(type_class_name);
    return ret;
}

static void
gig_init_types_once(void)
{
    init_core_oop();

    scm_c_define("G_TYPE_ARRAY", scm_from_size_t(G_TYPE_ARRAY));
    scm_c_define("G_TYPE_BOOLEAN", scm_from_size_t(G_TYPE_BOOLEAN));
    scm_c_define("G_TYPE_BOXED", scm_from_size_t(G_TYPE_BOXED));
    scm_c_define("G_TYPE_BYTE_ARRAY", scm_from_size_t(G_TYPE_BYTE_ARRAY));
    scm_c_define("G_TYPE_CHAR", scm_from_size_t(G_TYPE_CHAR));
    scm_c_define("G_TYPE_CLOSURE", scm_from_size_t(G_TYPE_CLOSURE));
    scm_c_define("G_TYPE_DOUBLE", scm_from_size_t(G_TYPE_DOUBLE));
    scm_c_define("G_TYPE_ENUM", scm_from_size_t(G_TYPE_ENUM));
    scm_c_define("G_TYPE_FLAGS", scm_from_size_t(G_TYPE_FLAGS));
    scm_c_define("G_TYPE_FLOAT", scm_from_size_t(G_TYPE_FLOAT));
    scm_c_define("G_TYPE_GTYPE", scm_from_size_t(G_TYPE_GTYPE));
    scm_c_define("G_TYPE_HASH_TABLE", scm_from_size_t(G_TYPE_HASH_TABLE));
    scm_c_define("G_TYPE_INT", scm_from_size_t(G_TYPE_INT));
    scm_c_define("G_TYPE_INT64", scm_from_size_t(G_TYPE_INT64));
    scm_c_define("G_TYPE_INTERFACE", scm_from_size_t(G_TYPE_INTERFACE));
    scm_c_define("G_TYPE_LONG", scm_from_size_t(G_TYPE_LONG));
    scm_c_define("G_TYPE_NONE", scm_from_size_t(G_TYPE_NONE));
    scm_c_define("G_TYPE_OBJECT", scm_from_size_t(G_TYPE_OBJECT));
    scm_c_define("G_TYPE_PARAM", scm_from_size_t(G_TYPE_PARAM));
    scm_c_define("G_TYPE_POINTER", scm_from_size_t(G_TYPE_POINTER));
    scm_c_define("G_TYPE_PTR_ARRAY", scm_from_size_t(G_TYPE_PTR_ARRAY));
    scm_c_define("G_TYPE_STRING", scm_from_size_t(G_TYPE_STRING));
    scm_c_define("G_TYPE_UCHAR", scm_from_size_t(G_TYPE_UCHAR));
    scm_c_define("G_TYPE_UINT", scm_from_size_t(G_TYPE_UINT));
    scm_c_define("G_TYPE_UINT64", scm_from_size_t(G_TYPE_UINT64));
    scm_c_define("G_TYPE_ULONG", scm_from_size_t(G_TYPE_ULONG));
    scm_c_define("G_TYPE_VALUE", scm_from_size_t(G_TYPE_VALUE));
    scm_c_define("G_TYPE_VARIANT", scm_from_size_t(G_TYPE_VARIANT));

    scm_c_define("G_TYPE_FLAG_ABSTRACT", scm_from_int(G_TYPE_FLAG_ABSTRACT));
    scm_c_define("SIZEOF_GVALUE", scm_from_size_t(sizeof(GValue)));

    scm_c_define("$object-ref-sink-ptr", scm_from_pointer(g_object_ref_sink, NULL));
    scm_c_define("$object-unref-ptr", scm_from_pointer(g_object_unref, NULL));
    scm_c_define("$param-spec-ref-sink-ptr", scm_from_pointer(g_param_spec_ref_sink, NULL));
    scm_c_define("$param-spec-unref-ptr", scm_from_pointer(g_param_spec_unref, NULL));
    scm_c_define("$variant-ref-sink-ptr", scm_from_pointer(g_variant_ref_sink, NULL));
    scm_c_define("$variant-unref-ptr", scm_from_pointer(g_variant_unref, NULL));
    scm_c_define("$null-ptr", scm_from_pointer(NULL, NULL));


    fundamental_type = scm_c_private_ref("gi oop", "<GFundamental>");
    boxed_type = scm_c_private_ref("gi oop", "<GBoxed>");
    enum_type = scm_c_private_ref("gi oop", "<GEnum>");
    flags_type = scm_c_private_ref("gi oop", "<GFlags>");
    make_fundamental_proc = scm_c_private_ref("gi oop", "%make-fundamental-class");

    sym_sort_key = scm_from_utf8_symbol("sort-key");

    gtype_hash_var = scm_c_private_lookup("gi oop", "%gtype-hash");
    reverse_hash_var = scm_c_private_lookup("gi oop", "%reverse-hash");
    info_hash_var = scm_c_private_lookup("gi oop", "%info-hash");

    type_less_p_proc = scm_c_make_gsubr("type-<?", 2, 0, 0, type_less_p);

    scm_c_define_gsubr("$type-register-static-simple", 3, 0, 0,
                       scm_g_type_register_static_simple_unsafe);
    scm_c_define_gsubr("$type-name", 1, 0, 0, scm_g_type_name_unsafe);
    scm_c_define_gsubr("$type-parent", 1, 0, 0, scm_g_type_parent_unsafe);

    scm_c_define_gsubr("$type-define-full", 2, 1, 0, scm_gig_type_define_full_unsafe);
    scm_c_define_gsubr("$make-type-with-gtype", 2, 1, 0, scm_make_type_with_gtype_unsafe);
    gig_il_type_func = scm_c_define_gsubr("^type", 2, 1, 0, gig_il_type);
    gig_il_untyped_flags_func =
        scm_c_define_gsubr("^untyped-flags", 3, 0, 0, gig_il_untyped_flags);
    gig_il_untyped_enum_func = scm_c_define_gsubr("^untyped-enum", 3, 0, 0, gig_il_untyped_enum);
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
    scm_c_define_gsubr("%allocate-boxed", 1, 0, 0, scm_allocate_boxed);

    atexit(gig_type_free_types);
}

void
gig_init_types()
{
    static size_t type_init;
    if (g_once_init_enter(&type_init)) {
        gig_init_types_once();
        g_once_init_leave(&type_init, 1);
    }
}
