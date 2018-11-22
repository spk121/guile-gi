/* -*- Mode: C; c-basic-offset: 4 -*- */


#include <libguile.h>
#include <glib.h>
#include <glib-object.h>
#include <girepository.h>

#include "gi_gtype.h"

GQuark gtype_wrapper_key;
GQuark gtype_base_info_key;

/* In C, a GType is an integer.  It indicates a GObject type.
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

 /* A <gtype> is a foreign object type that contains a C GType
    integer. */

static SCM
get_hash_table(void)
{
    SCM hashtable;
    static int first = TRUE;
    static SCM sym;
    if (first)
    {
        first = FALSE;
        sym = scm_from_utf8_symbol("%gtypes");
    }

    hashtable = scm_module_variable(scm_current_module(), sym);
    g_assert(scm_is_true(scm_hash_table_p(scm_variable_ref(hashtable))));
    return scm_variable_ref(hashtable);
}


/* re pyg_type_get_bases */
static SCM
scm_gtype_get_bases(SCM self)
{
    GType gtype;
    GType parent_type;
    GType *interfaces;
    guint n_interfaces;
    SCM bases;

    scm_assert_foreign_object_type(gi_gtype_type, self);
    gtype = gi_gtype_get_type(self);
    parent_type = g_type_parent(gtype);
    interfaces = g_type_interfaces(gtype, &n_interfaces);

    /* Put the parent at the beginning of the list */
    if (parent_type)
        bases = scm_list_1(gi_gtype_c2g(parent_type));
    else
        bases = SCM_EOL;

    /* And traverse interfaces */
    if (n_interfaces) {
        for (guint i = 0; i < n_interfaces; i++) {
            bases = scm_append(scm_list_2(bases, scm_list_1(gi_gtype_c2g(interfaces[i]))));
        }
    }
    g_free(interfaces);
    return bases;
}

////////////////////////////////////////////////////////////////

/* Lookup a gtype by number, or name, or whatever */
GType
gi_gtype_from_scm(SCM obj)
{
    GType type;

    /* Scheme-wrapped GTypes are gtypes, naturally. */
    if (SCM_IS_A_P(obj, gi_gtype_type)) {
        return gi_gtype_get_type(obj);
    }

    /* if OBJ is a string, maybe it names a gtype. */
    if (scm_is_string(obj)) {
        char *str;
        str = scm_to_utf8_string(obj);
        type = g_type_from_name(str);
        free(str);

        if (type != 0)
            return type;
    }

    /* Some integers are gtypes, but, using non-GType-integer as a
       GType can cause a crash, so we only convert GTypes we've seen
       before.*/
    if (scm_is_exact_integer(obj)) {
        size_t val;
        SCM entry;

        val = scm_to_size_t(obj);
        entry = scm_hash_ref(get_hash_table(), obj, SCM_BOOL_F);
        if (!scm_is_false(entry))
            return val;
    }

    scm_misc_error("gi_gtype_from_scm",
        "cannot convert '~a' to a GType",
        scm_list_1(obj));
    g_return_val_if_reached(0);
}

GType gi_infer_gtype_from_scm(SCM obj)
{
    if (scm_is_eq(obj, SCM_BOOL_F) || scm_is_eq(obj, SCM_BOOL_T))
        return G_TYPE_BOOLEAN;
    else if (scm_is_exact_integer(obj))
        return G_TYPE_LONG;
    else if (scm_is_real(obj))
        return G_TYPE_DOUBLE;
    else if (scm_is_string(obj))
        return G_TYPE_STRING;
    else
        g_critical("Could not infer typecode from object");

    return G_TYPE_INT;
}

static SCM
scm_to_gtype(SCM obj)
{
    GType type;
    type = gi_gtype_from_scm(obj);
    if (type == 0)
        return SCM_BOOL_F;
    return gi_gtype_c2g(type);
}

static SCM
scm_integer_to_gtype_unsafe(SCM obj)
{
    size_t val = scm_to_size_t(obj);
    return gi_gtype_c2g(val);
}

#if 0
static GQuark
gu_type_key(GType type) {
    GQuark key;

    g_return_val_if_reached(0);
#if 0
    if (g_type_is_a(type, G_TYPE_INTERFACE)) {
        key = guginterface_type_key;
    }
    else if (g_type_is_a(type, G_TYPE_ENUM)) {
        key = gugenum_class_key;
    }
    else if (g_type_is_a(type, G_TYPE_FLAGS)) {
        key = gugflags_class_key;
    }
    else if (g_type_is_a(type, G_TYPE_POINTER)) {
        key = gugpointer_class_key;
    }
    else if (g_type_is_a(type, G_TYPE_BOXED)) {
        key = gugboxed_type_key;
    }
    else {
        key = gugobject_class_key;
    }

    return key;
#endif
}

static SCM
gir_GType_get_scheme_type(SCM self)
{
    GQuark key;
    GType type;
    gpointer ptr;
    SCM scm_type;

    scm_assert_foreign_object_type(gi_gtype_type, self);
    type = gi_gtype_get_type(self);
    key = gu_type_key(type);
    ptr = g_type_get_qdata(type, key);
    if (!ptr)
        return SCM_NONE;

    return SCM_PACK_POINTER(ptr);
}

static SCM
gir_GType_set_scheme_type_x(SCM self, SCM value)
{
    g_return_val_if_reached(SCM_UNSPECIFIED);

#if 0
    GQuark key;
    GType type;
    gpointer ptr;
    SCM scm_type;

    scm_assert_foreign_object_type(gi_gtype_type, self);
    type = gi_gtype_get_type(self);
    key = gu_type_key(type);
    if (value == SCM_NONE)
        g_type_set_qdata(type, key, NULL);
    else if (SCM_IS_A_P(value, gi_gtype_type))
        g_type_set_qdata(type, key, value);
    else
        scm_misc_error("GType-set-scheme-data!", "Value '~A' must be NONE or a type object",
            scm_list_1(value));
#endif

    return SCM_UNSPECIFIED;
}
#endif

static SCM
scm_gtype_get_name(SCM self)
{
    GType type;
    const char *name;

    scm_assert_foreign_object_type(gi_gtype_type, self);
    type = gi_gtype_get_type(self);
    name = g_type_name(type);
    if (name)
        return scm_from_utf8_string(name);
    else
        g_critical("No name found for GType %zu", type);

    return scm_from_latin1_string("invalid");
}

static SCM
scm_gtype_get_parent(SCM self)
{
    GType type;
    scm_assert_foreign_object_type(gi_gtype_type, self);
    type = gi_gtype_get_type(self);
    return gi_gtype_c2g(g_type_parent(type));
}

static SCM
scm_gtype_get_fundamental(SCM self)
{
    GType type;
    scm_assert_foreign_object_type(gi_gtype_type, self);
    type = gi_gtype_get_type(self);
    return gi_gtype_c2g(g_type_fundamental(type));
}

static SCM
scm_gtype_get_children(SCM self)
{
    GType type;
    GType *children;
    guint n_children, i;
    SCM entry, ret;

    scm_assert_foreign_object_type(gi_gtype_type, self);
    type = gi_gtype_get_type(self);
    children = g_type_children(type, &n_children);
    ret = SCM_EOL;
    for (i = 0; i < n_children; i++) {
        entry = gi_gtype_c2g(children[i]);
        ret = scm_append(scm_list_2(ret, scm_list_1(entry)));
    }
    g_free(children);
    return ret;
}

static SCM
scm_gtype_get_interfaces(SCM self)
{
    GType type;
    GType *interfaces;
    guint n_interfaces, i;
    SCM entry, ret;

    scm_assert_foreign_object_type(gi_gtype_type, self);
    type = gi_gtype_get_type(self);
    interfaces = g_type_interfaces(type, &n_interfaces);
    ret = SCM_EOL;
    for (i = 0; i < n_interfaces; i++) {
        entry = gi_gtype_c2g(interfaces[i]);
        ret = scm_append(scm_list_2(ret, scm_list_1(entry)));
    }
    g_free(interfaces);
    return ret;
}

static SCM
scm_gtype_get_depth(SCM self)
{
    GType type;
    scm_assert_foreign_object_type(gi_gtype_type, self);
    type = gi_gtype_get_type(self);
    return scm_from_uint(g_type_depth(type));
}

static SCM
scm_gtype_is_interface_p(SCM self)
{
    GType type;
    scm_assert_foreign_object_type(gi_gtype_type, self);
    type = gi_gtype_get_type(self);
    return scm_from_bool(G_TYPE_IS_INTERFACE(type));
}

static SCM
scm_gtype_is_classed_p(SCM self)
{
    GType type;
    scm_assert_foreign_object_type(gi_gtype_type, self);
    type = gi_gtype_get_type(self);
    return scm_from_bool(G_TYPE_IS_CLASSED(type));
}

static SCM
scm_gtype_is_instantiatable_p(SCM self)
{
    GType type;
    scm_assert_foreign_object_type(gi_gtype_type, self);
    type = gi_gtype_get_type(self);
    return scm_from_bool(G_TYPE_IS_INSTANTIATABLE(type));
}

static SCM
scm_gtype_is_derivable_p(SCM self)
{
    GType type;
    scm_assert_foreign_object_type(gi_gtype_type, self);
    type = gi_gtype_get_type(self);
    return scm_from_bool(G_TYPE_IS_DERIVABLE(type));
}

static SCM
scm_gtype_is_deep_derivable_p(SCM self)
{
    GType type;
    scm_assert_foreign_object_type(gi_gtype_type, self);
    type = gi_gtype_get_type(self);
    return scm_from_bool(G_TYPE_IS_DEEP_DERIVABLE(type));
}

static SCM
scm_gtype_is_abstract_p(SCM self)
{
    GType type;
    scm_assert_foreign_object_type(gi_gtype_type, self);
    type = gi_gtype_get_type(self);
    return scm_from_bool(G_TYPE_IS_ABSTRACT(type));
}

static SCM
scm_gtype_is_value_abstract_p(SCM self)
{
    GType type;
    scm_assert_foreign_object_type(gi_gtype_type, self);
    type = gi_gtype_get_type(self);
    return scm_from_bool(G_TYPE_IS_VALUE_ABSTRACT(type));
}

static SCM
scm_gtype_is_value_type_p(SCM self)
{
    GType type;
    scm_assert_foreign_object_type(gi_gtype_type, self);
    type = gi_gtype_get_type(self);
    return scm_from_bool(G_TYPE_IS_VALUE_TYPE(type));
}

static SCM
scm_gtype_has_value_table_p(SCM self)
{
    GType type;
    scm_assert_foreign_object_type(gi_gtype_type, self);
    type = gi_gtype_get_type(self);
    return scm_from_bool(G_TYPE_HAS_VALUE_TABLE(type));
}

static SCM
scm_string_to_gtype(SCM s_type_name)
{
    char *type_name;
    GType type;

    SCM_ASSERT_TYPE(scm_is_string(s_type_name), s_type_name, SCM_ARG1, "string->gtype", "string");

    type_name = scm_to_utf8_string(s_type_name);
    type = g_type_from_name(type_name);
    if (type == 0)
        scm_misc_error("string->gtype", "unknown type name '~A'", scm_list_1(s_type_name));

    return gi_gtype_c2g(type);
}

static SCM
scm_gtype_is_a_p(SCM self, SCM gparent)
{
    GType parent, child;

    scm_assert_foreign_object_type(gi_gtype_type, self);

    parent = gi_gtype_from_scm(gparent);
    if (parent == 0)
        scm_misc_error("gtype-is-a?", "Cannot infer a GType from ~A", scm_list_1(gparent));
    child = gi_gtype_get_type(self);
    return scm_from_bool(g_type_is_a(child, parent));
}

static SCM
scm_gtype_is_value_p(SCM self)
{
    GType type;
    gboolean ret;

    scm_assert_foreign_object_type(gi_gtype_type, self);
    type = gi_gtype_get_type(self);
    ret = G_TYPE_IS_VALUE(type);
    return scm_from_bool(ret);
}

static SCM
scm_gtype_is_param_p(SCM self)
{
    GType type;
    gboolean ret;

    scm_assert_foreign_object_type(gi_gtype_type, self);
    type = gi_gtype_get_type(self);
    ret = G_TYPE_IS_PARAM(type);
    return scm_from_bool(ret);
}

static SCM
scm_gtype_equal_p(SCM self, SCM other)
{
    scm_assert_foreign_object_type(gi_gtype_type, self);
    scm_assert_foreign_object_type(gi_gtype_type, other);
    return scm_from_bool(gi_gtype_get_type(self) == gi_gtype_get_type(other));
}

SCM gi_gtype_c2g(GType type)
{
    SCM wrapper;
    gpointer ptr;
    GType parent;

    parent = g_type_parent(type);
    if (parent != 0)
        gi_gtype_c2g(parent);

    /* Do we already have a wrapper for this type? */
    ptr = g_type_get_qdata(type, gtype_wrapper_key);
    if (!ptr)
    {
        g_debug("Creating new GType wrapper: %zu %s", type, g_type_name(type));

        wrapper = scm_make_foreign_object_0(gi_gtype_type);
        gi_gtype_set_type(wrapper, type);
        g_type_set_qdata(type, gtype_wrapper_key, SCM_UNPACK_POINTER(wrapper));
        scm_hash_set_x(get_hash_table(), scm_from_size_t(type), wrapper);
    }
    else
    {
        wrapper = SCM_PACK_POINTER(ptr);
    }
    return wrapper;
}

void
gi_gtype_finalizer(SCM self)
{
    GType type = gi_gtype_get_type(self);
    g_debug("Finalizing GType wrapper: %zu %s", type, g_type_name(type));
    g_type_set_qdata(type, gtype_wrapper_key, NULL);
}

void
gi_init_gtype(void)
{
    gi_init_gtype_type();

    SCM gtype_wrapper_hash = scm_c_make_hash_table(10);
    scm_permanent_object(scm_c_define("%gtypes", gtype_wrapper_hash));
    gtype_wrapper_key = g_quark_from_static_string("guile-gi-gtype-wrapper");
    gtype_base_info_key = g_quark_from_static_string("gtype-base-info");

#define D(x) scm_permanent_object(scm_c_define(#x, gi_gtype_c2g (x)))
    // D(G_TYPE_INVALID);
    D(G_TYPE_NONE);
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

    scm_c_define_gsubr("->gtype", 1, 0, 0, scm_to_gtype);
    scm_c_define_gsubr("integer->gtype-unsafe", 1, 0, 0, scm_integer_to_gtype_unsafe);
    scm_c_define_gsubr("string->gtype", 1, 0, 0, scm_string_to_gtype);
    scm_c_define_gsubr("gtype-get-name", 1, 0, 0, scm_gtype_get_name);
    scm_c_define_gsubr("gtype-get-parent", 1, 0, 0, scm_gtype_get_parent);
    scm_c_define_gsubr("gtype-get-fundamental", 1, 0, 0, scm_gtype_get_fundamental);
    scm_c_define_gsubr("gtype-get-children", 1, 0, 0, scm_gtype_get_children);
    scm_c_define_gsubr("gtype-get-interfaces", 1, 0, 0, scm_gtype_get_interfaces);
    scm_c_define_gsubr("gtype-get-depth", 1, 0, 0, scm_gtype_get_depth);
    scm_c_define_gsubr("gtype-is-interface?", 1, 0, 0, scm_gtype_is_interface_p);
    scm_c_define_gsubr("gtype-is-classed?", 1, 0, 0, scm_gtype_is_classed_p);
    scm_c_define_gsubr("gtype-is-instantiatable?", 1, 0, 0, scm_gtype_is_instantiatable_p);
    scm_c_define_gsubr("gtype-is-derivable?", 1, 0, 0, scm_gtype_is_derivable_p);
    scm_c_define_gsubr("gtype-is-deep-derivable?", 1, 0, 0, scm_gtype_is_deep_derivable_p);
    scm_c_define_gsubr("gtype-is-abstract?", 1, 0, 0, scm_gtype_is_abstract_p);
    scm_c_define_gsubr("gtype-is-value-abstract?", 1, 0, 0, scm_gtype_is_value_abstract_p);
    scm_c_define_gsubr("gtype-is-value-type?", 1, 0, 0, scm_gtype_is_value_type_p);
    scm_c_define_gsubr("gtype-has-value-table?", 1, 0, 0, scm_gtype_has_value_table_p);
    scm_c_define_gsubr("gtype-is-a?", 2, 0, 0, scm_gtype_is_a_p);
    scm_c_define_gsubr("gtype-get-bases", 1, 0, 0, scm_gtype_get_bases);
    scm_c_define_gsubr("gtype-is-value?", 1, 0, 0, scm_gtype_is_value_p);
    scm_c_define_gsubr("gtype-is-param?", 1, 0, 0, scm_gtype_is_param_p);
    scm_c_define_gsubr("gtype=?", 2, 0, 0, scm_gtype_equal_p);
}
