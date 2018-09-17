/* -*- Mode: C; c-basic-offset: 4 -*- */
#include <libguile.h>
#include <glib.h>
#include <glib-object.h>
#include "gir_xguile.h"

SCM GuGType_Type;
SCM GuGType_Type_Store;

////////////////////////////////////////////////////////////////
// GType_Type: A foreign object that is an envelope for a GType, which
//   is really a size_t, and that mocks up Python types, somewhat.

// GuGType Instance: A foreign object with the following slots
//  - slot 1: 'ob_type' - an SCM exact integer holding a refcount
//  - slot 2: 'tp_name' - an SCM string holding the type name
//  - slot 3: 'tp_type' -
//  - slot 4: 'tp_dict' - an SCM hash table
//  - slot 5: 'tp_flags' - an SCM exact integer holding flags
//  - slot 5: 'type', an SCM exact integer holding a GType aka size_t
//

// This bit does nothing.
#define GU_TPFLAGS_DEFAULT 1
// If this bit is present, then subclassing this type is allowed
#define GU_TPFLAGS_BASETYPE 2
// This bit is set when the type object is allocated on the heap.
#define GU_TPFLAGS_HEAPTYPE 4
// This bit is set when the type is using
#define GU_TPFLAGS_HAVE_GC 8

#define MAKE_GTYPE_TYPE                                                         \
    do {                                                                \
    GuGType_Type =                                                      \
        scm_make_foreign_object_type(scm_from_latin1_symbol("<GType>"), \
                                     scm_list_n (scm_from_latin1_symbol("ob_type"), \
                                                 scm_from_latin1_symbol("ob_refcnt"), \
                                                 scm_from_latin1_symbol("tp_name"), \
                                                 scm_from_latin1_symbol("tp_type"), \
                                                 scm_from_latin1_symbol("tp_dict"), \
                                                 scm_from_latin1_symbol("tp_flags"), \
                                                 scm_from_latin1_symbol("gtype"), \
                                                 SCM_UNDEFINED),        \
                                     NULL);                             \
    } while(FALSE)

#define GTYPE_OB_TYPE_SLOT 0
#define GTYPE_OB_REFCNT_SLOT 1
#define GTYPE_TP_NAME_SLOT 2
#define GTYPE_TP_TYPE_SLOT 3
#define GTYPE_TP_DICT_SLOT 4
#define GTYPE_TP_FLAGS_SLOT 5
#define GTYPE_GTYPE_SLOT 6
#define GTYPE_N_SLOTS 7

GType
GType_get_type (SCM gtype)
{
    void *ptr;
    GType type;

    scm_assert_foreign_object_type (GuGType_Type, gtype);
    ptr = scm_foreign_object_ref (gtype, GTYPE_GTYPE_SLOT);
    if (!ptr)
        return 0;
    type = (GType) scm_to_size_t (SCM_PACK_POINTER (ptr));
    return type;
}

unsigned
GType_get_tp_flags (SCM gtype)
{
    unsigned flags;
    void *ptr;

    scm_assert_foreign_object_type (GuGType_Type, gtype);
    ptr = scm_foreign_object_ref (gtype, GTYPE_TP_FLAGS_SLOT);
    if (!ptr)
        return 0;
    else
        return scm_to_uint(SCM_PACK_POINTER(ptr));
}

static SCM
gir_integer_to_GType(SCM sval)
{
    size_t val;
    SCM ret;
    char *name;
    void *slots[GTYPE_N_SLOTS];

    SCM_ASSERT_TYPE(scm_is_exact_integer(sval), sval, SCM_ARG1, "integer->GType", "exact integer");
    val = scm_to_size_t (sval);
    g_debug("gir_integer_to_GType: val is %zu", val);
    name = g_strdup_printf("Integer %zu", val);

    slots[GTYPE_OB_TYPE_SLOT] = NULL;
    slots[GTYPE_OB_REFCNT_SLOT] = SCM_UNPACK_POINTER (scm_from_int (1));
    slots[GTYPE_TP_NAME_SLOT] = SCM_UNPACK_POINTER (scm_from_utf8_string(name));
    slots[GTYPE_TP_TYPE_SLOT] = NULL;
    slots[GTYPE_TP_DICT_SLOT] = NULL;
    slots[GTYPE_TP_FLAGS_SLOT] = SCM_UNPACK_POINTER (scm_from_uint(GU_TPFLAGS_DEFAULT));
    slots[GTYPE_GTYPE_SLOT] = SCM_UNPACK_POINTER (scm_from_uint (val));
    ret = scm_make_foreign_object_n(GuGType_Type, GTYPE_N_SLOTS, slots);
    free (name);
    return ret;
}

static SCM
gir_GType_to_integer(SCM gtype)
{
    GType type;

    scm_assert_foreign_object_type(GuGType_Type, gtype);
    type = GType_get_type (gtype);
    return scm_from_size_t(type);
}


////////////////////////////////////////////////////////////////
/* If OBJ is a string, we look for a GType by that name.
   Otherwise, check that it is one of our GObject wrapper types. */
GType
gu_type_from_object(SCM obj)
{
    SCM gtype;
    GType type;

    g_return_val_if_fail (obj != NULL, 0);

    /* if OBJ is a string, maybe it names a gtype. */
    if (scm_is_string (obj)) {
        char *str;
        str = scm_to_utf8_string (obj);
        type = g_type_from_name (str);
        free (str);

        if (type != 0)
            return type;
        else
            scm_misc_error (NULL, "Cannot find a GType named '~A'", scm_list_1 (obj));
    }

    if (SCM_IS_A_P (obj, GuGType_Type))
        return GType_get_type (obj);

    /* Finally, look for a __gtype__ attribute on the object itself.  If
       it exists, it should contain a GType. */
    SCM ref = scm_slot_ref (obj, scm_from_latin1_symbol("__gtype__"));
    if (scm_is_true (ref) && SCM_IS_A_P (ref, GuGType_Type))
        return GType_get_type (gtype);

    scm_misc_error (NULL, "Cannot infer the GType of '~A'", obj);
    return 0;
}

SCM
gir_to_GType (SCM obj)
{
    GType type = gu_type_from_object(obj);
    if (type)
        return gir_integer_to_GType (scm_from_int (type));
    return SCM_BOOL_F;
}

static GQuark
gu_type_key(GType type) {
    GQuark key;

    g_return_val_if_reached (0);
#if 0
    if (g_type_is_a(type, G_TYPE_INTERFACE)) {
        key = guginterface_type_key;
    } else if (g_type_is_a(type, G_TYPE_ENUM)) {
        key = gugenum_class_key;
    } else if (g_type_is_a(type, G_TYPE_FLAGS)) {
        key = gugflags_class_key;
    } else if (g_type_is_a(type, G_TYPE_POINTER)) {
        key = gugpointer_class_key;
    } else if (g_type_is_a(type, G_TYPE_BOXED)) {
        key = gugboxed_type_key;
    } else {
        key = gugobject_class_key;
    }

    return key;
#endif
}

static SCM
gir_GType_get_scheme_type (SCM self)
{
    GQuark key;
    GType type;
    gpointer ptr;
    SCM scm_type;

    scm_assert_foreign_object_type (GuGType_Type, self);
    type = GType_get_type (self);
    key = gu_type_key (type);
    ptr = g_type_get_qdata(type, key);
    if (!ptr)
        return SCM_NONE;

    return SCM_PACK_POINTER(ptr);
}

static SCM
gir_GType_set_scheme_type_x (SCM self, SCM value)
{
    g_return_val_if_reached (SCM_UNSPECIFIED);

#if 0
    GQuark key;
    GType type;
    gpointer ptr;
    SCM scm_type;

    scm_assert_foreign_object_type (GuGType_Type, self);
    type = GType_get_type (self);
    key = gu_type_key (type);
    if (value == SCM_NONE)
        g_type_set_qdata(type, key, NULL);
    else if (SCM_IS_A_P (value, GuGType_Type))
        g_type_set_qdata(type, key, value);
    else
        scm_misc_error ("GType-set-scheme-data!", "Value '~A' must be NONE or a type object",
                        scm_list_1 (value));
#endif

    return SCM_UNSPECIFIED;
}

static SCM
gir_GType_get_name (SCM self)
{
    GType type;
    const char *name;

    scm_assert_foreign_object_type (GuGType_Type, self);
    type = GType_get_type (self);
    name = g_type_name(type);
    if (name)
        return scm_from_utf8_string (name);

    return scm_from_latin1_string("invalid");
}

static SCM
gir_GType_get_parent (SCM self)
{
    GType type;
    scm_assert_foreign_object_type (GuGType_Type, self);
    type = GType_get_type (self);
    return scm_from_uint (g_type_parent (type));
}

static SCM
gir_GType_get_fundamental (SCM self)
{
    GType type;
    scm_assert_foreign_object_type (GuGType_Type, self);
    type = GType_get_type (self);
    return scm_from_uint (g_type_fundamental (type));
}

static SCM
gir_GType_get_children (SCM self)
{
    GType type;
    GType *children;
    guint n_children, i;
    SCM entry, ret;

    scm_assert_foreign_object_type (GuGType_Type, self);
    type = GType_get_type (self);
    children = g_type_children(type, &n_children);
    ret = SCM_EOL;
    for (i = 0; i < n_children; i ++) {
        entry = scm_make_foreign_object_1 (GuGType_Type,
                                           scm_from_size_t (children[i]));
        ret = scm_append (scm_list_2 (ret, scm_list_1 (entry)));
    }
    g_free (children);
    return ret;
}

static SCM
gir_GType_get_interfaces (SCM self)
{
    GType type;
    GType *interfaces;
    guint n_interfaces, i;
    SCM entry, ret;

    scm_assert_foreign_object_type (GuGType_Type, self);
    type = GType_get_type (self);
    interfaces = g_type_interfaces(type, &n_interfaces);
    ret = SCM_EOL;
    for (i = 0; i < n_interfaces; i ++) {
        entry = scm_make_foreign_object_1 (GuGType_Type,
                                           scm_from_size_t (interfaces[i]));
        ret = scm_append (scm_list_2 (ret, scm_list_1 (entry)));
    }
    g_free (interfaces);
    return ret;
}

static SCM
gir_GType_get_depth (SCM self)
{
    GType type;
    scm_assert_foreign_object_type (GuGType_Type, self);
    type = GType_get_type (self);
    return scm_from_uint (g_type_depth (type));
}

static SCM
gir_GType_is_interface_p (SCM self)
{
    GType type;
    scm_assert_foreign_object_type (GuGType_Type, self);
    type = GType_get_type (self);
    return scm_from_bool (G_TYPE_IS_INTERFACE (type));
}

static SCM
gir_GType_is_classed_p (SCM self)
{
    GType type;
    scm_assert_foreign_object_type (GuGType_Type, self);
    type = GType_get_type (self);
    return scm_from_bool (G_TYPE_IS_CLASSED (type));
}

static SCM
gir_GType_is_instantiatable_p (SCM self)
{
    GType type;
    scm_assert_foreign_object_type (GuGType_Type, self);
    type = GType_get_type (self);
    return scm_from_bool (G_TYPE_IS_INSTANTIATABLE (type));
}

static SCM
gir_GType_is_derivable_p (SCM self)
{
    GType type;
    scm_assert_foreign_object_type (GuGType_Type, self);
    type = GType_get_type (self);
    return scm_from_bool (G_TYPE_IS_DERIVABLE (type));
}

static SCM
gir_GType_is_deep_derivable_p (SCM self)
{
    GType type;
    scm_assert_foreign_object_type (GuGType_Type, self);
    type = GType_get_type (self);
    return scm_from_bool (G_TYPE_IS_DEEP_DERIVABLE (type));
}

static SCM
gir_GType_is_abstract_p (SCM self)
{
    GType type;
    scm_assert_foreign_object_type (GuGType_Type, self);
    type = GType_get_type (self);
    return scm_from_bool (G_TYPE_IS_ABSTRACT (type));
}

static SCM
gir_GType_is_value_abstract_p (SCM self)
{
    GType type;
    scm_assert_foreign_object_type (GuGType_Type, self);
    type = GType_get_type (self);
    return scm_from_bool (G_TYPE_IS_VALUE_ABSTRACT (type));
}

static SCM
gir_GType_is_value_type_p (SCM self)
{
    GType type;
    scm_assert_foreign_object_type (GuGType_Type, self);
    type = GType_get_type (self);
    return scm_from_bool (G_TYPE_IS_VALUE_TYPE (type));
}

static SCM
gir_GType_has_value_table_p (SCM self)
{
    GType type;
    scm_assert_foreign_object_type (GuGType_Type, self);
    type = GType_get_type (self);
    return scm_from_bool (G_TYPE_HAS_VALUE_TABLE (type));
}

static SCM
gir_string_to_GType (SCM s_type_name)
{
    char *type_name;
    GType type;

    SCM_ASSERT_TYPE (scm_is_string (s_type_name), s_type_name, SCM_ARG1, "string-to-GType", "string");

    type_name = scm_to_utf8_string(s_type_name);
    type = g_type_from_name (type_name);
    if (type == 0)
        scm_misc_error ("GType-from-name", "unknown type name '~A'", scm_list_1 (s_type_name));

    return scm_make_foreign_object_1 (GuGType_Type, scm_from_size_t (type));
}

static SCM
gir_GType_is_a_p (SCM self, SCM gparent)
{
    GType parent, child;

    scm_assert_foreign_object_type (self, GuGType_Type);

    parent = gu_type_from_object(gparent);
    if (parent == 0)
        scm_misc_error ("GType-is-a?", "Cannot infer a GType from ~A", scm_list_1 (gparent));
    child = GType_get_type (self);
    return scm_from_bool (g_type_is_a(child, parent));
}


void
gir_init_g_type(void)
{
    MAKE_GTYPE_TYPE;
    GuGType_Type_Store = scm_c_define ("<GType>", GuGType_Type);

#define D(x) scm_permanent_object(scm_c_define(#x, scm_make_foreign_object_1(GuGType_Type, scm_from_size_t(x))))
    D(G_TYPE_INVALID);
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

    scm_c_define_gsubr("integer->GType", 1, 0, 0, gir_integer_to_GType);
    scm_c_define_gsubr("GType->integer", 1, 0, 0, gir_GType_to_integer);
    scm_c_define_gsubr("->GType", 1, 0, 0, gir_to_GType);
    scm_c_define_gsubr("GType-get-scheme-type", 1, 0, 0, gir_GType_get_scheme_type);
    scm_c_define_gsubr("GType-set-scheme-type!", 2, 0, 0, gir_GType_set_scheme_type_x);
    scm_c_define_gsubr("GType-get-name", 1, 0, 0, gir_GType_get_name);
    scm_c_define_gsubr("GType-get-parent", 1, 0, 0, gir_GType_get_parent);
    scm_c_define_gsubr("GType-get-children", 1, 0, 0, gir_GType_get_children);
    scm_c_define_gsubr("GType-get-interfaces", 1, 0, 0, gir_GType_get_interfaces);
    scm_c_define_gsubr("GType-get-depth", 1, 0, 0, gir_GType_get_depth);
    scm_c_define_gsubr("GType-is-interface?", 1, 0, 0, gir_GType_is_interface_p);
    scm_c_define_gsubr("GType-is-classed?", 1, 0, 0, gir_GType_is_classed_p);
    scm_c_define_gsubr("GType-is-instantiatable?", 1, 0, 0, gir_GType_is_instantiatable_p);
    scm_c_define_gsubr("GType-is-derivable?", 1, 0, 0, gir_GType_is_derivable_p);
    scm_c_define_gsubr("GType-is-deep-derivable?", 1, 0, 0, gir_GType_is_deep_derivable_p);
    scm_c_define_gsubr("GType-is-abstract?", 1, 0, 0, gir_GType_is_abstract_p);
    scm_c_define_gsubr("GType-is-value-abstract?", 1, 0, 0, gir_GType_is_value_abstract_p);
    scm_c_define_gsubr("GType-is-value-type?", 1, 0, 0, gir_GType_is_value_type_p);
    scm_c_define_gsubr("GType-has-value-table?", 1, 0, 0, gir_GType_has_value_table_p);
    scm_c_define_gsubr("string->GType", 1, 0, 0, gir_string_to_GType);
    scm_c_define_gsubr("GType-is-a?", 2, 0, 0, gir_GType_is_a_p);

}
