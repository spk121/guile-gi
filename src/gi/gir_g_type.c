/* -*- Mode: C; c-basic-offset: 4 -*- */
#include <libguile.h>
#include <glib.h>
#include <glib-object.h>
#include <girepository.h>

#include "gir_g_type.h"
#include "gir_xguile.h"
#include "gir-object.h"

extern GQuark guginterface_type_key;
extern SCM GuGInterface_Type;
static SCM GuGType_new_with_interfaces(GType gtype);
static SCM GType_get_bases(GType gtype);

SCM GuGType_Type;
SCM GuGType_Type_Store;
SCM GType_Lookup_Table;
SCM GType_Lookup_Table_Store;

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
GuGType_get_type (SCM gtype)
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
    void *ptr;

    scm_assert_foreign_object_type (GuGType_Type, gtype);
    ptr = scm_foreign_object_ref (gtype, GTYPE_TP_FLAGS_SLOT);
    if (!ptr)
        return 0;
    else
        return scm_to_uint(SCM_PACK_POINTER(ptr));
}

static void GuGType_incref (SCM self)
{
    void *ptr;
    unsigned refcnt;
    ptr = scm_foreign_object_ref (self, GTYPE_OB_REFCNT_SLOT);
    if (!ptr) {
	refcnt = scm_to_uint (SCM_PACK_POINTER (ptr));
	scm_foreign_object_set_x (self, GTYPE_OB_REFCNT_SLOT, scm_from_uint(refcnt + 1));
    }
}
static void
GuGType_set_tp_name (SCM self, const char *name)
{
    SCM sname = scm_from_utf8_string (name);
    scm_assert_foreign_object_type (GuGType_Type, self);
    scm_foreign_object_set_x (self, GTYPE_TP_NAME_SLOT, sname);
}

static void
GuGType_set_tp_dict (SCM self, SCM dict)
{
    scm_assert_foreign_object_type (GuGType_Type, self);
    scm_foreign_object_set_x (self, GTYPE_TP_DICT_SLOT, dict);
}


static SCM
GuGType_make_from_GType(GType val)
{
    SCM ret;
    char *name;
    void *slots[GTYPE_N_SLOTS];

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
gir_integer_to_GuGType(SCM sval)
{
    size_t val;

    SCM_ASSERT_TYPE(scm_is_exact_integer(sval), sval, SCM_ARG1, "integer->GType", "exact integer");
    val = scm_to_size_t (sval);
    g_debug("gir_integer_to_GType: val is %zu", val);

    return GuGType_make_from_GType(val);;
}

static SCM
gir_GType_to_integer(SCM gtype)
{
    GType type;

    scm_assert_foreign_object_type(GuGType_Type, gtype);
    type = GuGType_get_type (gtype);
    return scm_from_size_t(type);
}




/* re pygi_type_import_by_name */
SCM
GuGType_import_by_name (const char *namespace_, const char *name)
{
    gchar *module_name;
    SCM s_module;
    SCM s_object;

    /* In pygi_type_import_by_name, this proc would import a
       dynamically created module named "gi.repository.{namespace_},
       and then search for a wrapper using the {name} as a key.  This
       doesn't translate directly in Guile because Guile modules can't
       be purely virtual, e.g., they need to always have a file on the
       filesystem.  So, unless we actually create modules like (gi
       repository {namespace}) with files on the filesystem, this
       strategy won't work.

       As a workaround, we'll create a hashtable mapping namespace/name
       to GuGType, for now.
    */
    
    module_name = g_strconcat("gi.repository.", namespace_, name, NULL);
    s_module = scm_from_utf8_string (module_name);
    s_object = scm_hash_ref(GType_Lookup_Table, s_module, SCM_BOOL_F);
    g_free (module_name);

    return s_object;
}

/* re pygi_type_import_by_g_type */
SCM
GuGType_import_by_GType (GType g_type)
{
    GIRepository *repository;
    GIBaseInfo *info;
    SCM type;

    repository = g_irepository_get_default();

    info = g_irepository_find_by_gtype (repository, g_type);
    if (info == NULL) {
        return SCM_NONE;
    }

    type = GuGType_import_by_GIBaseInfo (info);
    g_base_info_unref (info);

    return type;
}

/* re pygi_type_import_by_gi_info */
SCM
GuGType_import_by_GIBaseInfo (GIBaseInfo *info)
{
    return GuGType_import_by_name (g_base_info_get_namespace (info),
				   g_base_info_get_name (info));
}

/**
 * pygobject_lookup_class:
 * @gtype: the GType of the GObject subclass.
 *
 * This function looks up the wrapper class used to represent
 * instances of a GObject represented by @gtype.  If no wrapper class
 * or interface has been registered for the given GType, then a new
 * type will be created.
 *
 * Does not set an exception when NULL is returned.
 *
 * Returns: The wrapper class for the GObject or NULL if the
 *          GType has no registered type and a new type couldn't be created
 */
/* re pygobject_lookup_class */
SCM
GuGType_lookup_by_GType(GType gtype)
{
    SCM gu_type;

    if (gtype == G_TYPE_INTERFACE)
        return GuGInterface_Type;
    
    gu_type = g_type_get_qdata(gtype, gugobject_class_key);
    if (gu_type == NULL) {
        gu_type = g_type_get_qdata(gtype, guginterface_type_key);

        if (gu_type == NULL) {
            gu_type = GuGType_import_by_GType(gtype);
        }

        if (scm_is_false (gu_type)) {
            gu_type = GuGType_new_with_interfaces(gtype);
            // PyErr_Clear ();
            g_type_set_qdata(gtype, guginterface_type_key, gu_type);
        }
    }
    
    return gu_type;
}


SCM
GType_get_bases(GType gtype)
{
    GType *interfaces;
    GType parent_type, interface_type;
    guint n_interfaces;
    SCM gu_parent_type, gu_interface_type;
    SCM bases = SCM_EOL;
    guint i;
    
    if (G_UNLIKELY(gtype == G_TYPE_OBJECT))
        return NULL;

    /* Lookup the parent type */
    parent_type = g_type_parent(gtype);
    gu_parent_type = GuGType_lookup_by_GType(parent_type);
    interfaces = g_type_interfaces(gtype, &n_interfaces);

    /* We will always put the parent at the first position in bases */
    GuGType_incref(gu_parent_type);  /* GuTuple_SetItem steals a reference */
    bases = scm_list_1 (gu_parent_type);

    /* And traverse interfaces */
    if (n_interfaces) {
	for (i = 0; i < n_interfaces; i++) {
	    interface_type = interfaces[i];
	    gu_interface_type = GuGType_lookup_by_GType(interface_type);
            GuGType_incref(gu_interface_type); /* PyTuple_SetItem steals a reference */
	    bases = scm_list_append(bases, gu_interface_type);
	}
    }
    g_free(interfaces);
    return bases;
}


/**
 * pygobject_new_with_interfaces
 * @gtype: the GType of the GObject subclass.
 *
 * Creates a new PyTypeObject from the given GType with interfaces attached in
 * bases.
 *
 * Returns: a PyTypeObject for the new type or NULL if it couldn't be created
 */
static SCM
GuGType_new_with_interfaces(GType gtype)
{
    // PyGILState_STATE state;
    SCM type;
    SCM dict;
    SCM gu_parent_type;
    SCM bases;

    // state = PyGILState_Ensure();

    bases = GType_get_bases(gtype);
    if (scm_is_false(bases))
	return SCM_NONE;

    type = GuGType_make_from_GType(gtype);

    gu_parent_type = scm_c_list_ref (bases, 0);

    dict = scm_c_make_hash_table (10);
    scm_hash_set_x (dict, scm_from_latin1_string("__gtype__"), type);
    
    /* set up __doc__ descriptor on type */
    scm_hash_set_x (dict, scm_from_latin1_string("__doc__"), scm_from_latin1_string("FIXME"));
    //scm_hash_set_x (dict, scm_from_latin1_string("__doc__"), pyg_object_descr_doc_get());

    /* Something special to point out that it's not accessible through
     * gi.repository */
    scm_hash_set_x (dict,
		    scm_from_latin1_string("__module__"),
		    scm_from_latin1_string("__gi__"));

    GuGType_set_tp_name (type, g_type_name (gtype));
    GuGType_set_tp_dict (type, dict);
    g_type_set_qdata(gtype, gugobject_class_key, type);
    
    return type;
}



////////////////////////////////////////////////////////////////

/* If OBJ is a string, we look for a GType by that name.
   Otherwise, check that it is one of our GObject wrapper types. */
GType
gu_type_from_object(SCM obj)
{
    GType type;
    SCM dict, ref;

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
        return GuGType_get_type (obj);

    /* Finally, look for a __gtype__ attribute on the object itself.  If
       it exists, it should contain a GType. */
    if (SCM_IS_A_P (obj, GuGObject_Type)) {
	dict = scm_slot_ref (obj, scm_from_latin1_symbol("tp_dict"));
	if (scm_is_hash_table (dict)) {
	    ref = scm_hash_ref (dict, scm_from_latin1_string("__gtype__"),
				SCM_BOOL_F);
	    if (scm_is_true (ref) && SCM_IS_A_P (ref, GuGType_Type))
		return GuGType_get_type (ref);
	}
    }

    scm_misc_error (NULL, "Cannot infer the GType of '~A'", scm_list_1 (obj));
    return 0;
}

static SCM
gir_to_GuGType (SCM obj)
{
    GType type = gu_type_from_object(obj);
    if (type)
        return gir_integer_to_GuGType (scm_from_int (type));
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
    type = GuGType_get_type (self);
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
    type = GuGType_get_type (self);
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
    type = GuGType_get_type (self);
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
    type = GuGType_get_type (self);
    return scm_from_uint (g_type_parent (type));
}

static SCM
gir_GType_get_fundamental (SCM self)
{
    GType type;
    scm_assert_foreign_object_type (GuGType_Type, self);
    type = GuGType_get_type (self);
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
    type = GuGType_get_type (self);
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
    type = GuGType_get_type (self);
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
    type = GuGType_get_type (self);
    return scm_from_uint (g_type_depth (type));
}

static SCM
gir_GType_is_interface_p (SCM self)
{
    GType type;
    scm_assert_foreign_object_type (GuGType_Type, self);
    type = GuGType_get_type (self);
    return scm_from_bool (G_TYPE_IS_INTERFACE (type));
}

static SCM
gir_GType_is_classed_p (SCM self)
{
    GType type;
    scm_assert_foreign_object_type (GuGType_Type, self);
    type = GuGType_get_type (self);
    return scm_from_bool (G_TYPE_IS_CLASSED (type));
}

static SCM
gir_GType_is_instantiatable_p (SCM self)
{
    GType type;
    scm_assert_foreign_object_type (GuGType_Type, self);
    type = GuGType_get_type (self);
    return scm_from_bool (G_TYPE_IS_INSTANTIATABLE (type));
}

static SCM
gir_GType_is_derivable_p (SCM self)
{
    GType type;
    scm_assert_foreign_object_type (GuGType_Type, self);
    type = GuGType_get_type (self);
    return scm_from_bool (G_TYPE_IS_DERIVABLE (type));
}

static SCM
gir_GType_is_deep_derivable_p (SCM self)
{
    GType type;
    scm_assert_foreign_object_type (GuGType_Type, self);
    type = GuGType_get_type (self);
    return scm_from_bool (G_TYPE_IS_DEEP_DERIVABLE (type));
}

static SCM
gir_GType_is_abstract_p (SCM self)
{
    GType type;
    scm_assert_foreign_object_type (GuGType_Type, self);
    type = GuGType_get_type (self);
    return scm_from_bool (G_TYPE_IS_ABSTRACT (type));
}

static SCM
gir_GType_is_value_abstract_p (SCM self)
{
    GType type;
    scm_assert_foreign_object_type (GuGType_Type, self);
    type = GuGType_get_type (self);
    return scm_from_bool (G_TYPE_IS_VALUE_ABSTRACT (type));
}

static SCM
gir_GType_is_value_type_p (SCM self)
{
    GType type;
    scm_assert_foreign_object_type (GuGType_Type, self);
    type = GuGType_get_type (self);
    return scm_from_bool (G_TYPE_IS_VALUE_TYPE (type));
}

static SCM
gir_GType_has_value_table_p (SCM self)
{
    GType type;
    scm_assert_foreign_object_type (GuGType_Type, self);
    type = GuGType_get_type (self);
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

    return GuGType_make_from_GType (type);
}

static SCM
gir_GType_is_a_p (SCM self, SCM gparent)
{
    GType parent, child;

    scm_assert_foreign_object_type (self, GuGType_Type);

    parent = gu_type_from_object(gparent);
    if (parent == 0)
        scm_misc_error ("GType-is-a?", "Cannot infer a GType from ~A", scm_list_1 (gparent));
    child = GuGType_get_type (self);
    return scm_from_bool (g_type_is_a(child, parent));
}


void
gir_init_g_type(void)
{
    MAKE_GTYPE_TYPE;
    GuGType_Type_Store = scm_c_define ("<GType>", GuGType_Type);
    GType_Lookup_Table = scm_c_make_hash_table(37);
    GType_Lookup_Table_Store = scm_c_define("*Type-Lookup-Table*", GType_Lookup_Table);
    
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

    scm_c_define_gsubr("integer->GType", 1, 0, 0, gir_integer_to_GuGType);
    scm_c_define_gsubr("GType->integer", 1, 0, 0, gir_GType_to_integer);
    scm_c_define_gsubr("->GType", 1, 0, 0, gir_to_GuGType);
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
