/* -*- Mode: C; c-basic-offset: 4 -*- */
#include <libguile.h>
#include <glib.h>
#include <glib-object.h>
#include "gir_xguile.h"
#include "gir_g_type.h"

/* Data that belongs to the GObject instance, not the Python wrapper */
typedef struct _GuGObjectData {
    SCM type; /* wrapper type for this instance */
    GSList *closures;
} GuGObjectData;

////////////////////////////////////////////////////////////////
/* GuGObject Type: A foreign object type that is an envelope for
   GObject* types */
SCM GuGObject_Type;
SCM GuGObject_Type_Store;

GType PY_TYPE_OBJECT = 0;
GQuark gugobject_custom_key;
GQuark gugobject_class_key;
GQuark gugobject_class_init_key;
GQuark gugobject_wrapper_key;
GQuark gugobject_has_updated_constructor_key;
GQuark gugobject_instance_data_key;



void gugobject_finalize (SCM x);

typedef enum {
	      GUGOBJECT_USING_TOGGLE_REF = 1 << 0,
	      GUGOBJECT_IS_FLOATING_REF = 1 << 1,
	      GUGOBJECT_GOBJECT_WAS_FLOATING = 1 << 2
} GuGObjectFlags;

/* GuGObject Instance: A foreign object with the following slots
   - slot 0: 'ob_type', the SCM foreign-object type used to create this instance
   - slot 1: 'ob_refcnt', an SCM exact integer reference count
   - slot 2: 'obj', a C GObject* pointer
   - slot 3: 'inst_dict', an SCM hash-table
   - slot 4: 'weakreflist', an SCM weak-vector
   - slot 5: flags, an SCM exact-integer of GuGObjectFlags
*/
#define MAKE_GUGOBJECT_TYPE						\
    do {								\
	GuGObject_Type =						\
	    scm_make_foreign_object_type(scm_from_latin1_symbol("<GObject>"), \
			scm_list_n (scm_from_latin1_symbol ("ob_type"), \
			    scm_from_latin1_symbol ("ob_refcnt"), \
				scm_from_latin1_symbol ("obj"), \
				scm_from_latin1_symbol ("inst_dict"), \
				scm_from_latin1_symbol ("weakreflist"), \
				scm_from_latin1_symbol ("flags"), \
				SCM_UNDEFINED),	\
			gugobject_finalize);		\
    } while(FALSE)

#define GUGOBJECT_OB_TYPE_SLOT 0
#define GUGOBJECT_OB_REFCNT_SLOT 1
#define GUGOBJECT_OBJ_SLOT 2
#define GUGOBJECT_INST_DICT_SLOT 3
#define GUGOBJECT_WEAKREFLIST_SLOT 4
#define GUGOBJECT_FLAGS_SLOT 5
#define GUGOBJECT_N_SLOTS 6

static void
GObject_decref (SCM gobj)
{
	void *ptr;
	int refcnt;
	scm_t_struct_finalize func;
	g_assert (SCM_IS_A_P(GuGObject_Type, gobj));

	ptr = scm_foreign_object_ref (gobj, GUGOBJECT_OB_REFCNT_SLOT);
	if (!ptr) {
		refcnt = scm_to_int (SCM_PACK_POINTER (ptr));
		refcnt --;
		scm_foreign_object_set_x (gobj, GUGOBJECT_OB_REFCNT_SLOT, SCM_UNPACK_PTR (scm_from_int (refcnt)));
		if (refcnt == 0) {
			gugobject_finalize(gobj);
		}
	}
}

static void
GObject_incref (SCM gobj)
{
	void *ptr;
	int refcnt;
	g_assert (SCM_IS_A_P (GuGObject_Type, gobj));

	ptr = scm_foreign_object_ref (gobj, GUGOBJECT_OB_REFCNT_SLOT);
	if (!ptr) {
		refcnt = scm_to_int (SCM_PACK_PTR(ptr));
		refcnt ++;
		scm_foreign_object_set_x (gobj, GUGOBJECT_OB_REFCNT_SLOT, SCM_UNPACK_PTR (scm_from_int (refcnt)));
	}
}

static GObject *
GObject_get_obj (SCM gobj)
{
	void *ptr;
	scm_assert_foreign_object_type(GuGObject_Type, gobj);

	ptr = scm_foreign_object_ref (gobj, GUGOBJECT_OBJ_SLOT);
	if (!ptr)
		return NULL;
	return (GObject *) ptr;
}
static SCM
pyg_object_peek_inst_data(GObject *obj)
{
    return SCM_PACK_POINTER (g_object_get_qdata(obj, gugobject_instance_data_key));
}

static inline gboolean
gugobject_toggle_ref_is_active (SCM self)
{
	scm_assert_foreign_object_type(GuGObject_Type, self);
	GObject_set_flags(self, GObject_get_flags(self) & GUGOBJECT_USING_TOGGLE_REF);
}

static inline gboolean
gugobject_toggle_ref_is_required (SCM self)
{
	return (GObject_get_inst_dict(self) != NULL);
}

static void
gug_toggle_notify (gpointer data, GObject *object, gboolean is_last_ref)
{
    SCM self;
	void *ptr;

    ptr = g_object_get_qdata (object, gugobject_wrapper_key);
    if (ptr) {
		self = SCM_UNPACK_POINTER(ptr);
        if (is_last_ref)
            GObject_decref(self);
        else
            GObject_incref(self);
    }
}

/* Called when the inst_dict is first created; switches the 
     reference counting strategy to start using toggle ref to keep the
     wrapper alive while the GObject lives.  In contrast, while
     inst_dict was NULL the python wrapper is allowed to die at
     will and is recreated on demand. */

static inline void
gugobject_toggle_ref_ensure (SCM self)
{
	GObject *obj;
    if (gugobject_toggle_ref_is_active (self))
        return;

    if (!gugobject_toggle_ref_is_required (self))
        return;

	obj = GObject_get_obj (self);
    if (obj == NULL)
        return;

    g_assert(obj->ref_count >= 1);

	GObject_set_flags(self, GObject_get_flags(self) | GUGOBJECT_USING_TOGGLE_REF);
    /* Note that add_toggle_ref will never immediately call back into 
         pyg_toggle_notify */

    Gu_INCREF(self);
    g_object_add_toggle_ref(obj, gug_toggle_notify, NULL);
    g_object_unref(obj);
}

/**
 * pygobject_register_wrapper:
 * @self: the wrapper instance
 *
 * In the constructor of PyGTK wrappers, this function should be
 * called after setting the obj member.  It will tie the wrapper
 * instance to the GObject so that the same wrapper instance will
 * always be used for this GObject instance.
 */

void
gugobject_register_wrapper(SCM self)
{
    g_return_if_fail(self != NULL);
    g_return_if_fail(SCM_IS_A_P (self, GuGObject_Type));

    g_assert(GObject_get_refcnt(self) >= 1);

    /* save wrapper pointer so we can access it later */
    g_object_set_qdata_full(GObject_get_obj(self), gugobject_wrapper_key, self, NULL);

    gugobject_toggle_ref_ensure (self);
}

/**
 * pygobject_new_full:
 * @obj: a GObject instance.
 * @steal: whether to steal a ref from the GObject or add (sink) a new one.
 * @g_class: the GObjectClass
 *
 * This function gets a reference to a wrapper for the given GObject
 * instance.  If a wrapper has already been created, a new reference
 * to that wrapper will be returned.  Otherwise, a wrapper instance
 * will be created.
 *
 * Returns: a reference to the wrapper for the GObject.
 */
static SCM
gu_g_object_new_full(GObject *obj, gboolean steal, gpointer g_class)
{
    SCM self;

    if (obj == NULL) {
	return SCM_NONE;
    }

    /* If the GObject already has a PyObject wrapper stashed in its qdata, re-use it.
     */
    self = SCM_PACK_POINTER (g_object_get_qdata(obj, gugobject_wrapper_key));
    if (self != NULL) {
	/* Note the use of "pygobject_ref_sink" here only deals with PyObject
	 * wrapper ref counts and has nothing to do with GObject.
	 */
	// pygobject_ref_sink(self);

	/* If steal is true, we also want to decref the incoming GObjects which
	 * already have a Python wrapper because the wrapper is already holding a
	 * strong reference.
	 */
	if (steal)
	    g_object_unref (obj);

    } else {

	/* create wrapper */
	GuGObjectData *inst_data = pyg_object_peek_inst_data(obj);
	SCM tp;
	if (inst_data)
	    tp = inst_data->type;
	else {
	    if (g_class)
			tp = pygobject_lookup_class(G_OBJECT_CLASS_TYPE(g_class));
	    else
			tp = pygobject_lookup_class(G_OBJECT_TYPE(obj));
	}
	g_assert(tp != NULL);
        
	/* need to bump type refcount if created with
	   pygobject_new_with_interfaces(). fixes bug #141042 */
	if (GType_get_tp_flags(tp) & GU_TPFLAGS_HEAPTYPE)
	    GU_INCREF(tp);
	void *vals[GUGOBJECT_N_SLOTS];
	vals[GUGOBJECT_OB_TYPE_SLOT] = NULL;
	vals[GUGOBJECT_OB_REFCNT_SLOT] = SCM_PACK_POINTER (scm_from_int(1));
	vals[GUGOBJECT_OBJ_SLOT] = obj;
	vals[GUGOBJECT_INST_DICT_SLOT] = NULL;
	vals[GUGOBJECT_WEAKREFLIST_SLOT] = NULL;
	vals[GUGOBJECT_FLAGS_SLOT] = SCM_PACK_POINTER (scm_from_uint(0));
	self = scm_make_foreign_object_n (GuGObject_Type, GUGOBJECT_N_SLOTS, vals);

	/* If we are not stealing a ref or the object is floating,
	 * add a regular ref or sink the object. */
	if (g_object_is_floating (obj))
	    GObject_set_flags(self, GObject_get_flags(self) | GUGOBJECT_GOBJECT_WAS_FLOATING);
	if (!steal || GObject_get_flags(self) & GUGOBJECT_GOBJECT_WAS_FLOATING)
	    g_object_ref_sink (obj);

	gu_gobject_register_wrapper(self);
    }

    return self;
}


static SCM
gu_g_object_new(GObject *obj)
{
    return gu_g_object_new_full(obj,
				/*steal=*/FALSE,
				NULL);
}

static SCM
gir_make_GObject (SCM gtype, SCM args_alist)
{
    GType type;
    GObjectClass *class;
    char **names;
    GValue *values;
    size_t n_params;
  
    scm_assert_foreign_object_type (GuGType_Type, type);

    type = gu_type_from_object (gtype);
    if (G_TYPE_IS_ABSTRACT(type)) {
	scm_misc_error ("make-GObject",
			"cannot create instance of abstract (non-instantiable) type '~A'",
			scm_list_1 (gtype));
    }
    class = g_type_class_ref (type);
    if (class == NULL)
	scm_misc_error ("make-GObject",
			"could not get a reference to type class from type '~A'",
			scm_list_1 (gtype));

    if (scm_is_list (args_alist)) {
	n_params = scm_to_size_t (scm_length (args_alist));
	names = g_malloc (n_params * sizeof(char *));
	values = g_malloc (n_params * sizeof (GValue));
	for (int i = 0; i < n_params; i ++) {
	    SCM entry;
	    gchar *key_str;
	    GParamSpec *pspec;

	    entry = scm_list_ref (args_alist, scm_from_int (i));
	    if (!scm_is_pair (entry))
		scm_misc_error ("make-GObject",
				"Expected a key/value pair at '~A'", scm_list_1(entry));
	    else if (!scm_is_string (scm_car (entry)))
		scm_misc_error ("make-GObject",
				"Expected a string at '~A'", scm_list_1(scm_car(entry)));
	    names[i] = scm_to_utf8_string (scm_car (entry));
	    pspec = g_object_class_find_property (class, names[i]);
	    if (!pspec)
		scm_misc_error ("make-GObject", "gobject '~A' does not support property '~A'",
				scm_list_2 (gtype, scm_car));
	    g_value_init(&values[i], G_PARAM_SPEC_VALUE_TYPE(pspec));
	    gug_value_from_scm_with_error(&values[i], scm_cdr(entry));
	}
    }

    GObject *new = g_object_new_with_properties (type, n_params, names, values);
    if (!new)
	scm_misc_error ("make-GObject", "could not create object", SCM_EOL);
  
    for (int i = 0; i < n_params; i ++) {
	g_free(names[i]);
	g_value_unset(&values[i]);
    }
    g_free (names);
    g_type_class_unref(class);
  
    if (new)
	return scm_make_foreign_object_1(GuGObject_Type, new);

    return SCM_BOOL_F;
}
    
void
gir_init_gobject(void)
{
    gugobject_wrapper_key = g_quark_from_static_string("PyGObject::wrapper");
    MAKE_GUGOBJECT_TYPE;
    GuGObject_Type_Store = scm_c_define("<GObject>", GuGObject_Type);

    gugobject_custom_key = g_quark_from_static_string("PyGObject::custom");
    gugobject_class_key = g_quark_from_static_string("PyGObject::class");
    gugobject_class_init_key = g_quark_from_static_string("PyGObject::class-init");
    gugobject_wrapper_key = g_quark_from_static_string("PyGObject::wrapper");
    gugobject_has_updated_constructor_key =
        g_quark_from_static_string("PyGObject::has-updated-constructor");
    gugobject_instance_data_key = g_quark_from_static_string("PyGObject::instance-data");	

    scm_c_define_gsubr("make-GObject", 2, 0, 0, gir_make_GObject);
}
