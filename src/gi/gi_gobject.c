/* -*- Mode: C; c-basic-offset: 4 -*- */
#include "gi_gobject.h"
#include "gi_gvalue.h"
#include "gi_gtype.h"
#include "gi_gsignal.h"
#include "gi_gparamspec.h"
#include "gi_ginterface.h"
#include "gir_func.h"
#include "gir_type.h"
#include <glib-object.h>
#include <glib.h>
#include <girepository.h>

GQuark gi_gobject_instance_data_key;

typedef struct _GuileSpecifiedGObjectClassData {
    SCM disposer;
    char padding[56];
} GuileSpecifiedGObjectClassData;

typedef struct _GuileSpecifiedGObjectInstanceData {
    SCM obj;
    char padding[56];
} GuileSpecifiedGObjectInstanceData;

#define GUILE_SPECIFIED_GOBJECT_CLASS_SIZE         (sizeof (GuileSpecifiedGObjectClassData))
#define GUILE_SPECIFIED_GOBJECT_INSTANCE_SIZE      (sizeof (GuileSpecifiedGObjectInstanceData))
#define PROPERTY_ID_OFFSET (5)

typedef struct _GuileSpecifiedGObjectClassInfo
{
    GType type;
    gsize parent_class_size;
    gsize parent_instance_size;
    GPtrArray *properties;
    GPtrArray *signals;
    SCM disposer;
} GuileSpecifiedGObjectClassInfo;

static GQuark gi_gobject_wrapper_key;
static GQuark gi_gobject_custom_key;
static GQuark gi_gobject_class_key;

static void
set_guile_specified_property (GObject *object, guint property_id,
	      const GValue *value, GParamSpec *pspec);
static void
get_guile_specified_property (GObject *object, guint property_id,
	      GValue *value, GParamSpec *pspec);
static void
dispose (GObject *object);
static void
finalize (GObject *object);
static SCM
gi_get_property_value (const char *func, SCM instance, GParamSpec *pspec);
static inline int
gugobject_clear(SCM self);



/* re pygobject-object.c:61 gclosure from pyfunc */
GClosure *
gclosure_from_scm_func (SCM object, SCM func)
{
    GSList *l;
    GuGObjectData *inst_data;
    GObject *obj;

    obj = gi_gobject_get_obj (object);
    inst_data = gi_gobject_peek_inst_data(obj);
    if (inst_data) {
        for (l = inst_data->closures; l; l = l->next) {
            GuGClosure *guclosure = l->data;
            int res = scm_is_true (scm_eq_p (guclosure->callback, func));
            if (res) {
                return (GClosure*)guclosure;
            }
        }
    }
    return NULL;
}

/* re pygobject-object.c:103 pygobject_data_free */
static void
gugobject_data_free (GuGObjectData *data)
{
    GSList *closures, *tmp;

    tmp = closures = data->closures;
    data->closures = NULL;
    data->type = NULL;
    while (tmp) {
 	GClosure *closure = tmp->data;
 
          /* we get next item first, because the current link gets
           * invalidated by gugobject_unwatch_closure */
 	tmp = tmp->next;
 	g_closure_invalidate(closure);
    }
 
    if (data->closures != NULL)
 	g_warning("invalidated all closures, but data->closures != NULL !");

    g_free(data);
}

static inline GuGObjectData *
gugobject_data_new(void)
{
    GuGObjectData *data;
    data = g_new0(GuGObjectData, 1);
    return data;
}

static inline GuGObjectData *
gugobject_get_inst_data(SCM self)
{
    GObject *obj;
    GuGObjectData *inst_data;

    obj = gi_gobject_get_obj (self);
    if (G_UNLIKELY(!obj))
        return NULL;
    inst_data = g_object_get_qdata(obj, gi_gobject_instance_data_key);
    if (inst_data == NULL)
    {
        inst_data = gugobject_data_new();

        inst_data->type = self;

        g_object_set_qdata_full(obj, gi_gobject_instance_data_key,
                                inst_data, (GDestroyNotify) gugobject_data_free);
    }
    return inst_data;
}

/* Looks up the wrapper class used to represent instances of
 * GObject represented by @gtype. If no wrapper class has been
 * registered for GType, then a new type will be created.
 * returns the wrapper class, or #f */
/* re pygobject-object.c:953, pygobject_lookup_class */
#if 0
SCM
gi_gobject_lookup_class(GType gtype)
{
    SCM gu_type;
    if (gtype == G_TYPE_INTERFACE)
	return gi_ginterface_type;

    gu_type = g_type_get_qdata(gtype, gi_gobject_class_key);
    if (gu_type == NULL) {
	gu_type = g_type_get_qdata(gtype, gi_ginterface_type_key);
	//if (gu_type == NULL) {
	//    gu_type = gi_gtype_import_by_g_type(gtype);
	}
	if (gu_type == NULL) {
	    gu_type = gi_gobject_new_with_interfaces (gtype);
	    g_type_set_qdata (gtype, gi_ginterface_type_key, gu_type);
	}
    }
    return gu_type;
}
#endif

/* re pygobject-object.c:980, pygobject_new_full */
static SCM
gi_gobject_new_full (GObject *obj, gboolean steal, gpointer g_class)
{
    gpointer ptr;
    SCM self;
    if (obj == NULL)
	return SCM_BOOL_F;

    /* Check if this obj is wrapped already. */
    ptr = g_object_get_qdata (obj, gi_gobject_wrapper_key);
    if (ptr) {
	self = ptr;
	if (steal)
	    g_object_unref (obj);
    } else {
	/* create wrapper */
	GuGObjectData *inst_data = gi_gobject_peek_inst_data (obj);
	SCM tp;
	if (inst_data)
	    tp = inst_data->type;
	else {
	    /* if (g_class) */
	    /* 	tp = gugobject_lookup_class (G_OBJECT_CLASS_TYPE (g_class)); */
	    /* else */
	    /* 	tp = gugobject_lookup_class (G_OBJECT_TYPE (obj)); */
	    tp = gi_gtype_c2g(G_OBJECT_TYPE (obj));
	}
	g_assert (tp != NULL && scm_is_true (tp));

	//if (gi_gtype_get_flags (tp) & Gu_TPFLAGS_HEAPTYPE)
	//    Gu_INCREF(tp);
	self = scm_make_foreign_object_0(gi_gobject_type);
	gi_gobject_set_ob_type (self, gi_gtype_get_type (tp));
	gi_gobject_set_inst_dict (self, SCM_BOOL_F);
	gi_gobject_set_weakreflist (self, SCM_BOOL_F);
	gi_gobject_set_flags (self, 0);
	gi_gobject_set_obj (self, obj);

	if (g_object_is_floating (obj))
	    gi_gobject_set_flags (self, GI_GOBJECT_GOBJECT_WAS_FLOATING);
	if (!steal || gi_gobject_get_flags(self) & GI_GOBJECT_GOBJECT_WAS_FLOATING)
	    g_object_ref_sink (obj);

	// gi_gobject_register_wrapper (self);
    }
    return self;
}

/* re pygobject-object.c: 1059 pygobject_new */
static SCM
gi_gobject_new (GObject *obj)
{
    return gi_gobject_new_full (obj, FALSE, NULL);
}

/* re pygobject-object.c: 1067 pygobject_unwatch_closure */
static void
gugobject_unwatch_closure(gpointer data, GClosure *closure)
{
    GuGObjectData *inst_data = data;

    inst_data->closures = g_slist_remove (inst_data->closures, closure);
}

/* Adds a closure to the list of watched closures for the wrapper.
 * The closure should be created with gi_gclosure_new. */
/* re pygobject-object.c:1093 pygobject_watch_closure */
static void
gugobject_watch_closure(SCM self, GClosure *closure)
{
    GuGObjectData *data;

    g_return_if_fail(SCM_IS_A_P(self, gi_gobject_type));
    g_return_if_fail(closure != NULL);
    
    data = gugobject_get_inst_data(self);
    g_return_if_fail(data != NULL);
    g_return_if_fail(g_slist_find(data->closures, closure) == NULL);
    
    data->closures = g_slist_prepend(data->closures, closure);
    g_closure_add_invalidate_notifier(closure, data, gugobject_unwatch_closure);
}

static void
gug_toggle_notify (gpointer data, GObject *object, gboolean is_last_ref)
{
    /* Avoid thread safety problems by using qdata for wrapper retrieval
     * instead of the user data argument.
     * See: https://bugzilla.gnome.org/show_bug.cgi?id=709223
     */
    SCM self = g_object_get_qdata (object, gi_gobject_wrapper_key);
    /* if (self) { */
    /*     if (is_last_ref) */
    /*         Py_DECREF(self); */
    /*     else */
    /*         Py_INCREF(self); */
    /* } */
}

static inline gboolean
gugobject_toggle_ref_is_required (SCM self)
{
    SCM dict = gi_gobject_get_inst_dict (self);
    // FIXME - maybe check if hash table is not empty
    return scm_is_true (dict);
}

static inline gboolean
gugobject_toggle_ref_is_active (SCM self)
{
    unsigned flags = gi_gobject_get_flags(self);
    return flags & GI_GOBJECT_USING_TOGGLE_REF;
}



/****************************************************************/
/* SCM GObject behavior                                         */

/* Compare with pygobject-object.c:1115 pygobject_dealloc */
void
gi_gobject_finalizer (SCM self)
{
    /* Clear out the weak reference vector, if it exists. */
    gi_gobject_set_weakreflist(self, SCM_BOOL_F);

    /* MLG - PyGObject says this is here because finalization might
     * cause a new type to be created?  Does that matter to use? */
    gugobject_get_inst_data(self);
    gugobject_clear(self);
}

static inline int
gugobject_clear(SCM self)
{
    GObject *obj = gi_gobject_get_obj (self);
    if (obj) {
        g_object_set_qdata_full(obj, gi_gobject_wrapper_key, NULL, NULL);
        if (gugobject_toggle_ref_is_active (self)) {
            g_object_remove_toggle_ref(obj, gug_toggle_notify, NULL);
	    unsigned flags = gi_gobject_get_flags (self);
	    gi_gobject_set_flags (self, flags & ~GI_GOBJECT_USING_TOGGLE_REF);
        } else {
            g_object_unref(obj);
        }
	gi_gobject_set_obj(self, NULL);
    }
    gi_gobject_set_inst_dict(self, SCM_BOOL_F);
    return 0;
}

/* re pygobject-object.c:1660 connect_helper */
static SCM
connect_helper (SCM self, gchar *name, SCM callback, SCM extra_args, SCM object, gboolean after)
{
    guint sigid;
    GQuark detail = 0;
    GClosure *closure = NULL;
    gulong handlerid;
    GSignalQuery query_info;

    if (!g_signal_parse_name(name, G_OBJECT_TYPE (gi_gobject_get_obj (self),
						  &sigid, &detail, TRUE))) {
	scm_misc_error ("connect_helper",
			"~A: unknown signal name ~A",
			scm_list_2 (self, scm_from_utf8_string(name)));
    }

    g_signal_query (sigid, &query_info);
    if (g_type_get_qdata (gtype, gi_gobject_custom_key) == NULL) {
	/* The signal is implemented by a non-Scheme class. */
	closure = gi_gsignal_closure_new (self, query_info.itype,
					  query_info.signal_name, callback,
					  extra_args, object);
    }

    if (!closure) {
	/* The signal is implemented at the Scheme level, probably */
	closure = gug_closure_new (callback, extra_args, object);
    }

    gugobject_watch_closure (self, closure);
    handlerid = g_signal_connect_closure_by_id (gi_gobject_get_obj (self), sigid, detail, closure after);

    return scm_from_ulong (handlerid);
}

static SCM
scm_signal_connect (SCM self, SCM s_name, SCM proc, SCM rest)
{
    scm_assert_foreign_object_type (gi_gobject_type, self);

    char *name = scm_to_utf8_string (name);
    SCM ret = connect_helper (self, name, proc, rest, SCM_BOOL_F, FALSE);
    free (name);
    return ret;
}

/* re pygobject-object.c:2064 pygobject_disconnect_by_func */
static SCM
scm_gobject_disconnect_by_func(SCM self, SCM func)
{
    GClosure *closure = NULL;
    GObject *obj;
    guint retval;
    
    scm_assert_foreign_object_type (gi_gobject_type, self);
    SCM_ASSERT (scm_is_true (scm_procedure_p (func)), func, SCM_ARG2,
		"gobject-disconnect-by-func");

    closure = gclosure_from_scm_func(self, func);
    if (!closure)
	scm_misc_error ("gobject-disconnect-by-func",
			"nothing connected to ~S",
			scm_list_1 (func));

    obj = gi_gobject_get_obj (self);
    retval = g_signal_handlers_disconnect_matched(obj,
						  G_SIGNAL_MATCH_CLOSURE,
						  0, 0,
						  closure,
						  NULL, NULL);
    return scm_from_uint (retval);
}

/* pygobject-object.c: 2098, pygobject_handler_block_by_func */
static SCM
scm_gobject_handler_block_by_func(SCM self, SCM func)
{
    GClosure *closure = NULL;
    GObject *obj;
    guint retval;
    
    scm_assert_foreign_object_type (gi_gobject_type, self);
    SCM_ASSERT (scm_is_true (scm_procedure_p (func)), func, SCM_ARG2,
		"gobject-handler-block-by-func");

    closure = gclosure_from_scm_func(self, func);
    if (!closure)
	scm_misc_error ("gobject-handler-block-by-func",
			"nothing connected to ~S",
			scm_list_1 (func));
    
    obj = gi_gobject_get_obj (self);
    retval = g_signal_handlers_block_matched(obj,
					     G_SIGNAL_MATCH_CLOSURE,
					     0, 0,
					     closure,
					     NULL, NULL);
    return scm_from_uint (retval);
}

/* pygobject-object.c: 2032, pygobject_handler_unblock_by_func */
static SCM
scm_gobject_handler_unblock_by_func(SCM self, SCM func)
{
    GClosure *closure = NULL;
    GObject *obj;
    guint retval;
    
    scm_assert_foreign_object_type (gi_gobject_type, self);
    SCM_ASSERT (scm_is_true (scm_procedure_p (func)), func, SCM_ARG2,
		"gobject-handler-unblock-by-func");

    closure = gclosure_from_scm_func(self, func);
    if (!closure)
	scm_misc_error ("gobject-handler-unblock-by-func",
			"nothing connected to ~S",
			scm_list_1 (func));
    
    obj = gi_gobject_get_obj (self);
    retval = g_signal_handlers_unblock_matched(obj,
					       G_SIGNAL_MATCH_CLOSURE,
					       0, 0,
					       closure,
					       NULL, NULL);
    return scm_from_uint (retval);
}


static void
marshal_signals (GClosure *closure,
		 GValue *return_value,
		 guint n_param_values,
		 const GValue *param_values,
		 gpointer invocation_hint,
		 gpointer marshal_data)
{
    
}

static void
make_new_signal (SignalSpec *signal_spec, gpointer user_data)
{
    GType instance_type = GPOINTER_TO_SIZE (user_data);
    g_signal_newv (signal_spec->signal_name,
		   instance_type,
		   signal_spec->signal_flags,
		   NULL, /* closure */
		   signal_spec->accumulator,
		   signal_spec->accu_data,
		   marshal_signals,
		   signal_spec->return_type,
		   signal_spec->n_params,
		   signal_spec->param_types);
}

static void
init_guile_specified_gobject_class(GObjectClass *class, gpointer class_info)
{
    GType type = G_TYPE_FROM_CLASS (class);
    GuileSpecifiedGObjectClassInfo *init_info = class_info;
    size_t n_properties = init_info->properties->len;
    GParamSpec **properties = (GParamSpec **) init_info->properties->pdata;
    GuileSpecifiedGObjectClassData *self;

    class->set_property = set_guile_specified_property;
    class->get_property = get_guile_specified_property;
    class->dispose = dispose;
    class->finalize = finalize;
    
    /* Since the parent type could be anything, some pointer math is
     * required to figure out where our part of the object class is
     * located. */
    self = (GuileSpecifiedGObjectClassData *) ((char *) class + init_info->parent_class_size);
    self->disposer = init_info->disposer;

    g_ptr_array_foreach (init_info->signals,
			 (GFunc) make_new_signal,
			 GSIZE_TO_POINTER (type));

    for (size_t i = 1; i <= n_properties; i ++)
	g_object_class_install_property (class, i, properties[i-1]);
}


static void
init_instance(GTypeInstance *instance, gpointer class_ptr)
{
    GType type = G_TYPE_FROM_CLASS(class_ptr);
    GType parent_type = g_type_parent (type);
    guint n_properties;
    GParamSpec **properties;
    GTypeQuery query;
    GuileSpecifiedGObjectInstanceData *instance_data;
    SCM inst_dict;
    SCM obj;

    g_type_query(parent_type, &query);

    instance_data = (GuileSpecifiedGObjectInstanceData *) ((char *) instance + query.instance_size);
    properties = g_object_class_list_properties (class_ptr, &n_properties);
    
    /* This is both the Guile-side representation of this object and
       the location in memory where the properties are stored. */
    obj = scm_make_foreign_object_0 (gi_gobject_type);
    gi_gobject_set_ob_type (obj, type);
    inst_dict = scm_make_hash_table (scm_from_int (10));
    gi_gobject_set_obj (obj, (GObject *) instance);
    gi_gobject_set_inst_dict (obj, inst_dict);
    gi_gobject_set_weakreflist (obj, SCM_BOOL_F);
    gi_gobject_set_flags (obj, 0);

    /* Ref count */
    gi_gobject_set_ob_refcnt (obj, 1);

    /* We're using a hash table as the property variable store for
       this object. */
    for (guint i = 0; i < n_properties; i ++) {
	SCM sval;
	GValue _default = G_VALUE_INIT;
	
	g_value_init (&_default, G_PARAM_SPEC_TYPE (properties[i]));
	sval = gi_gvalue_as_scm (&_default, TRUE);
	scm_hash_set_x (inst_dict,
			scm_from_utf8_string (g_param_spec_get_name (properties[i])),
			sval);
    }
    
    instance_data->obj = obj;
    g_object_set_qdata (G_OBJECT(instance), gi_gobject_wrapper_key, obj);
}

static void
wrap_object (GObject *object)
{
    /* Somehow, you managed to make a pointer to a Guile-defined class
     * object without actually making the Scheme wrapper.  Let's try
     * to add it now. */
    
}

static void
get_guile_specified_property (GObject *object, guint property_id,
	      GValue *value, GParamSpec *pspec)
{
    gpointer ptr;
    SCM obj;
    SCM inst_dict;
    SCM svalue;

    /* Find the guile representation of OBJECT */
    ptr = g_object_get_qdata (object, gi_gobject_wrapper_key);
    if (!ptr) {
	wrap_object (object);
	ptr = g_object_get_qdata (object, gi_gobject_wrapper_key);
    }

    obj = ptr;

    g_assert (gi_gobject_get_obj (obj) == object);
    
    /* We're using a hash table as the property variable store for
       this object. */
    inst_dict = gi_gobject_get_inst_dict (obj);
    svalue = scm_hash_ref (inst_dict,
			   scm_from_utf8_string (g_param_spec_get_name (pspec)),
			   SCM_BOOL_F);
    gi_gvalue_from_scm (value, svalue);
}

static void
set_guile_specified_property (GObject *object, guint property_id,
	      const GValue *value, GParamSpec *pspec)
{
    gpointer ptr;
    SCM obj;
    SCM inst_dict;
    SCM svalue;

    /* Find the guile representation of OBJECT */
    ptr = g_object_get_qdata (object, gi_gobject_wrapper_key);
    if (!ptr) {
	wrap_object (object);
	ptr = g_object_get_qdata (object, gi_gobject_wrapper_key);
    }

    obj = ptr;

    g_assert (gi_gobject_get_obj (obj) == object);

    /* We're using a hash table as the property variable store for
       this object. */
    inst_dict = gi_gobject_get_inst_dict (obj);
    svalue = gi_gvalue_as_scm (value, TRUE);
    scm_hash_set_x (inst_dict,
		    scm_from_utf8_string (g_param_spec_get_name (pspec)),
		    svalue);
}

static void
dispose (GObject *object)
{
    g_critical ("dispose: implement me");
}

static void
finalize (GObject *object)
{
    g_critical ("finalize: implement me");
}


static GType
register_guile_specified_gobject_type (const char *type_name,
	       GType parent_type,
	       GPtrArray *properties,
	       GPtrArray *signals,
	       SCM disposer)
{
    GTypeInfo type_info;
    GuileSpecifiedGObjectClassInfo *class_init_info;
    GTypeQuery query;
    GType new_type;

    memset (&type_info, 0, sizeof (type_info));

    /* This data will needed when the class is dynamically instantiated. */
    class_init_info = g_new0(GuileSpecifiedGObjectClassInfo, 1);
    class_init_info->disposer = disposer;
    class_init_info->properties = properties;
    class_init_info->signals = signals;

    type_info.class_data = class_init_info;

    /* Register it. */
    g_type_query(parent_type, &query);
    type_info.class_size = query.class_size + GUILE_SPECIFIED_GOBJECT_CLASS_SIZE;
    class_init_info->parent_class_size = query.class_size;
    type_info.instance_size = query.instance_size + GUILE_SPECIFIED_GOBJECT_INSTANCE_SIZE;
    class_init_info->parent_instance_size = query.instance_size;
    type_info.class_init = (GClassInitFunc) init_guile_specified_gobject_class;
    type_info.instance_init = init_instance;
    new_type = g_type_register_static (parent_type,
				       type_name,
				       &type_info,
				       0);
    class_init_info->type = new_type;
    
    /* Mark this type as a Guile-specified type. */
    g_type_set_qdata(new_type,
		     gi_gobject_custom_key,
		     GINT_TO_POINTER(1));
    return new_type;
}

/* The procedure is the top-level entry point for defining a new
   GObject type in Guile. */
static SCM
scm_register_guile_specified_gobject_type (SCM s_type_name,
		   SCM s_parent_type,
		   SCM s_properties,
		   SCM s_signals,
		   SCM s_disposer)
{
    char *type_name;
    GType parent_type;
    GType new_type;
    size_t n_properties, n_signals;
    GPtrArray *properties;
    GPtrArray *signals;
    
    SCM_ASSERT (scm_is_string (s_type_name),
		s_type_name,
		SCM_ARG1,
		"register-type");
    scm_assert_foreign_object_type (gi_gtype_type, s_parent_type);
    // SCM_ASSERT (scm_is_true (scm_list_p (s_properties)), s_properties, SCM_ARG3, "register-type");
    // SCM_ASSERT (scm_is_true (scm_list_p (s_signals)), s_signals, SCM_ARG4, "register-type");
    // SCM_ASSERT (scm_is_true (scm_procedure_p (s_disposer)), s_disposer, SCM_ARG5, "register-type");

    type_name = scm_to_utf8_string (s_type_name);
    parent_type = gi_gtype_get_type (s_parent_type);
    properties = g_ptr_array_new ();
    signals = g_ptr_array_new_with_free_func ((GDestroyNotify) gi_free_signalspec);

    if (scm_is_true (scm_list_p (s_properties))) {
	n_properties = scm_to_size_t (scm_length (s_properties));
	for (size_t i = 0; i < n_properties; i ++) {
	    GParamSpec *pspec;
	    pspec = gi_gparamspec_from_scm (scm_list_ref (s_properties, scm_from_size_t (i)));
	    g_ptr_array_add (properties, pspec);
	}
    }

    if (scm_is_true (scm_list_p (s_signals))) {
	n_signals = scm_to_size_t (scm_length (s_signals));
	for (size_t i = 0; i < n_signals; i ++) {
	    SignalSpec *sspec;
	    sspec = gi_signalspec_from_obj (scm_list_ref (s_signals , scm_from_size_t (i)));
	    g_ptr_array_add (signals, sspec);
	}
    }

    if (scm_is_true (scm_procedure_p (s_disposer)))
	new_type = register_guile_specified_gobject_type (type_name,
				  parent_type,
				  properties,
				  signals,
				  s_disposer);
    else
	new_type = register_guile_specified_gobject_type (type_name,
				  parent_type,
				  properties,
				  signals,
				  SCM_BOOL_F);
    return gi_gtype_c2g (new_type);
}


	

static SCM
scm_make_gobject (SCM s_gtype, SCM s_prop_alist)
{
    GType type;
    GObject *obj;
    GObjectClass *class;
    guint n_prop;
    SCM sobj;
    gpointer ptr;

    scm_assert_foreign_object_type (gi_gtype_type, s_gtype);
    
    type = gi_gtype_get_type (s_gtype);
    obj = g_object_new_with_properties (type, 0, NULL, NULL);
    class = G_OBJECT_GET_CLASS(obj);
    if (scm_is_true (scm_list_p (s_prop_alist))) {
	n_prop = scm_to_int (scm_length (s_prop_alist));

	for (guint i = 0; i < n_prop; i ++) {
	    SCM entry = scm_list_ref (s_prop_alist, scm_from_uint (i));
	    if (scm_is_true (scm_pair_p (entry))
		&& scm_is_string (scm_car (entry))) {
		char *key = scm_to_utf8_string (scm_car (entry));
		GParamSpec *pspec = g_object_class_find_property (class, key);
		if (!pspec) {
		    scm_misc_error ("make-gobject",
				    "unknown object parameter ~A",
				    scm_list_1 (entry));
		}
		else {
		    GValue value = G_VALUE_INIT;
		    if (G_IS_PARAM_SPEC_CHAR (pspec)) {
			g_value_init (&value, G_TYPE_CHAR);
			g_value_set_schar (&value, scm_to_int8 (scm_cdr (entry)));
		    }
		    else if (G_IS_PARAM_SPEC_UCHAR (pspec)) {
			g_value_init (&value, G_TYPE_UCHAR);
			g_value_set_uchar (&value, scm_to_uint8 (scm_cdr (entry)));
		    }
		    else if (G_IS_PARAM_SPEC_INT (pspec)) {
			g_value_init (&value, G_TYPE_INT);
			g_value_set_int (&value, scm_to_int (scm_cdr (entry)));
		    }
		    else if (G_IS_PARAM_SPEC_UINT (pspec)) {
			g_value_init (&value, G_TYPE_UINT);
			g_value_set_uint (&value, scm_to_uint (scm_cdr (entry)));
		    }
		    else if (G_IS_PARAM_SPEC_LONG (pspec)) {
			g_value_init (&value, G_TYPE_LONG);
			g_value_set_uint (&value, scm_to_long (scm_cdr (entry)));
		    }
		    else if (G_IS_PARAM_SPEC_ULONG (pspec)) {
			g_value_init (&value, G_TYPE_ULONG);
			g_value_set_ulong (&value, scm_to_ulong (scm_cdr (entry)));
		    }
		    else if  (G_IS_PARAM_SPEC_INT64 (pspec)) {
			g_value_init (&value, G_TYPE_INT64);
			g_value_set_int64 (&value, scm_to_int64 (scm_cdr (entry)));
		    }
		    else if (G_IS_PARAM_SPEC_UINT64 (pspec)) {
			g_value_init (&value, G_TYPE_UINT64);
			g_value_set_uint64 (&value, scm_to_uint64 (scm_cdr (entry)));
			
		    }
		    else if (G_IS_PARAM_SPEC_FLOAT (pspec)) {
			g_value_init (&value, G_TYPE_FLOAT);
			
			g_value_set_float (&value, scm_to_double (scm_cdr (entry)));
		    }
		    else if (G_IS_PARAM_SPEC_DOUBLE (pspec)) {
			g_value_init (&value, G_TYPE_DOUBLE);
			
			g_value_set_double(&value, scm_to_double (scm_cdr (entry)));
		    }
		    else if (G_IS_PARAM_SPEC_ENUM (pspec)) {
			g_value_init (&value, G_TYPE_ENUM);
			g_value_set_enum (&value, scm_to_ulong (scm_cdr (entry)));
		    }
		    else if (G_IS_PARAM_SPEC_FLAGS (pspec)) {
			g_value_init (&value, G_TYPE_FLAGS);
			g_value_set_flags (&value, scm_to_ulong (scm_cdr (entry)));
		    }
		    else
			g_abort();
		    
		    g_object_set_property (obj, key, &value);
		}
		free (key);
	    }
	}
    }

    ptr = g_object_get_qdata (obj, gi_gobject_wrapper_key);

    g_assert (ptr);
    sobj = ptr;
	
    g_assert (gi_gobject_get_obj (sobj) == obj);
    return sobj;
}

static SCM
scm_gobject_is_object_p (SCM self)
{
    GObject *obj;
    gboolean ret;

    scm_assert_foreign_object_type (gi_gobject_type, self);
    obj = gi_gobject_get_obj (self);
    ret = G_IS_OBJECT (obj);
    return scm_from_bool (ret);
}

static SCM
scm_gobject_type (SCM self)
{
    GType type;
    GObject *obj;

    scm_assert_foreign_object_type (gi_gobject_type, self);
    
    obj = gi_gobject_get_obj (self);
    type = G_OBJECT_TYPE (obj);

    return gi_gtype_c2g (type);
}

static SCM
scm_gobject_type_name (SCM self)
{
    const char *name;
    GObject *obj;

    scm_assert_foreign_object_type (gi_gobject_type, self);
    
    obj = gi_gobject_get_obj (self);
    name = G_OBJECT_TYPE_NAME (obj);

    return scm_from_utf8_string (name);
}

static SCM
scm_gobject_is_floating_p (SCM self)
{
    GObject *obj;
    gboolean ret;

    scm_assert_foreign_object_type (gi_gobject_type, self);
    obj = gi_gobject_get_obj (self);
    ret = g_object_is_floating (obj);
    return scm_from_bool (ret);
}

static GIPropertyInfo *
lookup_property_from_object_info (GIObjectInfo *info, const gchar *attr_name)
{
    gssize n_infos;
    gint i;

    n_infos = g_object_info_get_n_properties (info);
    for (i = 0; i < n_infos; i++) {
        GIPropertyInfo *property_info;

        property_info = g_object_info_get_property (info, i);
        g_assert (info != NULL);

        if (strcmp (attr_name, g_base_info_get_name (property_info)) == 0) {
            return property_info;
        }

        g_base_info_unref (property_info);
    }

    return NULL;
}

static GIPropertyInfo *
lookup_property_from_interface_info (GIInterfaceInfo *info,
                                     const gchar *attr_name)
{
    gssize n_infos;
    gint i;

    n_infos = g_interface_info_get_n_properties (info);
    for (i = 0; i < n_infos; i++) {
        GIPropertyInfo *property_info;

        property_info = g_interface_info_get_property (info, i);
        g_assert (info != NULL);

        if (strcmp (attr_name, g_base_info_get_name (property_info)) == 0) {
            return property_info;
        }

        g_base_info_unref (property_info);
    }

    return NULL;
}


/* re _pygi_lookup_property_from_g_type */
static GIPropertyInfo *
gi_lookup_property_from_g_type (GType g_type, const gchar *attr_name)
{
    GIPropertyInfo *ret = NULL;
    GIRepository *repository;
    GIBaseInfo *info;

    repository = g_irepository_get_default();
    info = g_irepository_find_by_gtype (repository, g_type);
    if (info == NULL)
       return NULL;

    if (GI_IS_OBJECT_INFO (info))
        ret = lookup_property_from_object_info ((GIObjectInfo *) info,
                                                attr_name);
    else if (GI_IS_INTERFACE_INFO (info))
        ret = lookup_property_from_interface_info ((GIInterfaceInfo *) info,
                                                   attr_name);

    g_base_info_unref (info);
    return ret;
}

/* re pygi_marshal_from_py_basic_type */
static gboolean
gi_marshal_from_scm_basic_type (const char *func,
				SCM object, /* in */
                                 GIArgument *arg,      /* out */
                                 GITypeTag   type_tag,
                                 GITransfer  transfer,
                                 gpointer   *cleanup_data /* out */)
{
    switch (type_tag) {
        case GI_TYPE_TAG_VOID:
            g_warn_if_fail (transfer == GI_TRANSFER_NOTHING);
	    arg->v_pointer = scm_to_pointer (object);
	    *cleanup_data = arg->v_pointer;
	    return TRUE;

        case GI_TYPE_TAG_INT8:
	    arg->v_int8 = scm_to_int8 (object);
	    return TRUE;

        case GI_TYPE_TAG_UINT8:
	    arg->v_uint8 = scm_to_uint8 (object);
	    return TRUE;

        case GI_TYPE_TAG_INT16:
	    arg->v_int16 = scm_to_int16 (object);
	    return TRUE;

        case GI_TYPE_TAG_UINT16:
	    arg->v_uint16 = scm_to_uint16 (object);
	    return TRUE;

        case GI_TYPE_TAG_INT32:
	    arg->v_int32 = scm_to_int32 (object);
	    return TRUE;

        case GI_TYPE_TAG_UINT32:
	    arg->v_uint32 = scm_to_uint32 (object);
	    return TRUE;

        case GI_TYPE_TAG_INT64:
	    arg->v_int64 = scm_to_int64 (object);
	    return TRUE;

        case GI_TYPE_TAG_UINT64:
	    arg->v_uint64 = scm_to_uint64 (object);
	    return TRUE;

        case GI_TYPE_TAG_BOOLEAN:
	    arg->v_boolean = scm_is_true (object);
	    return TRUE;

        case GI_TYPE_TAG_FLOAT:
	    arg->v_float = scm_to_double (object);
	    return TRUE;

        case GI_TYPE_TAG_DOUBLE:
	    arg->v_double = scm_to_double (object);
	    return TRUE;

        case GI_TYPE_TAG_GTYPE:
	    arg->v_size = gi_gtype_get_type (object);
	    return TRUE;

        case GI_TYPE_TAG_UNICHAR:
	    arg->v_uint32 = SCM_CHAR (object);
	    return TRUE;

        case GI_TYPE_TAG_UTF8:
	    arg->v_string = scm_to_utf8_string (object);
	    *cleanup_data = arg->v_string;
	    return TRUE;

        case GI_TYPE_TAG_FILENAME:
	    arg->v_string = scm_to_locale_string (object);
	    *cleanup_data = arg->v_string;
	    return TRUE;

        default:
	    scm_misc_error (func, "Type tag ~a not supported", scm_list_1 (scm_from_uint (type_tag)));
	    return TRUE;
    }

    return TRUE;
}


static GIArgument
gi_argument_from_object (const char *func,
			 SCM object,
			 GITypeInfo *type_info,
			 GITransfer  transfer)
{
    GIArgument arg;
    GITypeTag type_tag;
    gpointer cleanup_data = NULL;

    memset(&arg, 0, sizeof(GIArgument));
    type_tag = g_type_info_get_tag (type_info);

    switch (type_tag) {
#if 0	
        case GI_TYPE_TAG_ARRAY:
        {
            ssize_t py_length;
            guint length, i;
            gboolean is_zero_terminated;
            GITypeInfo *item_type_info;
            gsize item_size;
            GArray *array;
            GITransfer item_transfer;

            if (object == Py_None) {
                arg.v_pointer = NULL;
                break;
            }

            /* Note, strings are sequences, but we cannot accept them here */
            if (!PySequence_Check (object) || 
#if PY_VERSION_HEX < 0x03000000
                PyString_Check (object) || 
#endif
                PyUnicode_Check (object)) {
                PyErr_SetString (PyExc_TypeError, "expected sequence");
                break;
            }

            py_length = PySequence_Length (object);
            if (py_length < 0)
                break;

            if (!pygi_guint_from_pyssize (py_length, &length))
                break;

            is_zero_terminated = g_type_info_is_zero_terminated (type_info);
            item_type_info = g_type_info_get_param_type (type_info, 0);

            /* we handle arrays that are really strings specially, see below */
            if (g_type_info_get_tag (item_type_info) == GI_TYPE_TAG_UINT8)
               item_size = 1;
            else
               item_size = sizeof (GIArgument);

            array = g_array_sized_new (is_zero_terminated, FALSE, (guint)item_size, length);
            if (array == NULL) {
                g_base_info_unref ( (GIBaseInfo *) item_type_info);
                PyErr_NoMemory();
                break;
            }

            if (g_type_info_get_tag (item_type_info) == GI_TYPE_TAG_UINT8 &&
                PYGLIB_PyBytes_Check(object)) {

                memcpy(array->data, PYGLIB_PyBytes_AsString(object), length);
                array->len = length;
                goto array_success;
            }


            item_transfer = transfer == GI_TRANSFER_CONTAINER ? GI_TRANSFER_NOTHING : transfer;

            for (i = 0; i < length; i++) {
                PyObject *py_item;
                GIArgument item;

                py_item = PySequence_GetItem (object, i);
                if (py_item == NULL) {
                    goto array_item_error;
                }

                item = _pygi_argument_from_object (py_item, item_type_info, item_transfer);

                Py_DECREF (py_item);

                if (PyErr_Occurred()) {
                    goto array_item_error;
                }

                g_array_insert_val (array, i, item);
                continue;

array_item_error:
                /* Free everything we have converted so far. */
                _pygi_argument_release ( (GIArgument *) &array, type_info,
                                         GI_TRANSFER_NOTHING, GI_DIRECTION_IN);
                array = NULL;

                _PyGI_ERROR_PREFIX ("Item %u: ", i);
                break;
            }

array_success:
            arg.v_pointer = array;

            g_base_info_unref ( (GIBaseInfo *) item_type_info);
            break;
        }
        case GI_TYPE_TAG_INTERFACE:
        {
            GIBaseInfo *info;
            GIInfoType info_type;

            info = g_type_info_get_interface (type_info);
            info_type = g_base_info_get_type (info);

            switch (info_type) {
                case GI_INFO_TYPE_CALLBACK:
                    /* This should be handled in invoke() */
                    g_assert_not_reached();
                    break;
                case GI_INFO_TYPE_BOXED:
                case GI_INFO_TYPE_STRUCT:
                case GI_INFO_TYPE_UNION:
                {
		    
                    GType g_type;
                    SCM s_type;
                    gboolean is_foreign = (info_type == GI_INFO_TYPE_STRUCT) &&
                                          (g_struct_info_is_foreign ((GIStructInfo *) info));

                    g_type = g_registered_type_info_get_g_type ( (GIRegisteredTypeInfo *) info);
                    s_type = gi_type_import_by_gi_info ( (GIBaseInfo *) info);

		    scm_make_foreign_object_0 (gi_gobject_type);
                    /* Note for G_TYPE_VALUE g_type:
                     * This will currently leak the GValue that is allocated and
                     * stashed in arg.v_pointer. Out argument marshaling for caller
                     * allocated GValues already pass in memory for the GValue.
                     * Further re-factoring is needed to fix this leak.
                     * See: https://bugzilla.gnome.org/show_bug.cgi?id=693405
                     */
                    pygi_arg_struct_from_py_marshal (object,
                                                     &arg,
                                                     NULL, /*arg_name*/
                                                     info, /*interface_info*/
                                                     g_type,
                                                     py_type,
                                                     transfer,
                                                     FALSE, /*copy_reference*/
                                                     is_foreign,
                                                     g_type_info_is_pointer (type_info));

                    Py_DECREF (py_type);
                    break;
                }
                case GI_INFO_TYPE_ENUM:
                case GI_INFO_TYPE_FLAGS:
                {
                    if (!pygi_gint_from_py (object, &arg.v_int))
                        break;

                    break;
                }
                case GI_INFO_TYPE_INTERFACE:
                case GI_INFO_TYPE_OBJECT:
                    /* An error within this call will result in a NULL arg */
                    pygi_arg_gobject_out_arg_from_py (object, &arg, transfer);
                    break;

                default:
                    g_assert_not_reached();
            }
            g_base_info_unref (info);
            break;
        }
    case GI_TYPE_TAG_GLIST:
        case GI_TYPE_TAG_GSLIST:
        {
            Py_ssize_t length;
            GITypeInfo *item_type_info;
            GSList *list = NULL;
            GITransfer item_transfer;
            Py_ssize_t i;

            if (object == Py_None) {
                arg.v_pointer = NULL;
                break;
            }

            length = PySequence_Length (object);
            if (length < 0) {
                break;
            }

            item_type_info = g_type_info_get_param_type (type_info, 0);
            g_assert (item_type_info != NULL);

            item_transfer = transfer == GI_TRANSFER_CONTAINER ? GI_TRANSFER_NOTHING : transfer;

            for (i = length - 1; i >= 0; i--) {
                PyObject *py_item;
                GIArgument item;

                py_item = PySequence_GetItem (object, i);
                if (py_item == NULL) {
                    goto list_item_error;
                }

                item = _pygi_argument_from_object (py_item, item_type_info, item_transfer);

                Py_DECREF (py_item);

                if (PyErr_Occurred()) {
                    goto list_item_error;
                }

                if (type_tag == GI_TYPE_TAG_GLIST) {
                    list = (GSList *) g_list_prepend ( (GList *) list, item.v_pointer);
                } else {
                    list = g_slist_prepend (list, item.v_pointer);
                }

                continue;

list_item_error:
                /* Free everything we have converted so far. */
                _pygi_argument_release ( (GIArgument *) &list, type_info,
                                         GI_TRANSFER_NOTHING, GI_DIRECTION_IN);
                list = NULL;

                _PyGI_ERROR_PREFIX ("Item %zd: ", i);
                break;
            }

            arg.v_pointer = list;

            g_base_info_unref ( (GIBaseInfo *) item_type_info);

            break;
        }
        case GI_TYPE_TAG_GHASH:
        {
            Py_ssize_t length;
            PyObject *keys;
            PyObject *values;
            GITypeInfo *key_type_info;
            GITypeInfo *value_type_info;
            GITypeTag key_type_tag;
            GHashFunc hash_func;
            GEqualFunc equal_func;
            GHashTable *hash_table;
            GITransfer item_transfer;
            Py_ssize_t i;


            if (object == Py_None) {
                arg.v_pointer = NULL;
                break;
            }

            length = PyMapping_Length (object);
            if (length < 0) {
                break;
            }

            keys = PyMapping_Keys (object);
            if (keys == NULL) {
                break;
            }

            values = PyMapping_Values (object);
            if (values == NULL) {
                Py_DECREF (keys);
                break;
            }

            key_type_info = g_type_info_get_param_type (type_info, 0);
            g_assert (key_type_info != NULL);

            value_type_info = g_type_info_get_param_type (type_info, 1);
            g_assert (value_type_info != NULL);

            key_type_tag = g_type_info_get_tag (key_type_info);

            switch (key_type_tag) {
                case GI_TYPE_TAG_UTF8:
                case GI_TYPE_TAG_FILENAME:
                    hash_func = g_str_hash;
                    equal_func = g_str_equal;
                    break;
                default:
                    hash_func = NULL;
                    equal_func = NULL;
            }

            hash_table = g_hash_table_new (hash_func, equal_func);
            if (hash_table == NULL) {
                PyErr_NoMemory();
                goto hash_table_release;
            }

            item_transfer = transfer == GI_TRANSFER_CONTAINER ? GI_TRANSFER_NOTHING : transfer;

            for (i = 0; i < length; i++) {
                PyObject *py_key;
                PyObject *py_value;
                GIArgument key;
                GIArgument value;

                py_key = PyList_GET_ITEM (keys, i);
                py_value = PyList_GET_ITEM (values, i);

                key = _pygi_argument_from_object (py_key, key_type_info, item_transfer);
                if (PyErr_Occurred()) {
                    goto hash_table_item_error;
                }

                value = _pygi_argument_from_object (py_value, value_type_info, item_transfer);
                if (PyErr_Occurred()) {
                    _pygi_argument_release (&key, type_info, GI_TRANSFER_NOTHING, GI_DIRECTION_IN);
                    goto hash_table_item_error;
                }

                g_hash_table_insert (hash_table, key.v_pointer,
                                     _pygi_arg_to_hash_pointer (&value, value_type_info));
                continue;

hash_table_item_error:
                /* Free everything we have converted so far. */
                _pygi_argument_release ( (GIArgument *) &hash_table, type_info,
                                         GI_TRANSFER_NOTHING, GI_DIRECTION_IN);
                hash_table = NULL;

                _PyGI_ERROR_PREFIX ("Item %zd: ", i);
                break;
            }

            arg.v_pointer = hash_table;

hash_table_release:
            g_base_info_unref ( (GIBaseInfo *) key_type_info);
            g_base_info_unref ( (GIBaseInfo *) value_type_info);
            Py_DECREF (keys);
            Py_DECREF (values);
            break;
        }
        case GI_TYPE_TAG_ERROR:
            PyErr_SetString (PyExc_NotImplementedError, "error marshalling is not supported yet");
            /* TODO */
            break;
#endif
        default:
            /* Ignores cleanup data for now. */
            gi_marshal_from_scm_basic_type (func, object, &arg, type_tag, transfer, &cleanup_data);
            break;
    }

    return arg;
}


/* re pygi_set_property_value */
static int
gi_set_property_value (const char *func,
		       SCM instance,
		       GParamSpec *pspec,
		       SCM svalue)
{
    GIPropertyInfo *property_info = NULL;
    GITypeInfo *type_info = NULL;
    GITypeTag type_tag;
    GITransfer transfer;
    GValue value = { 0, };
    GIArgument arg = { 0, };
    gint ret_value = -1;

    /* The owner_type of the pspec gives us the exact type that introduced the
     * property, even if it is a parent class of the instance in question. */
    property_info = gi_lookup_property_from_g_type (pspec->owner_type,
						    pspec->name);

    if (property_info == NULL)
        goto out;

    if (! (pspec->flags & G_PARAM_WRITABLE))
        goto out;

    type_info = g_property_info_get_type (property_info);
    transfer = g_property_info_get_ownership_transfer (property_info);
    arg = gi_argument_from_object (func, svalue, type_info, transfer);

    g_value_init (&value, G_PARAM_SPEC_VALUE_TYPE (pspec));

    /* FIXME: Lots of types still unhandled */
    type_tag = g_type_info_get_tag (type_info);
    switch (type_tag) {
        case GI_TYPE_TAG_INTERFACE:
        {
            GIBaseInfo *info;
            GIInfoType info_type;
            GType type;

            info = g_type_info_get_interface (type_info);
            type = g_registered_type_info_get_g_type (info);
            info_type = g_base_info_get_type (info);

            g_base_info_unref (info);

            switch (info_type) {
                case GI_INFO_TYPE_ENUM:
                    g_value_set_enum (&value, arg.v_int);
                    break;
                case GI_INFO_TYPE_FLAGS:
                    g_value_set_flags (&value, arg.v_uint);
                    break;
                case GI_INFO_TYPE_INTERFACE:
                case GI_INFO_TYPE_OBJECT:
                    g_value_set_object (&value, arg.v_pointer);
                    break;
                case GI_INFO_TYPE_BOXED:
                case GI_INFO_TYPE_STRUCT:
                case GI_INFO_TYPE_UNION:
                    if (g_type_is_a (type, G_TYPE_BOXED)) {
                        g_value_set_boxed (&value, arg.v_pointer);
                    } else if (g_type_is_a (type, G_TYPE_VARIANT)) {
                        g_value_set_variant (&value, arg.v_pointer);
                    } else {
			scm_misc_error (func,
					"Setting properties of type ~S is not implemented",
					scm_list_1 (scm_from_utf8_string (g_type_name (type))));
                        goto out;
                    }
                    break;
                default:
		    scm_misc_error (func,
				    "Setting properties of type ~S is not implemented",
				    scm_list_1 (scm_from_utf8_string (g_type_name (type))));
                    goto out;
            }
            break;
        }
        case GI_TYPE_TAG_BOOLEAN:
            g_value_set_boolean (&value, arg.v_boolean);
            break;
        case GI_TYPE_TAG_INT8:
            g_value_set_schar (&value, arg.v_int8);
            break;
        case GI_TYPE_TAG_INT16:
        case GI_TYPE_TAG_INT32:
            if (G_VALUE_HOLDS_LONG (&value))
                g_value_set_long (&value, arg.v_long);
            else
                g_value_set_int (&value, arg.v_int);
            break;
        case GI_TYPE_TAG_INT64:
            if (G_VALUE_HOLDS_LONG (&value))
                g_value_set_long (&value, arg.v_long);
            else
                g_value_set_int64 (&value, arg.v_int64);
            break;
        case GI_TYPE_TAG_UINT8:
            g_value_set_uchar (&value, arg.v_uint8);
            break;
        case GI_TYPE_TAG_UINT16:
        case GI_TYPE_TAG_UINT32:
            if (G_VALUE_HOLDS_ULONG (&value))
                g_value_set_ulong (&value, arg.v_ulong);
            else
                g_value_set_uint (&value, arg.v_uint);
            break;
        case GI_TYPE_TAG_UINT64:
            if (G_VALUE_HOLDS_ULONG (&value))
                g_value_set_ulong (&value, arg.v_ulong);
            else
                g_value_set_uint64 (&value, arg.v_uint64);
            break;
        case GI_TYPE_TAG_FLOAT:
            g_value_set_float (&value, arg.v_float);
            break;
        case GI_TYPE_TAG_DOUBLE:
            g_value_set_double (&value, arg.v_double);
            break;
        case GI_TYPE_TAG_GTYPE:
            g_value_set_gtype (&value, arg.v_size);
            break;
        case GI_TYPE_TAG_UTF8:
        case GI_TYPE_TAG_FILENAME:
            g_value_set_string (&value, arg.v_string);
            break;
        case GI_TYPE_TAG_GHASH:
            g_value_set_boxed (&value, arg.v_pointer);
            break;
        case GI_TYPE_TAG_GLIST:
            if (G_VALUE_HOLDS_BOXED(&value))
                g_value_set_boxed (&value, arg.v_pointer);
            else
                g_value_set_pointer (&value, arg.v_pointer);
            break;
        case GI_TYPE_TAG_ARRAY:
        {
            /* This is assumes GI_TYPE_TAG_ARRAY is always a GStrv
             * https://bugzilla.gnome.org/show_bug.cgi?id=688232
             */
            GArray *arg_items = (GArray*) arg.v_pointer;
            gchar** strings;
            guint i;

            if (arg_items == NULL)
                goto out;

            strings = g_new0 (char*, arg_items->len + 1);
            for (i = 0; i < arg_items->len; ++i) {
                strings[i] = g_array_index (arg_items, GIArgument, i).v_string;
            }
            strings[arg_items->len] = NULL;
            g_value_take_boxed (&value, strings);
            g_array_free (arg_items, TRUE);
            break;
        }
        default:
	    scm_misc_error (func,
			    "Setting properties of type %S is not implemented",
			    scm_list_1 (scm_from_utf8_string (g_type_tag_to_string (g_type_info_get_tag (type_info)))));
            goto out;
    }

    g_object_set_property (gi_gobject_get_obj (instance), pspec->name, &value);
    g_value_unset (&value);

    ret_value = 0;

out:
    if (property_info != NULL)
        g_base_info_unref (property_info);
    if (type_info != NULL)
        g_base_info_unref (type_info);

    return ret_value;
}

/* re pygobject_set_property */
static SCM
scm_gobject_set_property_x (SCM self, SCM sname, SCM sval)
{
    GObject *obj;
    char *name;
    GParamSpec *pspec;

    scm_assert_foreign_object_type (gi_gobject_type, self);
    SCM_ASSERT (scm_is_string (sname), sname, SCM_ARG2, "gobject-set-property!");

    obj = gi_gobject_get_obj (self);
    name = scm_to_utf8_string (sname);
    pspec = g_object_class_find_property (G_OBJECT_GET_CLASS (obj), name);
    free (name);
    if (!pspec)
	scm_misc_error ("gobject-set-property!",
			"object of type ~S does not have property ~S",
			scm_list_2(scm_from_utf8_string (g_type_name (G_OBJECT_TYPE (obj))),
				   sname));

    gi_set_property_value ("gobject-set-property!", self, pspec, sval);
    return SCM_UNSPECIFIED;
}

/* re pygi_get_property_value_by_name */
static SCM
gi_get_property_value_by_name (const char *func, SCM self, gchar *param_name)
{
    GParamSpec *pspec;
    GObjectClass *oclass;
    GObject *obj;

    obj = gi_gobject_get_obj (self);
    oclass = G_OBJECT_GET_CLASS (obj);

    pspec = g_object_class_find_property (oclass, param_name);
    if (!pspec) {
	scm_misc_error (func,
			"object of type ~S does not have a property ~S",
			scm_list_2 (self,
				    scm_from_utf8_string (param_name)));
    }
    return gi_get_property_value (func, self, pspec);
}

/* re pygi_get_property_value */
static SCM
gi_get_property_value (const char *func, SCM instance, GParamSpec *pspec)
{
    GValue value = { 0, };
    SCM svalue = SCM_BOOL_F;
    GType fundamental;
    gboolean handled;
    
    if (!(pspec->flags & G_PARAM_READABLE)) {
	scm_misc_error (func, "property ~S is not readable",
			scm_list_1 (scm_from_utf8_string (g_param_spec_get_name (pspec))));
    }

    g_value_init (&value, G_PARAM_SPEC_VALUE_TYPE (pspec));
    g_object_get_property (gi_gobject_get_obj(instance), pspec->name, &value);
    fundamental = G_TYPE_FUNDAMENTAL (G_VALUE_TYPE (&value));

    svalue = gi_gvalue_to_scm_basic_type (&value, fundamental, &handled);
    if (!handled)

	/* FIXME: else, attempt to marshal through GI */
	
	/* Fallback to GValue marshalling. */
	gi_param_gvalue_as_scm (&value, TRUE, pspec);

    g_value_unset (&value);
    return svalue;
}
    
static SCM
scm_gobject_get_property (SCM self, SCM sname)
{
    char *param_name;
    SCM ret;
    
    scm_assert_foreign_object_type (gi_gobject_type, self);
    SCM_ASSERT (scm_is_string (sname), sname, SCM_ARG2, "gobject-get-property");
    param_name = scm_to_utf8_string (sname);

    ret = gi_get_property_value_by_name ("gobject-get-property", self, param_name);

    free (param_name);

    return ret;
}


/* A procedure suitable as a record-type printer. */
SCM
scm_gobject_printer (SCM self, SCM port)
{
    scm_assert_foreign_object_type (gi_gobject_type, self);
    scm_simple_format (port,
		       scm_from_utf8_string("~s [~s] <~s>"),
		       scm_list_3 (scm_from_utf8_string (g_type_name (gi_gobject_get_ob_type(self))),
				   scm_from_int (gi_gobject_get_ob_type (self)),
				   scm_from_uintmax ((uintmax_t)gi_gobject_get_obj(self))));
    return SCM_UNSPECIFIED;
}

SCM
gi_arg_gobject_to_scm (GIArgument *arg, GITransfer transfer)
{
    SCM s_obj;

    if (arg->v_pointer == NULL) {
	s_obj = SCM_BOOL_F;
    } else if (G_IS_PARAM_SPEC(arg->v_pointer)) {
	// s_obj = gi_gparam_spec_new (arg->v_pointer);
	g_assert_not_reached ();
	if (transfer == GI_TRANSFER_EVERYTHING)
	    //g_param_spec_unref (arg->v_pointer);
	    g_assert_not_reached ();
    } else {
	s_obj = gi_gobject_new_full (arg->v_pointer,
				     transfer == GI_TRANSFER_EVERYTHING, /* steal */
				     NULL); /* type */
    }
    return s_obj;
}

SCM
gi_arg_gobject_to_scm_called_from_c (GIArgument *arg, GITransfer transfer)
{
    SCM object;
    /* IN pygobject, they say this is a hack to work around GTK+
       sending signals with floating widgets in them.  Not sure if
       that applies to us. */
    if (arg->v_pointer != NULL
	&& transfer == GI_TRANSFER_NOTHING
	&& !G_IS_PARAM_SPEC (arg->v_pointer)) {
	g_object_ref (arg->v_pointer);
	object = gi_arg_gobject_to_scm (arg, GI_TRANSFER_EVERYTHING);
	g_object_force_floating (arg->v_pointer);
    } else {
	object = gi_arg_gobject_to_scm (arg, transfer);
    }
    return object;
}

void
gi_init_gobject (void)
{
    gi_init_gtype ();
    gi_init_gvalue ();
    gi_init_gsignal ();
    gi_init_gparamspec ();

    gi_init_gobject_type ();
    gir_init_funcs();
    gir_init_func2();
    gir_init_types();
    
    gi_gobject_wrapper_key = g_quark_from_static_string ("GuGObject::wrapper");
    gi_gobject_custom_key = g_quark_from_static_string ("GuGObject::custom");
    gi_gobject_instance_data_key = g_quark_from_static_string("GuGObject::instance-data");
    
    scm_c_define_gsubr ("register-type", 5, 0, 0, scm_register_guile_specified_gobject_type);
    scm_c_define_gsubr ("make-gobject", 1, 1, 0, scm_make_gobject);
    scm_c_define_gsubr ("gobject-is-object?", 1, 0, 0, scm_gobject_is_object_p);
    scm_c_define_gsubr ("gobject-type", 1, 0, 0, scm_gobject_type);
    scm_c_define_gsubr ("gobject-type-name", 1, 0, 0, scm_gobject_type_name);
    scm_c_define_gsubr ("gobject-is-floating?", 1, 0, 0, scm_gobject_is_floating_p);
    scm_c_define_gsubr ("gobject-set-property!", 3, 0, 0, scm_gobject_set_property_x);
    scm_c_define_gsubr ("gobject-get-property", 2, 0, 0, scm_gobject_get_property);
    scm_c_define_gsubr ("gobject-disconnect-by-func", 2, 0, 0, scm_gobject_disconnect_by_func);
    scm_c_define_gsubr ("gobject-handler-block-by-func", 2, 0, 0, scm_gobject_handler_block_by_func);
    scm_c_define_gsubr ("gobject-handler-unblock-by-func", 2, 0, 0, scm_gobject_handler_unblock_by_func);
    scm_c_define_gsubr ("gobject-printer", 2, 0, 0, scm_gobject_printer);
    scm_c_define_gsubr ("signal-connect", 3, 0, 1, scm_signal_connect);
    scm_c_export ("register-type",
		  "make-gobject",
		  "gobject-is-object?",
		  "gobject-type",
		  "gobject-type-name",
		  "gobject-is-floating?",
		  "gobject-set-property!",
		  "gobject-get-property",
		  "gobject-disconnect-by-func",
		  "gobject-handler-block-by-func",
		  "gobject-handler-unblock-by-func",
		  "gobject-printer",
		  "signal-connect",
		  NULL);
}

#ifdef STANDALONE
int main(int argc, char **argv)
{
    scm_init_guile();
    
    gi_init_gobject ();
    scm_shell(argc, argv);
    return 0;
}

#endif
