/* -*- Mode: C; c-basic-offset: 4 -*- */
#ifndef _GUGOBJECT_H_
#define _GUGOBJECT_H_

#include <libguile.h>

#include <glib.h>
#include <glib-object.h>
#include "pycompat.h"

G_BEGIN_DECLS

typedef void (* GuClosureExceptionHandler) (GValue *ret, guint n_param_values, const GValue *params);

typedef struct _GuGClosure {
    GClosure closure;
    SCM callback;
    SCM extra_args; /* list of extra args to pass to callback */
    SCM swap_data; /* other object for gtk_signal_connect__object */
    GuClosureExceptionHandler exception_handler;
} GuGClosure;

/* Data that belongs to the GObject instance, not the Guile wrapper */
typedef struct _GuGObjectData {
    SCM type; /* wrapper type for this instance */
    GSList *closures;   /* A list of GuGClosures */
} GuGObjectData;

////////////////////////////////////////////////////////////////
/* GuType Type: A foreign object type that is an envelope for a
    a grip of type-related things*/
extern SCM GuType_Type;
typedef SCM SCM_TYPE;

/* GuObject Instance: A foreign object with the following slots
   - slot 0: 'ob_type', the SCM foreign-object type used to create this instance
   - slot 1: 'ob_refcnt', an SCM exact integer reference count
   - slot 2: 'tp_name', an SCM string of the type name
   - slot 2: 'tp_type', an SCM foreign object type
   - slot 3: 'tp_dict', a hash table 
   - FIXME
*/
#define MAKE_GUTYPE_TYPE						\
    do {								\
	GuType_Type =							\
	    scm_make_foreign_object_type(scm_from_latin1_symbol("<Type>"), \
					 scm_list_n (scm_from_latin1_symbol("ob_type"), \
						     scm_from_latin1_symbol("ob_refcnt"), \
						     scm_from_latin1_symbol("tp_name"), \
						     scm_from_latin1_symbol("tp_type"), \
						     scm_from_latin1_symbol("tp_dict"), \
						     SCM_UNDEFINED),	\
					 NULL);				\
    } while(FALSE)

#define GUTYPE_OB_TYPE_SLOT 0
#define GUTYPE_OB_REFCNT_SLOT 1
#define GUTYPE_TP_NAME_SLOT 2
#define GUTYPE_TP_TYPE_SLOT 3
#define GUTYPE_TP_DICT_SLOT 4

#define gutype_get_tp_name(v) (scm_to_utf8_string(scm_foreign_object_ref((v), GUTYPE_TP_NAME_SLOT)))
#define gutype_get_tp_dict(v) (scm_foreign_object_ref((v), GUTYPE_TP_DICT_SLOT))

////////////////////////////////////////////////////////////////
/* GuGObject Type: A foreign object type that is an envelope for
   GObject* types */
extern SCM GuGObject_Type;
typedef SCM SCM_GOBJECT;
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

#define gugobject_get(v) ((GObject *)scm_foreign_object_ref((v), GUGOBJECT_OBJ_SLOT))
#define gugobject_set(x,v) (scm_foreign_object_set_x((x),GUGOBJECT_OBJ_SLOT,(v)))
#define gugobject_get_ob_type(v) (SCM_PACK_POINTER(scm_foreign_object_ref((v), GUGOBJECT_OB_TYPE_SLOT)))
#define gugobject_check(v,base) (scm_is_eq(scm_foreign_object_ref((v), GUGOBJECT_OB_TYPE_SLOT), base))
#define gugobject_get_weakreflist(v) (scm_foreign_object_ref((v), GUGOBJECT_WEAKREFLIST_SLOT))
#define gugobject_set_weakreflist(v,x) (scm_foreign_object_set_x((v),GUGOBJECT_WEAKREFLIST_SLOT,(x)))
#define gugobject_get_flags(x) (scm_to_long(scm_foreign_object_ref((x), GUGOBJECT_FLAGS_SLOT)))
#define gugobject_set_flags(x,v) (scm_foreign_object_set_x((x),GUGOBJECT_FLAGS_SLOT,scm_from_long(v)))
#define gugobject_get_inst_dict(v) (scm_foreign_object_ref((v), GUGOBJECT_INST_DICT_SLOT))
#define gugobject_set_inst_dict(v,x) (scm_foreign_object_set_x((v),GUGOBJECT_INST_DICT_SLOT,(x)))

////////////////////////////////////////////////////////////////
/* GuGBoxed Type: A foreign object type that is an envelope for
   generic pointers tagged with a GType */
extern SCM GuGBoxed_Type;
typedef SCM SCM_GBOXED;

/* GuGBoxed Instance: A foreign object with the following slots
   - slot 0: 'ob_type', the SCM foreign-object type used to create this instance
   - slot 1: 'ob_refcnt', an SCM exact integer reference count
   - slot 2: 'boxed', a C void* 
   - slot 3: 'gtype', an SCM exact-integer holding a GType
   - slot 4: 'free_on_dealloc', an SCM boolean
*/
#define MAKE_GUGBOXED_TYPE						\
    do {								\
	GuGBoxed_Type =						\
	    scm_make_foreign_object(scm_from_latin1_symbol("<GBoxed>"), \
				    scm_list_n (scm_from_latin1_symbol ("ob_type"), \
						scm_from_latin1_symbol ("ob_refcnt"), \
						scm_from_latin1_symbol ("boxed"), \
						scm_from_latin1_symbol ("gtype"), \
						scm_from_latin1_symbol ("free_on_dealloc"), \
						SCM_UNDEFINED),			\
				    NULL);				\
    } while(FALSE)

#define GUGBOXED_OB_TYPE_SLOT 0
#define GUGBOXED_OB_REFCNT_SLOT 1
#define GUGBOXED_BOXED_SLOT 2
#define GUGBOXED_GTYPE_SLOT 3
#define GUGBOXED_FREE_ON_DEALLOC_SLOT 4

#define gug_boxed_get_ptr(v)  ((gpointer)scm_foreign_object_ref((v), GUGBOXED_BOXED_SLOT))
#define gug_boxed_get(v,t)  ((t *)gug_boxed_get_ptr(v))
#define gug_boxed_set_ptr(v,p)  (scm_foreign_object_set_x((v), GUGBOXED_BOXED_SLOT, (gpointer)p))
#define gug_boxed_get_ob_type(v) (scm_foreign_object_ref((v), GUGBOXED_OB_TYPE_SLOT))
#define gug_boxed_get_gtype(v)  (scm_foreign_object_ref((v), GUGBOXED_GTYPE_SLOT))
#define gug_boxed_check(v,typecode) (scm_is_eq(gug_boxed_get_ob_type(v), GuGBoxed_Type) \
				     && (gug_boxed_get_gtype(v) == typecode))

////////////////////////////////////////////////////////////////
/* GuGPointer Type: A foreign object type that is an envelope for
   generic pointers tagged with a GType */
extern SCM GuGPointer_Type;
typedef SCM SCM_GPOINTER;

/* GuGPointer Instance: A foreign object with the following slots
   - slot 0: 'ob_type', the SCM foreign-object type used to create this instance
   - slot 1: 'ob_refcnt', an SCM exact integer reference count
   - slot 2: 'pointer', a C void* 
   - slot 3: 'gtype', an SCM exact-integer holding a GType
*/
#define MAKE_GUGPOINTER_TYPE						\
    do {								\
	GuGPointer_Type =						\
	    scm_make_foreign_object(scm_from_latin1_symbol("<GPointer>"), \
				    scm_list_n (scm_from_latin1_symbol ("ob_type"), \
						scm_from_latin1_symbol ("ob_refcnt"), \
						scm_from_latin1_symbol ("pointer"), \
						scm_from_latin1_symbol ("gtype"), \
						SCM_UNDEFINED),			\
				    NULL);				\
    } while(FALSE)

#define GUGPOINTER_OB_TYPE_SLOT 0
#define GUGPOINTER_OB_REFCNT_SLOT 1
#define GUGPOINTER_POINTER_SLOT 2
#define GUGPOINTER_GTYPE_SLOT 3

#define gug_pointer_get_ptr(v)  ((gpointer)scm_foreign_object_ref((v), GUGPOINTER_POINTER_SLOT))
#define gug_pointer_get(v,t)  ((t *)gug_pointer_get_ptr(v))
#define gug_pointer_set_ptr(v,p)  (scm_foreign_object_set_x((v), GUGPOINTER_POINTER_SLOT, (gpointer)p))
#define gug_pointer_get_ob_type(v) (scm_foreign_object_ref((v), GUGPOINTER_OB_TYPE_SLOT))
#define gug_pointer_get_gtype(v)  (scm_foreign_object_ref((v), GUGPOINTER_GTYPE_SLOT))
#define gug_pointer_check(v,typecode) (scm_is_eq(gug_pointer_get_ob_type(v), GuGPointer_Type) \
				     && (gug_pointer_get_gtype(v) == typecode))


////////////////////////////////////////////////////////////////
/* GuGParamSpec Type: A foreign object type that is an envelope for a
   GParamSpec* */
extern SCM GuGParamSpec_Type;
typedef SCM SCM_GPARAMSPEC;

typedef void (*GuGFatalExceptionFunc) (void);
typedef void (*GuGThreadBlockFunc) (void);

/* GuGParamSpec Instance: A foreign object with the following slots
   - slot 0: 'ob_type', the SCM foreign-object type used to create this instance
   - slot 1: 'ob_refcnt', an SCM exact integer reference count
   - slot 2: 'pspec', a C GParamSpec*
*/
#define MAKE_GUGPARAMSPEC_TYPE						\
    do {								\
	GuGParamSpec_Type = \
	    scm_make_foreign_object(scm_from_latin1_symbol("<GParamSpec>"), \
				    scm_list_n (scm_from_latin1_symbol ("ob_type"), \
						scm_from_latin1_symbol ("ob_refcnt"), \
						scm_from_latin1_symbol ("pspec"), \
						SCM_UNDEFINED),			\
				    NULL);				\
    } while(FALSE)

#define GUGPARAMSPEC_OB_TYPE_SLOT 0
#define GUGPARAMSPEC_OB_REFCNT_SLOT 1
#define GUGPARAMSPEC_PSPEC_SLOT 2

#define gug_param_spec_get(v)  ((GParamSpec *)scm_foreign_object_ref((v), GUGPARAMSPEC_PSPEC_SLOT))
#define gug_param_spec_set(v,p)  (scm_foreign_object_set_x((v), GUGPARAMSPEC_PSPEC_SLOT, (gpointer)p))
#define gug_param_spec_get_ob_type(v) (scm_foreign_object_ref((v), GUGPARAMSPEC_OB_TYPE_SLOT))
#define gug_param_spec_check(v) (scm_is_eq(gug_param_spec_get_ob_type(v), GuGParamSpec_Type))

////////////////////////////////////////////////////////////////
/* GuGObjectWeakRef_Type */
extern SCM_TYPE GuGObjectWeakRef_Type;
typedef SCM SCM_GOBJECTWEAKREF;

/* GuGObjectWeakRef:
   - slot 0: 'ob_type', the SCM foreign-object type used to create this instance
   - slot 1: 'ob_refcnt', an SCM exact integer reference count
   - slot 2: 'obj', a C GObject*
   - slot 3: 'callback', an SCM (maybe gets pased to a visitproc?)
   - slot 4: 'user_data', an SCM (maybe gets pased to a visitproc?)
   - slot 5: 'have_floating_ref', an SCM boolean
*/
#define MAKE_GUGOBJECTWEAKREF_TYPE						\
    do {								\
	GuGObjectWeakRef_Type = 						\
	    scm_make_foreign_object(scm_from_latin1_symbol("<GObjectWeakRef>"), \
				    scm_list_n (scm_from_latin1_symbol ("ob_type"), \
						scm_from_latin1_symbol ("ob_refcnt"), \
						scm_from_latin1_symbol ("obj"), \
						scm_from_latin1_symbol ("callback"), \
						scm_from_latin1_symbol ("user_data"), \
						scm_from_latin1_symbol ("have_floating_ref"), \
						NULL),			\
				    NULL);				\
    } while(FALSE)

#define GUGOBJECTWEAKREF_OB_TYPE_SLOT 0
#define GUGOBJECTWEAKREF_OB_REFCNT_SLOT 1
#define GUGOBJECTWEAKREF_OBJ_SLOT 2
#define GUGOBJECTWEAKREF_CALLBACK_SLOT 3
#define GUGOBJECTWEAKREF_USER_DATA_SLOT 4
#define GUGOBJECTWEAKREF_HAVE_FLOATING_REF_SLOT 5

////////////////////////////////////////////////////////////////
/* GuGPropsIter_Type */
extern SCM_TYPE GuGPropsIter_Type;
typedef SCM SCM_GPROPSITER;
void gug_props_iter_finalize(SCM_GPROPSITER self);

/* GuGPropsIter:
   - slot 0: 'ob_type', the SCM foreign-object type used to create this instance
   - slot 1: 'ob_refcnt', an SCM exact integer reference count
   - slot 2: 'props' C GParamSpec**
   - slot 3: 'n_props', an SCM exact integer
   - slot 4: 'index', an SCM exact integer
*/
#define MAKE_GUGPROPSITER_TYPE \
    do {								\
	GuGPropsIter_Type = 						\
	    scm_make_foreign_object(scm_from_latin1_symbol("<GPropsIter>"), \
				    scm_list_n (scm_from_latin1_symbol ("ob_type"), \
						scm_from_latin1_symbol ("ob_refcnt"), \
						scm_from_latin1_symbol ("props"), \
						scm_from_latin1_symbol ("n_props"), \
						scm_from_latin1_symbol ("index"), \
						SCM_UNDEFINED),			\
				    gug_props_iter_finalize);				\
    } while(FALSE)

#define GUGPROPSITER_OB_TYPE 0
#define GUGPROPSITER_OB_REFCNT_SLOT 1
#define GUGPROPSITER_PROPS_SLOT 2
#define GUGPROPSITER_N_PROPS_SLOT 3
#define GUGPROPSITER_INDEX_SLOT 4

#define gugpropsiter_get_props(v) ((GParamSpec **)scm_foreign_object_ref((v), GUGPROPSITER_PROPS_SLOT))
#define gugpropsiter_set_props(v,x) (scm_foreign_object_set_x((v), GUGPROPSITER_PROPS_SLOT, (x)))
#define gugpropsiter_get_index(v) (scm_to_int(scm_foreign_object_ref((v), GUGPROPSITER_INDEX_SLOT)))
#define gugpropsiter_set_index(v,x) (scm_foreign_object_set_x((v), GUGPROPSITER_INDEX_SLOT, scm_from_int (x)))
#define gugpropsiter_get_n_props(v) (scm_to_int(scm_foreign_object_ref((v), GUGPROPSITER_N_PROPS_SLOT)))

////////////////////////////////////////////////////////////////
/* GuGPropsIter_Type */
extern SCM_TYPE GuGPropsDescr_Type;
typedef SCM SCM_GPROPSDESCR;

/* GuGPropsDescr:
   - slot 0: 'ob_type', the SCM foreign-object type used to create this instance
   - slot 1: 'ob_refcnt', an SCM exact integer reference count
   - slot 2: 'descr', an SCM
*/
#define MAKE_GUGPROPSDESCR_TYPE \
    do {								\
	GuGPropsDescr_Type = 						\
	    scm_make_foreign_object(scm_from_latin1_symbol("<GPropsDescr>"), \
				    scm_list_n (scm_from_latin1_symbol ("ob_type"), \
						scm_from_latin1_symbol ("ob_refcnt"), \
						scm_from_latin1_symbol ("descr"), \
						SCM_UNDEFINED),			\
				    NULL);				\
    } while(FALSE)

#define GUGPROPSDESCR_OB_TYPE 0
#define GUGPROPSDESCR_OB_REFCNT_SLOT 1
#define GUGPROPSDESCR_DESCR_SLOT 2

////////////////////////////////////////////////////////////////
/* GuGProps_Type */
extern SCM_TYPE GuGProps_Type;
typedef SCM SCM_GPROPS;
void gugobject_gprops_finalize(SCM_GPROPS self);

/* GuGPropsDescr:
   - slot 0: 'ob_type', the SCM foreign-object type used to create this instance
   - slot 1: 'ob_refcnt', an SCM exact integer reference count
   - slot 2: 'obj', an SCM
   - slot 3: 'gtype', an SCM exact integer
*/
#define MAKE_GUGPROPS_TYPE \
    do {								\
	GuGProps_Type = 						\
	    scm_make_foreign_object(scm_from_latin1_symbol("<GProps>"), \
				    scm_list_n (scm_from_latin1_symbol ("ob_type"), \
						scm_from_latin1_symbol ("ob_refcnt"), \
						scm_from_latin1_symbol ("obj"), \
						scm_from_latin1_symbol ("gtype"), \
						SCM_UNDEFINED),			\
				    gugobject_gprops_finalize);				\
    } while(FALSE)

#define GUGPROPS_OB_TYPE 0
#define GUGPROPS_OB_REFCNT_SLOT 1
#define GUGPROPS_OBJ_SLOT 2
#define GUGPROPS_GTYPE_SLOT 3

#define gugprops_get_obj(v) ((SCM)scm_foreign_object_ref((v), GUGPROPS_OBJ_SLOT))
#define gugprops_set_obj(v,x) (scm_foreign_object_set_x((v), GUGPROPS_OBJ_SLOT, (x)))
#define gugprops_get_gtype(v) ((GType)scm_to_int(scm_foreign_object_ref((v), GUGPROPS_GTYPE_SLOT)))

////////////////////////////////////////////////////////////////
typedef int (*GuGClassInitFunc) (gpointer gclass,
				 SCM_TYPE guclass);

/* Returns an SCM containing a foreign-object type */
typedef SCM_TYPE (*GuGTypeRegistrationFunction) (const gchar *name,
						 gpointer data);

////////////////////////////////////////////////////////////////

extern GType Gu_TYPE_OBJECT;
extern GQuark gugobject_instance_data_key;
extern GQuark gugobject_custom_key;
extern GQuark gugobject_wrapper_key;
extern GQuark gugobject_class_key;
extern GQuark gugobject_class_init_key;

extern SCM_TYPE GuGObjectWeakRef_Type;
extern SCM_TYPE GuGPropsIter_Type;
extern SCM_TYPE GuGPropsDescr_Type;
extern SCM_TYPE GuGProps_Type;
extern SCM_TYPE GuGObject_Type;
extern SCM_TYPE *GuGObject_MetaType;

////////////////////////////////////////////////////////////////
GuGObjectData *gug_object_peek_inst_data(GObject *obj);
gboolean      gugobject_prepare_construct_properties  (GObjectClass *class,
                                                       SCM kwargs,
                                                       guint *n_params,
                                                       GParameter **params);
void          gugobject_register_class   (SCM dict,
                                          const gchar *type_name,
                                          GType gtype, SCM_TYPE type,
                                          SCM bases);
void          gugobject_register_wrapper (SCM self);
SCM           gugobject_new              (GObject *obj);
SCM           gugobject_new_full         (GObject *obj, gboolean steal, gpointer g_class);
void          gugobject_sink             (GObject *obj);
SCM_TYPE      gugobject_lookup_class     (GType gtype);
void          gugobject_watch_closure    (SCM self, GClosure *closure);
int           gui_object_register_types  (SCM d);
void          gugobject_ref_float        (SCM_GOBJECT self);
void          gugobject_ref_sink         (SCM_GOBJECT self);
SCM           gug_object_new             (SCM self, SCM args, SCM kwargs);

GClosure *    gclosure_from_gufunc(SCM_GOBJECT object, SCM func);
void          gugobject_register_class(SCM dict, const gchar *type_name,
				       GType gtype, SCM_TYPE type,
				       SCM static_bases);
void          gugobject_register_wrapper(SCM self);
SCM_TYPE      gugobject_lookup_class(GType gtype);
SCM           gugobject_new(GObject *obj);
void          gugobject_watch_closure(SCM self, GClosure *closure);
SCM           gugobject_new_full(GObject *obj, gboolean steal, gpointer g_class);

SCM           gugobject_init(int req_major, int req_minor, int req_micro);


/**
 * PYLIST_FROMGLIBLIST:
 * @type: the type of the GLib list e.g. #GList or #GSList
 * @prefix: the prefix of functions that manipulate a list of the type
 * given by type.
 *
 * A macro that creates a type specific code block which converts a GLib
 * list (#GSList or #GList) to a Python list. The first two args of the macro
 * are used to specify the type and list function prefix so that the type
 * specific macros can be generated.
 *
 * The rest of the args are for the standard args for the type specific
 * macro(s) created from this macro.
 */
 #define PYLIST_FROMGLIBLIST(type,prefix,py_list,list,item_convert_func,\
                            list_free,list_item_free)  \
G_STMT_START \
{ \
    gint i, len; \
    SCM item; \
    void (*glib_list_free)(type*) = list_free; \
    GFunc glib_list_item_free = (GFunc)list_item_free;  \
 \
    len = prefix##_length(list); \
    py_list = PyList_New(len); \
    for (i = 0; i < len; i++) { \
        gpointer list_item = prefix##_nth_data(list, i); \
 \
        item = item_convert_func; \
        PyList_SetItem(py_list, i, item); \
    } \
    if (glib_list_item_free != NULL) \
        prefix##_foreach(list, glib_list_item_free, NULL); \
    if (glib_list_free != NULL) \
        glib_list_free(list); \
} G_STMT_END

/**
 * PYLIST_FROMGLIST:
 * @py_list: the name of the Python list
 *
 * @list: the #GList to be converted to a Python list
 *
 * @item_convert_func: the function that converts a list item to a Python
 * object. The function must refer to the list item using "@list_item" and
 * must return a #PyObject* object. An example conversion function is:
 * [[
 * PyString_FromString(list_item)
 * ]]
 * A more elaborate function is:
 * [[
 * gug_boxed_new(GTK_TYPE_RECENT_INFO, list_item, TRUE, TRUE)
 * ]]
 * @list_free: the name of a function that takes a single arg (the list) and
 * frees its memory. Can be NULL if the list should not be freed. An example
 * is:
 * [[
 * g_list_free
 * ]]
 * @list_item_free: the name of a #GFunc function that frees the memory used
 * by the items in the list or %NULL if the list items do not have to be
 * freed. A simple example is:
 * [[
 * g_free
 * ]]
 *
 * A macro that adds code that converts a #GList to a Python list.
 *
 */
#define PYLIST_FROMGLIST(py_list,list,item_convert_func,list_free,\
                         list_item_free) \
        PYLIST_FROMGLIBLIST(GList,g_list,py_list,list,item_convert_func,\
                            list_free,list_item_free)

/**
 * PYLIST_FROMGSLIST:
 * @py_list: the name of the Python list
 *
 * @list: the #GSList to be converted to a Python list
 *
 * @item_convert_func: the function that converts a list item to a Python
 * object. The function must refer to the list item using "@list_item" and
 * must return a #PyObject* object. An example conversion function is:
 * [[
 * PyString_FromString(list_item)
 * ]]
 * A more elaborate function is:
 * [[
 * gug_boxed_new(GTK_TYPE_RECENT_INFO, list_item, TRUE, TRUE)
 * ]]
 * @list_free: the name of a function that takes a single arg (the list) and
 * frees its memory. Can be %NULL if the list should not be freed. An example
 * is:
 * [[
 * g_list_free
 * ]]
 * @list_item_free: the name of a #GFunc function that frees the memory used
 * by the items in the list or %NULL if the list items do not have to be
 * freed. A simple example is:
 * [[
 * g_free
 * ]]
 *
 * A macro that adds code that converts a #GSList to a Python list.
 *
 */
#define PYLIST_FROMGSLIST(py_list,list,item_convert_func,list_free,\
                          list_item_free) \
        PYLIST_FROMGLIBLIST(GSList,g_slist,py_list,list,item_convert_func,\
                            list_free,list_item_free)

/**
 * PYLIST_ASGLIBLIST
 * @type: the type of the GLib list e.g. GList or GSList
 * @prefix: the prefix of functions that manipulate a list of the type
 * given by type e.g. g_list or g_slist
 *
 * A macro that creates a type specific code block to be used to convert a
 * Python list to a GLib list (GList or GSList). The first two args of the
 * macro are used to specify the type and list function prefix so that the
 * type specific macros can be generated.
 *
 * The rest of the args are for the standard args for the type specific
 * macro(s) created from this macro.
 */
#define PYLIST_ASGLIBLIST(type,prefix,py_list,list,check_func,\
                           convert_func,child_free_func,errormsg,errorreturn) \
G_STMT_START \
{ \
    Py_ssize_t i, n_list; \
    GFunc glib_child_free_func = (GFunc)child_free_func;        \
 \
    if (!(py_list = PySequence_Fast(py_list, ""))) { \
        errormsg; \
        return errorreturn; \
    } \
    n_list = PySequence_Fast_GET_SIZE(py_list); \
    for (i = 0; i < n_list; i++) { \
        SCM py_item = PySequence_Fast_GET_ITEM(py_list, i); \
 \
        if (!check_func) { \
            if (glib_child_free_func) \
                    prefix##_foreach(list, glib_child_free_func, NULL); \
            prefix##_free(list); \
            Gu_DECREF(py_list); \
            errormsg; \
            return errorreturn; \
        } \
        list = prefix##_prepend(list, convert_func); \
    };			    \
        Gu_DECREF(py_list); \
        list =  prefix##_reverse(list); \
} \
G_STMT_END
/**
 * PYLIST_ASGLIST
 * @py_list: the Python list to be converted
 * @list: the #GList list to be converted
 * @check_func: the expression that takes a #PyObject* arg (must be named
 * @py_item) and returns an int value indicating if the Python object matches
 * the required list item type (0 - %False or 1 - %True). An example is:
 * [[
 * (PyString_Check(py_item)||PyUnicode_Check(py_item))
 * ]]
 * @convert_func: the function that takes a #PyObject* arg (must be named
 * py_item) and returns a pointer to the converted list object. An example
 * is:
 * [[
 * gugobject_get(py_item)
 * ]]
 * @child_free_func: the name of a #GFunc function that frees a GLib list
 * item or %NULL if the list item does not have to be freed. This function is
 * used to help free the items in a partially created list if there is an
 * error. An example is:
 * [[
 * g_free
 * ]]
 * @errormsg: a function that sets up a Python error message. An example is:
 * [[
 * GuErr_SetString(GuExc_TypeError, "strings must be a sequence of" "strings
 * or unicode objects")
 * ]]
 * @errorreturn: the value to return if an error occurs, e.g.:
 * [[
 * %NULL
 * ]]
 *
 * A macro that creates code that converts a Python list to a #GList. The
 * returned list must be freed using the appropriate list free function when
 * it's no longer needed. If an error occurs the child_free_func is used to
 * release the memory used by the list items and then the list memory is
 * freed.
 */
#define PYLIST_ASGLIST(py_list,list,check_func,convert_func,child_free_func,\
                       errormsg,errorreturn) \
        PYLIST_ASGLIBLIST(GList,g_list,py_list,list,check_func,convert_func,\
                          child_free_func,errormsg,errorreturn)

/**
 * PYLIST_ASGSLIST
 * @py_list: the Python list to be converted
 * @list: the #GSList list to be converted
 * @check_func: the expression that takes a #PyObject* arg (must be named
 * @py_item) and returns an int value indicating if the Python object matches
 * the required list item type (0 - %False or 1 - %True). An example is:
 * [[
 * (PyString_Check(py_item)||PyUnicode_Check(py_item))
 * ]]
 * @convert_func: the function that takes a #PyObject* arg (must be named
 * py_item) and returns a pointer to the converted list object. An example
 * is:
 * [[
 * gugobject_get(py_item)
 * ]]
 * @child_free_func: the name of a #GFunc function that frees a GLib list
 * item or %NULL if the list item does not have to be freed. This function is
 * used to help free the items in a partially created list if there is an
 * error. An example is:
 * [[
 * g_free
 * ]]
 * @errormsg: a function that sets up a Python error message. An example is:
 * [[
 * GuErr_SetString(GuExc_TypeError, "strings must be a sequence of" "strings
 * or unicode objects")
 * ]]
 * @errorreturn: the value to return if an error occurs, e.g.:
 * [[
 * %NULL
 * ]]
 *
 * A macro that creates code that converts a Python list to a #GSList. The
 * returned list must be freed using the appropriate list free function when
 * it's no longer needed. If an error occurs the child_free_func is used to
 * release the memory used by the list items and then the list memory is
 * freed.
 */
#define PYLIST_ASGSLIST(py_list,list,check_func,convert_func,child_free_func,\
                        errormsg,errorreturn) \
        PYLIST_ASGLIBLIST(GSList,g_slist,py_list,list,check_func,convert_func,\
                          child_free_func,errormsg,errorreturn)

G_END_DECLS

#endif /* !_GUGOBJECT_H_ */
