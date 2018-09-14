/* -*- Mode: C; c-basic-offset: 4 -*- */
#ifndef _GUGOBJECT_H_
#define _GUGOBJECT_H_

#include <libguile.h>

#include <glib.h>
#include <glib-object.h>
#include "pycompat.h"

G_BEGIN_DECLS

#define PyObject_HEAD SCM __fixme;
struct _GuBadType {
  void *x;
};
typedef struct _GuBadType PyTypeObject;
typedef struct _GuBadType PyObject;

/* PyGClosure is a _private_ structure */
typedef void (* GuClosureExceptionHandler) (GValue *ret, guint n_param_values, const GValue *params);
typedef struct _PyGClosure PyGClosure;
typedef struct _PyGObjectData PyGObjectData;

struct _GuGClosure {
    GClosure closure;
    SCM callback;
    SCM extra_args; /* list of extra args to pass to callback */
    SCM swap_data; /* other object for gtk_signal_connect__object */
    GuClosureExceptionHandler exception_handler;
};

typedef enum {
    GUGOBJECT_USING_TOGGLE_REF = 1 << 0,
    GUGOBJECT_IS_FLOATING_REF = 1 << 1,
    GUGOBJECT_GOBJECT_WAS_FLOATING = 1 << 2
} GuGObjectFlags;

typedef struct {
    ssize_t ob_refcnt;
    SCM ob_type;
    GObject *obj;
    SCM inst_dict; /* the instance dictionary */
    SCM weakreflist; /* list of weak references */
    GuGObjectFlags flags;
} GuGObject;

#define gugobject_get(v) (((GuGObject *)(v))->obj)
//#define pygobject_check(v,base) (scm_is_true (scm_eq_p (((GuGObject *)(v))->ob_ty

typedef struct {
    ssize_t ob_refcnt;
    SCM ob_type;
    gpointer boxed;
    GType gtype;
    gboolean free_on_dealloc;
} GuGBoxed;

#define gug_boxed_get(v,t)      ((t *)((GuGBoxed *)(v))->boxed)
#define gug_boxed_get_ptr(v)    (((GuGBoxed *)(v))->boxed)
#define gug_boxed_set_ptr(v,p)  (((GuGBoxed *)(v))->boxed = (gpointer)p)
#define gug_boxed_check(v,typecode) (GuObject_TypeCheck(v, GuGBoxed_Type) && gug_foreign_object_type_get_gtype(v) == typecode)

typedef struct {
    ssize_t ob_refcnt;
    SCM ob_type;
    gpointer pointer;
    GType gtype;
} GuGPointer;

#define gug_pointer_get(v,t)      ((t *)((GuGPointer *)(v))->pointer)
#define gug_pointer_get_ptr(v)    (((GuGPointer *)(v))->pointer)
#define gug_pointer_set_ptr(v,p)  (((GuGPointer *)(v))->pointer = (gpointer)p)
//#define gug_pointer_check(v,typecode) (PyObject_TypeCheck(v, &PyGPointer_Type) && ((PyGPointer *)(v))->gtype == typecode)
#define gug_pointer_check(v,typecode) (((GuGPointer *)(v))->gtype == typecode)

typedef void (*GuGFatalExceptionFunc) (void);
typedef void (*GuGThreadBlockFunc) (void);

typedef struct {
    ssize_t ob_refcnt;
    GParamSpec *pspec;
} PyGParamSpec;

#define gug_param_spec_get(v)    (((GuGParamSpec *)v)->pspec)
#define gug_param_spec_set(v,p)  (((GuGParamSpec *)v)->pspec = (GParamSpec*)p)
#define gug_param_spec_check(v)  (PyObject_TypeCheck(v, &PyGParamSpec_Type))

/* Deprecated in favor of lower case with underscore macros above. */
#define GuGParamSpec_Get    gug_param_spec_get
#define GuGParamSpec_Check  gug_param_spec_check

typedef int (*GuGClassInitFunc) (gpointer gclass,
				 SCM guclass /* contains a SCM foreign-object type */
				 );

/* Returns an SCM containing a foreign-object type */
typedef SCM (*GuGTypeRegistrationFunction) (const gchar *name,
					    gpointer data);

struct _GuGObject_Functions {
    /* 
     * All field names in here are considered private,
     * use the macros below instead, which provides stability
     */
    void (* register_class)(SCM dict,
			    const gchar *class_name,
			    GType gtype,
			    SCM type, /* contains a SCM foreign-object type */
			    SCM bases);
    void (* register_wrapper)(SCM self);

    /* Returns an SCM foreign-object class */
    SCM(* lookup_class)(GType type);
    SCM (* newgobj)(GObject *obj);

    GClosure *(* closure_new)(SCM callback, SCM extra_args,
			      SCM swap_data);
    void      (* object_watch_closure)(SCM self, GClosure *closure);
    GDestroyNotify destroy_notify;

    GType (* type_from_object)(SCM obj);
    SCM (* type_wrapper_new)(GType type);

    gint (* enum_get_value)(GType enum_type, SCM obj, gint *val);
    gint (* flags_get_value)(GType flag_type, SCM obj, guint *val);
    void (* register_gtype_custom)(GType gtype,
			    SCM (* from_func)(const GValue *value),
			    int (* to_func)(GValue *value, SCM obj));
    int (* value_from_guobject)(GValue *value, SCM obj);
    SCM (* value_as_guobject)(const GValue *value, gboolean copy_boxed);

    void (* register_interface)(SCM dict, const gchar *class_name,
				GType gtype,
				SCM type /* contains a SCM foreign-object type */
				);

    SCM boxed_type;
    void (* register_boxed)(SCM dict, const gchar *class_name,
			    GType boxed_type,
			    SCM type);
    SCM (* boxed_new)(GType boxed_type, gpointer boxed,
			    gboolean copy_boxed, gboolean own_ref);

    SCM pointer_type;
    void (* register_pointer)(SCM dict, const gchar *class_name,
			      GType pointer_type, SCM type);
    SCM (* pointer_new)(GType boxed_type, gpointer pointer);

    void (* enum_add_constants)(SCM module, GType enum_type,
				const gchar *strip_prefix);
    void (* flags_add_constants)(SCM module, GType flags_type,
				 const gchar *strip_prefix);

    const gchar *(* constant_strip_prefix)(const gchar *name,
				     const gchar *strip_prefix);

    gboolean (* error_check)(GError **error);

    /* hooks to register handlers for getting GDK threads to cooperate
     * with python threading */
    void (* set_thread_block_funcs) (GuGThreadBlockFunc block_threads_func,
				     GuGThreadBlockFunc unblock_threads_func);
    GuGThreadBlockFunc block_threads;
    GuGThreadBlockFunc unblock_threads;

    SCM paramspec_type;
    SCM (* paramspec_new)(GParamSpec *spec);
    GParamSpec *(*paramspec_get)(SCM tuple);
    int (*pyobj_to_unichar_conv)(SCM pyobj, void* ptr);
    gboolean (*parse_constructor_args)(GType        obj_type,
                                       char       **arg_names,
                                       char       **prop_names,
                                       GParameter  *params,
                                       guint       *nparams,
				       SCM        gu_args   /* SCM list of arguments */
				       );

    SCM (* param_gvalue_as_pyobject) (const GValue* gvalue, 
				      gboolean copy_boxed,
				      const GParamSpec* pspec);
    int (* gvalue_from_param_pyobject) (GValue* value, 
                                        SCM py_obj, 
					const GParamSpec* pspec);
    SCM enum_type;
    SCM (*enum_add)(SCM module,
			  const char *type_name_,
			  const char *strip_prefix,
			  GType gtype);
    SCM (*enum_from_gtype)(GType gtype, int value);
    
    SCM flags_type;
    SCM (*flags_add)(SCM module,
		     const char *type_name_,
		     const char *strip_prefix,
		     GType gtype);
    PyObject* (*flags_from_gtype)(GType gtype, guint value);

    gboolean threads_enabled;
    int       (*enable_threads) (void);

    int       (*gil_state_ensure) (void);
    void      (*gil_state_release) (int flag);
    
    void      (*register_class_init) (GType gtype, GuGClassInitFunc class_init);
    void      (*register_interface_info) (GType gtype, const GInterfaceInfo *info);
    void      (*closure_set_exception_handler) (GClosure *closure, GuClosureExceptionHandler handler);

    void      (*add_warning_redirection) (const char *domain,
                                          SCM warning);
    void      (*disable_warning_redirections) (void);

    /* type_register_custom API now removed, but leave a pointer here to not
     * break ABI. */
    void      *_type_register_custom;

    gboolean  (*gerror_exception_check) (GError **error);
    SCM (*option_group_new) (GOptionGroup *group);
    GType (* type_from_object_strict) (SCM obj, gboolean strict);

    SCM (* newgobj_full)(GObject *obj, gboolean steal, gpointer g_class);
    SCM object_type;
    int (* value_from_pyobject_with_error)(GValue *value, SCM obj);
};


/* Deprecated, only available for API compatibility. */
#define gug_threads_enabled           TRUE
#define gug_gil_state_ensure          GugILState_Ensure
#define gug_gil_state_release         GugILState_Release
#define gug_begin_allow_threads       Py_BEGIN_ALLOW_THREADS
#define gug_end_allow_threads         Py_END_ALLOW_THREADS
#define gug_enable_threads()
#define gug_set_thread_block_funcs(a, b)
#define gug_block_threads()
#define gug_unblock_threads()


#ifndef _INSIDE_GUGOBJECT_

#if defined(NO_IMPORT) || defined(NO_IMPORT_GUGOBJECT)
extern struct _GuGObject_Functions *_GuGObject_API;
#else
struct _GuGObject_Functions *_GuGObject_API;
#endif

#if 0
#define gugobject_register_class    (_GuGObject_API->register_class)
#define gugobject_register_wrapper  (_GuGObject_API->register_wrapper)
#define gugobject_lookup_class      (_GuGObject_API->lookup_class)
#define gugobject_new               (_GuGObject_API->newgobj)
#define gugobject_new_full          (_GuGObject_API->newgobj_full)
#define GuGObject_Type              (*_GuGObject_API->object_type)
#define gug_closure_new             (_GuGObject_API->closure_new)
#define gugobject_watch_closure     (_GuGObject_API->object_watch_closure)
#define gug_closure_set_exception_handler (_GuGObject_API->closure_set_exception_handler)
#define gug_destroy_notify          (_GuGObject_API->destroy_notify)
#define gug_type_from_object_strict   (_GuGObject_API->type_from_object_strict)
#define gug_type_from_object        (_GuGObject_API->type_from_object)
//#define gug_type_wrapper_new        (_GuGObject_API->type_wrapper_new)
//#define gug_enum_get_value          (_GuGObject_API->enum_get_value)
//#define gug_flags_get_value         (_GuGObject_API->flags_get_value)
#define gug_register_gtype_custom   (_GuGObject_API->register_gtype_custom)
#define gug_value_from_pyobject     (_GuGObject_API->value_from_pyobject)
#define gug_value_from_pyobject_with_error (_GuGObject_API->value_from_pyobject_with_error)
#define gug_value_as_pyobject       (_GuGObject_API->value_as_pyobject)
#define gug_register_interface      (_GuGObject_API->register_interface)
#define GugBoxed_Type               (*_GuGObject_API->boxed_type)
#define gug_register_boxed          (_GuGObject_API->register_boxed)
#define gug_boxed_new               (_GuGObject_API->boxed_new)
#define GugPointer_Type             (*_GuGObject_API->pointer_type)
#define gug_register_pointer        (_GuGObject_API->register_pointer)
#define gug_pointer_new             (_GuGObject_API->pointer_new)
#define gug_enum_add_constants      (_GuGObject_API->enum_add_constants)
#define gug_flags_add_constants     (_GuGObject_API->flags_add_constants)
#define gug_constant_strip_prefix   (_GuGObject_API->constant_strip_prefix)
#define gug_error_check             (_GuGObject_API->error_check)
#define GugParamSpec_Type           (*_GuGObject_API->paramspec_type)
#define gug_param_spec_new          (_GuGObject_API->paramspec_new)
#define gug_param_spec_from_object  (_GuGObject_API->paramspec_get)
#define gug_pyobj_to_unichar_conv   (_GuGObject_API->pyobj_to_unichar_conv)
//#define gug_parse_constructor_args  (_GuGObject_API->parse_constructor_args)
#define gug_param_gvalue_as_pyobject   (_GuGObject_API->value_as_pyobject)
#define gug_param_gvalue_from_pyobject (_GuGObject_API->gvalue_from_param_pyobject)
#define GugEnum_Type                (*_GuGObject_API->enum_type)
#define gug_enum_add                (_GuGObject_API->enum_add)
#define gug_enum_from_gtype         (_GuGObject_API->enum_from_gtype)
#define GugFlags_Type               (*_GuGObject_API->flags_type)
#define gug_flags_add               (_GuGObject_API->flags_add)
#define gug_flags_from_gtype        (_GuGObject_API->flags_from_gtype)
#define gug_register_class_init     (_GuGObject_API->register_class_init)
#define gug_register_interface_info (_GuGObject_API->register_interface_info)
#define gug_add_warning_redirection   (_GuGObject_API->add_warning_redirection)
#define gug_disable_warning_redirections (_GuGObject_API->disable_warning_redirections)
#define gug_gerror_exception_check (_GuGObject_API->gerror_exception_check)
#define gug_option_group_new       (_GuGObject_API->option_group_new)
#endif

/**
 * gugobject_init:
 * @req_major: minimum version major number, or -1
 * @req_minor: minimum version minor number, or -1
 * @req_micro: minimum version micro number, or -1
 * 
 * Imports and initializes the 'gobject' python module.  Can
 * optionally check for a required minimum version if @req_major,
 * @req_minor, and @req_micro are all different from -1.
 * 
 * Returns: a new reference to the gobject module on success, NULL in
 * case of failure (and raises ImportError).
 **/
static inline SCM 
gugobject_init(int req_major, int req_minor, int req_micro)
{
    SCM gobject, cobject;
    
    gobject = GuImport_ImportModule("gi._gobject");
    if (!gobject) {
        if (GuErr_Occurred())
        {
            SCM type, value, traceback;
            SCM py_orig_exc;
            GuErr_Fetch(&type, &value, &traceback);
            py_orig_exc = GuObject_Repr(value);
            Gu_XDECREF(type);
            Gu_XDECREF(value);
            Gu_XDECREF(traceback);


            {
                /* Can not use GuErr_Format because it doesn't have
                 * a format string for dealing with PyUnicode objects
                 * like PyUnicode_FromFormat has
                 */
                /* SCM errmsg = PyUnicode_FromFormat("could not import gobject (error was: %U)", */
                /*                                         py_orig_exc); */
		// FIXME: fix format 
		SCM errmsg = scm_from_utf8_string("could not import gobject");
                if (errmsg) {
                   GuErr_SetObject(GuExc_ImportError,
                                   errmsg);
                   Gu_DECREF(errmsg);
                }
                /* if errmsg is NULL then we might have OOM
                 * PyErr should already be set and trying to
                 * return our own error would be futile
                 */
            }
            Gu_DECREF(py_orig_exc);
        } else {
            GuErr_SetString(GuExc_ImportError,
                            "could not import gobject (no error given)");
        }
        return NULL;
    }

    cobject = GuObject_GetAttrString(gobject, "_GuGObject_API");
    if (cobject && GuCapsule_CheckExact(cobject)) {
        _GuGObject_API = (struct _GuGObject_Functions *) GuCapsule_GetPointer(cobject, "gobject._GuGObject_API");
        Gu_DECREF (cobject);
    } else {
        GuErr_SetString(GuExc_ImportError,
                        "could not import gobject (could not find _GuGObject_API object)");
        Gu_XDECREF (cobject);
        Gu_DECREF(gobject);
        return NULL;
    }

    if (req_major != -1)
    {
        int found_major, found_minor, found_micro;
        SCM version;

        version = GuObject_GetAttrString(gobject, "gugobject_version");
        if (!version) {
            GuErr_SetString(GuExc_ImportError,
                            "could not import gobject (version too old)");
            Gu_DECREF(gobject);
            return NULL;
        }
        if (!GuArg_ParseTuple(version, "iii",
                              &found_major, &found_minor, &found_micro)) {
            GuErr_SetString(GuExc_ImportError,
                            "could not import gobject (version has invalid format)");
            Gu_DECREF(version);
            Gu_DECREF(gobject);
            return NULL;
        }
        Gu_DECREF(version);
        if (req_major != found_major ||
            req_minor >  found_minor ||
            (req_minor == found_minor && req_micro > found_micro)) {
            GuErr_Format(GuExc_ImportError,
                         "could not import gobject (version mismatch, %d.%d.%d is required, "
                         "found %d.%d.%d)", req_major, req_minor, req_micro,
                         found_major, found_minor, found_micro);
            Gu_DECREF(gobject);
            return NULL;
        }
    }
    return gobject;
}

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
    }; \
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

#endif /* !_INSIDE_GUGOBJECT_ */

G_END_DECLS

#endif /* !_GUGOBJECT_H_ */

#if 0
typedef struct {
    PyObject_HEAD
    gpointer boxed;
    GType gtype;
    gboolean free_on_dealloc;
} GugBoxed;

// FIXME: these shoudl be slot set/get on a foreign object

#define gug_boxed_get(v,t)      ((t *)((GugBoxed *)(v))->boxed)
#define gug_boxed_get_ptr(v)    (((GugBoxed *)(v))->boxed)
#define gug_boxed_set_ptr(v,p)  (((GugBoxed *)(v))->boxed = (gpointer)p)
#define gug_boxed_check(v,typecode) (PyObject_TypeCheck(v, &GugBoxed_Type) && ((GugBoxed *)(v))->gtype == typecode)

gpointer
_gug_boxed_get (SCM v);

#define gug_boxed_get(v, t) ((t *)_gug_boxed_get(v))
#endif

