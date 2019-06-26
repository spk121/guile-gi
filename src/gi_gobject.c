// Copyright (C), 2018, 2019 Michael L. Gran

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
#include "gi_gobject.h"
#include "gi_gvalue.h"
#include "gi_gsignal.h"
#include "gi_gparamspec.h"
#include "gi_signal_closure.h"
#include "gi_util.h"
#include "gir_typelib.h"
#include "gir_type.h"
#include <glib-object.h>
#include <glib.h>
#include <girepository.h>
#include <libguile.h>
#include <errno.h>

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
// static GQuark gi_gobject_class_key;

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
            int res = scm_is_eq (guclosure->callback, func);
            if (res) {
                return (GClosure*)guclosure;
            }
        }
    }
    return NULL;
}

/* re pygobject-object.c:103 pygobject_data_free */
static void
gugobject_data_free(GuGObjectData *data)
{
    GSList *closures, *tmp;

    tmp = closures = data->closures;
    data->closures = NULL;
    data->type = SCM_BOOL_F;
    while (tmp)
    {
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

static inline GuGObjectData * __attribute__((malloc))
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


/* re pygobject-object.c:980, pygobject_new_full */
static SCM
gi_gobject_new_full(GIObjectInfo *info, GObject *obj, gboolean steal, gpointer g_class)
{
    gpointer ptr;
    SCM self;
    if (obj == NULL)
        return SCM_BOOL_F;

    /* Check if this obj is wrapped already. */
    ptr = g_object_get_qdata(obj, gi_gobject_wrapper_key);
    if (ptr)
    {
        self = SCM_PACK_POINTER (ptr);
        if (steal)
            g_object_unref(obj);
    }
    else
    {
        /* create wrapper */
        GuGObjectData *inst_data = gi_gobject_peek_inst_data(obj);
        SCM tp;
        if (inst_data)
            tp = inst_data->type;
        else
        {
            /* if (g_class) */
            /*  tp = gugobject_lookup_class (G_OBJECT_CLASS_TYPE (g_class)); */
            /* else */
            /*  tp = gugobject_lookup_class (G_OBJECT_TYPE (obj)); */
            //tp = gi_gtype_c2g(G_OBJECT_TYPE(obj));
            tp = scm_from_size_t(G_OBJECT_TYPE(obj));
        }
        g_assert(scm_is_true(tp));

        //if (gi_gtype_get_flags (tp) & Gu_TPFLAGS_HEAPTYPE)
        //    Gu_INCREF(tp);
        self = gir_type_make_object(G_OBJECT_TYPE(obj),
                                    obj,
                                    0);

        if (g_object_is_floating(obj))
            scm_foreign_object_unsigned_set_x(self,
                                              FLAGS_SLOT,
                                              GI_GOBJECT_GOBJECT_WAS_FLOATING);
        if (!steal
            || (gi_gobject_get_flags(self) & GI_GOBJECT_GOBJECT_WAS_FLOATING))
            g_object_ref_sink(obj);
    }
    return self;
}

/* re pygobject-object.c: 1059 pygobject_new */
SCM
gi_gobject_new (GIObjectInfo *info, GObject *obj)
{
    return gi_gobject_new_full (info, obj, FALSE, NULL);
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

    g_return_if_fail(gir_type_get_gtype_from_obj(self) > G_TYPE_INVALID);
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

    // SCM self = SCM_PACK_POINTER (g_object_get_qdata (object, gi_gobject_wrapper_key));
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

static SCM
connect_helper (SCM self, gchar *name, SCM callback, SCM extra_args,
                SCM object, gboolean after)
{
    guint sigid;
    GQuark detail = 0;
    GClosure *closure = NULL;
    gulong handlerid;
    GSignalQuery query_info;

    GObject *obj = scm_foreign_object_ref (self, OBJ_SLOT);
    GType gtype = G_OBJECT_TYPE(obj);

    if (!g_signal_parse_name(name, gtype,
                             &sigid, &detail, TRUE))
    {
        scm_misc_error ("connect_helper",
                        "~A: unknown signal name ~A",
                        scm_list_2 (self, scm_from_utf8_string(name)));
    }

    g_signal_query (sigid, &query_info);
    if (g_type_get_qdata (gtype, gi_gobject_custom_key) == NULL)
    {
        /* The signal is implemented by a non-Scheme class. */
        closure = gi_signal_closure_new (self, query_info.itype,
                                         query_info.signal_name, callback,
                                         extra_args);
    }

    if (!closure)
    {
        /* The signal is implemented at the Scheme level, probably */
        // closure = gug_closure_new (callback, extra_args, object);
        closure = gi_signal_closure_new (self, query_info.itype,
                                         query_info.signal_name, callback,
                                         extra_args);
        //g_critical ("unimplemented");
        //g_abort();
    }

    gugobject_watch_closure (self, closure);
    handlerid = g_signal_connect_closure_by_id (obj,
                                                sigid, detail, closure,
                                                after);

    return scm_from_ulong (handlerid);
}

static SCM
scm_signal_connect (SCM self, SCM s_name, SCM proc, SCM rest)
{
    if (gir_type_get_gtype_from_obj(self) <= G_TYPE_INVALID)
        scm_error (scm_arg_type_key, "signal-connect",
                   "Wrong type in position 1: ~S",
                   scm_list_1 (self),
                   scm_list_1 (self));

    char *name = scm_to_utf8_string (s_name);
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

    SCM_ASSERT (gir_type_get_gtype_from_obj (self) > G_TYPE_INVALID,
                self, SCM_ARG1, "gobject-disconnect-by-func");
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

    SCM_ASSERT (gir_type_get_gtype_from_obj (self) > G_TYPE_INVALID,
                self, SCM_ARG1, "gobject-handler-block-by-func");
    SCM_ASSERT (scm_is_true (scm_procedure_p (func)), func, SCM_ARG2,
                "gobject-handler-block-by-func");

    closure = gclosure_from_scm_func(self, func);
    if (!closure)
        scm_misc_error ("gobject-handler-block-by-func",
                        "nothing connected to ~S",
                        scm_list_1 (func));

    obj = scm_foreign_object_ref (self, OBJ_SLOT);
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

    SCM_ASSERT (gir_type_get_gtype_from_obj (self) > G_TYPE_INVALID,
                self, SCM_ARG1, "gobject-handler-unblock-by-func");
    SCM_ASSERT (scm_is_true (scm_procedure_p (func)), func, SCM_ARG2,
                "gobject-handler-unblock-by-func");

    closure = gclosure_from_scm_func(self, func);
    if (!closure)
        scm_misc_error ("gobject-handler-unblock-by-func",
                        "nothing connected to ~S",
                        scm_list_1 (func));

    obj = scm_foreign_object_ref (self, OBJ_SLOT);
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
    obj = gir_type_make_object (type, instance, 0);
    inst_dict = scm_make_hash_table (scm_from_int (10));
    scm_foreign_object_set_x(obj, INST_DICT_SLOT, SCM_UNPACK_POINTER (inst_dict));

    /* We're using a hash table as the property variable store for
       this object. */
    for (guint i = 0; i < n_properties; i ++)
    {
        SCM sval;
        const GValue* _default;

        _default = g_param_spec_get_default_value (properties[i]);
        sval = gi_gvalue_as_scm (_default, TRUE);
        scm_hash_set_x (inst_dict,
                        scm_from_utf8_string (g_param_spec_get_name (properties[i])),
                        sval);
    }

    instance_data->obj = obj;
    g_object_set_qdata (G_OBJECT(instance),
                        gi_gobject_wrapper_key,
                        SCM_UNPACK_POINTER (obj));
}

static void
wrap_object (GObject *object)
{
    /* Somehow, you managed to make a pointer to a Guile-defined class
     * object without actually making the Scheme wrapper.  Let's try
     * to add it now. */
    g_assert_not_reached ();
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
    if (!ptr)
    {
        wrap_object (object);
        ptr = g_object_get_qdata (object, gi_gobject_wrapper_key);
    }

    obj = SCM_PACK_POINTER (ptr);

    g_assert (scm_foreign_object_ref (obj, OBJ_SLOT) == object);

    /* We're using a hash table as the property variable store for
       this object. */
    inst_dict = SCM_PACK_POINTER (scm_foreign_object_ref(obj, INST_DICT_SLOT));
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

    obj = SCM_PACK_POINTER (ptr);

    g_assert (scm_foreign_object_ref(obj, OBJ_SLOT) == object);

    /* We're using a hash table as the property variable store for
       this object. */
    inst_dict = SCM_PACK_POINTER (scm_foreign_object_ref (obj, INST_DICT_SLOT));
    svalue = gi_gvalue_as_scm (value, TRUE);
    scm_hash_set_x (inst_dict,
                    scm_from_utf8_string (g_param_spec_get_name (pspec)),
                    svalue);
}

static void
dispose (GObject *object)
{
    GType type, parent_type;
    gpointer _parent_class;
    GObjectClass *parent_class;

    type = G_OBJECT_TYPE (object);
    parent_type = g_type_parent (type);

    g_assert (G_TYPE_IS_CLASSED (type));
    g_assert (G_TYPE_IS_CLASSED (parent_type));

    g_info ("dispose is currently just calling the parent's dispose");

    g_debug ("disposing parent type");
    _parent_class = g_type_class_ref (parent_type);
    parent_class = G_OBJECT_CLASS (_parent_class);

    parent_class->dispose (object);

    g_type_class_unref (_parent_class);
}

static void
finalize (GObject *object)
{
    g_info ("finalization is not yet implemented, this is a noop");
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

    type_name = scm_to_utf8_string (s_type_name);

    parent_type = scm_to_gtype (s_parent_type);

    if (scm_is_false (gir_type_get_scheme_type (parent_type)))
        scm_misc_error ("register-type",
                        "type ~S lacks introspection",
                        scm_list_1 (s_parent_type));

    SCM_UNBND_TO_BOOL_F (s_properties);
    SCM_UNBND_TO_BOOL_F (s_signals);
    SCM_UNBND_TO_BOOL_F (s_disposer);

    SCM_ASSERT_TYPE (scm_is_false (s_properties) ||
                     scm_is_list (s_properties),
                     s_properties, SCM_ARG3,
                     "register-type", "list of param specs or #f");

    SCM_ASSERT_TYPE (scm_is_false (s_signals) ||
                     scm_is_list (s_signals),
                     s_signals, SCM_ARG4,
                     "register-type", "list of signal specs or #f");

    SCM_ASSERT_TYPE (scm_is_false (s_disposer) ||
                     scm_is_true (scm_procedure_p (s_disposer)),
                     s_disposer, SCM_ARG5,
                     "register-type", "procedure or #f");

    properties = g_ptr_array_new ();
    signals = g_ptr_array_new_with_free_func ((GDestroyNotify) gi_free_signalspec);

    if (scm_is_list (s_properties))
    {
        n_properties = scm_to_size_t (scm_length (s_properties));
        for (size_t i = 0; i < n_properties; i ++)
        {
            GParamSpec *pspec;
            pspec = gi_gparamspec_from_scm (scm_list_ref (s_properties, scm_from_size_t (i)));
            g_ptr_array_add (properties, pspec);
        }
    }

    if (scm_is_list (s_signals))
    {
        n_signals = scm_to_size_t (scm_length (s_signals));
        for (size_t i = 0; i < n_signals; i ++)
        {
            SignalSpec *sspec;
            sspec = gi_signalspec_from_obj (scm_list_ref (s_signals , scm_from_size_t (i)));
            g_ptr_array_add (signals, sspec);
        }

    }

    new_type = register_guile_specified_gobject_type (type_name,
                                                      parent_type,
                                                      properties,
                                                      signals,
                                                      s_disposer);

    gir_type_define(new_type);
    return gir_type_get_scheme_type (new_type);
}

static void*
scm_dynwind_or_bust (char *subr, void *mem)
{
    if (mem)
        scm_dynwind_free (mem);
    else
    {
        errno = ENOMEM;
        scm_syserror (subr);
    }
    return mem;
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
    const char **keys;
    GValue *values;

    type = scm_to_gtype (s_gtype);

    SCM_ASSERT_TYPE (G_TYPE_IS_CLASSED (type), s_gtype, SCM_ARG1,
                     "make-gobject",
                     "typeid derived from G_TYPE_OBJECT or "
                     "scheme type derived from <GObject>");

    if (scm_is_false (gir_type_get_scheme_type (type)))
        scm_misc_error ("make-gobject",
                        "type ~S lacks introspection",
                        scm_list_1 (s_gtype));

    scm_dynwind_begin (0);

    if (scm_is_true (scm_list_p (s_prop_alist))) {
        class = g_type_class_ref (type);
        scm_dynwind_unwind_handler (g_type_class_unref, class,
                                    SCM_F_WIND_EXPLICITLY);

        n_prop = scm_to_int (scm_length (s_prop_alist));
        keys = scm_dynwind_or_bust ("make-gobject",
                                    calloc (n_prop, sizeof (char *)));
        values = scm_dynwind_or_bust ("make-gobject",
                                      calloc (n_prop, sizeof (GValue)));

        for (guint i = 0; i < n_prop; i++) {
            SCM entry = scm_list_ref (s_prop_alist, scm_from_uint (i));

            SCM_ASSERT_TYPE (scm_is_true (scm_pair_p (entry)),
                             s_prop_alist, SCM_ARG2,
                             "make-gobject", "alist of strings to objects");

            SCM_ASSERT_TYPE (scm_is_string (scm_car (entry)),
                             s_prop_alist, SCM_ARG2,
                             "make-gobject", "alist of strings to objects");

            keys[i] = scm_dynwind_or_bust ("make-gobject",
                                           scm_to_utf8_string (scm_car (entry)));
            GParamSpec *pspec = g_object_class_find_property (class, keys[i]);
            if (!pspec) {
                scm_misc_error ("make-gobject",
                                "unknown object parameter ~S",
                                scm_list_1 (entry));
            }
            else {
                GValue* value = &values[i];
                if (G_IS_PARAM_SPEC_CHAR (pspec)) {
                    g_value_init (value, G_TYPE_CHAR);
                    g_value_set_schar (value, scm_to_int8 (scm_cdr (entry)));
                }
                else if (G_IS_PARAM_SPEC_UCHAR (pspec)) {
                    g_value_init (value, G_TYPE_UCHAR);
                    g_value_set_uchar (value, scm_to_uint8 (scm_cdr (entry)));
                }
                else if (G_IS_PARAM_SPEC_INT (pspec)) {
                    g_value_init (value, G_TYPE_INT);
                    g_value_set_int (value, scm_to_int (scm_cdr (entry)));
                }
                else if (G_IS_PARAM_SPEC_UINT (pspec)) {
                    g_value_init (value, G_TYPE_UINT);
                    g_value_set_uint (value, scm_to_uint (scm_cdr (entry)));
                }
                else if (G_IS_PARAM_SPEC_LONG (pspec)) {
                    g_value_init (value, G_TYPE_LONG);
                    g_value_set_uint (value, scm_to_long (scm_cdr (entry)));
                }
                else if (G_IS_PARAM_SPEC_ULONG (pspec)) {
                    g_value_init (value, G_TYPE_ULONG);
                    g_value_set_ulong (value, scm_to_ulong (scm_cdr (entry)));
                }
                else if  (G_IS_PARAM_SPEC_INT64 (pspec)) {
                    g_value_init (value, G_TYPE_INT64);
                    g_value_set_int64 (value, scm_to_int64 (scm_cdr (entry)));
                }
                else if (G_IS_PARAM_SPEC_UINT64 (pspec)) {
                    g_value_init (value, G_TYPE_UINT64);
                    g_value_set_uint64 (value, scm_to_uint64 (scm_cdr (entry)));
                }
                else if (G_IS_PARAM_SPEC_FLOAT (pspec)) {
                    g_value_init (value, G_TYPE_FLOAT);
                    g_value_set_float (value, scm_to_double (scm_cdr (entry)));
                }
                else if (G_IS_PARAM_SPEC_DOUBLE (pspec)) {
                    g_value_init (value, G_TYPE_DOUBLE);
                    g_value_set_double (value, scm_to_double (scm_cdr (entry)));
                }
                else if (G_IS_PARAM_SPEC_ENUM (pspec)) {
                    g_value_init (value, G_TYPE_ENUM);
                    g_value_set_enum (value, scm_to_ulong (scm_cdr (entry)));
                }
                else if (G_IS_PARAM_SPEC_FLAGS (pspec)) {
                    g_value_init (value, G_PARAM_SPEC_VALUE_TYPE(pspec));
                    g_value_set_flags (value, scm_to_ulong (scm_cdr (entry)));
                }
                else if (G_IS_PARAM_SPEC_STRING (pspec)) {
                    g_value_init (value, G_TYPE_STRING);
                    g_value_set_string (value, scm_to_utf8_string (scm_cdr (entry)));
                }
                else if (G_IS_PARAM_SPEC_OBJECT (pspec))
                {
                    SCM src = scm_cdr(entry);
                    GType src_type = gir_type_get_gtype_from_obj(src);
                    GType dest_type = G_PARAM_SPEC_VALUE_TYPE(pspec);
                    if (g_type_is_a (src_type, dest_type))
                    {
                        g_value_init (value, dest_type);
                        g_value_set_object (value, scm_foreign_object_ref (src, OBJ_SLOT));
                    }
                    else
                        scm_misc_error ("make-gobject",
                                        "unable to convert parameter ~S of type ~S into a ~S",
                                        scm_list_3 (src,
                                                    scm_from_utf8_string(g_type_name(src_type)),
                                                    scm_from_utf8_string(g_type_name(dest_type))));
                }
                else
                    scm_misc_error ("make-gobject",
                                    "unable to convert parameter ~S",
                                    scm_list_1 (entry));
            }
        }
    }
    else {
        n_prop = 0;
        keys = NULL;
        values = NULL;
    }

    obj = g_object_new_with_properties (type, n_prop, keys, values);
    scm_dynwind_end ();

    g_assert (obj);

    ptr = g_object_get_qdata (obj, gi_gobject_wrapper_key);

    if (ptr)
        sobj = SCM_PACK_POINTER (ptr);
    else {
        sobj = gir_type_make_object (type, obj, GI_TRANSFER_EVERYTHING);
        g_object_set_qdata (G_OBJECT (obj), gi_gobject_wrapper_key,
                            SCM_UNPACK_POINTER (sobj));
    }

    g_assert (scm_foreign_object_ref (sobj, OBJ_SLOT) == obj);
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

    return scm_from_size_t(type);
    //return gi_gtype_c2g (type);
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

/* re pygobject_set_property */
static SCM
scm_gobject_set_property_x (SCM self, SCM sname, SCM svalue)
{
#define FUNC_NAME "gobject-set-property!"
    GObject *obj;
    char *name;
    GParamSpec *pspec;
    GValue value = { 0, };

    SCM_ASSERT (G_TYPE_IS_CLASSED (gir_type_get_gtype_from_obj (self)),
                self, SCM_ARG1, FUNC_NAME);
    SCM_ASSERT (scm_is_string (sname), sname, SCM_ARG2, FUNC_NAME);

    obj = gi_gobject_get_obj (self);
    name = scm_to_utf8_string (sname);
    pspec = g_object_class_find_property (G_OBJECT_GET_CLASS (obj), name);
    free (name);
    if (!pspec)
        scm_misc_error (FUNC_NAME,
                        "object of type ~S does not have property ~S",
                        scm_list_2(scm_from_utf8_string (g_type_name (G_OBJECT_TYPE (obj))),
                                   sname));

    if (!(pspec->flags & G_PARAM_WRITABLE)) {
        scm_misc_error (FUNC_NAME, "property ~S is not writable",
                        scm_list_1 (scm_from_utf8_string (g_param_spec_get_name (pspec))));
    }

    g_value_init (&value, G_PARAM_SPEC_VALUE_TYPE (pspec));
    gi_gvalue_from_scm_with_error (FUNC_NAME, &value, svalue, SCM_ARG3);

    g_object_set_property (gi_gobject_get_obj(self), pspec->name, &value);

    return SCM_UNSPECIFIED;
#undef FUNC_NAME
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

    if (!(pspec->flags & G_PARAM_READABLE)) {
        scm_misc_error (func, "property ~S is not readable",
                        scm_list_1 (scm_from_utf8_string (g_param_spec_get_name (pspec))));
    }

    g_value_init (&value, G_PARAM_SPEC_VALUE_TYPE (pspec));
    g_object_get_property (gi_gobject_get_obj(instance), pspec->name, &value);

    svalue = gi_param_gvalue_as_scm (&value, TRUE, pspec);

    g_value_unset (&value);
    return svalue;
}

static SCM
scm_gobject_get_property (SCM self, SCM sname)
{
    char *param_name;
    SCM ret;

    SCM_ASSERT (G_TYPE_IS_CLASSED (gir_type_get_gtype_from_obj (self)),
                self, SCM_ARG1, "gobject-get-property");
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
        g_assert_not_reached();
#if 0
        s_obj = gi_gobject_new_full (arg->v_pointer,
                                     transfer == GI_TRANSFER_EVERYTHING, /* steal */
                                     NULL); /* type */
#endif
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
    gi_init_gobject_type ();
    gi_gobject_wrapper_key = g_quark_from_static_string ("GuGObject::wrapper");
    gi_gobject_custom_key = g_quark_from_static_string ("GuGObject::custom");
    gi_gobject_instance_data_key = g_quark_from_static_string("GuGObject::instance-data");

    scm_c_define_gsubr ("register-type", 2, 3, 0, scm_register_guile_specified_gobject_type);
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
