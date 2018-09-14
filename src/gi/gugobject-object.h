#ifndef _PYGOBJECT_OBJECT_H_
#define _PYGOBJECT_OBJECT_H_

#include <libguile.h>
#include <glib-object.h>
// #include "gugi-python-compat.h"
#include "gugobject-internal.h"

/* Data that belongs to the GObject instance, not the Python wrapper */
struct _PyGObjectData {
    PyTypeObject *type; /* wrapper type for this instance */
    GSList *closures;
};

extern GType GU_TYPE_OBJECT;
extern GQuark gugobject_instance_data_key;
extern GQuark gugobject_custom_key;
extern GQuark gugobject_wrapper_key;
extern GQuark gugobject_class_key;
extern GQuark gugobject_class_init_key;

#if 0
extern PyTypeObject PyGObjectWeakRef_Type;
extern PyTypeObject PyGPropsIter_Type;
extern PyTypeObject PyGPropsDescr_Type;
extern PyTypeObject PyGProps_Type;
#endif
extern SCM GuGObject_Type;
extern SCM GuGObject_MetaType;
#if 0

static inline PyGObjectData *
pyg_object_peek_inst_data(GObject *obj)
{
    return ((PyGObjectData *)
            g_object_get_qdata(obj, pygobject_instance_data_key));
}

gboolean      pygobject_prepare_construct_properties  (GObjectClass *class,
                                                       PyObject *kwargs,
                                                       guint *n_params,
                                                       GParameter **params);
#endif
void          gugobject_register_class   (SCM dict,
                                          const gchar *type_name,
                                          GType gtype, PyTypeObject *type,
                                          PyObject *bases);

void          gugobject_register_wrapper (SCM self);
SCM           gugobject_new              (GObject *obj);
SCM           gugobject_new_full         (GObject *obj, gboolean steal, gpointer g_class);
void          gugobject_sink             (GObject *obj);

SCM           gugobject_lookup_class     (GType gtype);
void          gugobject_watch_closure    (SCM self, GClosure *closure);
int           gui_object_register_types  (SCM d);
void          gugobject_ref_float(GuGObject *self);
#if 0
void          pygobject_ref_sink(PyGObject *self);
#endif
SCM           gug_object_new             (SCM        self, SCM       args, SCM       kwargs);
#if 0
GClosure *    gclosure_from_pyfunc(PyGObject *object, PyObject *func);
#endif
#endif /*_PYGOBJECT_OBJECT_H_*/
