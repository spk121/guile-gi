#ifndef _GI_GOBJECT_H_
#define _GI_GOBJECT_H_
#include <libguile.h>
#include <glib-object.h>
#include <girepository.h>
#include "__gi_gobject.h"

G_BEGIN_DECLS
    typedef void (*GuClosureExceptionHandler)(GValue *ret, guint n_param_values,
                                              const GValue *params);

typedef struct _GuGClosure
{
    GClosure closure;
    SCM callback;
    SCM swap_data;              /* other object for gtk_signal_connect__object */
    GuClosureExceptionHandler exception_handler;
    GISignalInfo *signal_info;
} GuGClosure;

typedef enum
{
    GI_GOBJECT_USING_TOGGLE_REF = 1 << 0,
    GI_GOBJECT_IS_FLOATING_REF = 1 << 1,
    GI_GOBJECT_GOBJECT_WAS_FLOATING = 1 << 2
} GuGObjectFlags;

/* Data that belongs to the GObject instance, not the Python wrapper */
/* re pygobject-object.h: 10, _PyGObjectData */
typedef struct _GuGObjectData
{
    SCM type;                   /* wrapper type for this instance */
    GSList *closures;
} GuGObjectData;

extern GQuark gi_gobject_instance_data_key;

/* re pyg_object_peek_inst_sata */
static inline GuGObjectData *
gi_gobject_peek_inst_data(GObject *obj)
{
    return ((GuGObjectData *)
            g_object_get_qdata(obj, gi_gobject_instance_data_key));
}

void gi_init_gobject(void);
GClosure *gclosure_from_scm_func(SCM object, SCM func);
SCM gi_gobject_lookup_class(GType);
SCM gi_gobject_new(GIObjectInfo *info, GObject *obj);
SCM scm_gobject_printer(SCM self, SCM port);

G_END_DECLS
#endif
