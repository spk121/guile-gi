/* -*- Mode: C; c-basic-offset: 4 -*- */
#ifndef _GIR_GOBJECT_H_
#define _GIR_GOBJECT_H_

#include <libguile.h>
#include <glib-object.h>


/* Data that belongs to the GObject instance, not the Python wrapper */
typedef struct _GuGObjectData {
    SCM type; /* wrapper type for this instance */

    /* This data of this list contains SCM of type GuGClosure */
    GSList *closures;
} GuGObjectData;

extern GType PY_TYPE_OBJECT;
extern GQuark gugobject_instance_data_key;
extern GQuark gugobject_custom_key;
extern GQuark gugobject_wrapper_key;
extern GQuark gugobject_class_key;
extern GQuark gugobject_class_init_key;

extern SCM GuGObject_Type;

typedef enum {
	      GUGOBJECT_USING_TOGGLE_REF = 1 << 0,
	      GUGOBJECT_IS_FLOATING_REF = 1 << 1,
	      GUGOBJECT_GOBJECT_WAS_FLOATING = 1 << 2
} GuGObjectFlags;

////////////////////////////////////////////////////////////////
// GuGObject getters and setters
SCM GuGObject_get_ob_type(SCM gobj);
void GuGObject_set_ob_type(SCM obj, SCM ob_type);
ssize_t GuGObject_get_ob_refcnt(SCM obj);
void GuGObject_set_ob_refcnt(SCM obj, ssize_t refcnt);
GObject *GuGObject_get_obj (SCM gobj);
void GuGObject_set_obj(SCM obj, GObject *ptr, scm_t_pointer_finalizer finalizer);
GuGObjectFlags GuGObject_get_flags (SCM gobj);
void GuGObject_set_flags (SCM gobj, GuGObjectFlags flags);
SCM GuGObject_get_inst_dict (SCM gobj);

/* re pyg_object_peek_inst_data */
GuGObjectData *GObject_peek_inst_data(GObject *obj);

/* re gclosure_from_pyfunc */
GClosure *GuGObject_get_closure_from_proc(SCM gobj, SCM gfunc);
void gugobject_register_wrapper(SCM self);

void gir_init_gobject(void);

#endif
