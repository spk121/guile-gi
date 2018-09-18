#ifndef _GIR_GOBJECT_H_
#define _GIR_GOBJECT_H_

#include <libguile.h>
#include <glib-object.h>

extern GQuark gugobject_class_key;

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


GClosure *GuGObject_get_closure_from_proc(SCM gobj, SCM gfunc);
void gir_init_gobject(void);

#endif
