/* -*- Mode: C; c-basic-offset: 4 -*- */
#include <libguile.h>
#include <glib.h>
#include "gi_gboxed.h"

GQuark gugboxed_type_key;

void
gi_gboxed_finalizer (SCM self)
{
    if (gi_gboxed_get_free_on_dealloc (self) && gi_gboxed_get_ptr (self))
	g_boxed_free (gi_gboxed_get_gtype (self), gi_gboxed_get_ptr (self));
    gi_gboxed_set_ptr(self, NULL);
}

#if 0
/* Registers a wrapper for a boxed type.  The wrapper class is a
 * subclass of GBoxed.  Unlike the parent class <GBoxed> where gtype
 * is a variable, this registers a class where GType is fixed.  A
 * reference to the wrapper class is stored in the module. */

SCM
gi_register_gboxed (SCM module, const gchar *class_name,
		    GType boxed_type)
{
    SCM type;

    g_return_if_fail(dict != NULL);
    g_return_if_fail(class_name != NULL);
    g_return_if_fail(boxed_type != 0);

    // FIXME: impossible from C?
}
#endif

SCM
gi_gboxed_new (GType boxed_type, gpointer boxed, gboolean copy_boxed, gboolean own_ref)
{
    void *ptr;
    SCM tp;
    SCM self;

    if (!boxed)
        return SCM_UNDEFINED;

    self = scm_make_foreign_object_0 (gi_gboxed_type);

    if (copy_boxed)
        boxed = g_boxed_copy(boxed_type, boxed);
    gi_gboxed_set_ptr (self, boxed);
    gi_gboxed_set_gtype (self, boxed_type);
    gi_gboxed_set_free_on_dealloc (self, own_ref);
    return self;
}

static SCM
_wrap_gi_gboxed_copy (SCM self)
{
    return gi_gboxed_new (gi_gboxed_get_gtype (self),
			  gi_gboxed_get_ptr (self),
			  TRUE,
			  TRUE);
}

void
gi_init_gboxed (void)
{
    gi_init_gboxed_type ();
    gugboxed_type_key = g_quark_from_static_string("guile-gi::gboxed");
    scm_c_define_gsubr ("gboxed-copy", 1, 0, 0, _wrap_gi_gboxed_copy);
    scm_c_export ("gboxed-copy",
		  NULL);
}
