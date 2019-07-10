#ifndef _GIG_OBJECT_H_
#define _GIG_OBJECT_H_

#include <libguile.h>
#include <glib.h>
#include <glib-object.h>
#include <girepository.h>

extern SCM gig_object_type;

SCM gig_object_ref(gpointer object);
SCM gig_object_take(gpointer object);
SCM gig_object_transfer(gpointer object, GITransfer transfer);
GObject *gig_object_peek(SCM object);

void gig_init_object();

#endif
