#ifndef _GIG_BOXED_H_
#define _GIG_BOXED_H_

#include <libguile.h>
#include <glib.h>
#include <glib-object.h>
#include <girepository.h>

extern SCM gig_boxed_type;
gpointer gig_boxed_peek(SCM boxed);
SCM gig_boxed_transfer(GType type, gpointer object, GITransfer transfer);

#endif
