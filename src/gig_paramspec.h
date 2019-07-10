#ifndef _GIG_PARAMSPEC_H_
#define _GIG_PARAMSPEC_H_

#include <libguile.h>
#include <glib.h>
#include <glib-object.h>
#include <girepository.h>

extern SCM gig_paramspec_type;

SCM gig_paramspec_take(GParamSpec *spec);
SCM gig_paramspec_ref(GParamSpec *spec);
SCM gig_paramspec_transfer(GParamSpec *spec, GITransfer transfer);
GParamSpec *gig_paramspec_peek(SCM spec);

#endif
