#ifndef _GIG_OBJECT_H_
#define _GIG_OBJECT_H_

#include "core.h"
#include <libguile.h>
#include <glib.h>
#include <glib-object.h>
#include <girepository.h>

GIG_API void gig_init_object(void);
SCM gig_property_define(GType type, GIPropertyInfo *info, const char *_namespace, SCM defs);

#endif
