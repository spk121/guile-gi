#ifndef _GIG_OBJECT_H_
#define _GIG_OBJECT_H_

#include <libguile.h>
#include <girepository.h>
#include "core.h"
#include "gig_types.h"

GIG_API SCM gig_property_define(gtype_t type, GIPropertyInfo *info, const char *_namespace, SCM defs);

GIG_API void gig_init_object(void);

#endif
