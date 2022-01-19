#ifndef _GIG_OBJECT_H_
#define _GIG_OBJECT_H_

#include <libguile.h>
#include <girepository.h>
#include "clib.h"
#include "gig_types.h"

SCM gig_property_define(gtype_t type, GIPropertyInfo *info, const char *_namespace, SCM defs);

GIG_API void gig_init_object(void);

#endif
