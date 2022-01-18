#ifndef Y_OBJECT_H
#define Y_OBJECT_H

#include <libguile.h>
#include <girepository.h>
#include "x.h"

SCM define_property(GType type, GIPropertyInfo *info, const char *_namespace, SCM defs);

GIG_API void init_object(void);

#endif

