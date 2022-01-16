#ifndef CORE_REPOSITORY_H
#define CORE_REPOSITORY_H

#include <girepository.h>
#include "x.h"

typedef GIBaseInfo *(*Repository_nested)(GIBaseInfo *info, int n);

int has_loaded_gobject(void);
void init_repository(void);
#endif
