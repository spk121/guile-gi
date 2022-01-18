#ifndef Y_REPOSITORY_H
#define Y_REPOSITORY_H

#include <girepository.h>
#include "x.h"

typedef GIBaseInfo *(*Repository_nested)(GIBaseInfo *info, int n);

int has_loaded_gobject(void);

void nested_infos(GIBaseInfo *base,
                                 int *n_methods,
                  Repository_nested *method,
                                 int *n_properties,
                                 Repository_nested *property,
                  int *n_signals, Repository_nested *sign);

GIG_API void init_repository(void);
#endif
