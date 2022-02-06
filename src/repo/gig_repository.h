#ifndef GIG_REPOSITORY_H
#define GIG_REPOSITORY_H

#include <girepository.h>
#include "core.h"

typedef GIBaseInfo *(*GigRepositoryNested)(GIBaseInfo *info, int n);

/**
 * gig_repository_nested_infos:
 * @base: the info to look up nested infos in
 * @n_methods: (out): the number of methods in base
 * @n_properties: (out): the number of properties in base
 * @n_signals: (out): the number of signals in base
 * @method: (out) (nullable): function by which methods are retrieved
 * @property: (out) (nullable): function by which properties are retrieved
 * @signal: (out) (nullable): function by which signals are retrieved
 * Fetches all information of nested infos in BASE.
 */
void gig_repository_nested_infos(GIBaseInfo *base,
                                 int *n_methods,
                                 GigRepositoryNested *method,
                                 int *n_properties,
                                 GigRepositoryNested *property,
                                 int *n_signals, GigRepositoryNested *signal);


GIG_API void gig_init_repository(void);

#endif
