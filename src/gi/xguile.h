#ifndef XGUILE_H
#define XGUILE_H

#include <libguile.h>

typedef SCM SCM_OBJECT;
typedef SCM SCM_TYPE_OBJECT;

int scm_is_foreign_object_type (SCM type);
int scm_is_struct (SCM x);
  
#endif
