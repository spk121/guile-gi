#ifndef XGUILE_H
#define XGUILE_H

#include <libguile.h>
extern SCM SCM_NONE;

int scm_is_foreign_object_type (SCM type);
int scm_is_struct (SCM x);
int scm_is_gobject (SCM x);  
#endif
