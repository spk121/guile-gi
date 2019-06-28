#ifndef _GI_GPARAMSPEC_H_
#define _GI_GPARAMSPEC_H_

#include "__gi_gparamspec.h"
GParamSpec *gi_gparamspec_from_scm(SCM x);

void gi_paramspec_finalizer(SCM self);
void gi_init_gparamspec(void);

#endif
