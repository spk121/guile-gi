#ifndef _GI_GENUM_H_
#define _GI_GENUM_H_
#include "__gi_genum.h"

gboolean gi_genum_check (SCM x);

SCM gi_genum_from_gtype (GType gtype, int value);

SCM gi_genum_add (const char *typename,
		  const char *strip_prefix,
		  GType gtype);

void gi_init_genum(void);
#endif
