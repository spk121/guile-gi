#ifndef _GI_GFLAGS_H_
#define _GI_GFLAGS_H_

#include <libguile.h>
#include <glib.h>
#include <glib-object.h>
#include "__gi_gflags.h"
#include "__gi_gflagscollection.h"

extern GQuark gugflags_class_key;

gboolean gi_gflags_check (SCM x);

SCM gi_gflags_add (const char *type_name,
		   const char *strip_prefix,
		   GType gtype);
SCM gi_gflags_from_gtype (GType gtype,
			  guint value);
int gi_flags_get_value(GType flags_type, SCM obj, guint *val);

void gi_init_gflags(void);
#endif
