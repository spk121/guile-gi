#ifndef _GI_GTYPE_H_
#define _GI_GTYPE_H_

#include <libguile.h>
#include <glib-object.h>
#include "__gi_gtype.h"

G_BEGIN_DECLS

GType     gi_gtype_from_scm (SCM obj);
GType     gi_infer_gtype_from_scm(SCM obj);
SCM       gi_gtype_c2g (GType type);

void      gi_init_gtype (void);

G_END_DECLS

#endif
