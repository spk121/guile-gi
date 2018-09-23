#ifndef _GIR_GTYPE_TYPE_H_
#define _GIR_GTYPE_TYPE_H

#include <libguile.h>
#include <glib.h>
#include <glib-object.h>
#include <girepository.h>

extern SCM GuGType_Type;
extern SCM GuGTypeWrapper_Type;

// This bit does nothing.
#define GU_TPFLAGS_DEFAULT 1
// If this bit is present, then subclassing this type is allowed
#define GU_TPFLAGS_BASETYPE 2
// This bit is set when the type object is allocated on the heap.
#define GU_TPFLAGS_HEAPTYPE 4
// This bit is set when the type is using 
#define GU_TPFLAGS_HAVE_GC 8

SCM GuGType_import_by_name (const char *namespace_, const char *name);
SCM GuGType_import_by_GIBaseInfo (GIBaseInfo *info);
SCM GuGType_import_by_GType (GType g_type);
GType GType_get_type (SCM gtype);
unsigned GType_get_tp_flags (SCM gtype);
GType gu_type_from_object(SCM obj);
SCM GuGType_lookup_by_GType(GType gtype);
GType GuGType_get_type (SCM gtype);



void gir_init_g_type(void);
#endif
