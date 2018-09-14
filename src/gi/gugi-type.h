/* -*- Mode: C; c-basic-offset: 4 -*-
 * pygtk- Python bindings for the GTK toolkit.
 * Copyright (C) 1998-2003  James Henstridge
 *               2004-2008  Johan Dahlin
 *   pyginterface.c: wrapper for the gobject library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */

#ifndef __GUGI_TYPE_H__ 
#define __GIGI_TYPE_H__

#include <libguile.h>
#include <glib-object.h>
#include <girepository.h>
#include "gugobject-internal.h"

#define GUGOBJECT_REGISTER_GTYPE(d, type, name, gtype)      \
  {                                                         \
    SCM o;					    \
    GUGLIB_REGISTER_TYPE(d, type, name);                    \
    GuDict_SetItemString(type.tp_dict, "__gtype__",         \
			 o=gug_type_wrapper_new(gtype));    \
    Gu_DECREF(o);                                           \
}

// extern GuTypeObject GuGTypeWrapper_Type;
extern SCM GuGTypeWrapper_Type;

typedef SCM (* fromvaluefunc)(const GValue *value);
typedef int (*tovaluefunc)(GValue *value, SCM obj);

typedef struct {
    fromvaluefunc fromvalue;
    tovaluefunc tovalue;
} GugTypeMarshal;

GugTypeMarshal *gug_type_lookup(GType type);

gboolean gug_gtype_is_custom (GType gtype);

void gug_register_gtype_custom(GType gtype,
                               fromvaluefunc from_func,
                               tovaluefunc to_func);

int gugi_type_register_types(PyObject *d);

SCM gug_object_descr_doc_get(void);
SCM gug_type_wrapper_new (GType type);
GType     gug_type_from_object_strict (PyObject *obj, gboolean strict);
GType     gug_type_from_object (SCM obj);

int gug_pyobj_to_unichar_conv (SCM py_obj, void* ptr);

GClosure *gug_closure_new(PyObject *callback, SCM extra_args, SCM swap_data);
GClosure *gug_signal_class_closure_get(void);
void      gug_closure_set_exception_handler(GClosure *closure,
                                            GuClosureExceptionHandler handler);

SCM gugi_type_import_by_g_type (GType g_type);
SCM gugi_type_import_by_name (const char *namespace_, const char *name);
SCM gugi_type_import_by_gi_info (GIBaseInfo *info);
SCM gugi_type_get_from_g_type (GType g_type);

#endif /* __GUGI_TYPE_H__ */

