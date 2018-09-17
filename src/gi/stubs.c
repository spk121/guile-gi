#include <libguile.h>
#include <glib.h>
#include <glib-object.h>
#include "pycompat.h"
#include "stubs.h"
#include "gir_gobject.h"
#include "gir_g_type.h"
#include "gir_g_value.h"

SCM GuGInterface_Type = SCM_BOOL_F; // a Type
SCM GuType_Type = SCM_BOOL_F; // a type


/*
 * pyg_enum_add
 * Dynamically create a class derived from PyGEnum based on the given GType.
 */
SCM
gug_enum_add (SCM module,
	      const char * typename,
	      const char * strip_prefix,
	      GType        gtype)
{
    GuGILState_STATE state;
    SCM instance_dict, stub, values, o;
    GEnumClass *eclass;
    guint i;

    g_debug ("In stub version of gug_enum_add");
    return SCM_UNSPECIFIED;
#if 0    
    g_return_val_if_fail(typename != NULL, NULL);
    if (!g_type_is_a (gtype, G_TYPE_ENUM)) {
        PyErr_Format (PyExc_TypeError, "Trying to register gtype '%s' as enum when in fact it is of type '%s'",
                      g_type_name (gtype), g_type_name (G_TYPE_FUNDAMENTAL (gtype)));
        return NULL;
    }

    state = PyGILState_Ensure();

    /* Create a new type derived from GEnum. This is the same as:
     * >>> stub = type(typename, (GEnum,), {})
     */
    instance_dict = PyDict_New();
    stub = PyObject_CallFunction((PyObject *)&PyType_Type, "s(O)O",
                                 typename, (PyObject *)&PyGEnum_Type,
                                 instance_dict);
    Py_DECREF(instance_dict);
    if (!stub) {
	PyErr_SetString(PyExc_RuntimeError, "can't create const");
	PyGILState_Release(state);
	return NULL;
    }

    ((PyTypeObject *)stub)->tp_flags &= ~Py_TPFLAGS_BASETYPE;

    if (module)
	PyDict_SetItemString(((PyTypeObject *)stub)->tp_dict,
			     "__module__",
			     PYGLIB_PyUnicode_FromString(PyModule_GetName(module)));

    g_type_set_qdata(gtype, pygenum_class_key, stub);

    o = pyg_type_wrapper_new(gtype);
    PyDict_SetItemString(((PyTypeObject *)stub)->tp_dict, "__gtype__", o);
    Py_DECREF(o);

    if (module) {
	/* Add it to the module name space */
	PyModule_AddObject(module, (char*)typename, stub);
	Py_INCREF(stub);
    }

    /* Register enum values */
    eclass = G_ENUM_CLASS(g_type_class_ref(gtype));

    values = PyDict_New();
    for (i = 0; i < eclass->n_values; i++) {
	PyObject *item, *intval;
      
        intval = PYGLIB_PyLong_FromLong(eclass->values[i].value);
	item = pyg_enum_val_new(stub, gtype, intval);
	PyDict_SetItem(values, intval, item);
        Py_DECREF(intval);

	if (module) {
	    char *prefix;

	    prefix = g_strdup(pyg_constant_strip_prefix(eclass->values[i].value_name, strip_prefix));
	    PyModule_AddObject(module, prefix, item);
	    g_free(prefix);

	    Py_INCREF(item);
	}
    }

    PyDict_SetItemString(((PyTypeObject *)stub)->tp_dict,
			 "__enum_values__", values);
    Py_DECREF(values);

    g_type_class_unref(eclass);

    PyGILState_Release(state);
    return stub;
#endif
}


SCM
gug_flags_add (SCM module,
	      const char * typename,
	      const char * strip_prefix,
	      GType        gtype)
{
    GuGILState_STATE state;
    SCM instance_dict, stub, values, o;
    GEnumClass *eclass;
    guint i;

    g_debug ("In stub version of gug_flags_add");
    return SCM_UNSPECIFIED;
}

SCM gug_value_as_scm(const GValue *value, gboolean copy_boxed)
{
  g_debug("In stub version of gug_value_as_scm");
  return SCM_UNSPECIFIED;
}

SCM GuObject_CallFunction(SCM callable, const char *format, ...)
{
    va_list va;
  va_start(va, format);
  va_end(va);
  g_debug("In the stub version of GuObject_CallFunction");
  return SCM_UNSPECIFIED;
}

int       gug_value_from_scm(GValue *value, SCM obj)
{
  g_debug("In the stub version of gug_value_from_scm");
  value = NULL;
  return FALSE;
}

SCM
gugi_call_do_get_property       (SCM instance,
                                 GParamSpec *pspec)
{
  g_debug("in the stub version of gugi_call_do_get_property");
  return SCM_UNSPECIFIED;
}

SCM gug_param_spec_new (GParamSpec *pspec)
{
  g_debug("in thestub version of gug_param_spec_new");
  return SCM_UNSPECIFIED;
}


const GInterfaceInfo * gug_lookup_interface_info(GType gtype)
{
  g_debug("In the stub version of gug_lookup_interface_info");
  return NULL;
}

const char *GUGLIB_GuUnicode_AsString(SCM val)
{
  g_debug("In the stub version of GUGLIB_GuUnicode_AsString");
  return NULL;
}

const char *gug_foreign_object_type_get_name (SCM x /* a type */)
{
  g_debug("In the stub version of gug_foreign_object_type_get_name");
  return NULL;
}

SCM gug_type_wrapper_new (GType type)
{
  g_debug("In the stub version of gug_type_wrapper_new");
  return SCM_UNSPECIFIED;
}

SCM gug_foreign_object_type_get_dict(SCM x /* a type */)
{
  g_debug("In the stub version of gug_foreign_object_type_get_dict");
  return SCM_UNSPECIFIED;
}

SCM gug_object_descr_doc_get(void)
{
  g_debug("In the stub version of gug_object_descr_doc_get");
  return SCM_UNSPECIFIED;
}

SCM gug_foreign_object_type_get_bases(SCM class)
{
  g_debug("In the stub version of gug_foreign_object_type_get_bases");
  return SCM_UNSPECIFIED;
}

SCM gug_foreign_object_type_get_base(SCM class)
{
  g_debug("In the stub version of gug_foreign_object_type_get_base");
  return SCM_UNSPECIFIED;
}

void gug_register_interface_info(GType gtype, const
				 GInterfaceInfo *info)
{
  g_debug("In the stub version of gug_register_interface_info");
  return;
}

int main(int argc, char **argv)
{
  scm_init_guile();
  // gui_object_register_types(SCM_BOOL_F);
  gir_init_g_type();
  gir_init_g_value();
  gir_init_gobject();
  scm_shell(argc, argv);
  return 0;
}
