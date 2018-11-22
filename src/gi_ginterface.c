#include "gi_ginterface.h"
#include "gi_gtype.h"

GQuark gi_ginterface_type_key;
GQuark gi_ginterface_info_key;

SCM ginterface_wrapper_hash;


/* Registers a foreign object as the wrapper for a GInterface.
 * Also, stores it in a lookup hash table. */
/* re pyginterface.c:66, pyg_register_interface */
SCM
gi_register_ginterface (const gchar *class_name,
			GType gtype)
{
  SCM o;
  gpointer ptr;

  if (gtype) {
    o = gi_gtype_c2g(gtype);
    ptr = g_type_get_qdata (gtype, gi_ginterface_type_key (gtype));
    
    scm_hash_set_x (gi_gobject_get_tp_dict (type),
		    scm_from_utf8_string ("__gtype__"),
		    o);
  }
  g_type_set_qdata (gtype, gi_ginterface_type_key, type);
  scm_hash_set_x (ginteface_wrapper_hash,
		  scm_from_utf8_string (class_name),
		  type);
}

void gi_register_ginterface_info (GType gtype,
				  const GInterfaceInfo *info)
{
  g_type_set_qdata (gtype, gi_ginterface_info_key, (gpointer) info);
}

const GInterfaceInfo *gi_lookup_ginterface_info (GType gtype)
{
  return g_type_get_qdata(gtype, gi_ginterface_info_key);
}

void
gi_init_ginterface (void)
{
  gi_ginterface_type_key = g_quark_from_static_string("GuGInterface::type");
  gi_ginterface_info_key = g_quark_from_static_string("GuGInterface::info");
  ginterface_wrapper_hash = scm_c_make_hash_table (10);
}
   
