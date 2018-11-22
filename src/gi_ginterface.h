#ifndef _GI_GINTERFACE_H_
#define _GI_GINTERFACE_H_

#include "__gi_ginterface.h"

extern GQuark gi_ginterface_type_key;
extern GQuark gi_ginterface_info_key;

void gi_register_ginterface (SCM dict,
			    const gchar *class_name,
			    GType gtype,
			    SCM type);
const GInterfaceInfo *gi_lookup_ginterface_info (GType gtype);
void gi_register_ginterface_info (GType gtypee,
				  const GInterfaceInfo *info);
int gi_ginterface_register_types(SCM d);


#endif
