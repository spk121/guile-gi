/* -*- Mode: C; c-basic-offset: 4 -*- */
#include <libguile.h>
#include <glib.h>
#include <glib-object.h>

#include "gir_ginterface.h"

GQuark guginterface_type_key;
GQuark guginterface_info_key;

static void GuGInterface_finalize(SCM x);

////////////////////////////////////////////////////////////////
// GuGInterface Type: A foreign object type that in an envelope for a
SCM GuGInterface_Type;
SCM GuGInterface_Type_Store;

// tp_name, tp_dict
/* GuGObject Instance: A foreign object with the following slots
   - slot 0: 'ob_type', the SCM foreign-object type used to create this instance
   - slot 1: 'ob_refcnt', an SCM exact integer holding a C ssize_t
   - slot 2: 'obj', an SCM pointer object holding a C GObject* pointer
   - slot 3: 'inst_dict', an SCM hash-table
   - slot 4: 'weakreflist', an SCM weak-vector
   - slot 5: flags, an SCM exact-integer of GuGObjectFlags
*/
#define MAKE_GUGINTERFACE_TYPE						\
    do {								\
	GuGInterface_Type =						\
	    scm_make_foreign_object_type(scm_from_latin1_symbol("<GInterface>"), \
			scm_list_n (scm_from_latin1_symbol ("ob_type"), \
			    scm_from_latin1_symbol ("ob_refcnt"), \
				scm_from_latin1_symbol ("obj"), \
				scm_from_latin1_symbol ("inst_dict"), \
				scm_from_latin1_symbol ("weakreflist"), \
				scm_from_latin1_symbol ("flags"), \
				SCM_UNDEFINED),	\
			GuGInterface_finalize);		\
    } while(FALSE)

#define GUGINTERFACE_OB_TYPE_SLOT 0
#define GUGINTERFACE_OB_REFCNT_SLOT 1
#define GUGINTERFACE_OBJ_SLOT 2
#define GUGINTERFACE_INST_DICT_SLOT 3
#define GUGINTERFACE_WEAKREFLIST_SLOT 4
#define GUGINTERFACE_FLAGS_SLOT 5
#define GUGINTERFACE_N_SLOTS 6

static void
GuGInterface_finalize(SCM x)
{
}

void
gir_init_ginterface(void)
{
    guginterface_type_key = g_quark_from_static_string("GuGInterface::type");
    guginterface_info_key = g_quark_from_static_string("GuGInterface::info");

    MAKE_GUGINTERFACE_TYPE;
    GuGInterface_Type_Store = scm_c_define("<GInterface>", GuGInterface_Type);
}
