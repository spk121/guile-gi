#ifndef _GI_BASICTYPE_H_
#define _GI_BASICTYPE_H_
#include <girepository.h>
SCM
gi_marshal_to_scm_basic_type (GIArgument  *arg,
                               GITypeTag type_tag);

gboolean
gi_marshal_from_scm_basic_type (SCM object, /* in */
                                 GIArgument *arg,      /* out */
                                 GITypeTag   type_tag,
				gboolean is_ptr,
                                 GITransfer  transfer,
				gpointer   *cleanup_data /* out */);

#endif
