#ifndef _GI_GIARGUMENT_H_
#define _GI_GIARGUMENT_H_

#include <girepository.h>
#include <libguile.h>
typedef gssize (*GuGIArgArrayLengthPolicy) (gsize item_index,
					    void *user_data1,
					    void *user_data2);

gboolean
gi_giargument_to_gssize (const char *func,
			 GIArgument *arg_in,
                         GITypeTag  type_tag,
                         gssize *gssize_out);
gssize
gi_argument_array_length_marshal (gsize length_arg_index,
				  void *user_data1,
				  void *user_data2);
GArray *
gi_giargument_to_array (GIArgument  *arg,
		      GuGIArgArrayLengthPolicy array_length_policy,
		      void        *user_data1,
		      void        *user_data2,
		      GITypeInfo  *type_info,
			gboolean    *out_free_array);

GArray* _pygi_argument_to_array (GIArgument  *arg,
                                 GuGIArgArrayLengthPolicy array_length_policy,
                                 void        *user_data1,
                                 void        *user_data2,
                                 GITypeInfo  *type_info,
                                 gboolean    *out_free_array);

SCM
gi_giargument_to_object (GIArgument  *arg,
			 GITypeInfo *type_info,
			 GITransfer transfer);
void
gi_giargument_release (GIArgument   *arg,
                        GITypeInfo  *type_info,
                        GITransfer   transfer,
		       GIDirection  direction);
#endif
