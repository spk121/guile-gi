#ifndef _GI_GIARGUMENT_H_
#define _GI_GIARGUMENT_H_

typedef gssize (*GuGIArgArrayLengthPolicy) (gsize item_index,
					    void *user_data1,
					    void *user_data2);


GArray* _pygi_argument_to_array (GIArgument  *arg,
                                 GuGIArgArrayLengthPolicy array_length_policy,
                                 void        *user_data1,
                                 void        *user_data2,
                                 GITypeInfo  *type_info,
                                 gboolean    *out_free_array);


#endif
