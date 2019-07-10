#ifndef GI_TYPE_TAG
#define GI_TYPE_TAG
#include <girepository.h>
G_GNUC_PURE gboolean gi_type_tag_is_integer(GITypeTag x);
G_GNUC_PURE gboolean gi_type_tag_is_integer_least32(GITypeTag x);
G_GNUC_PURE gboolean gi_type_tag_is_signed_integer(GITypeTag x);
G_GNUC_PURE gboolean gi_type_tag_is_real_number(GITypeTag x);
G_GNUC_PURE gboolean gi_type_tag_is_number(GITypeTag x);
G_GNUC_PURE gboolean gi_type_tag_is_string(GITypeTag x);
G_GNUC_PURE gsize gi_type_tag_item_size(GITypeTag x);
#endif
