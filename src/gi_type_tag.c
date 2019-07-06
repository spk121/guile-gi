#include "gi_type_tag.h"

gboolean gi_type_tag_is_integer(GITypeTag x)
{
    if ((x == GI_TYPE_TAG_INT8)
        || (x == GI_TYPE_TAG_UINT8)
        || (x == GI_TYPE_TAG_INT16)
        || (x == GI_TYPE_TAG_UINT16)
        || (x == GI_TYPE_TAG_INT32)
        || (x == GI_TYPE_TAG_UINT32)
        || (x == GI_TYPE_TAG_INT64)
        || (x == GI_TYPE_TAG_UINT64))
        return TRUE;
    return FALSE;
}

gboolean gi_type_tag_is_integer_least32(GITypeTag x)
{
    if ((x == GI_TYPE_TAG_INT32)
        || (x == GI_TYPE_TAG_UINT32)
        || (x == GI_TYPE_TAG_INT64)
        || (x == GI_TYPE_TAG_UINT64))
        return TRUE;
    return FALSE;
}

gboolean gi_type_tag_is_signed_integer(GITypeTag x)
{
    if ((x == GI_TYPE_TAG_INT8)
        || (x == GI_TYPE_TAG_INT16)
        || (x == GI_TYPE_TAG_INT32)
        || (x == GI_TYPE_TAG_INT64))
        return TRUE;
    return FALSE;
}

gboolean gi_type_tag_is_real_number(GITypeTag x)
{
    if ((x == GI_TYPE_TAG_FLOAT) || (x == GI_TYPE_TAG_DOUBLE))
        return TRUE;
    return FALSE;
}

gboolean gi_type_tag_is_number(GITypeTag x)
{
    return gi_type_tag_is_real_number(x) || gi_type_tag_is_integer(x);
}

gboolean gi_type_tag_is_string(GITypeTag x)
{
    if ((x == GI_TYPE_TAG_UTF8)
        || (x == GI_TYPE_TAG_FILENAME))
        return TRUE;
    return FALSE;
}

gsize gi_type_tag_item_size(GITypeTag x)
{
    gsize sz;
    switch(x) {
    case GI_TYPE_TAG_INT8:
    case GI_TYPE_TAG_UINT8:
        sz = 1;
        break;
    case GI_TYPE_TAG_INT16:
    case GI_TYPE_TAG_UINT16:
        sz = 2;
        break;
    case GI_TYPE_TAG_INT32:
    case GI_TYPE_TAG_UINT32:
    case GI_TYPE_TAG_UNICHAR:
        sz = 4;
        break;
    case GI_TYPE_TAG_INT64:
    case GI_TYPE_TAG_UINT64:
        sz = 8;
        break;
    case GI_TYPE_TAG_FLOAT:
        sz = sizeof(float);
        break;
    case GI_TYPE_TAG_DOUBLE:
        sz = sizeof(double);
        break;
    case GI_TYPE_TAG_GTYPE:
        sz = sizeof(GType);
        break;
    case GI_TYPE_TAG_BOOLEAN:
        sz = sizeof(gboolean);
        break;
   default:
        sz = -1;
        break;
    }
    return sz;
}
