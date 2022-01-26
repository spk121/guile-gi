SCM (*gig_get_value)(SCM value) = gig_get_value_bootup;
SCM (*gig_set_value)(SCM value, SCM x) = gig_set_value_bootup;

SCM
gig_get_value_wrapper(SCM value)
{
    return gig_get_value(value);
}

SCM
gig_get_value_bootup(SCM svalue)
{
    GValue *value =  scm_to_pointer(scm_get_value_slot(svalue));
    SCM guobj;
    int_t handled;
    GType fundamental = G_TYPE_FUNDAMENTAL(G_VALUE_TYPE(value));

    if (fundamental == G_TYPE_CHAR)
        return SCM_MAKE_CHAR(g_value_get_schar(value));
    else if (fundamental == G_TYPE_UCHAR)
        return SCM_MAKE_CHAR(g_value_get_uchar(value));

    guobj = gig_value_to_scm_basic_type(value, fundamental, &handled);
    if (!handled)
        guobj = scm_from_pointer(g_value_get_pointer(value), NULL);
    return guobj;
}

SCM
gig_value_to_scm_basic_type(const GValue *value, GType fundamental, int *handled)
{
    *handled = TRUE;
    switch (fundamental) {
    case G_TYPE_CHAR:
        return scm_from_int8(g_value_get_schar(value));
    case G_TYPE_UCHAR:
        return scm_from_uint8(g_value_get_uchar(value));
    case G_TYPE_BOOLEAN:
        return scm_from_bool(g_value_get_boolean(value));
    case G_TYPE_INT:
        return scm_from_int(g_value_get_int(value));
    case G_TYPE_UINT:
        return scm_from_uint(g_value_get_uint(value));
    case G_TYPE_LONG:
        return scm_from_long(g_value_get_long(value));
    case G_TYPE_ULONG:
        return scm_from_ulong(g_value_get_ulong(value));
    case G_TYPE_INT64:
        return scm_from_int64(g_value_get_int64(value));
    case G_TYPE_UINT64:
        return scm_from_uint64(g_value_get_uint64(value));
    case G_TYPE_ENUM:
        return gig_int_to_enum(g_value_get_enum(value), G_VALUE_TYPE(value));
    case G_TYPE_FLAGS:
        return gig_uint_to_flags(g_value_get_flags(value), G_VALUE_TYPE(value));
    case G_TYPE_FLOAT:
        return scm_from_double(g_value_get_float(value));
    case G_TYPE_DOUBLE:
        return scm_from_double(g_value_get_double(value));
    case G_TYPE_STRING:
    {
        const char *str = g_value_get_string(value);
        if (str)
            return scm_from_utf8_string(str);
        else
            return SCM_BOOL_F;
    }
    default:
        *handled = FALSE;
        return SCM_BOOL_F;
    }
    return_val_if_reached(SCM_BOOL_F);
}

void init_t_values(void)
{
    static int first = 1;
    if (first == 1) {
        first = 0;
        scm_c_define_gsubr("%get-value", 1, 0, 0, gig_get_value_wrapper);
        scm_c_define_gsubr("%set-value!", 2, 0, 0, gig_set_value_wrapper);
        scm_c_define_gsubr("%set-type!", 2, 0, 0, gig_value_set_type_wrapper);
    }
}
