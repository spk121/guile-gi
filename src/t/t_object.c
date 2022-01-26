#include <libguile.h>
#include <girepository.h>
#include "clib.h"

static SCM _fundamental_type;

#if 0
// Given a numerical GType, which is a C GObject subclass of
// G_TYPE_OBJECT, this returns a C GObject instance. Since the type
// system is not booted yet, it is of the SCM <GFundamental> base
// class.

// Kinda useless, TBH.
static SCM
bootstrap_scm_make_gobject(SCM s_gtype, SCM s_prop_keylist)
{
#define FUNC "%make-gobject"
    GType type;
    GObject *obj;
    GObjectClass *_class;
    size_t n_prop;
    const char **keys;
    GValue *values;

    type = scm_to_size_t(s_gtype);

    SCM_ASSERT_TYPE(G_TYPE_IS_CLASSED(type), s_gtype, SCM_ARG1, FUNC,
                    "typeid derived from G_TYPE_OBJECT or scheme type derived from <GObject>");

    scm_dynwind_begin(0);

    if (scm_is_list(s_prop_keylist) && (scm_c_length(s_prop_keylist) != 0)) {
        _class = g_type_class_ref(type);
        scm_dynwind_unwind_handler(g_type_class_unref, _class, SCM_F_WIND_EXPLICITLY);

        n_prop = scm_c_length(s_prop_keylist) / 2;
        keys = scm_dynfree(xcalloc(n_prop, sizeof(char *)));
        values = scm_dynfree(xcalloc(n_prop, sizeof(GValue)));

        SCM iter = s_prop_keylist;
        for (size_t i = 0; i < n_prop; i++, iter = scm_cddr(iter)) {
            SCM key = scm_car(iter);
            SCM s_value = scm_cadr(iter);

            SCM_ASSERT_TYPE(scm_is_keyword(key), key, SCM_ARGn, FUNC, "keyword");

            key = scm_keyword_to_string(key);

            keys[i] = scm_dynfree(scm_to_utf8_string(key));
            GParamSpec *pspec = g_object_class_find_property(_class, keys[i]);
            if (!pspec) {
                scm_misc_error(FUNC, "unknown object parameter ~S", scm_list_1(key));
            }
            else {
                GValue *value = &values[i];
                g_value_init(value, G_PARAM_SPEC_VALUE_TYPE(pspec));
                if (bootstrap_value_from_scm(value, s_value))
                    scm_misc_error(FUNC, "unable to convert parameter ~S", scm_list_1(s_value));
            }
        }
    }
    else {
        n_prop = 0;
        keys = NULL;
        values = NULL;
    }

    obj = g_object_new_with_properties(type, n_prop, keys, values);
    scm_dynwind_end();

    if (G_IS_INITIALLY_UNOWNED(obj))
        g_object_ref_sink(obj);

    SCM pointer = scm_from_pointer(obj, g_object_unref);
    return scm_make_with_value(_fundamental_type, pointer);
#undef FUNC
}
#endif

static SCM
bootstrap_scm_emit(SCM self, SCM signal, SCM s_detail, SCM args)
{
    GObject *obj;
    GValue *values, retval = G_VALUE_INIT;
    GSignalQuery query_info;
    unsigned sigid;
    GQuark detail;
    SCM ret = SCM_EOL;

    // obj = gig_object_peek(self);
    obj = scm_to_pointer(scm_get_value_slot(self));

    signal_lookup("%emit", obj, signal, s_detail, &sigid, &query_info, &detail);

    if (SCM_UNBNDP(args))
        args = SCM_EOL;
    if (!(query_info.signal_flags & G_SIGNAL_DETAILED || SCM_UNBNDP(s_detail)))
        args = scm_cons(s_detail, args);

    if (scm_c_length(args) != query_info.n_params)
        scm_misc_error("%emit", "~A: signal ~A has ~d params, but ~d were supplied",
                       scm_list_4(self, signal, scm_from_uint32(query_info.n_params),
                                  scm_length(args)));

    values = xcalloc(query_info.n_params + 1, sizeof(GValue));
    g_value_init(values, G_OBJECT_TYPE(obj));
    bootstrap_value_from_scm_with_error(values, self, "%emit", SCM_ARG1);
    SCM iter = args;
    for (unsigned i = 0; i < query_info.n_params; i++, iter = scm_cdr(iter)) {
        g_value_init(values + i + 1, query_info.param_types[i]);
        bootstrap_value_from_scm_with_error(values + i + 1, scm_car(iter), "%emit", SCM_ARGn);
    }

    if (query_info.return_type != G_TYPE_NONE)
        g_value_init(&retval, query_info.return_type);
    debug_ffi("%s - emitting signal", g_signal_name(sigid));
    g_signal_emitv(values, sigid, detail, &retval);

    if (query_info.return_type != G_TYPE_NONE)
        ret = scm_cons(gig_value_as_scm(&retval, FALSE), ret);

    SCM output_mask = scm_slot_ref(signal, scm_from_utf8_symbol("output-mask");    
    if (scm_is_bitvector(output_mask)) {
        size_t offset, length;
        gssize pos = 0, inc;
        scm_t_array_handle handle;
        const uint32_t *bits;

        if (scm_c_bitvector_length(output_mask) != query_info.n_params + 1)
            scm_misc_error(NULL, "~S has an invalid bitmask", scm_list_1(signal));

        bits = scm_bitvector_elements(output_mask, &handle, &offset, &length, &inc);
        pos = offset;

        for (unsigned i = 0; i < query_info.n_params + 1; i++, pos += inc) {
            size_t word_pos = pos / 32;
            size_t mask = 1L << (pos % 32);

            if (bits[word_pos] & mask)
                ret = scm_cons(bootstrap_value_as_scm(values + i, FALSE), ret);
        }
        scm_array_handle_release(&handle);
        ret = scm_reverse_x(ret, SCM_EOL);
    }
    for (size_t narg = 0; narg < query_info.n_params + 1; narg++)
        g_value_unset(values + narg);
    free(values);
    if (scm_is_null(ret))
        return SCM_UNSPECIFIED;
    else
        return scm_values(ret);
}

void
bootstrap_init_object(void)
{
    _fundamental_type = scm_c_public_ref("(gi core fundamental)", "<GFundamental>");
    // scm_c_define_gsubr("%make-gobject", 1, 1, 0, bootstrap_scm_make_gobject);
    scm_c_define_gsubr("%emit", 2, 1, 1, bootstrap_scm_emit);
}
