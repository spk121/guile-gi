/* -*- Mode: C; c-basic-offset: 4 -*- */
#include "gi_gsignal.h"
#include "gir_type.h"

SignalSpec *
gi_signalspec_from_obj(SCM obj)
{
    char *name;
    GType return_type;
    guint n_params;
    GType *params;
    SCM sparams;
    GSignalFlags flags;
    SignalSpec *spec = NULL;

    name = scm_to_utf8_string(scm_list_ref(obj, scm_from_int(0)));
    return_type = scm_to_gtype(scm_list_ref(obj, scm_from_int(1)));
    sparams = scm_list_ref(obj, scm_from_int(2));
    n_params = scm_to_uint(scm_length(sparams));
    params = g_new0(GType, n_params);

    for (guint i = 0; i < n_params; i++)
        params[i] = scm_to_size_t(scm_list_ref(sparams, scm_from_int(i)));
    flags = scm_to_uint(scm_list_ref(obj, scm_from_int(3)));

    spec = g_new0(SignalSpec, 1);
    spec->signal_name = name;
    spec->signal_flags = flags;
    spec->accumulator = NULL;
    spec->accu_data = NULL;
    spec->return_type = return_type;
    spec->n_params = n_params;
    spec->param_types = params;
    return spec;
}

void
gi_free_signalspec(SignalSpec *spec)
{
    if (spec) {
        if (spec->param_types) {
            g_free(spec->param_types);
            spec->param_types = NULL;
        }
        g_free(spec);
    }
}

void
gi_init_gsignal(void)
{

#define D(x) scm_permanent_object(scm_c_define(#x, scm_from_ulong(x)))
    D(G_SIGNAL_RUN_FIRST);
    D(G_SIGNAL_RUN_LAST);
    D(G_SIGNAL_RUN_CLEANUP);
    D(G_SIGNAL_NO_RECURSE);
    D(G_SIGNAL_DETAILED);
    D(G_SIGNAL_ACTION);
    D(G_SIGNAL_NO_HOOKS);
    D(G_SIGNAL_MUST_COLLECT);
    D(G_SIGNAL_DEPRECATED);
#undef D
    scm_c_export("G_SIGNAL_RUN_FIRST",
                 "G_SIGNAL_RUN_LAST",
                 "G_SIGNAL_RUN_CLEANUP",
                 "G_SIGNAL_NO_RECURSE",
                 "G_SIGNAL_DETAILED",
                 "G_SIGNAL_ACTION",
                 "G_SIGNAL_NO_HOOKS", "G_SIGNAL_MUST_COLLECT", "G_SIGNAL_DEPRECATED", NULL);
}
