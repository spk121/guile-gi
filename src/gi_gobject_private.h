#ifndef _GI_GOBJECT_PRIVATE_H_
#define _GI_GOBJECT_PRIVATE_H_
#include <libguile.h>
#include <glib.h>
#include <glib-object.h>

GParamSpec *gi_gparamspec_from_scm(SCM x);

typedef struct _SignalSpec
{
    const gchar *signal_name;
    GSignalFlags signal_flags;
    GSignalAccumulator accumulator;
    gpointer accu_data;
    GType return_type;
    guint n_params;
    GType *param_types;
} SignalSpec;

SignalSpec *gi_signalspec_from_obj(SCM obj);
void gi_free_signalspec(SignalSpec *spec);

#endif
