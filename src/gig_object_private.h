#ifndef _GIG_OBJECT_PRIVATE_H_
#define _GIG_OBJECT_PRIVATE_H_
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

SCM guile_signal;

SCM private_make_gobject_proc;

SignalSpec *gig_signalspec_from_obj(SCM obj);
void gig_free_signalspec(SignalSpec *spec);
void init_gi_oop();

typedef enum
{
    OBJECT_SLOT_OBJECT,
    OBJECT_SLOT_COUNT
} ObjectSlot;

SCM object_ref(SCM object, ObjectSlot slot);

typedef enum
{
    SIGNAL_SLOT_NAME,
    SIGNAL_SLOT_FLAGS,
    SIGNAL_SLOT_ACCUMULATOR,
    SIGNAL_SLOT_RETURN_TYPE,
    SIGNAL_SLOT_PARAM_TYPES,
    SIGNAL_SLOT_COUNT
} SignalSlot;

SCM signal_ref(SCM signal, SignalSlot slot);

void gig_init_object_private(void);


#endif
