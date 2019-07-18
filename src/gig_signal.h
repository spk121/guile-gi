#ifndef GIG_SIGNAL_H
#define GIG_SIGNAL_H
#include <girepository.h>
#include <libguile.h>

// *INDENT-OFF*
G_BEGIN_DECLS
// *INDENT-ON*

typedef struct _GigSignalSpec
{
    const gchar *signal_name;
    GSignalFlags signal_flags;
    GSignalAccumulator accumulator;
    gpointer accu_data;
    GType return_type;
    guint n_params;
    GType *param_types;
} GigSignalSpec;

SCM gig_signal_type;

GigSignalSpec *gig_signalspec_from_obj(SCM obj);
void gig_free_signalspec(GigSignalSpec *spec);

typedef enum
{
    SIGNAL_SLOT_NAME,
    SIGNAL_SLOT_FLAGS,
    SIGNAL_SLOT_ACCUMULATOR,
    SIGNAL_SLOT_RETURN_TYPE,
    SIGNAL_SLOT_PARAM_TYPES,
    SIGNAL_SLOT_COUNT
} GigSignalSlot;

SCM gig_signal_ref(SCM signal, GigSignalSlot slot);
SCM gig_make_signal(gsize n_slots, GigSignalSlot *slots, SCM *slot_values);

GClosure *gig_signal_closure_new(SCM instance, GType g_type, const gchar *signal_name,
                                 SCM callback);

void gig_init_signal();

G_END_DECLS
#endif
