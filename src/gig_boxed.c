#include "gig_boxed.h"
#include "gir_type.h"

static SCM make_boxed = SCM_UNDEFINED;
static SCM sym_value = SCM_UNDEFINED;

static SCM
gig_boxed_take(SCM type, gpointer object, scm_t_pointer_finalizer fin)
{
    if (SCM_UNBNDP(make_boxed))
        make_boxed = scm_c_private_ref("gi oop", "%make-boxed");

    return scm_call_2(make_boxed, type, scm_from_pointer(object, fin));
}

gpointer
gig_boxed_peek(SCM boxed)
{
    if (SCM_UNBNDP(sym_value))
        sym_value = scm_from_utf8_symbol("value");

    g_return_val_if_fail(SCM_IS_A_P(boxed, gig_boxed_type), NULL);
    return scm_to_pointer(scm_slot_ref(boxed, sym_value));
}

SCM
gig_boxed_transfer(GType type, gpointer object, GITransfer transfer)
{
    g_return_val_if_fail(G_TYPE_IS_BOXED(type), SCM_UNDEFINED);
    SCM scheme_type = gir_type_get_scheme_type(type);
    g_return_val_if_fail(SCM_SUBCLASSP(scheme_type, gig_boxed_type), SCM_UNDEFINED);

    switch (transfer)
    {
    default:
        return gig_boxed_take(scheme_type, object, NULL);
    }
}
