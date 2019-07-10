#include "gig_paramspec.h"
#include "gir_type.h"

static SCM make_paramspec_proc = SCM_UNDEFINED;
static SCM sym_pspec = SCM_UNDEFINED;

SCM
gig_paramspec_take(GParamSpec *spec)
{
    if (SCM_UNBNDP(make_paramspec_proc))
        make_paramspec_proc = scm_c_private_ref("gi oop", "%make-paramspec");

    SCM scm_type = gir_type_get_scheme_type(G_PARAM_SPEC_TYPE(spec));
    if (scm_is_false(scm_type)) {
        SCM missing = scm_from_utf8_string(g_type_name(G_PARAM_SPEC_TYPE(spec)));
        scm_misc_error("%gig-paramspec-take",
                       "type ~S is not initialized, did you forget to load GObject?",
                       scm_list_1(missing));
    }
    g_return_val_if_fail(SCM_SUBCLASSP(scm_type, gig_paramspec_type), SCM_UNDEFINED);
    return scm_call_2(make_paramspec_proc,
                      scm_type,
                      scm_from_pointer(spec, (scm_t_pointer_finalizer)g_param_spec_unref));
}

SCM
gig_paramspec_ref(GParamSpec *spec)
{
    return gig_paramspec_take(g_param_spec_ref_sink(spec));
}

SCM
gig_paramspec_transfer(GParamSpec *spec, GITransfer transfer)
{
    switch (transfer) {
    default:
        return gig_paramspec_ref(spec);
    }
}

GParamSpec *
gig_paramspec_peek(SCM pspec)
{
    if (SCM_UNBNDP(sym_pspec))
        sym_pspec = scm_from_utf8_symbol("pspec");

    g_return_val_if_fail(SCM_IS_A_P(pspec, gig_paramspec_type), NULL);
    return scm_to_pointer(scm_slot_ref(pspec, sym_pspec));
}
