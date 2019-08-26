#include "gig_closure.h"
#include "gig_value.h"
#include "gig_type.h"
#include "gig_util.h"

typedef struct _GigClosure GigClosure;

struct _GigClosure
{
    GClosure closure;
    SCM callback;
    // potential flags if we want to use marshal_data for various purposes
    // (e.g. storing signal info)
    guint16 reserved;
};

static void
_gig_closure_invalidate(gpointer data, GClosure *closure)
{
    GigClosure *pc = (GigClosure *)closure;
    pc->callback = SCM_BOOL_F;
}

static void
_gig_closure_marshal(GClosure *closure, GValue *ret, guint n_params, const GValue *params,
                     gpointer hint, gpointer marshal_data)
{
    GigClosure *pc = (GigClosure *)closure;
    SCM args = scm_make_list(scm_from_uint(n_params), SCM_UNDEFINED);

    SCM iter = args;
    for (guint i = 0; i < n_params; i++, iter = scm_cdr(iter))
        scm_set_car_x(iter, gig_value_as_scm(params + i, FALSE));
    SCM _ret = scm_apply_0(pc->callback, args);

    if (G_IS_VALUE(ret) && gig_value_from_scm(ret, _ret) != 0) {
        GType ret_type = G_VALUE_TYPE(ret);

        if (ret_type == G_TYPE_INVALID)
            g_warning("failed to convert return value to invalid type");
        else {
            const gchar *type_name = g_type_name(ret_type);
            g_warning("failed to convert return value to %s",
                      type_name ? type_name : "bizarre unnamed type");
        }
    }
}

GClosure *
gig_closure_new(SCM callback)
{
    GClosure *closure = g_closure_new_simple(sizeof(GigClosure), NULL);
    GigClosure *gig_closure = (GigClosure *)closure;
    g_closure_add_invalidate_notifier(closure, NULL, _gig_closure_invalidate);
    g_closure_set_marshal(closure, _gig_closure_marshal);
    // FIXME: what about garbage collection?
    gig_closure->callback = callback;
    return closure;
}

static SCM
invoke_closure(SCM closure, SCM return_type, SCM args)
{
    SCM_ASSERT_TYPE(SCM_IS_A_P(closure, gig_closure_type), closure, SCM_ARG1, "%invoke-closure",
                    "closure");
    GClosure *real_closure = gig_type_peek_typed_object(closure, gig_closure_type);
    SCM_ASSERT_TYPE(scm_is_list(args), args, SCM_ARG2, "%invoke-closure", "list");

    gsize nargs = scm_to_size_t(scm_length(args));
    GValue *params = g_new0(GValue, nargs);
    GValue *retval = g_new0(GValue, 1);
    g_value_init(retval, scm_to_gtype(return_type));
    if (G_VALUE_TYPE(retval) == G_TYPE_INVALID) {
        g_free(retval);
        goto out;
    }

    SCM ret = SCM_UNDEFINED;

    SCM iter = args;
    for (gsize narg = 0; narg < nargs; narg++, iter = scm_cdr(iter)) {
        const GValue *arg = gig_type_peek_typed_object(scm_car(iter), gig_value_type);
        if (arg == NULL)
            goto out;
        g_value_init(params + narg, G_VALUE_TYPE(arg));
        g_value_copy(arg, params + narg);
    }

    g_closure_invoke(real_closure, retval, nargs, params, NULL);
    ret = gig_type_transfer_object(G_TYPE_VALUE, retval, GI_TRANSFER_EVERYTHING);

  out:
    for (gsize narg = 0; narg < nargs; narg++)
        g_value_unset(params + narg);
    g_free(params);
    return ret;
}

static SCM
procedure_to_closure(SCM procedure)
{
    SCM_ASSERT_TYPE(scm_is_true(scm_procedure_p(procedure)), procedure, SCM_ARG1,
                    "procedure->closure", "procedure");
    GClosure *cls = gig_closure_new(procedure);
    g_closure_ref(cls);
    g_closure_sink(cls);
    return gig_type_transfer_object(G_TYPE_CLOSURE, cls, GI_TRANSFER_EVERYTHING);
}

void
gig_init_closure()
{
    scm_c_define_gsubr("procedure->closure", 1, 0, 0, procedure_to_closure);
    scm_c_define_gsubr("%invoke-closure", 3, 0, 0, invoke_closure);
}
