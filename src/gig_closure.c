#include "gig_closure.h"
#include "gig_value.h"

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
    GigClosure *pc = (GigClosure *) closure;
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
            g_warning("failed to convert return value to %s", type_name ? type_name : "bizarre unnamed type");
        }
    }
}

GClosure *gig_closure_new(SCM callback)
{
    GClosure *closure = g_closure_new_simple(sizeof(GigClosure), NULL);
    GigClosure *gig_closure = (GigClosure *)closure;
    g_closure_add_invalidate_notifier(closure, NULL, _gig_closure_invalidate);
    g_closure_set_marshal(closure, _gig_closure_marshal);
    // FIXME: what about garbage collection?
    gig_closure->callback = callback;
    return closure;
}
