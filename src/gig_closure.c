#include "gig_closure.h"
#include "gig_value.h"
#include "gig_type.h"
#include "gig_util.h"

typedef struct _GigClosure GigClosure;

struct _GigClosure
{
    GClosure closure;
    SCM callback;
    SCM inout_mask;
    // potential flags if we want to use marshal_data for various purposes
    // (e.g. storing signal info)
    guint16 reserved;
};

static void
_gig_closure_invalidate(gpointer data, GClosure *closure)
{
    GigClosure *pc = (GigClosure *)closure;
    SCM old_callback = pc->callback;
    pc->callback = SCM_BOOL_F;
    scm_gc_unprotect_object(old_callback);
    if (!SCM_UNBNDP(pc->inout_mask)) {
        SCM inout_mask = pc->inout_mask;
        pc->inout_mask = SCM_UNDEFINED;
        scm_gc_unprotect_object(inout_mask);
    }
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

    if (G_IS_VALUE(ret) && gig_value_from_scm(ret, scm_c_value_ref(_ret, 0)) != 0) {
        GType ret_type = G_VALUE_TYPE(ret);

        if (ret_type == G_TYPE_INVALID)
            scm_misc_error(NULL, "failed to convert return value to invalid type", SCM_EOL);
        else {
            const gchar *type_name = g_type_name(ret_type);
            if (type_name)
                scm_misc_error(NULL, "failed to convert value to ~S",
                               scm_list_1(scm_from_utf8_string(type_name)));
            else
                scm_misc_error(NULL, "failed to convert return value to bizarre unnamed type",
                               SCM_EOL);
        }
    }
    if (!SCM_UNBNDP(pc->inout_mask)) {
        gsize idx = 1, nvalues = scm_c_nvalues(_ret) - 1, offset, length;
        gssize pos = 0, inc;
        scm_t_array_handle handle;
        const guint32 *bits;
        int err;

        if (nvalues > n_params)
            scm_misc_error(NULL, "~S returned more values than we can unpack",
                           scm_list_1(pc->callback));

        gsize bit_count = scm_to_size_t(scm_bit_count(SCM_BOOL_T, pc->inout_mask));
        if (bit_count < nvalues)
            scm_misc_error(NULL, "~S returned more values than we should unpack",
                           scm_list_1(pc->callback));
        if (bit_count > nvalues)
            scm_misc_error(NULL, "~S returned less values than we should unpack",
                           scm_list_1(pc->callback));


        bits = scm_bitvector_elements(pc->inout_mask, &handle, &offset, &length, &inc);
        pos = offset;

        for (guint i = 0; i <= n_params; i++, pos += inc) {
            gsize word_pos = pos / 32;
            gsize mask = 1L << (pos % 32);

            if (bits[word_pos] & mask)
                g_warn_if_fail(!gig_value_from_scm((GValue*)(params + i),
                                                   scm_c_value_ref(_ret, idx++)));
        }
        scm_array_handle_release(&handle);
    }
}

GClosure *
gig_closure_new(SCM callback, SCM inout_mask)
{
    GClosure *closure = g_closure_new_simple(sizeof(GigClosure), NULL);
    GigClosure *gig_closure = (GigClosure *)closure;
    g_closure_add_invalidate_notifier(closure, NULL, _gig_closure_invalidate);
    g_closure_set_marshal(closure, _gig_closure_marshal);
    // FIXME: what about garbage collection?
    gig_closure->callback = scm_gc_protect_object(callback);
    if (SCM_UNBNDP(inout_mask))
        gig_closure->inout_mask = SCM_UNDEFINED;
    else
        gig_closure->inout_mask = scm_gc_protect_object(inout_mask);
    return closure;
}

static SCM
invoke_closure(SCM closure, SCM return_type, SCM args)
{
    SCM_ASSERT_TYPE(SCM_IS_A_P(closure, gig_closure_type), closure, SCM_ARG1, "%invoke-closure",
                    "closure");
    GClosure *real_closure = gig_type_peek_typed_object(closure, gig_closure_type);
    SCM_ASSERT_TYPE(scm_is_list(args), args, SCM_ARG2, "%invoke-closure", "list");

    gsize nargs = scm_c_length(args);
    GValue *params = g_new0(GValue, nargs);
    GValue *retval = g_new0(GValue, 1);
    SCM ret = SCM_UNDEFINED;
    SCM iter = args;

    g_value_init(retval, scm_to_gtype(return_type));
    if (G_VALUE_TYPE(retval) == G_TYPE_INVALID) {
        g_free(retval);
        goto out;
    }

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
procedure_to_closure(SCM procedure, SCM inout_mask)
{
    SCM_ASSERT_TYPE(scm_is_true(scm_procedure_p(procedure)), procedure, SCM_ARG1,
                    "procedure->closure", "procedure");
    SCM_ASSERT_TYPE(SCM_UNBNDP(inout_mask) ||
                    scm_is_true(scm_bitvector_p(inout_mask)), procedure, SCM_ARG2,
                    "procedure->closure", "bitvector");
    GClosure *cls = gig_closure_new(procedure, inout_mask);
    g_closure_ref(cls);
    g_closure_sink(cls);
    return gig_type_transfer_object(G_TYPE_CLOSURE, cls, GI_TRANSFER_EVERYTHING);
}

void
gig_init_closure()
{
    scm_c_define_gsubr("procedure->closure", 1, 1, 0, procedure_to_closure);
    scm_c_define_gsubr("%invoke-closure", 3, 0, 0, invoke_closure);
}
