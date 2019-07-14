// Copyright (C), 2019 2018 Michael L. Gran

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
#include "gi_signal_closure.h"
#include "gig_object.h"
#include "gi_gvalue.h"
#include "gig_argument.h"

typedef void (*GigClosureExceptionHandler)(GValue *ret, guint n_param_values,
                                           const GValue *params);


typedef struct _GigClosure
{
    GClosure closure;
    SCM callback;
    SCM swap_data;              /* other object for gtk_signal_connect__object */
    GigClosureExceptionHandler exception_handler;
    GISignalInfo *signal_info;
} GigClosure;

static GISignalInfo *
lookup_signal_from_g_type(GType g_type, const gchar *signal_name)
{
    GIRepository *repository;
    GIBaseInfo *info;
    GISignalInfo *signal_info = NULL;

    repository = g_irepository_get_default();
    info = g_irepository_find_by_gtype(repository, g_type);
    if (info == NULL)
        return NULL;

    if (GI_IS_OBJECT_INFO(info))
        signal_info = g_object_info_find_signal((GIObjectInfo *)info, signal_name);
    else if (GI_IS_INTERFACE_INFO(info))
        signal_info = g_interface_info_find_signal((GIInterfaceInfo *)info, signal_name);

    g_base_info_unref(info);
    return signal_info;
}

static void
signal_closure_invalidate(gpointer data, GClosure *closure)
{
    GigClosure *pc = (GigClosure *) closure;

    pc->callback = SCM_BOOL_F;
    pc->swap_data = SCM_BOOL_F;

    g_base_info_unref(pc->signal_info);
    pc->signal_info = NULL;
}

static void
gi_signal_closure_marshal(GClosure *closure,
                          GValue *return_value,
                          guint n_param_values,
                          const GValue *param_values,
                          gpointer invocation_hint, gpointer marshal_data)
{
    GigClosure *pc = (GigClosure *) closure;
    SCM params, ret = SCM_BOOL_F;
    guint i;
    GISignalInfo *signal_info;
    gint n_sig_info_args;
    gint sig_info_highest_arg;
#if 0
    GSList *list_item = NULL;
#endif
    GSList *pass_by_ref_structs = NULL;


    // Here we take the parameters in *param_values and call the
    // Scheme function stored in *closure.

    signal_info = pc->signal_info;
    if (signal_info) {
        n_sig_info_args = g_callable_info_get_n_args(signal_info);
        g_assert_cmpint(n_sig_info_args, >=, 0);
        /* the first argument to a signal callback is instance,
         * but instance is not counted in the introspection data */
        sig_info_highest_arg = n_sig_info_args + 1;
        g_assert_cmpint(sig_info_highest_arg, ==, n_param_values);
    }

    /* construct a scheme list for the parameter values */
    params = SCM_EOL;
    // FIXME: handle swap
    /* gboolean swap = G_CCLOSURE_SWAP_DATA(closure); */
    for (i = 0; i < n_param_values; i++) {
        SCM item = gi_gvalue_as_scm(&param_values[i], FALSE);
        if (scm_is_false(item)) {
            if (i == 0)         // self or this
                goto out;
        }
        params = scm_cons(item, params);
    }
    params = scm_reverse_x(params, SCM_EOL);
    g_debug("invoking callback with %d arguments", scm_to_int(scm_length(params)));
    ret = scm_apply_0(pc->callback, params);

    if (G_IS_VALUE(return_value) && gi_gvalue_from_scm(return_value, ret) != 0) {
        scm_misc_error("callback", "can't convert return value to desired type", SCM_EOL);
    }

  out:
    g_slist_free(pass_by_ref_structs);
}

GClosure *
gi_signal_closure_new(SCM instance, GType g_type, const gchar *signal_name, SCM callback)
{
    GClosure *closure = NULL;
    GigClosure *gig_closure = NULL;
    GISignalInfo *signal_info = NULL;

    g_return_val_if_fail(scm_is_true(instance), NULL);

    signal_info = lookup_signal_from_g_type(g_type, signal_name);

    closure = g_closure_new_simple(sizeof(GigClosure), NULL);
    g_closure_add_invalidate_notifier(closure, NULL, signal_closure_invalidate);
    g_closure_set_marshal(closure, gi_signal_closure_marshal);

    gig_closure = (GigClosure *) closure;

    gig_closure->signal_info = signal_info;
    gig_closure->callback = callback;

    return closure;
}
