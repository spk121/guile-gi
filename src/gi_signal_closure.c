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
#include "gi_gobject.h"
#include "gi_gvalue.h"
#include "gi_giargument.h"
#include "gi_gboxed.h"

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
    GuGClosure *pc = (GuGClosure *) closure;

    pc->callback = SCM_BOOL_F;
    pc->extra_args = SCM_BOOL_F;
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
    GuGClosure *pc = (GuGClosure *) closure;
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
    n_sig_info_args = g_callable_info_get_n_args(signal_info);
    g_assert_cmpint(n_sig_info_args, >=, 0);
    /* the first argument to a signal callback is instance,
     * but instance is not counted in the introspection data */
    sig_info_highest_arg = n_sig_info_args + 1;
    g_assert_cmpint(sig_info_highest_arg, ==, n_param_values);

    /* construct a scheme list for the parameter values */
    params = SCM_EOL;
    // FIXME: handle swap
    /* gboolean swap = G_CCLOSURE_SWAP_DATA(closure); */
    for (i = 0; i < n_param_values; i++) {
        if (TRUE /*i == 0 */ ) {
            /* We know that the first argument is always some sort of
             * 'self' or 'this'. */
            SCM item = gi_gvalue_as_scm(&param_values[i], FALSE);

            if (scm_is_false(item)) {
                goto out;
            }
            params = scm_append(scm_list_2(params, scm_list_1(item)));
        }
        else if (i < (guint) sig_info_highest_arg) {
            /* The rest of the parameters could be anything, so we query
             * the arginfo for more information */

            GIArgInfo arg_info;
            GITypeInfo type_info;
            GITypeTag type_tag;
            GIArgument arg = { 0, };
            SCM item = SCM_BOOL_F;
#if 0
            gboolean free_array = FALSE;
            gboolean pass_struct_by_ref = FALSE;
#endif

            g_callable_info_load_arg(signal_info, i - 1, &arg_info);
            g_arg_info_load_type(&arg_info, &type_info);

            arg = gi_giargument_from_g_value(&param_values[i], &type_info);

            type_tag = g_type_info_get_tag(&type_info);
            if (type_tag == GI_TYPE_TAG_ARRAY) {
                g_assert_not_reached();
#if 0
                /* Skip the self argument of param_values */
                arg.v_pointer = _pygi_argument_to_array(&arg,
                                                        gi_giargument_array_length_marshal,
                                                        (void *)(param_values + 1),
                                                        signal_info, &type_info, &free_array);
#endif
            }

            /* Hack to ensure struct arguments are passed-by-reference allowing
             * callback implementors to modify the struct values. This is needed
             * for keeping backwards compatibility and should be removed in future
             * versions which support signal output arguments as return values.
             * See: https://bugzilla.gnome.org/show_bug.cgi?id=735486
             *
             * Note the logic here must match the logic path taken in _pygi_argument_to_object.
             */
            else if (type_tag == GI_TYPE_TAG_INTERFACE) {
                g_assert_not_reached();
#if 0
                GIBaseInfo *info = g_type_info_get_interface(&type_info);
                GIInfoType info_type = g_base_info_get_type(info);

                if (info_type == GI_INFO_TYPE_STRUCT ||
                    info_type == GI_INFO_TYPE_BOXED || info_type == GI_INFO_TYPE_UNION) {

                    GType gtype = g_registered_type_info_get_g_type((GIRegisteredTypeInfo *) info);
                    gboolean is_foreign = (info_type == GI_INFO_TYPE_STRUCT) &&
                        (g_struct_info_is_foreign((GIStructInfo *) info));

                    if (!is_foreign && !g_type_is_a(gtype, G_TYPE_VALUE) &&
                        g_type_is_a(gtype, G_TYPE_BOXED)) {
                        pass_struct_by_ref = TRUE;
                    }
                }

                g_base_info_unref(info);
#endif
            }
#if 0
            if (pass_struct_by_ref) {
                /* transfer everything will ensure the struct is not copied when wrapped. */
                item = _pygi_argument_to_object(&arg, &type_info, GI_TRANSFER_EVERYTHING);
                if (item && PyObject_IsInstance(item, (PyObject *) & PyGIBoxed_Type)) {
                    ((PyGBoxed *) item)->free_on_dealloc = FALSE;
                    pass_by_ref_structs = g_slist_prepend(pass_by_ref_structs, item);
                }

            }
            else {
                item = _pygi_argument_to_object(&arg, &type_info, GI_TRANSFER_NOTHING);
            }

            if (free_array) {
                g_array_free(arg.v_pointer, FALSE);
            }

            if (item == NULL) {
                PyErr_Print();
                goto out;
            }
#endif
            else {
                item = gi_gvalue_as_scm(&param_values[i], FALSE);
            }
            params = scm_append(scm_list_2(params, scm_list_1(item)));
        }
    }
    /* params passed to function may have extra arguments */
    if (scm_is_true(pc->extra_args)) {
        params = scm_append(scm_list_2(params, scm_list_1(pc->extra_args)));
    }
    /* Now we actuall do the call! */
    ret = scm_apply_0(pc->callback, params);
#if 0
    if (ret == NULL) {
        if (pc->exception_handler)
            pc->exception_handler(return_value, n_param_values, param_values);
        else
            PyErr_Print();
        goto out;
    }
#endif

    if (G_IS_VALUE(return_value) && gi_gvalue_from_scm(return_value, ret) != 0) {
        scm_misc_error("callback", "can't convert return value to desired type", SCM_EOL);
#if 0
        if (pc->exception_handler)
            pc->exception_handler(return_value, n_param_values, param_values);
        else
            PyErr_Print();
#endif
    }
    // Py_DECREF(ret);

    /* Run through the list of structs which have been passed by reference and
     * check if they are being held longer than the duration of the callback
     * execution. This is determined if the ref count is greater than 1.
     * A single ref is held by the argument list and any more would mean the callback
     * stored a ref somewhere else. In this case we make an internal copy of
     * the boxed struct so Python can own the memory to it.
     */
#if 0
    list_item = pass_by_ref_structs;
    while (list_item) {
        PyObject *item = list_item->data;
        if (Py_REFCNT(item) > 1) {
            pygi_boxed_copy_in_place((PyGIBoxed *) item);
        }
        list_item = g_slist_next(list_item);
    }
#endif

  out:
    g_slist_free(pass_by_ref_structs);
    // Py_DECREF(params);
    // PyGILState_Release(state);
}

GClosure *
gi_signal_closure_new(SCM instance,
                      GType g_type, const gchar *signal_name, SCM callback, SCM extra_args)
{
    GClosure *closure = NULL;
    GuGClosure *gugi_closure = NULL;
    GISignalInfo *signal_info = NULL;

    g_return_val_if_fail(scm_is_true(instance), NULL);

    signal_info = lookup_signal_from_g_type(g_type, signal_name);
    if (signal_info == NULL)
        return NULL;

    closure = g_closure_new_simple(sizeof(GuGClosure), NULL);
    g_closure_add_invalidate_notifier(closure, NULL, signal_closure_invalidate);
    g_closure_set_marshal(closure, gi_signal_closure_marshal);

    gugi_closure = (GuGClosure *) closure;

    gugi_closure->signal_info = signal_info;
    gugi_closure->callback = callback;

    if (scm_is_true(scm_list_p((extra_args)))) {
        gugi_closure->extra_args = extra_args;
    }

    return closure;
}
