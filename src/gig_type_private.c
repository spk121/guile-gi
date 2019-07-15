#include "gig_type_private.h"

static GSList *_boxed_funcs = NULL;

static void
_boxed_copy(ffi_cif *cif, void *ret, void **ffi_args, void *user_data)
{
    GType type = GPOINTER_TO_SIZE(user_data);
#if GIG_DEBUG_TRANSFERS
    g_debug("boxed_copy(%s, %p)", g_type_name(type), *(gpointer *) ffi_args[0]);
#endif
    *(ffi_arg *) ret = (ffi_arg) g_boxed_copy(type, *(gpointer *) ffi_args[0]);
}

static void
_boxed_free(ffi_cif *cif, void *ret, void **ffi_args, void *user_data)
{
    GType type = GPOINTER_TO_SIZE(user_data);
#if GIG_DEBUG_TRANSFERS
    g_debug("boxed_free(%s, %p)", g_type_name(type), *(gpointer *) ffi_args[0]);
#endif
    g_boxed_free(type, *(gpointer *) ffi_args[0]);
}

GigBoxedFuncs *
_boxed_funcs_for_type(GType type)
{
    GigBoxedFuncs *funcs = g_new0(GigBoxedFuncs, 1);

    funcs->atypes[0] = &ffi_type_pointer;

    funcs->copy_closure = ffi_closure_alloc(sizeof(ffi_closure), &(funcs->copy));
    funcs->free_closure = ffi_closure_alloc(sizeof(ffi_closure), &(funcs->free));

    g_assert(funcs->copy_closure != NULL && funcs->copy != NULL);
    g_assert(funcs->free_closure != NULL && funcs->free != NULL);

    g_assert(ffi_prep_cif(&(funcs->copy_cif), FFI_DEFAULT_ABI, 1, &ffi_type_pointer,
                          funcs->atypes) == FFI_OK);
    g_assert(ffi_prep_cif(&(funcs->free_cif), FFI_DEFAULT_ABI, 1, &ffi_type_void,
                          funcs->atypes) == FFI_OK);

    g_assert(ffi_prep_closure_loc(funcs->copy_closure, &(funcs->copy_cif), _boxed_copy,
                                  GSIZE_TO_POINTER(type), funcs->copy) == FFI_OK);
    g_assert(ffi_prep_closure_loc(funcs->free_closure, &(funcs->free_cif), _boxed_free,
                                  GSIZE_TO_POINTER(type), funcs->free) == FFI_OK);

    _boxed_funcs = g_slist_prepend(_boxed_funcs, funcs);

    return funcs;
}

static void
_boxed_funcs_free(GigBoxedFuncs *funcs)
{
    ffi_closure_free(funcs->copy_closure);
    ffi_closure_free(funcs->free_closure);

    funcs->copy_closure = NULL;
    funcs->free_closure = NULL;

    g_free(funcs);
}

void
_free_boxed_funcs()
{
    g_slist_free_full(_boxed_funcs, (GDestroyNotify) _boxed_funcs_free);
}
