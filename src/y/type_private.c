#include <assert.h>
#include "x.h"
#include "y/type_private.h"

static slist_t *_boxed_funcs = NULL;

static void _boxed_funcs_free(Boxed_funcs *funcs);

static void
_boxed_copy(ffi_cif *cif, void *ret, void **ffi_args, void *user_data)
{
    GType type = GPOINTER_TO_SIZE(user_data);
    debug_transfer("boxed_copy(%s, %p)", g_type_name(type), *(void **)ffi_args[0]);
    *(ffi_arg *)ret = (ffi_arg)g_boxed_copy(type, *(void **)ffi_args[0]);
}

static void
_boxed_free(ffi_cif *cif, void *ret, void **ffi_args, void *user_data)
{
    GType type = GPOINTER_TO_SIZE(user_data);
    debug_transfer("boxed_free(%s, %p)", g_type_name(type), *(void **)ffi_args[0]);
    g_boxed_free(type, *(void **)ffi_args[0]);
}

Boxed_funcs *
_boxed_funcs_for_type(GType type)
{
    Boxed_funcs *funcs = xcalloc(1, sizeof(Boxed_funcs));

    funcs->atypes[0] = &ffi_type_pointer;

    funcs->copy_closure = ffi_closure_alloc(sizeof(ffi_closure), &(funcs->copy));
    funcs->free_closure = ffi_closure_alloc(sizeof(ffi_closure), &(funcs->free));

    assert(funcs->copy_closure != NULL && funcs->copy != NULL);
    assert(funcs->free_closure != NULL && funcs->free != NULL);

    assert(ffi_prep_cif(&(funcs->copy_cif), FFI_DEFAULT_ABI, 1, &ffi_type_pointer,
                        funcs->atypes) == FFI_OK);
    assert(ffi_prep_cif(&(funcs->free_cif), FFI_DEFAULT_ABI, 1, &ffi_type_void,
                        funcs->atypes) == FFI_OK);

    assert(ffi_prep_closure_loc(funcs->copy_closure, &(funcs->copy_cif), _boxed_copy,
                                GSIZE_TO_POINTER(type), funcs->copy) == FFI_OK);
    assert(ffi_prep_closure_loc(funcs->free_closure, &(funcs->free_cif), _boxed_free,
                                GSIZE_TO_POINTER(type), funcs->free) == FFI_OK);

    slist_prepend(&_boxed_funcs, funcs);

    return funcs;
}

static void
_boxed_funcs_free(Boxed_funcs *funcs)
{
    ffi_closure_free(funcs->copy_closure);
    ffi_closure_free(funcs->free_closure);

    funcs->copy_closure = NULL;
    funcs->free_closure = NULL;

    free(funcs);
}

void
_free_boxed_funcs()
{
    slist_free(&_boxed_funcs, _boxed_funcs_free);
}

