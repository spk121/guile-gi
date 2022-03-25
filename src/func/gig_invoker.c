#include "../core.h"
#include "gig_invoker.h"
#include <libguile.h>
#include <stddef.h>
#include <sys/types.h>          /* ssize_t */
#include <ffi.h>

static ffi_type *
arg_type_to_ffi_type(GigArgType type)
{
    if (type == GIG_ARG_TYPE_VOID)
        return &ffi_type_void;
    else if (type == GIG_ARG_TYPE_POINTER)
        return &ffi_type_pointer;
    else if (type == GIG_ARG_TYPE_GBOOLEAN)
        return &ffi_type_uint;
    else if (type == GIG_ARG_TYPE_INT8)
        return &ffi_type_sint8;
    else if (type == GIG_ARG_TYPE_UINT8)
        return &ffi_type_uint8;
    else if (type == GIG_ARG_TYPE_INT16)
        return &ffi_type_sint16;
    else if (type == GIG_ARG_TYPE_UINT16)
        return &ffi_type_uint16;
    else if (type == GIG_ARG_TYPE_INT32 || (sizeof(int) == 4 && type == GIG_ARG_TYPE_GBOOLEAN))
        return &ffi_type_sint32;
    else if (type == GIG_ARG_TYPE_UINT32
             || type == GIG_ARG_TYPE_UNICHAR
             || (sizeof(size_t) == 4 && type == GIG_ARG_TYPE_GTYPE))
        return &ffi_type_uint32;
    else if (type == GIG_ARG_TYPE_INT64 || (sizeof(int) == 8 && type == GIG_ARG_TYPE_GBOOLEAN))
        return &ffi_type_sint64;
    else if (type == GIG_ARG_TYPE_UINT64 || (sizeof(size_t) == 8 && type == GIG_ARG_TYPE_GTYPE))
        return &ffi_type_uint64;
    else if (type == GIG_ARG_TYPE_FLOAT)
        return &ffi_type_float;
    else if (type == GIG_ARG_TYPE_DOUBLE)
        return &ffi_type_double;
    else if (type == GIG_ARG_TYPE_ENUM || type == GIG_ARG_TYPE_FLAGS)
        return &ffi_type_sint32;
    else
        // Strings, interfaces, lists, hash, error.
        return &ffi_type_pointer;
}

// Note that libffi requires that returns types be at least 'long' in
// size, but long is either 4 or 8 bytes depending on the platform,
// which is why we do this strange sign extension here.
static void
extract_ffi_return_value(GigArgType type, GigArgument *ffi_value, GigArgument *arg)
{
    if (type == GIG_ARG_TYPE_INT8)
        arg->v_int8 = (int8_t)ffi_value->v_long;
    else if (type == GIG_ARG_TYPE_UINT8)
        arg->v_uint8 = (uint8_t)ffi_value->v_ulong;
    else if (type == GIG_ARG_TYPE_INT16)
        arg->v_int16 = (int16_t)ffi_value->v_long;
    else if (type == GIG_ARG_TYPE_UINT16)
        arg->v_uint16 = (uint16_t)ffi_value->v_ulong;
    else if (type == GIG_ARG_TYPE_INT32 || (sizeof(int) == 4 && type == GIG_ARG_TYPE_GBOOLEAN))
        arg->v_int32 = (int32_t)ffi_value->v_long;
    else if (type == GIG_ARG_TYPE_UINT32
             || type == GIG_ARG_TYPE_UNICHAR
             || (sizeof(size_t) == 4 && type == GIG_ARG_TYPE_GTYPE))
        arg->v_uint32 = (uint32_t)ffi_value->v_ulong;
    else if (type == GIG_ARG_TYPE_INT64 || (sizeof(int) == 8 && type == GIG_ARG_TYPE_GBOOLEAN))
        arg->v_int64 = (int64_t)ffi_value->v_int64;
    else if (type == GIG_ARG_TYPE_UINT64 || (sizeof(size_t) == 8 && type == GIG_ARG_TYPE_GTYPE))
        arg->v_uint64 = (int64_t)ffi_value->v_uint64;
    else if (type == GIG_ARG_TYPE_FLOAT)
        arg->v_float = ffi_value->v_float;
    else if (type == GIG_ARG_TYPE_DOUBLE)
        arg->v_double = ffi_value->v_double;
    else if (type == GIG_ARG_TYPE_ENUM)
        arg->v_int32 = (int32_t)ffi_value->v_long;
    else if (type == GIG_ARG_TYPE_FLAGS)
        arg->v_uint32 = (uint32_t)ffi_value->v_long;
    else
        arg->v_pointer = ffi_value->v_pointer;

}

_Bool
gig_invoke_func(void *address, GigArgMap *amap, const GigArgument *in_args, int n_in_args,
                const GigArgument *out_args, int n_out_args,
                GigArgument *return_value, GError **error)
{
    int in_pos = 0, out_pos = 0;
    int n_args, n_invoke_args;
    ffi_type **atypes, *rtype;
    GError *local_error = NULL;
    void *error_address = &local_error;
    ffi_cif cif;
    GigArgType rtag;

    rtag = amap->return_val.meta.arg_type;
    rtype = arg_type_to_ffi_type(rtag);

    n_args = amap->len;
    if (amap->is_method) {
        if (n_in_args == 0) {
            g_set_error(error,
                        G_INVOKE_ERROR,
                        G_INVOKE_ERROR_ARGUMENT_MISMATCH,
                        "Too few \"in\" arguments (handling this)");
        }
        n_invoke_args = n_args + 1;
        in_pos++;
    }
    else
        n_invoke_args = n_args;

    if (amap->can_throw_gerror)
        /* Add an argument for the GError */
        n_invoke_args++;

    atypes = xcalloc(n_invoke_args, sizeof(ffi_type *));
    void **args = xcalloc(n_invoke_args, sizeof(void *));

    if (amap->is_method) {
        atypes[0] = &ffi_type_pointer;
        args[0] = (void *)&in_args[0];
    }
    for (int i = 0; i < n_args; i++) {
        int offset = (amap->is_method ? 1 : 0);
        if (amap->pdata[i].meta.is_in && !amap->pdata[i].meta.is_out) {
            atypes[i + offset] = arg_type_to_ffi_type(amap->pdata[i].meta.arg_type);
            if (in_pos >= n_in_args) {
                g_set_error(error,
                            G_INVOKE_ERROR,
                            G_INVOKE_ERROR_ARGUMENT_MISMATCH,
                            "Too few \"in\" arguments (handling in)");
            }

            args[i + offset] = (void *)&in_args[in_pos];
            in_pos++;
        }
        else if (!amap->pdata[i].meta.is_in && amap->pdata[i].meta.is_out) {
            atypes[i + offset] = &ffi_type_pointer;
            if (out_pos >= n_out_args) {
                g_set_error(error,
                            G_INVOKE_ERROR,
                            G_INVOKE_ERROR_ARGUMENT_MISMATCH,
                            "Too few \"out\" arguments (handling out)");
            }

            args[i + offset] = (void *)&out_args[out_pos];
            out_pos++;
        }
        else if (amap->pdata[i].meta.is_in && amap->pdata[i].meta.is_out) {
            atypes[i + offset] = &ffi_type_pointer;
            if (in_pos >= n_in_args) {
                g_set_error(error,
                            G_INVOKE_ERROR,
                            G_INVOKE_ERROR_ARGUMENT_MISMATCH,
                            "Too few \"in\" arguments (handling inout)");
            }

            if (out_pos >= n_out_args) {
                g_set_error(error,
                            G_INVOKE_ERROR,
                            G_INVOKE_ERROR_ARGUMENT_MISMATCH,
                            "Too few \"out\" arguments (handling inout)");
            }
            args[i + offset] = (void *)&in_args[in_pos];
            in_pos++;
            out_pos++;
        }
    }
    if (amap->can_throw_gerror) {
        args[n_invoke_args - 1] = &error_address;
        atypes[n_invoke_args - 1] = &ffi_type_pointer;
    }
    ffi_prep_cif(&cif, FFI_DEFAULT_ABI, n_invoke_args, rtype, atypes);

    // There is extra munging required for return types, because
    // libffi return values need to be at least "long" size, and the
    // pointer needs to point to the correct start location in the
    // storage.
    GigArgument ffi_return_value;
    void *return_value_p;
    switch (rtag) {
    case GIG_ARG_TYPE_FLOAT:
        return_value_p = &(ffi_return_value.v_float);
        break;
    case GIG_ARG_TYPE_DOUBLE:
        return_value_p = &(ffi_return_value.v_double);
        break;
    case GIG_ARG_TYPE_INT64:
    case GIG_ARG_TYPE_UINT64:
        return_value_p = &(ffi_return_value.v_uint64);
        break;
    default:
        return_value_p = &(ffi_return_value.v_long);
    }

    ffi_call(&cif, address, return_value_p, args);

    bool success;
    if (local_error) {
        g_propagate_error(error, local_error);
        success = false;
    }
    else {
        extract_ffi_return_value(rtag, &ffi_return_value, return_value);
        success = true;
    }
    return success;
}
