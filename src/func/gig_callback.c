// Copyright (C) 2019, 2020, 2021, 2022 Michael L. Gran

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

#include <assert.h>
#include <libguile.h>
#include <libguile/hooks.h>
#include <ffi.h>
#include "../core.h"
#include "gig_argument.h"
#include "gig_callback_priv.h"
#include "gig_function_priv.h"

typedef struct _GigCallback GigCallback;
struct _GigCallback
{
    GICallbackInfo *callback_info;
    GigArgMap *amap;
    ffi_closure *closure;
    ffi_cif cif;
    union
    {
        SCM s_func;
        void *c_func;
    };
    char *name;
    void *callback_ptr;
    ffi_type **atypes;
};

static slist_t *callback_list = NULL;

#if HAVE_SCM_HOOKS
SCM gig_before_c_callback_hook;
SCM gig_before_callback_hook;
#endif
SCM gig_callback_thread_fluid;

static ffi_type *amap_entry_to_ffi_type(GigArgMapEntry *entry);
static void callback_free(GigCallback *gcb);
static void gig_fini_callback(void);

static void
convert_ffi_arg_to_giargument(void *_ffi_arg, ffi_type *arg_type, bool unpack, GIArgument *giarg)
{
    if (unpack)
        _ffi_arg = ((void **)_ffi_arg)[0];

    if (arg_type == &ffi_type_pointer)
        giarg->v_pointer = _ffi_arg;
    else if (arg_type == &ffi_type_void)
        giarg->v_pointer = _ffi_arg;
    else if (arg_type == &ffi_type_sint)
        giarg->v_int = *(int *)_ffi_arg;
    else if (arg_type == &ffi_type_uint)
        giarg->v_uint = *(unsigned *)_ffi_arg;
    else if (arg_type == &ffi_type_sint8)
        giarg->v_int8 = *(int8_t *)_ffi_arg;
    else if (arg_type == &ffi_type_uint8)
        giarg->v_uint8 = *(uint8_t *)_ffi_arg;
    else if (arg_type == &ffi_type_sint16)
        giarg->v_int16 = *(int16_t *)_ffi_arg;
    else if (arg_type == &ffi_type_uint16)
        giarg->v_uint16 = *(uint16_t *)_ffi_arg;
    else if (arg_type == &ffi_type_sint32)
        giarg->v_int32 = *(int32_t *)_ffi_arg;
    else if (arg_type == &ffi_type_uint32)
        giarg->v_uint32 = *(uint32_t *)_ffi_arg;
    else if (arg_type == &ffi_type_sint64)
        giarg->v_int64 = *(int64_t *)_ffi_arg;
    else if (arg_type == &ffi_type_uint64)
        giarg->v_uint64 = *(uint64_t *)_ffi_arg;
    else if (arg_type == &ffi_type_float)
        giarg->v_float = *(float *)_ffi_arg;
    else if (arg_type == &ffi_type_double)
        giarg->v_double = *(double *)_ffi_arg;
    else {
        gig_critical("Unhandled FFI type in %s: %d", __FILE__, __LINE__);
        giarg->v_pointer = _ffi_arg;
    }
}

static void
store_output(GigArgMapEntry *entry, void ***arg, GIArgument *value)
{
    GType gtype = G_TYPE_FUNDAMENTAL(entry->meta.gtype);
    size_t item_size = entry->meta.item_size;
    switch (gtype) {
    case G_TYPE_BOOLEAN:
        **(int **)arg = value->v_int;
        break;
    case G_TYPE_CHAR:
        **(char **)arg = value->v_int8;
        break;
    case G_TYPE_UCHAR:
        **(unsigned char **)arg = value->v_uint8;
        break;
    case G_TYPE_INT:
    {
        switch (item_size) {
        case 1:
            **(int8_t **)arg = value->v_int8;
            break;
        case 2:
            **(int16_t **)arg = value->v_int16;
            break;
        case 4:
            **(int32_t **)arg = value->v_int32;
            break;
        case 8:
            **(int64_t **)arg = value->v_int64;
            break;
        default:
            assert_not_reached();
        }
        break;
    }
    case G_TYPE_UINT:
    {
        switch (entry->meta.item_size) {
        case 1:
            **(uint8_t **)arg = value->v_uint8;
            break;
        case 2:
            **(uint16_t **)arg = value->v_uint16;
            break;
        case 4:
            **(uint32_t **)arg = value->v_uint32;
            break;
        case 8:
            **(uint64_t **)arg = value->v_uint64;
            break;
        default:
            assert_not_reached();
        }
        break;
    }
    case G_TYPE_INT64:
        **(int64_t **)arg = value->v_int64;
        break;
    case G_TYPE_UINT64:
        **(uint64_t **)arg = value->v_uint64;
        break;
    case G_TYPE_FLOAT:
        **(float **)arg = value->v_float;
        break;
    case G_TYPE_DOUBLE:
        **(double **)arg = value->v_double;
        break;
    case G_TYPE_STRING:
        **(char ***)arg = value->v_string;
        break;
    case G_TYPE_POINTER:
    case G_TYPE_BOXED:
    case G_TYPE_OBJECT:
        **(char ***)arg = value->v_pointer;
        break;
    default:
        gig_critical("Unhandled FFI type in %s: %d", __FILE__, __LINE__);
        **(char ***)arg = value->v_pointer;
        break;
    }
}

struct callback_binding_args
{
    ffi_cif *cif;
    void *ret;
    void **ffi_args;
    GigCallback *gcb;
};

// This is the core of a dynamically generated callback funcion.
// It converts FFI arguments to SCM arguments, calls a SCM function
// and then returns the result.
static void *
callback_binding_inner(struct callback_binding_args *args)
{
    ffi_cif *cif = args->cif;
    void *ret = args->ret;
    void **ffi_args = args->ffi_args;
    GigCallback *gcb = args->gcb;
    SCM s_args = SCM_EOL;
    SCM s_ret;

    assert(cif != NULL);
    assert(ret != NULL);
    assert(ffi_args != NULL);
    assert(gcb != NULL);

    unsigned n_args = cif->nargs;

    scm_load_goops();

    assert(scm_is_procedure(gcb->s_func));
    assert(n_args == g_callable_info_get_n_args(gcb->callback_info));
    assert(gcb->amap != NULL);

    GigArgMap *amap = gcb->amap;
    char *callback_name;
    if (amap->name)
        callback_name = concatenate3("callback:<", amap->name, ">");
    else
        callback_name = xstrdup("callback");

    // Do the two-step conversion from libffi arguments to GIArgument
    // to SCM arguments.
    for (unsigned i = 0; i < n_args; i++) {
        SCM s_entry = SCM_BOOL_F;
        GIArgument giarg;

        if (!amap->pdata[i].is_s_input)
            continue;

        convert_ffi_arg_to_giargument(ffi_args[i], cif->arg_types[i], amap->pdata[i].meta.is_ptr,
                                      &giarg);
        gig_argument_c_to_scm(callback_name, i, &amap->pdata[i].meta, &giarg, &s_entry, -1);
        s_args = scm_cons(s_entry, s_args);
    }
    s_args = scm_reverse_x(s_args, SCM_EOL);
    size_t length = scm_c_length(s_args);

#if HAVE_SCM_HOOKS
    if (!scm_is_empty_hook(gig_before_callback_hook))
        scm_c_activate_hook_3(gig_before_callback_hook,
                              scm_from_utf8_string(g_base_info_get_name(gcb->callback_info)),
                              gcb->s_func, s_args);
#endif

    // The actual call of the Scheme callback happens here.
    if (length < amap->s_input_req || length > amap->s_input_req + amap->s_input_opt)
        scm_error(scm_args_number_key,
                  callback_name,
                  "Wrong number of arguments to ~A, received ~A, expected ~A to ~A",
                  scm_list_4(gcb->s_func, scm_from_int(length),
                             scm_from_int(amap->s_input_req),
                             scm_from_int(amap->s_input_req + amap->s_input_opt)), SCM_BOOL_F);
    else
        s_ret = scm_apply_0(gcb->s_func, s_args);

    // Return values and output arguments start here.
    if (scm_is_false(s_ret))
        *(ffi_arg *)ret = FALSE;
    else {
        GIArgument giarg;
        size_t size;
        int start = 0;

        size_t n_values_ = scm_c_nvalues(s_ret);
        assert(n_values_ < G_MAXINT32);
        int in, out, n_values = (int)n_values_;
        gig_amap_c_count(amap, &in, &out);

        if (amap->return_val.meta.gtype != G_TYPE_NONE) {
            start = 1;
            SCM real_ret = scm_c_value_ref(s_ret, 0);
            gig_argument_scm_to_c(callback_name, 0, &amap->return_val.meta, real_ret, NULL, &giarg,
                                  &size);
            store_output(&(amap->return_val), (void ***)&ret, &giarg);

            if (amap->return_val.meta.has_size) {
                size_t c_output_pos = amap->return_val.child->c_output_pos;
                GIArgument tmp;
                tmp.v_int64 = size;
                store_output(amap->return_val.child, ffi_args[c_output_pos], &tmp);
            }
        }

        for (int c_output_pos = 0; c_output_pos < out; c_output_pos++) {
            GigArgMapEntry *entry = gig_amap_get_output_entry_by_c(amap, c_output_pos);
            if (!entry->is_s_output)
                continue;
            int real_cpos = entry - amap->pdata;
            if (entry->s_output_pos >= n_values)
                scm_misc_error(callback_name, "too few return values", SCM_EOL);
            SCM real_value = scm_c_value_ref(s_ret, entry->s_output_pos + start);
            gig_argument_scm_to_c(callback_name, real_cpos, &entry->meta, real_value, NULL, &giarg,
                                  &size);
            store_output(entry, ffi_args[c_output_pos], &giarg);

            if (amap->return_val.meta.has_size) {
                size_t size_pos = entry->child->c_output_pos;
                GIArgument tmp;
                tmp.v_int64 = size;
                store_output(entry->child, ffi_args[size_pos], &tmp);
            }
        }
    }
    free(callback_name);
    return (void *)1;
}

inline static void *
callback_binding_inner_v(void *args)
{
    return callback_binding_inner(args);
}

static void
callback_binding(ffi_cif *cif, void *ret, void **ffi_args, void *user_data)
{
    struct callback_binding_args args;
    args.cif = cif;
    args.ret = ret;
    args.ffi_args = ffi_args;
    args.gcb = user_data;

    scm_init_guile();

    // If we're not in the main thread, we catch and error at this
    // level.  But in the main thread, there is assuredly some higher
    // level catch.

    if (scm_is_true(scm_fluid_ref(gig_callback_thread_fluid)))
        callback_binding_inner(&args);
    else {
        if (NULL == scm_with_guile(callback_binding_inner_v, &args))
            scm_sad_quit();
    }
}

static void *
c_callback_binding_inner(struct callback_binding_args *args)
{

    ffi_cif *cif = args->cif;
    void *ret = args->ret;
    void **ffi_args = args->ffi_args;
    const char *name = "c callback";
    GigCallback *gcb = args->gcb;
    SCM s_args = SCM_UNDEFINED;

    assert(cif != NULL);
    assert(ret != NULL);
    assert(ffi_args != NULL);
    assert(gcb != NULL);

    unsigned n_args = cif->nargs;

    // we have either 0 args or 1 args, which is the already packed list
    assert(n_args <= 1);
    if (n_args)
        s_args = SCM_PACK(*(scm_t_bits *) (ffi_args[0]));

    if (SCM_UNBNDP(s_args))
        s_args = SCM_EOL;

#if HAVE_SCM_HOOKS
    if (!scm_is_empty_hook(gig_before_c_callback_hook))
        scm_c_activate_hook_3(gig_before_c_callback_hook,
                              scm_from_utf8_string(g_base_info_get_name(gcb->callback_info)),
                              scm_from_pointer(gcb->c_func, NULL), s_args);
#endif

    // Use 'name' instead of gcb->name, which is NULL for C callbacks.
    GError *error = NULL;
    SCM output = gig_callable_invoke(gcb->callback_info, gcb->c_func, gcb->amap, name, NULL,
                                     s_args, &error);

    if (error != NULL) {
        SCM err = scm_from_utf8_string(error->message);
        g_error_free(error);
        scm_misc_error("gi:c-callback", "~A", scm_list_1(err));
    }

    *(ffi_arg *)ret = SCM_UNPACK(output);
    return (void *)1;
}

inline static void *
c_callback_binding_inner_v(void *args)
{
    return c_callback_binding_inner(args);
}

static void
c_callback_binding(ffi_cif *cif, void *ret, void **ffi_args, void *user_data)
{
    struct callback_binding_args args;
    args.cif = cif;
    args.ret = ret;
    args.ffi_args = ffi_args;
    args.gcb = user_data;

    scm_init_guile();

    // If we're not in the main thread, we catch and error at this
    // level.  But in the main thread, there is assuredly some higher
    // level catch.

    if (scm_is_true(scm_fluid_ref(gig_callback_thread_fluid)))
        c_callback_binding_inner(&args);
    else {
        if (NULL == scm_with_guile(c_callback_binding_inner_v, &args))
            scm_sad_quit();
    }
}

// This procedure uses CALLBACK_INFO to create a dynamic FFI C closure
// to use as an entry point to the scheme procedure S_FUNC.
static GigCallback *
gig_callback_new(const char *name, GICallbackInfo *callback_info, SCM s_func)
{
    assert(scm_is_procedure(s_func));

    GigCallback *gcb = xcalloc(1, sizeof(GigCallback));
    ffi_type **ffi_args = NULL;
    ffi_type *ffi_ret_type;
    int n_args = g_callable_info_get_n_args(callback_info);

    SCM s_name = scm_procedure_name(s_func);
    if (scm_is_symbol(s_name)) {
        gcb->name = scm_to_utf8_symbol(s_name);
        gig_debug("Constructing C callback for %s", gcb->name);
    }
    else {
        gcb->name = concatenate("callback:", name);
        gig_debug("Construction a C Callback for an anonymous procedure");
    }

    gcb->s_func = s_func;
    gcb->callback_info = g_base_info_ref(callback_info);
    gcb->amap = gig_amap_new(gcb->name, gcb->callback_info);

    // STEP 1
    // Allocate the block of memory that FFI uses to hold a closure object,
    // and set a pointer to the corresponding executable address.
    gcb->closure = ffi_closure_alloc(sizeof(ffi_closure), &(gcb->callback_ptr));

    assert(gcb->closure != NULL);
    assert(gcb->callback_ptr != NULL);

    // STEP 2
    // Next, we begin to construct an FFI_CIF to describe the function call.

    // Initialize the argument info vectors.
    if (n_args > 0) {
        ffi_args = xcalloc(n_args, sizeof(ffi_type *));
        gcb->atypes = ffi_args;

        for (int i = 0; i < n_args; i++)
            ffi_args[i] = amap_entry_to_ffi_type(&gcb->amap->pdata[i]);
    }

    GITypeInfo *ret_type_info = g_callable_info_get_return_type(callback_info);
    ffi_ret_type = amap_entry_to_ffi_type(&gcb->amap->return_val);
    g_base_info_unref(ret_type_info);

    // Initialize the CIF Call Interface Struct.
    ffi_status prep_ok;
    prep_ok = ffi_prep_cif(&(gcb->cif), FFI_DEFAULT_ABI, n_args, ffi_ret_type, ffi_args);

    if (prep_ok != FFI_OK)
        scm_misc_error("gig-callback-new",
                       "closure call interface preparation error #~A",
                       scm_list_1(scm_from_int(prep_ok)));

    // STEP 3
    // Initialize the closure
    ffi_status closure_ok;
    closure_ok = ffi_prep_closure_loc(gcb->closure, &(gcb->cif), callback_binding, gcb,
                                      gcb->callback_ptr);

    if (closure_ok != FFI_OK)
        scm_misc_error("gig-callback-new",
                       "closure location preparation error #~A",
                       scm_list_1(scm_from_int(closure_ok)));

    return gcb;
}

static GigCallback *
gig_callback_new_for_callback(GICallbackInfo *info, void *c_func)
{
    int n_args = g_callable_info_get_n_args(info);

    // we only take one arg now, the scm arg list
    n_args = n_args == 0 ? 0 : 1;

    GigCallback *gcb = xcalloc(1, sizeof(GigCallback));

    gcb->name = NULL;

    gcb->c_func = c_func;
    gcb->callback_info = g_base_info_ref(info);
    gcb->amap = gig_amap_new(gcb->name, gcb->callback_info);

    if (n_args > 0) {
        gcb->atypes = xcalloc(1, sizeof(ffi_type *));
        gcb->atypes[0] = &ffi_type_pointer;
    }

    ffi_status prep_ok, closure_ok;
    prep_ok = ffi_prep_cif(&(gcb->cif), FFI_DEFAULT_ABI, n_args, &ffi_type_pointer, gcb->atypes);
    gig_return_val_if_fail(prep_ok == FFI_OK, NULL);

    gcb->closure = ffi_closure_alloc(sizeof(ffi_closure), &(gcb->callback_ptr));
    closure_ok = ffi_prep_closure_loc(gcb->closure, &(gcb->cif), c_callback_binding, gcb,
                                      gcb->callback_ptr);
    gig_return_val_if_fail(closure_ok == FFI_OK, NULL);

    return gcb;
}

void *
gig_callback_to_c(const char *name, GICallbackInfo *cb_info, SCM s_func)
{
    assert(cb_info != NULL);
    assert(scm_is_procedure(s_func));

    // Lookup s_func in the callback cache.
    slist_t *x = callback_list;
    GigCallback *gcb;

    // A callback is only a 'match' if it is the same Scheme produre
    // as well as the same GObject C Callback type.
    while (x != NULL) {
        gcb = x->data;
        if (scm_is_eq(gcb->s_func, s_func))
            return gcb->callback_ptr;
        x = x->next;
    }

    // Create a new entry if necessary.
    scm_gc_protect_object(s_func);
    gcb = gig_callback_new(name, cb_info, s_func);
    slist_prepend(&callback_list, gcb);
    return gcb->callback_ptr;
}

SCM
gig_callback_to_scm(const char *name, GICallbackInfo *info, void *callback)
{
    // we probably shouldn't cache this, because C callbacks can be
    // invalidated
    GigCallback *gcb = gig_callback_new_for_callback(info, callback);
    if (gcb == NULL)
        return SCM_BOOL_F;
    char *subr_name = concatenate("c-callback:", name);
    SCM subr = scm_c_make_gsubr(subr_name, 0, 0, 1, gcb->callback_ptr);
    free(subr_name);
    return subr;
}

static ffi_type *
amap_entry_to_ffi_type(GigArgMapEntry *entry)
{
    switch (entry->s_direction) {
    case GIG_ARG_DIRECTION_INOUT:
    case GIG_ARG_DIRECTION_OUTPUT:
    case GIG_ARG_DIRECTION_PREALLOCATED_OUTPUT:
        return &ffi_type_pointer;
    default:
        break;
    }

    if (entry->meta.is_ptr)
        return &ffi_type_pointer;
    else {
        GType fundamental_type = G_TYPE_FUNDAMENTAL(entry->meta.gtype);
        if (fundamental_type == G_TYPE_NONE)
            return &ffi_type_void;
        else if (fundamental_type == G_TYPE_BOOLEAN)
            return &ffi_type_sint;
        else if (fundamental_type == G_TYPE_CHAR)
            return &ffi_type_sint8;
        else if (fundamental_type == G_TYPE_UCHAR)
            return &ffi_type_uint8;
        else if (fundamental_type == G_TYPE_INT)
            switch (entry->meta.item_size) {
            case 1:
                return &ffi_type_sint8;
            case 2:
                return &ffi_type_sint16;
            case 4:
                return &ffi_type_sint32;
            case 8:
                return &ffi_type_sint64;
            default:
                assert_not_reached();
            }
        else if (fundamental_type == G_TYPE_UINT)
            switch (entry->meta.item_size) {
            case 1:
                return &ffi_type_uint8;
            case 2:
                return &ffi_type_uint16;
            case 4:
                return &ffi_type_uint32;
            case 8:
                return &ffi_type_uint64;
            default:
                assert_not_reached();
            }
        else if (fundamental_type == G_TYPE_INT64)
            return &ffi_type_sint64;
        else if (fundamental_type == G_TYPE_UINT64)
            return &ffi_type_uint64;
        else if (fundamental_type == G_TYPE_FLOAT)
            return &ffi_type_float;
        else if (fundamental_type == G_TYPE_DOUBLE)
            return &ffi_type_double;
        else if (fundamental_type == G_TYPE_GTYPE)
            switch (entry->meta.item_size) {
            case 4:
                return &ffi_type_sint32;
            case 8:
                return &ffi_type_sint64;
            default:
                assert_not_reached();
            }
        else if (fundamental_type == G_TYPE_FLAGS)
            return &ffi_type_uint32;
        else if (fundamental_type == G_TYPE_ENUM)
            return &ffi_type_sint32;
        else
            assert_not_reached();
    }
}

static SCM
scm_is_registered_callback_p(SCM s_proc)
{
    if (!scm_is_procedure(s_proc))
        scm_wrong_type_arg_msg("is-registered-callback?", 0, s_proc, "procedure");

    // Lookup s_func in the callback cache.
    slist_t *x = callback_list;
    GigCallback *gcb;

    while (x != NULL) {
        gcb = x->data;
        if (scm_is_eq(gcb->s_func, s_proc)) {
            return SCM_BOOL_T;
        }
        x = x->next;
    }
    return SCM_BOOL_F;
}

static SCM
scm_get_registered_callback_closure_pointer(SCM s_proc)
{
    if (!scm_is_procedure(s_proc))
        scm_wrong_type_arg_msg("get-registered-callback-closure-pointer", 0, s_proc, "procedure");

    // Lookup s_func in the callback cache.
    slist_t *x = callback_list;
    GigCallback *gcb;

    // If you use the same scheme procedure for different callbacks,
    // you're just going to get one closure pointer.
    while (x != NULL) {
        gcb = x->data;
        if (scm_is_eq(gcb->s_func, s_proc))
            return scm_from_pointer(gcb->callback_ptr, NULL);
        x = x->next;
    }
    return SCM_BOOL_F;
}

void
gig_init_callback(void)
{
    atexit(gig_fini_callback);

#if HAVE_SCM_HOOKS
    gig_before_c_callback_hook = scm_permanent_object(scm_make_hook(scm_from_size_t(3)));
    gig_before_callback_hook = scm_permanent_object(scm_make_hook(scm_from_size_t(3)));
    scm_c_define("%before-c-callback-hook", gig_before_c_callback_hook);
    scm_c_define("%before-callback-hook", gig_before_callback_hook);
#endif

    gig_callback_thread_fluid = scm_permanent_object(scm_make_thread_local_fluid(SCM_BOOL_F));
    scm_fluid_set_x(gig_callback_thread_fluid, SCM_BOOL_T);

    scm_c_define("%callback-thread-fluid", gig_callback_thread_fluid);

    scm_c_define_gsubr("is-registered-callback?", 1, 0, 0, scm_is_registered_callback_p);
    scm_c_define_gsubr("get-registered-callback-closure-pointer", 1, 0, 0,
                       scm_get_registered_callback_closure_pointer);
}

static void
callback_free(GigCallback *gcb)
{
    ffi_closure_free(gcb->closure);
    gcb->closure = NULL;

    gig_amap_free(gcb->amap);
    g_base_info_unref(gcb->callback_info);
    free(gcb->atypes);
    gcb->atypes = NULL;

    if (gcb->name) {
        free(gcb->name);
        gcb->name = NULL;
        // only Scheme callbacks have names and those callbacks need to
        // be GC'd.
        scm_gc_unprotect_object(gcb->s_func);
    }

    free(gcb);
}

inline static void
callback_free_v(void *gcb)
{
    callback_free(gcb);
}

static void
gig_fini_callback(void)
{
    gig_debug("Freeing callbacks");
    slist_free(&callback_list, callback_free_v);
    callback_list = NULL;
}
