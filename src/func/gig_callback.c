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
#include <string.h>
#include <libguile.h>
#include <libguile/hooks.h>
#include <ffi.h>
#include "../core.h"
#include "../type.h"
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
    // For a virtual function: its first C argument is the instance,
    // which is not in the argument map.
    bool is_method;
    // Its last C argument is a GError ** that the argument map
    // doesn't describe either.
    bool throws;
};

static slist_t *callback_list = NULL;

#if HAVE_SCM_HOOKS
SCM gig_before_c_callback_hook;
SCM gig_before_callback_hook;
#endif
SCM gig_callback_thread_fluid;

static ffi_type *amap_entry_to_ffi_type(GigArgMapEntry *entry);
static ffi_type *meta_to_ffi_type(GigTypeMeta *meta);
static void callback_free(GigCallback *gcb);
static void gig_fini_callback(void);

static void
convert_ffi_arg_to_giargument(void *_ffi_arg, ffi_type *arg_type, bool unpack, GIArgument *giarg)
{
    if (unpack)
        _ffi_arg = ((void **)_ffi_arg)[0];

    // _FFI_ARG points to the argument (or, unpacked, is the pointer).
    if (arg_type == &ffi_type_pointer)
        giarg->v_pointer = unpack ? _ffi_arg : *(void **)_ffi_arg;
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
    // The caller may pass NULL for an optional output.
    if (*arg == NULL)
        return;
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
    case G_TYPE_ENUM:
        **(int **)arg = value->v_int;
        break;
    case G_TYPE_FLAGS:
        **(unsigned **)arg = value->v_uint;
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

// A struct that C lent to a callback by pointer.  Scheme gets a copy,
// since it may keep its object longer than C keeps the struct, so
// what the callback did to the copy is carried back afterwards.
typedef struct
{
    void *c_struct;
    SCM s_struct;
    size_t size;
    bool is_value;
} LentStruct;

// True if ENTRY is a struct passed by pointer that a callback may be
// meant to fill in or modify: a caller-allocated output, like the
// GtkTreeIter of GtkTreeModel's get_iter, or an input, like the one
// its iter_next advances.
static bool
is_lent_struct(GigArgMapEntry *entry)
{
    GigTypeMeta *meta = &entry->meta;
    GType gtype = meta->gtype;
    bool preallocated = (entry->s_direction == GIG_ARG_DIRECTION_PREALLOCATED_OUTPUT);
    if (entry->s_direction != GIG_ARG_DIRECTION_INPUT && !preallocated)
        return false;
    // The typelib doesn't count a caller-allocated output as a pointer.
    if (!(meta->is_ptr || preallocated) || G_TYPE_FUNDAMENTAL(gtype) != G_TYPE_BOXED)
        return false;
    if (gtype == G_TYPE_ARRAY || gtype == G_TYPE_BYTE_ARRAY || gtype == G_TYPE_PTR_ARRAY
        || gtype == G_TYPE_HASH_TABLE)
        return false;
    if (gtype == G_TYPE_VALUE)
        // An input GValue is usually const.
        return preallocated;
    return meta->item_size > 0 && meta->transfer == GI_TRANSFER_NOTHING;
}

static void
return_lent_structs(LentStruct *lent, unsigned n)
{
    for (unsigned i = 0; i < n; i++) {
        void *copy = gig_type_peek_object(lent[i].s_struct);
        if (copy == NULL)
            continue;
        if (lent[i].is_value) {
            GValue *src = copy, *dest = lent[i].c_struct;
            if (G_VALUE_TYPE(src) != G_TYPE_INVALID) {
                if (G_VALUE_TYPE(dest) != G_TYPE_INVALID)
                    g_value_unset(dest);
                g_value_init(dest, G_VALUE_TYPE(src));
                g_value_copy(src, dest);
            }
        }
        // Write only what changed: an input may be read-only.
        else if (memcmp(lent[i].c_struct, copy, lent[i].size) != 0)
            memcpy(lent[i].c_struct, copy, lent[i].size);
    }
}

static SCM
apply_body(void *data)
{
    SCM *proc_and_args = data;
    return scm_apply_0(proc_and_args[0], proc_and_args[1]);
}

static SCM
catch_handler(void *data, SCM key, SCM args)
{
    *(SCM *)data = scm_cons(key, args);
    return SCM_UNSPECIFIED;
}

// A readable message for the exception (KEY . ARGS), which usually
// are (SUBR MESSAGE FORMAT-ARGS DATA).
static SCM
exception_message(SCM exception)
{
    SCM key = scm_car(exception), args = scm_cdr(exception);
    if (scm_ilength(args) >= 3 && scm_is_string(scm_cadr(args))
        && scm_is_true(scm_list_p(scm_caddr(args))))
        return scm_simple_format(SCM_BOOL_F, scm_cadr(args), scm_caddr(args));
    return scm_simple_format(SCM_BOOL_F, scm_from_utf8_string("~A: ~S"), scm_list_2(key, args));
}

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
    ffi_type **arg_types = cif->arg_types;
    SCM s_self = SCM_UNDEFINED;

    scm_load_goops();

    // A virtual function's instance comes first; the argument map
    // describes the rest.  Pass the instance as the first Scheme
    // argument, and index the other arguments from after it.
    if (gcb->is_method) {
        void *self = *(void **)ffi_args[0];
        if (self == NULL)
            s_self = SCM_BOOL_F;
        else
            s_self = gig_type_transfer_object(G_TYPE_FROM_INSTANCE(self), self,
                                              GI_TRANSFER_NOTHING);
        ffi_args++;
        arg_types++;
        n_args--;
    }
    // A function that can fail reports Scheme errors through its
    // GError ** argument, which comes last.
    GError **error = NULL;
    if (gcb->throws) {
        error = *(GError ***)ffi_args[n_args - 1];
        n_args--;
    }

    assert(scm_is_procedure(gcb->s_func));
    assert(n_args == g_callable_info_get_n_args(gcb->callback_info));
    assert(gcb->amap != NULL);

    GigArgMap *amap = gcb->amap;
    char *callback_name;
    if (amap->name)
        callback_name = concatenate3("callback:<", amap->name, ">");
    else
        callback_name = xstrdup("callback");

    // Conversions and the callback itself can raise a Scheme error.
    scm_dynwind_begin(0);
    scm_dynwind_free(callback_name);

    // Scanned by the garbage collector, which keeps the copies alive.
    LentStruct *lent = scm_gc_malloc(sizeof(LentStruct) * (n_args + 1), "callback structs");
    unsigned n_lent = 0;

    // Do the two-step conversion from libffi arguments to GIArgument
    // to SCM arguments.
    for (unsigned i = 0; i < n_args; i++) {
        SCM s_entry = SCM_BOOL_F;
        GIArgument giarg;

        if (!amap->pdata[i].is_s_input)
            continue;

        if (amap->pdata[i].s_direction == GIG_ARG_DIRECTION_INOUT)
            // The argument points to the value.
            convert_ffi_arg_to_giargument(*(void **)ffi_args[i],
                                          meta_to_ffi_type(&amap->pdata[i].meta),
                                          amap->pdata[i].meta.is_ptr, &giarg);
        else
            convert_ffi_arg_to_giargument(ffi_args[i], arg_types[i], amap->pdata[i].meta.is_ptr,
                                          &giarg);
        // An array whose length is another argument.
        size_t size = GIG_ARRAY_SIZE_UNKNOWN;
        GigArgMapEntry *size_entry = amap->pdata[i].child;
        if (amap->pdata[i].meta.has_size && size_entry != NULL
            && size_entry->s_direction == GIG_ARG_DIRECTION_INPUT) {
            GIArgument size_arg;
            SCM s_size = SCM_BOOL_F;
            int k = size_entry->i;
            convert_ffi_arg_to_giargument(ffi_args[k], arg_types[k], size_entry->meta.is_ptr,
                                          &size_arg);
            gig_argument_c_to_scm(callback_name, k, &size_entry->meta, &size_arg, &s_size, -1);
            if (scm_is_unsigned_integer(s_size, 0, SIZE_MAX))
                size = scm_to_size_t(s_size);
        }

        gig_argument_c_to_scm(callback_name, i, &amap->pdata[i].meta, &giarg, &s_entry, size);
        s_args = scm_cons(s_entry, s_args);

        if (giarg.v_pointer != NULL && SCM_INSTANCEP(s_entry) && is_lent_struct(&amap->pdata[i])) {
            void *copy = gig_type_peek_object(s_entry);
            bool is_value = (amap->pdata[i].meta.gtype == G_TYPE_VALUE);
            size_t size = amap->pdata[i].meta.item_size;
            // Bytes can only be carried back to a struct whose copy
            // is its bytes, not one that owns what it points to.
            if (copy != NULL && copy != giarg.v_pointer
                && (is_value || memcmp(giarg.v_pointer, copy, size) == 0)) {
                lent[n_lent].c_struct = giarg.v_pointer;
                lent[n_lent].s_struct = s_entry;
                lent[n_lent].size = size;
                lent[n_lent].is_value = is_value;
                n_lent++;
            }
        }
    }
    s_args = scm_reverse_x(s_args, SCM_EOL);
    size_t length = scm_c_length(s_args);
    if (gcb->is_method)
        s_args = scm_cons(s_self, s_args);

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
    else if (gcb->throws) {
        SCM caught = SCM_BOOL_F;
        s_ret = scm_internal_catch(SCM_BOOL_T, apply_body, &(SCM[2]) { gcb->s_func, s_args },
                                   catch_handler, &caught);
        if (scm_is_true(caught)) {
            // Return zeros (FALSE or NULL) with the error set.
            char *message = scm_to_utf8_string(exception_message(caught));
            g_set_error_literal(error, g_quark_from_static_string("guile-gi-scheme-error"), 0,
                                message);
            free(message);
            memset(ret, 0, cif->rtype->size > sizeof(ffi_arg) ? cif->rtype->size
                   : sizeof(ffi_arg));
            scm_dynwind_end();
            return (void *)1;
        }
    }
    else
        s_ret = scm_apply_0(gcb->s_func, s_args);

    return_lent_structs(lent, n_lent);

    // Return values and output arguments start here.  A caller-allocated
    // output is not among them: the callback filled in its argument.
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

            if (amap->return_val.meta.has_size && amap->return_val.child) {
                // ffi_args is indexed by position in the callback's
                // signature, not by output position.
                GIArgument tmp;
                tmp.v_int64 = size;
                store_output(amap->return_val.child, ffi_args[amap->return_val.child->i], &tmp);
            }
        }

        int n_preallocated = 0;
        for (int c_output_pos = 0; c_output_pos < out; c_output_pos++) {
            GigArgMapEntry *entry = gig_amap_get_output_entry_by_c(amap, c_output_pos);
            if (!entry->is_s_output)
                continue;
            if (entry->s_direction == GIG_ARG_DIRECTION_PREALLOCATED_OUTPUT) {
                n_preallocated++;
                continue;
            }
            int real_cpos = entry - amap->pdata;
            int s_output_pos = entry->s_output_pos - n_preallocated;
            if (s_output_pos + start >= n_values)
                scm_misc_error(callback_name, "too few return values", SCM_EOL);
            SCM real_value = scm_c_value_ref(s_ret, s_output_pos + start);
            gig_argument_scm_to_c(callback_name, real_cpos, &entry->meta, real_value, NULL, &giarg,
                                  &size);
            // ffi_args is indexed by position in the callback's
            // signature (real_cpos), not by output position.
            store_output(entry, ffi_args[real_cpos], &giarg);

            if (entry->meta.has_size && entry->child) {
                GIArgument tmp;
                tmp.v_int64 = size;
                store_output(entry->child, ffi_args[entry->child->i], &tmp);
            }
        }
    }
    scm_dynwind_end();
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
    // Virtual functions take their instance first.
    int offset = g_callable_info_is_method(callback_info) ? 1 : 0;
    gcb->is_method = offset;
    gcb->throws = g_callable_info_can_throw_gerror(callback_info);
    int n_c_args = offset + n_args + (gcb->throws ? 1 : 0);

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
    if (gcb->amap == NULL)
        gig_error("%s: callback type %s cannot be represented", name,
                  g_base_info_get_name(callback_info));

    // STEP 1
    // Allocate the block of memory that FFI uses to hold a closure object,
    // and set a pointer to the corresponding executable address.
    gcb->closure = ffi_closure_alloc(sizeof(ffi_closure), &(gcb->callback_ptr));

    assert(gcb->closure != NULL);
    assert(gcb->callback_ptr != NULL);

    // STEP 2
    // Next, we begin to construct an FFI_CIF to describe the function call.

    // Initialize the argument info vectors.
    if (n_c_args > 0) {
        ffi_args = xcalloc(n_c_args, sizeof(ffi_type *));
        gcb->atypes = ffi_args;

        if (offset)
            ffi_args[0] = &ffi_type_pointer;
        for (int i = 0; i < n_args; i++)
            ffi_args[i + offset] = amap_entry_to_ffi_type(&gcb->amap->pdata[i]);
        if (gcb->throws)
            ffi_args[n_c_args - 1] = &ffi_type_pointer;    // GError **
    }

    GITypeInfo *ret_type_info = g_callable_info_get_return_type(callback_info);
    ffi_ret_type = amap_entry_to_ffi_type(&gcb->amap->return_val);
    g_base_info_unref(ret_type_info);

    // Initialize the CIF Call Interface Struct.
    ffi_status prep_ok;
    prep_ok = ffi_prep_cif(&(gcb->cif), FFI_DEFAULT_ABI, n_c_args, ffi_ret_type, ffi_args);

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
    if (gcb->amap == NULL) {
        g_base_info_unref(gcb->callback_info);
        free(gcb);
        return NULL;
    }

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
        if (scm_is_eq(gcb->s_func, s_func) && g_base_info_equal(gcb->callback_info, cb_info))
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
    return meta_to_ffi_type(&entry->meta);
}

// The FFI type of a value of type META.
static ffi_type *
meta_to_ffi_type(GigTypeMeta *meta)
{
    if (meta->is_ptr)
        return &ffi_type_pointer;
    else {
        GType fundamental_type = G_TYPE_FUNDAMENTAL(meta->gtype);
        if (fundamental_type == G_TYPE_NONE)
            return &ffi_type_void;
        else if (fundamental_type == G_TYPE_BOOLEAN)
            return &ffi_type_sint;
        else if (fundamental_type == G_TYPE_CHAR)
            return &ffi_type_sint8;
        else if (fundamental_type == G_TYPE_UCHAR)
            return &ffi_type_uint8;
        else if (fundamental_type == G_TYPE_INT)
            switch (meta->item_size) {
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
            switch (meta->item_size) {
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
            switch (meta->item_size) {
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
        else if (fundamental_type == G_TYPE_POINTER || fundamental_type == G_TYPE_STRING
                 || fundamental_type == G_TYPE_BOXED || fundamental_type == G_TYPE_OBJECT
                 || fundamental_type == G_TYPE_INTERFACE || fundamental_type == G_TYPE_VARIANT
                 || fundamental_type == G_TYPE_PARAM)
            // e.g. a callback argument, whose type info isn't a pointer
            return &ffi_type_pointer;
        else
            assert_not_reached();
    }
    return &ffi_type_pointer;
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
