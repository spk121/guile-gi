// Copyright (C) 2018, 2019, 2020, 2021, 2022 Michael L. Gran

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
#include <ffi.h>
#include <stdbool.h>
#include <libguile/hooks.h>
#include "../type.h"
#include "gig_argument.h"
#include "gig_args_store.h"
#include "gig_util_priv.h"
#include "gig_arg_map_priv.h"
#include "gig_function_priv.h"
#include "gig_lib.h"
#include "gig_invoker.h"

typedef struct GigFunction_
{
    void *handle;
    ffi_closure *closure;
    ffi_cif cif;
    void *function_ptr;
    char *name;
    ffi_type **atypes;
    GigArgMap *amap;
} GigFunction;

static strval_t *function_cache = NULL;

SCM top_type;
SCM sym_self;

const char toxic_symbols[1][25] = {
    "g_thread_exit"
};

#define N_TOXIC_SYMBOLS 1

#if HAVE_SCM_HOOKS
SCM gig_before_function_hook;
#endif

static GigGsubr *check_gsubr_cache(const char *key, SCM self_type,
                                   int *required_input_count, int *optional_input_count,
                                   SCM *formals, SCM *specializers);
static GigGsubr *create_gsubr(void *handle, GigArgMap *amap, const char *name, const char *key,
                              SCM self_type, int *required_input_count, int *optional_input_count,
                              SCM *formals, SCM *specializers);
static void make_formals(GigArgMap *, int n_inputs, SCM self_type, SCM *formals,
                         SCM *specializers);
static void function_binding(ffi_cif *cif, void *ret, void **ffi_args, void *user_data);
static SCM function_invoke(void *handle, GigArgMap *amap, const char *name,
                           GObject *object, SCM args, GError **error);
static void function_free(GigFunction *fn);
static void gig_fini_function(void);

static SCM proc4function(GigArgMap *amap, const char *name, SCM self_type,
                         const char *namespace_name, const char *symbol, int *req, int *opt,
                         SCM *formals, SCM *specs);
static SCM proc4signal(GigArgMap *amap, const char *name, const char *symbol, SCM self_type,
                       int *req, int *opt, SCM *formals, SCM *specs);

// Returns a list of GTypes that used by this function call
GType *
gig_function_get_arg_gtypes(GICallableInfo *info, size_t *len)
{
    GigArgMap *amap;
    GType *types;

    amap = gig_amap_new(g_base_info_get_name(info), info);
    *len = 0;
    if (amap == NULL)
        return NULL;
    types = gig_amap_get_gtype_list(amap, len);
    gig_amap_free(amap);
    return types;
}

SCM
gig_function_define_full(const char *namespace_, size_t gtype, const char *long_name,
                         const char *short_name, const char *symbol, GigArgMap *amap)
{
    int required_input_count, optional_input_count;
    SCM formals, specializers, self_type = SCM_UNDEFINED;
    SCM proc = SCM_UNDEFINED;
    SCM def;
    SCM defs = SCM_EOL;
    bool is_toxic = false;

    for (int i = 0; i < N_TOXIC_SYMBOLS; i++) {
        if (strcmp(symbol, toxic_symbols[i]) == 0) {
            is_toxic = true;
            break;
        }
    }

    gig_debug_load("%s - bound to %s %s%s%s%s%s",
                   long_name,
                   (amap->is_method ? "method" : "function"),
                   (namespace_ ? namespace_ : ""),
                   (namespace_ ? " " : ""),
                   (gtype > 4 ? g_type_name(gtype) : ""), (gtype > 4 ? " " : ""), symbol);

    if (amap->is_method)
        self_type = gig_type_get_scheme_type(gtype);

    proc = proc4function(amap, long_name, self_type, namespace_, symbol,
                         &required_input_count, &optional_input_count, &formals, &specializers);
    if (SCM_UNBNDP(proc))
        goto end;

    def = scm_define_methods_from_procedure(long_name, proc, optional_input_count, formals,
                                            specializers);
    if (!SCM_UNBNDP(def))
        defs = scm_cons(def, defs);

    if (!is_toxic) {
        gig_debug_load("%s - shorthand for %s", short_name, long_name);
        def = scm_define_methods_from_procedure(short_name, proc, optional_input_count, formals,
                                                specializers);
        if (!SCM_UNBNDP(def))
            defs = scm_cons(def, defs);
    }

  end:
    return defs;
}

SCM
gig_signal_define_full(const char *namespace_, size_t gtype, const char *long_name,
                       const char *short_name, const char *symbol, GigArgMap *amap)
{
    int required_input_count, optional_input_count;
    SCM formals, specializers, self_type = SCM_UNDEFINED;
    SCM proc = SCM_UNDEFINED;
    SCM def;
    SCM defs = SCM_EOL;

    gig_debug_load("%s - bound to signal %s%s%s%s%s",
                   long_name,
                   (namespace_ ? namespace_ : ""),
                   (namespace_ ? " " : ""),
                   (gtype > 4 ? g_type_name(gtype) : ""), (gtype > 4 ? " " : ""), symbol);
    gig_debug_load("%s - shorthand for %s", short_name, long_name);

    self_type = gig_type_get_scheme_type(gtype);

    proc = proc4signal(amap, long_name, symbol, self_type,
                       &required_input_count, &optional_input_count, &formals, &specializers);
    if (SCM_UNBNDP(proc))
        goto end;

    def = scm_define_methods_from_procedure(long_name, proc, optional_input_count, formals,
                                            specializers);
    if (!SCM_UNBNDP(def))
        defs = scm_cons(def, defs);

    def = scm_define_methods_from_procedure(short_name, proc, optional_input_count, formals,
                                            specializers);
    if (!SCM_UNBNDP(def))
        defs = scm_cons(def, defs);

  end:
    return defs;
}

static char *
make_cache_key(const char *namespace_, const char *symbol, bool is_method)
{
    uint32_t x = 0;
    size_t len;
    char str[8];
    int i;

    // A half-assed attempt at making function keys quicker to look
    // up.
    len = strlen(symbol);
    for (i = 0; i < len; i++)
        x = 37u * x + (uint32_t)symbol[i];
    len = strlen(namespace_);
    for (i = 0; i < len; i++)
        x = 37u * x + (uint32_t)namespace_[i];
    for (i = 0; i < 6; i++) {
        str[i] = (x & 0x1F) + 48;
        x >>= 5;
    }

    // Disambiguate the rare case where the same C function is defined
    // both as a method and as a function.
    if (is_method)
        str[6] = 'M';
    else
        str[6] = 'F';
    str[7] = '\0';

    return concatenate3(str, symbol, namespace_);
}

static SCM
proc4function(GigArgMap *amap, const char *name, SCM self_type, const char *namespace_,
              const char *symbol, int *req, int *opt, SCM *formals, SCM *specializers)
{
    char *key;
    GigGsubr *func_gsubr;
    void *handle;

    // Is this already defined?
    key = make_cache_key(namespace_, symbol, amap->is_method);
    func_gsubr = check_gsubr_cache(key, self_type, req, opt, formals, specializers);

    // Find the C function
    handle = gig_lib_lookup(namespace_, symbol);
    if (!handle) {
        gig_debug_load("%s - could not find symbol %s in namespace %s", name, symbol, namespace_);
        return SCM_UNDEFINED;
    }

    if (!func_gsubr)
        func_gsubr =
            create_gsubr(handle, amap, name, key, self_type, req, opt, formals, specializers);

    if (!func_gsubr) {
        gig_debug_load("%s - could not create a gsubr", name);
        return SCM_UNDEFINED;
    }

    return scm_c_make_gsubr(name, 0, 0, 1, func_gsubr);
}

static SCM
proc4signal(GigArgMap *amap, const char *name, const char *symbol, SCM self_type, int *req,
            int *opt, SCM *formals, SCM *specializers)
{
    gig_amap_s_input_count(amap, req, opt);
    (*req)++;

    make_formals(amap, *req + *opt, self_type, formals, specializers);

    GigSignalSlot slots[] = { GIG_SIGNAL_SLOT_NAME, GIG_SIGNAL_SLOT_OUTPUT_MASK };
    SCM values[2];

    // use base_info name without transformations, otherwise we could screw things up
    values[0] = scm_from_utf8_string(symbol);
    values[1] = scm_c_make_bitvector(*req + *opt, SCM_BOOL_F);

    size_t offset, length;
    ssize_t pos = 0, inc;
    scm_t_array_handle handle;
    uint32_t *bits = scm_bitvector_writable_elements(values[1], &handle, &offset, &length, &inc);
    pos = offset + inc;

    /* Set up output mask.
     * This does not seem to affect argument handling all that much, but
     * we can probably take some work off the user when it comes to setting up
     * handlers.
     */
    for (int i = 1; i < *req + *opt; i++, pos += inc) {
        size_t word_pos = pos / 32;
        size_t mask = 1L << (pos % 32);
        GigArgMapEntry *entry = gig_amap_get_input_entry_by_s(amap, i - 1);
        if (entry->is_s_output)
            bits[word_pos] |= mask;
    }
    scm_array_handle_release(&handle);
    gig_amap_free(amap);

    SCM signal = gig_make_signal(2, slots, values);

    // check for collisions
    SCM current_definition = scm_current_module_definition(scm_from_utf8_symbol(name));
    if (scm_is_true(current_definition))
        for (SCM iter = scm_generic_function_methods(current_definition);
             scm_is_pair(iter); iter = scm_cdr(iter))
            if (scm_is_equal(*specializers, scm_method_specializers(scm_car(iter)))) {
                // we'd be overriding an already defined generic method, let's not do that
                scm_slot_set_x(signal, scm_from_utf8_symbol("procedure"),
                               scm_method_procedure(scm_car(iter)));
                break;
            }

    return signal;
}

static GigGsubr *
check_gsubr_cache(const char *key, SCM self_type, int *required_input_count,
                  int *optional_input_count, SCM *formals, SCM *specializers)
{
    // Check the cache to see if this function has already been created.
    GigFunction *gfn = (GigFunction *)strval_find_entry(function_cache, key);

    if (gfn == NULL)
        return NULL;

    gig_amap_s_input_count(gfn->amap, required_input_count, optional_input_count);

    if (gfn->amap->is_method)
        (*required_input_count)++;

    make_formals(gfn->amap,
                 *required_input_count + *optional_input_count, self_type, formals, specializers);

    return gfn->function_ptr;
}

SCM char_type;
SCM list_type;
SCM string_type;

// This computes the specializer for an argument of a method.  Notably
// here are special cases when using the direct GType-to-SCM class
// mapping would give unfortunate results.
static SCM
type_specializer(GigTypeMeta *meta)
{
    GigArgType t = meta->arg_type;

    // Return a specific class for arguments that can only have one
    // type scheme representation.
    switch (t) {
    case GIG_ARG_TYPE_UNKNOWN:
    case GIG_ARG_TYPE_VOID:
        return scm_get_unknown_class();
    case GIG_ARG_TYPE_INT8:
    case GIG_ARG_TYPE_UINT8:
        // either chars or integer
        return scm_get_unknown_class();
    case GIG_ARG_TYPE_INT16:
    case GIG_ARG_TYPE_UINT16:
    case GIG_ARG_TYPE_INT32:
    case GIG_ARG_TYPE_UINT32:
    case GIG_ARG_TYPE_INT64:
    case GIG_ARG_TYPE_UINT64:
        if (!meta->is_ptr)
            return scm_get_integer_class();
    case GIG_ARG_TYPE_FLOAT:
    case GIG_ARG_TYPE_DOUBLE:
        if (!meta->is_ptr)
            return scm_get_real_class();
    case GIG_ARG_TYPE_UTF8_STRING:
    case GIG_ARG_TYPE_LOCALE_STRING:
        // bytevectors or strings
        return scm_get_unknown_class();
    case GIG_ARG_TYPE_CALLBACK:
        return scm_get_applicable_class();
    case GIG_ARG_TYPE_ARRAY:
    case GIG_ARG_TYPE_GARRAY:
    case GIG_ARG_TYPE_GBYTEARRAY:
    case GIG_ARG_TYPE_GPTRARRAY:
        // a few differnt ways to represent arrays
        return scm_get_unknown_class();
    case GIG_ARG_TYPE_POINTER:
        // foreign pointer or bytevector
        return scm_get_unknown_class();
    default:
        if (meta->gtype)
            return gig_type_get_scheme_type(meta->gtype);
        return scm_get_unknown_class();
    }
}

static void
make_formals(GigArgMap *argmap, int n_inputs, SCM self_type, SCM *formals, SCM *specializers)
{
    SCM i_formal, i_specializer;

    i_formal = *formals = scm_make_list(scm_from_int(n_inputs), SCM_BOOL_F);
    i_specializer = *specializers = scm_make_list(scm_from_int(n_inputs), top_type);

    if (argmap->is_method) {
        scm_set_car_x(i_formal, sym_self);
        scm_set_car_x(i_specializer, self_type);

        i_formal = scm_cdr(i_formal);
        i_specializer = scm_cdr(i_specializer);
        n_inputs--;
    }

    for (int s = 0; s < n_inputs;
         s++, i_formal = scm_cdr(i_formal), i_specializer = scm_cdr(i_specializer)) {
        GigArgMapEntry *entry = gig_amap_get_input_entry_by_s(argmap, s);
        char *formal = make_scm_name(entry->name);
        scm_set_car_x(i_formal, scm_from_utf8_symbol(formal));
        // Don't force types on nullable input, as #f can also be used to represent
        // NULL.
        if (entry->meta.is_nullable)
            continue;

        SCM s_type = type_specializer(&entry->meta);
        if (!scm_is_unknown_class(s_type))
            scm_set_car_x(i_specializer, s_type);
    }
}

static GigGsubr *
create_gsubr(void *handle, GigArgMap *amap, const char *name, const char *key, SCM self_type,
             int *required_input_count, int *optional_input_count, SCM *formals, SCM *specializers)
{
    GigFunction *gfn;
    ffi_type *ffi_ret_type;

    gfn = xcalloc(1, sizeof(GigFunction));
    gfn->handle = handle;
    gfn->amap = amap;
    free(gfn->name);
    gfn->name = xstrdup(name);

    gig_amap_s_input_count(gfn->amap, required_input_count, optional_input_count);

    if (amap->is_method)
        (*required_input_count)++;

    make_formals(gfn->amap, *required_input_count + *optional_input_count,
                 self_type, formals, specializers);

    // STEP 1
    // Allocate the block of memory that FFI uses to hold a closure
    // object, and set a pointer to the corresponding executable
    // address.
    gfn->closure = ffi_closure_alloc(sizeof(ffi_closure), &(gfn->function_ptr));

    g_return_val_if_fail(gfn->closure != NULL, NULL);
    g_return_val_if_fail(gfn->function_ptr != NULL, NULL);

    // STEP 2
    // Next, we begin to construct an FFI_CIF to describe the function
    // call.

    // Initialize the argument info vectors.
    int have_args = 0;
    if (*required_input_count + *optional_input_count > 0) {
        gfn->atypes = xcalloc(1, sizeof(ffi_type *));
        gfn->atypes[0] = &ffi_type_pointer;
        have_args = 1;
    }
    else
        gfn->atypes = NULL;

    // The return type is also SCM, for which we use a pointer.
    ffi_ret_type = &ffi_type_pointer;

    // Initialize the CIF Call Interface Struct.
    ffi_status prep_ok;
    prep_ok = ffi_prep_cif(&(gfn->cif), FFI_DEFAULT_ABI, have_args, ffi_ret_type, gfn->atypes);

    if (prep_ok != FFI_OK)
        scm_misc_error("gir-function-create-gsubr",
                       "closure call interface preparation error #~A",
                       scm_list_1(scm_from_int(prep_ok)));

    // STEP 3
    // Initialize the closure
    ffi_status closure_ok;
    closure_ok = ffi_prep_closure_loc(gfn->closure, &(gfn->cif), function_binding, gfn,
                                      gfn->function_ptr);

    if (closure_ok != FFI_OK)
        scm_misc_error("gir-function-create-gsubr",
                       "closure location preparation error #~A",
                       scm_list_1(scm_from_int(closure_ok)));

    strval_add_entry(function_cache, key, (uintptr_t) gfn);

    return gfn->function_ptr;
}

static SCM
function_invoke(void *handle, GigArgMap *amap, const char *name, GObject *self,
                SCM args, GError **error)
{
    GigArgsStore *store;

    store = gig_args_store_new(amap->c_input_len, amap->c_output_len);
    gig_args_store_initialize(store, amap, name, self, args);

    // Make the actual call.
    // Use GObject's ffi to call the C function.
    if (self != NULL)
        gig_debug("%s - calling with self plus %d input and %d output arguments",
                  name, amap->c_input_len, amap->c_output_len);
    else
        gig_debug("%s - calling with %d input and %d output arguments",
                  name, amap->c_input_len, amap->c_output_len);
    gig_amap_dump(name, amap);

    GigArgument return_arg;
    return_arg.v_pointer = NULL;
    gig_invoke_func(handle, amap, store->in_args,
                    amap->c_input_len + (amap->is_method ? 1 : 0),
                    store->out_args, amap->c_output_len, &return_arg, error);
    SCM ret = gig_args_store_return_value(store, amap, name, self, args, 1, &return_arg);

    gig_args_store_free(store);

    return ret;
}

SCM
gig_callable_invoke(void *handle, GigArgMap *amap, const char *name, GObject *self,
                    SCM args, GError **error)
{
    return function_invoke(handle, amap, name, self, args, error);
}


// This is the core of a dynamically generated GICallable function wrapper.
// It converts FFI arguments to SCM arguments, converts those
// SCM arguments into GigArguments, calls the C function,
// and returns the results as an SCM packed into an FFI argument.
// Also, it converts GErrors into SCM misc-errors.
static void
function_binding(ffi_cif *cif, void *ret, void **ffi_args, void *user_data)
{
    GigFunction *gfn = user_data;
    GObject *self = NULL;
    SCM s_args = SCM_UNDEFINED;

    // When using GLib thread functions, could this be the entrypoint
    // into Guile for this thread?
    scm_init_guile();

    assert(cif != NULL);
    assert(ret != NULL);
    assert(ffi_args != NULL);
    assert(user_data != NULL);

    unsigned n_args = cif->nargs;

    // we have either 0 args or 1 args, which is the already packed list
    assert(n_args <= 1);

    if (n_args)
        s_args = SCM_PACK(*(scm_t_bits *) (ffi_args[0]));

    if (SCM_UNBNDP(s_args))
        s_args = SCM_EOL;

#if HAVE_SCM_HOOKS
    if (!scm_is_empty_hook(gig_before_function_hook))
        scm_c_activate_hook_2(gig_before_function_hook, scm_from_utf8_string(gfn->name), s_args);
#endif

    if (gfn->amap->is_method) {
        self = gig_type_peek_object(scm_car(s_args));
        s_args = scm_cdr(s_args);
    }

    // Then invoke the actual function
    GError *err = NULL;
    SCM output = function_invoke(gfn->handle, gfn->amap, gfn->name, self, s_args, &err);

    // If there is a GError, write an error and exit.
    if (err) {
        char str[256];
        memset(str, 0, 256);
        strncpy(str, err->message, 255);
        g_error_free(err);

        scm_misc_error(gfn->name, str, SCM_EOL);
        g_return_if_reached();
    }

    *(ffi_arg *)ret = SCM_UNPACK(output);
}

void
gig_init_function(void)
{
    function_cache = strval_new();

    top_type = scm_get_top_class();
    char_type = scm_get_char_class();
    list_type = scm_get_list_class();
    string_type = scm_get_string_class();

    sym_self = scm_from_utf8_symbol("self");

#if HAVE_SCM_HOOKS
    gig_before_function_hook = scm_permanent_object(scm_make_hook(scm_from_size_t(2)));
    scm_c_define("%before-function-hook", gig_before_function_hook);
#endif

    atexit(gig_fini_function);
}

static void
function_free(GigFunction *gfn)
{
    free(gfn->name);
    gfn->name = NULL;

    ffi_closure_free(gfn->closure);
    gfn->closure = NULL;

    free(gfn->atypes);
    gfn->atypes = NULL;

    gig_amap_free(gfn->amap);

    free(gfn);
}

inline static void
function_free_v(uintptr_t gfn)
{
    function_free((GigFunction *)gfn);
}


static void
gig_fini_function(void)
{
    gig_debug("Freeing functions");
    strval_free(function_cache, function_free_v);
    function_cache = NULL;
}
