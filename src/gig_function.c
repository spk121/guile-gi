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
#include <stdio.h>
#include <ffi.h>
#include "core.h"
#include "gig_argument.h"
#include "gig_arg_map.h"
#include <libguile.h>
#include "gig_function.h"
#include "gig_type.h"
#include "gig_signal.h"
#include "gig_function_args.h"

struct _GigFunction
{
    GIFunctionInfo *function_info;
    ffi_closure *closure;
    ffi_cif cif;
    void *function_ptr;
    char *name;
    ffi_type **atypes;
    GigArgMap *amap;
};

static keyval_t *function_cache;
static SCM function_hook = SCM_BOOL_F;

static GigGsubr *check_gsubr_cache(GICallableInfo *function_info, SCM self_type,
                                   int *required_input_count, int *optional_input_count,
                                   SCM *formals, SCM *specializers);
static GigGsubr *create_gsubr(GIFunctionInfo *function_info, const char *name, SCM self_type,
                              int *required_input_count, int *optional_input_count,
                              SCM *formals, SCM *specializers);
static void make_formals(GICallableInfo *, GigArgMap *, int n_inputs, SCM self_type,
                         SCM *formals, SCM *specializers);
static void function_binding(ffi_cif *cif, void *ret, void **ffi_args, void *user_data);
static SCM function_invoke(GIFunctionInfo *info, GigArgMap *amap, const char *name,
                           GObject *object, SCM args, GError **error);
static void function_free(GigFunction *fn);
static void gig_fini_function(void);

static SCM proc4function(GIFunctionInfo *info, const char *name, SCM self_type,
                         int *req, int *opt, SCM *formals, SCM *specs);
static SCM proc4signal(GISignalInfo *info, const char *name, SCM self_type,
                       int *req, int *opt, SCM *formals, SCM *specs);

// This procedure counts the number of arguments that the
// GObject Introspection FFI call is expecting.
static void
count_args(GICallableInfo *info, int *in, int *out)
{
    // Count the number of required input arguments, and store
    // the arg info in a newly allocate array.
    int n_args = g_callable_info_get_n_args(info);
    int n_input_args = 0;
    int n_output_args = 0;

    for (int i = 0; i < n_args; i++) {
        GIArgInfo *ai = g_callable_info_get_arg(info, i);
        GIDirection dir = g_arg_info_get_direction(ai);
        g_base_info_unref(ai);

        if (dir == GI_DIRECTION_IN)
            n_input_args++;
        else if (dir == GI_DIRECTION_OUT)
            n_output_args++;
        else if (dir == GI_DIRECTION_INOUT) {
            n_input_args++;
            n_output_args++;
        }
    }
    *in = n_input_args;
    *out = n_output_args;
}

// Returns TRUE if this function returns a single boolean.
static intbool_t
is_predicate(GICallableInfo *info)
{
    intbool_t predicate = FALSE;
    GITypeInfo *return_type;

    if (GI_IS_SIGNAL_INFO(info))
        return FALSE;

    return_type = g_callable_info_get_return_type(info);

    if (g_type_info_get_tag(return_type) == GI_TYPE_TAG_BOOLEAN
        && !g_type_info_is_pointer(return_type)) {
        int in, out;

        count_args(info, &in, &out);
        if (out == 0)
            predicate = TRUE;
    }
    g_base_info_unref(return_type);
    return predicate;
}

static intbool_t
is_destructive(GICallableInfo *info)
{
    intbool_t destructive = FALSE;
    int n_args = g_callable_info_get_n_args(info);

    for (int i = 0; i < n_args; i++) {
        GIArgInfo *ai = g_callable_info_get_arg(info, i);
        GITypeInfo *ti = g_arg_info_get_type(ai);
        intbool_t is_trivial;

        switch (g_type_info_get_tag(ti)) {
        case GI_TYPE_TAG_BOOLEAN:
        case GI_TYPE_TAG_DOUBLE:
        case GI_TYPE_TAG_FLOAT:
        case GI_TYPE_TAG_GTYPE:
        case GI_TYPE_TAG_INT8:
        case GI_TYPE_TAG_INT16:
        case GI_TYPE_TAG_INT32:
        case GI_TYPE_TAG_INT64:
        case GI_TYPE_TAG_UINT8:
        case GI_TYPE_TAG_UINT16:
        case GI_TYPE_TAG_UINT32:
        case GI_TYPE_TAG_UINT64:
        case GI_TYPE_TAG_UNICHAR:
            is_trivial = TRUE;
            break;
        default:
            is_trivial = FALSE;
        }
        g_base_info_unref(ti);

        if (!is_trivial) {
            destructive |= g_arg_info_is_caller_allocates(ai);
            destructive |= (g_arg_info_get_direction(ai) == GI_DIRECTION_INOUT);
        }
        g_base_info_unref(ai);
    }

    return destructive;
}

// For function and method names, we want a lowercase string of the
// form 'func-name-with-hyphens'
char *
gig_callable_info_make_name(GICallableInfo *info, const char *prefix)
{
    char *name, *str1 = NULL, *str2 = NULL;
    intbool_t predicate, destructive;

    predicate = is_predicate(info);
    destructive = is_destructive(info);
    if (prefix)
        str1 = g_name_to_scm_name(prefix);
    str2 = g_name_to_scm_name(g_base_info_get_name(info));

    int len = strlen(":!") + 1;
    if (str1)
        len += strlen(str1);
    if (str2)
        len += strlen(str2);
    name = xmalloc(len);

    if (!prefix) {
        if (destructive)
            snprintf(name, len, "%s!", str2);
        else if (predicate)
            snprintf(name, len, "%s?", str2);
        else
            return str2;
    }
    else {
        if (destructive)
            snprintf(name, len, "%s:%s!", str1, str2);
        else if (predicate)
            snprintf(name, len, "%s:%s?", str1, str2);
        else
            snprintf(name, len, "%s:%s", str1, str2);
    }
    free(str1);
    free(str2);
    return name;
}

SCM
gig_function_define(gtype_t type, GICallableInfo *info, const char *_namespace, SCM defs)
{
    scm_dynwind_begin(0);
    SCM def;
    intbool_t is_method = g_callable_info_is_method(info);

    char *function_name = NULL;
    char *method_name = NULL;
    function_name = scm_dynfree(gig_callable_info_make_name(info, _namespace));

    int required_input_count, optional_input_count;
    SCM formals, specializers, self_type = SCM_UNDEFINED;

    debug_load("%s - bound to %s %s%s%s",
               function_name,
               (GI_IS_SIGNAL_INFO(info) ? "signal" : "function"),
               (_namespace ? _namespace : ""), (_namespace ? "." : ""),
               g_base_info_get_name(info));

    if (is_method) {
        self_type = gig_type_get_scheme_type(type);
        return_val_if_fail(!SCM_UNBNDP(self_type), defs);
        method_name = scm_dynfree(gig_callable_info_make_name(info, NULL));
        debug_load("%s - shorthand for %s", method_name, function_name);
    }

    SCM proc = SCM_UNDEFINED;
    if (GI_IS_FUNCTION_INFO(info))
        proc = proc4function((GIFunctionInfo *)info, function_name, self_type,
                             &required_input_count, &optional_input_count,
                             &formals, &specializers);
    else if (GI_IS_SIGNAL_INFO(info))
        proc = proc4signal((GISignalInfo *)info, function_name, self_type,
                           &required_input_count, &optional_input_count, &formals, &specializers);
    else
        assert_not_reached();

    if (SCM_UNBNDP(proc))
        goto end;

    def = scm_define_methods_from_procedure(function_name, proc,
                                            optional_input_count, formals, specializers);
    if (!SCM_UNBNDP(def))
        defs = scm_cons(def, defs);
    if (is_method) {
        def =
            scm_define_methods_from_procedure(method_name, proc, optional_input_count, formals,
                                              specializers);
        if (!SCM_UNBNDP(def))
            defs = scm_cons(def, defs);
    }

  end:
    scm_dynwind_end();
    return defs;
}

static SCM
proc4function(GIFunctionInfo *info, const char *name, SCM self_type,
              int *req, int *opt, SCM *formals, SCM *specializers)
{
    GigGsubr *func_gsubr = check_gsubr_cache(info, self_type, req, opt,
                                             formals, specializers);
    if (!func_gsubr)
        func_gsubr = create_gsubr(info, name, self_type, req, opt, formals, specializers);

    if (!func_gsubr) {
        debug_load("%s - could not create a gsubr", name);
        return SCM_UNDEFINED;
    }

    return scm_c_make_gsubr(name, 0, 0, 1, func_gsubr);
}

static SCM
proc4signal(GISignalInfo *info, const char *name, SCM self_type, int *req, int *opt, SCM *formals,
            SCM *specializers)
{
    GigArgMap *amap;

    amap = gig_amap_new(name, info);
    if (amap == NULL)
        return SCM_UNDEFINED;

    gig_amap_s_input_count(amap, req, opt);
    (*req)++;

    make_formals(info, amap, *req + *opt, self_type, formals, specializers);

    GigSignalSlot slots[] = { GIG_SIGNAL_SLOT_NAME, GIG_SIGNAL_SLOT_OUTPUT_MASK };
    SCM values[2];

    // use base_info name without transformations, otherwise we could screw things up
    values[0] = scm_from_utf8_string(g_base_info_get_name(info));
    values[1] = scm_c_make_bitvector(*req + *opt, SCM_BOOL_F);

    size_t offset, length;
    gssize pos = 0, inc;
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
                scm_set_procedure_slot(signal, scm_method_procedure(scm_car(iter)));
                break;
            }

    return signal;
}

static GigGsubr *
check_gsubr_cache(GICallableInfo *function_info, SCM self_type, int *required_input_count,
                  int *optional_input_count, SCM *formals, SCM *specializers)
{
    // Check the cache to see if this function has already been created.
    GigFunction *gfn = keyval_find_entry(function_cache, function_info);

    if (gfn == NULL)
        return NULL;

    gig_amap_s_input_count(gfn->amap, required_input_count, optional_input_count);

    if (g_callable_info_is_method(gfn->function_info))
        (*required_input_count)++;

    make_formals(gfn->function_info,
                 gfn->amap,
                 *required_input_count + *optional_input_count, self_type, formals, specializers);

    return gfn->function_ptr;
}

static SCM
type_specializer(GigTypeMeta *meta)
{
    switch (meta->gtype) {
    case G_TYPE_POINTER:
        // special case: POINTER can also mean string, list or callback
        switch (meta->pointer_type) {
        case GIG_DATA_UTF8_STRING:
        case GIG_DATA_LOCALE_STRING:
            return scm_get_string_class();
        case GIG_DATA_LIST:
        case GIG_DATA_SLIST:
            return scm_get_list_class();
        case GIG_DATA_CALLBACK:
            return scm_get_applicable_class();
        default:
            return SCM_UNDEFINED;
        }
    case G_TYPE_UINT:
        // special case: Unicode characters
        if (meta->is_unichar)
            return scm_get_char_class();
        /* fall through */
    default:
        // usual case: refer to the already existing mapping of gtype_t to scheme type
        return gig_type_get_scheme_type(meta->gtype);
    }
}

static void
make_formals(GICallableInfo *callable,
             GigArgMap *argmap, int n_inputs, SCM self_type, SCM *formals, SCM *specializers)
{
    SCM i_formal, i_specializer;

    i_formal = *formals = scm_make_list(scm_from_int(n_inputs), SCM_BOOL_F);
    i_specializer = *specializers = scm_make_list(scm_from_int(n_inputs), scm_get_top_class());

    if (g_callable_info_is_method(callable)) {
        scm_set_car_x(i_formal, scm_from_utf8_symbol("self"));
        scm_set_car_x(i_specializer, self_type);

        i_formal = scm_cdr(i_formal);
        i_specializer = scm_cdr(i_specializer);
        n_inputs--;
    }

    for (int s = 0; s < n_inputs;
         s++, i_formal = scm_cdr(i_formal), i_specializer = scm_cdr(i_specializer)) {
        GigArgMapEntry *entry = gig_amap_get_input_entry_by_s(argmap, s);
        char *formal = scm_dynfree(g_name_to_scm_name(entry->name));
        scm_set_car_x(i_formal, scm_from_utf8_symbol(formal));
        // Don't force types on nullable input, as #f can also be used to represent
        // NULL.
        if (entry->meta.is_nullable)
            continue;

        SCM s_type = type_specializer(&entry->meta);
        if (!SCM_UNBNDP(s_type))
            scm_set_car_x(i_specializer, s_type);
    }
}

static GigGsubr *
create_gsubr(GIFunctionInfo *function_info, const char *name, SCM self_type,
             int *required_input_count, int *optional_input_count, SCM *formals, SCM *specializers)
{
    GigFunction *gfn;
    ffi_type *ffi_ret_type;
    GigArgMap *amap;

    amap = gig_amap_new(name, function_info);
    if (amap == NULL) {
        debug_load("%s - invalid argument map", name);
        return NULL;
    }

    gfn = xcalloc(1, sizeof(GigFunction));
    gfn->function_info = function_info;
    gfn->amap = amap;
    free(gfn->name);
    gfn->name = xstrdup(name);
    g_base_info_ref(function_info);

    gig_amap_s_input_count(gfn->amap, required_input_count, optional_input_count);

    if (g_callable_info_is_method(gfn->function_info))
        (*required_input_count)++;

    make_formals(gfn->function_info, gfn->amap, *required_input_count + *optional_input_count,
                 self_type, formals, specializers);

    // STEP 1
    // Allocate the block of memory that FFI uses to hold a closure
    // object, and set a pointer to the corresponding executable
    // address.
    gfn->closure = ffi_closure_alloc(sizeof(ffi_closure), &(gfn->function_ptr));

    return_val_if_fail(gfn->closure != NULL, NULL);
    return_val_if_fail(gfn->function_ptr != NULL, NULL);

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

    keyval_add_entry(function_cache, function_info, gfn);

    return gfn->function_ptr;
}

static SCM
function_invoke(GIFunctionInfo *func_info, GigArgMap *amap, const char *name, GObject *self,
                SCM args, GError **error)
{
    GigArgsStore store = { 0 };
    gig_args_store_initialize(&store, self, amap, args, name);

    // Make the actual call.
    // Use GObject's ffi to call the C function.

    g_debug("%s - calling with %lu input and %lu output arguments",
            name, store.in_len, store.out_len);
    gig_amap_dump(name, amap);

    GIArgument return_arg;
    return_arg.v_pointer = NULL;

    gboolean ok = g_function_info_invoke(func_info, store.in,
                                         store.in_len,
                                         store.out,
                                         store.out_len, &return_arg, error);

    SCM ret = gig_args_store_return_value(&store, self, amap, args, name, &return_arg, ok);

    return ret;

}

SCM
gig_callable_invoke(GICallableInfo *callable_info, void *callable, GigArgMap *amap,
                    const char *name, GObject *self, SCM args, GError **error)
{
    GigArgsStore store = { 0 };
    gig_args_store_initialize(&store, self, amap, args, name);

    GIArgument return_arg;
    intbool_t ok;

    // Make the actual call.
    // Use GObject's ffi to call the C function.

    g_debug("%s - calling with %lu input and %lu output arguments",
            name, store.in_len, store.out_len);
    gig_amap_dump(name, amap);

    ok = g_callable_info_invoke(callable_info, callable,
                                store.in,
                                store.in_len,
                                store.out,
                                store.out_len, &return_arg,
                                g_callable_info_is_method(callable_info),
                                g_callable_info_can_throw_gerror(callable_info), error);

    SCM ret = gig_args_store_return_value(&store, self, amap, args, name, &return_arg, ok);

    return ret;
}


// This is the core of a dynamically generated GICallable function wrapper.
// It converts FFI arguments to SCM arguments, converts those
// SCM arguments into GIArguments, calls the C function,
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

    scm_c_activate_hook_2(function_hook, scm_from_utf8_string(gfn->name), s_args);

    if (g_callable_info_is_method(gfn->function_info)) {
        self = gig_type_peek_object(scm_car(s_args));
        s_args = scm_cdr(s_args);
    }

    // Then invoke the actual function
    GError *err = NULL;
    SCM output = function_invoke(gfn->function_info, gfn->amap, gfn->name, self, s_args, &err);

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
    function_cache = keyval_new();
    function_hook = scm_c_public_ref("gi core hooks", "%before-function-hook");
    atexit(gig_fini_function);
}

static void
function_free(GigFunction *gfn)
{
    free(gfn->name);
    gfn->name = NULL;

    ffi_closure_free(gfn->closure);
    gfn->closure = NULL;

    g_base_info_unref(gfn->function_info);
    free(gfn->atypes);
    gfn->atypes = NULL;

    gig_amap_free(gfn->amap);

    free(gfn);
}

static void
gig_fini_function(void)
{
    debug_init("Freeing functions");
    keyval_free(function_cache, NULL, function_free);
    function_cache = NULL;
}
