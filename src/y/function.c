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
#include <libguile/hooks.h>
#include "x.h"
#include "y/util.h"
#include "y/arg_map.h"
#include "y/function.h"
#include "y/type.h"
#include "y/signal.h"
#include "y/arg.h"
#include "y/guile.h"
#include "y/function_args.h"

struct _Function
{
    GIFunctionInfo *function_info;
    ffi_closure *closure;
    ffi_cif cif;
    void *function_ptr;
    char *name;
    ffi_type **atypes;
    Arg_map *amap;
};

static keyval_t *function_cache;
SCM ensure_generic_proc;
SCM make_proc;
SCM add_method_proc;

SCM top_type;
SCM method_type;

SCM kwd_specializers;
SCM kwd_formals;
SCM kwd_procedure;

SCM sym_self;

SCM before_function_hook;

static Gsubr *check_gsubr_cache(GICallableInfo *function_info, SCM self_type,
                                   int *required_input_count, int *optional_input_count,
                                   SCM *formals, SCM *specializers);
static Gsubr *create_gsubr(GIFunctionInfo *function_info, const char *name, SCM self_type,
                              int *required_input_count, int *optional_input_count,
                              SCM *formals, SCM *specializers);
static void make_formals(GICallableInfo *, Arg_map *, int n_inputs, SCM self_type,
                         SCM *formals, SCM *specializers);
static void function_binding(ffi_cif *cif, void *ret, void **ffi_args, void *user_data);
static SCM invoke_function(GIFunctionInfo *info, Arg_map *amap, const char *name,
                           GObject *object, SCM args, GError **error);
static void free_function(Function *fn);
static void fini_function(void);
static SCM function_define1(const char *public_name, SCM proc, int opt, SCM formals,
                                SCM specializers);

static SCM proc4function(GIFunctionInfo *info, const char *name, SCM self_type,
                         int *req, int *opt, SCM *formals, SCM *specs);
static SCM proc4signal(GISignalInfo *info, const char *name, SCM self_type,
                       int *req, int *opt, SCM *formals, SCM *specs);

#define LOOKUP_DEFINITION(module)                                       \
    do {                                                                \
        SCM variable = scm_module_variable(module, name);               \
        if (scm_is_true(variable)) return scm_variable_ref(variable);   \
    } while (0)

static SCM
current_module_definition(SCM name)
{
    LOOKUP_DEFINITION(scm_current_module());
    return SCM_BOOL_F;
}

SCM
default_definition(SCM name)
{
    LOOKUP_DEFINITION(scm_current_module());
    LOOKUP_DEFINITION(scm_c_resolve_module("gi"));
    LOOKUP_DEFINITION(scm_c_resolve_module("guile"));
    return SCM_BOOL_F;
}

#undef LOOKUP_DEFINITION

SCM
define_function(GType type, GICallableInfo *info, const char *_namespace, SCM defs)
{
    scm_dynwind_begin(0);
    SCM def;
    int is_method = g_callable_info_is_method(info);

    char *function_name = NULL;
    char *method_name = NULL;
    function_name = dynfree(make_callable_name(info, _namespace));

    int required_input_count, optional_input_count;
    SCM formals, specializers, self_type = SCM_UNDEFINED;

    debug_load("%s - bound to %s %s%s%s",
                   function_name,
                   (GI_IS_SIGNAL_INFO(info) ? "signal" : "function"),
                   (_namespace ? _namespace : ""), (_namespace ? "." : ""),
                   g_base_info_get_name(info));

    if (is_method) {
        self_type = get_scheme_type(type);
        return_val_if_fail(!SCM_UNBNDP(self_type), defs);
        method_name = dynfree(make_callable_name(info, NULL));
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

    def = function_define1(function_name, proc, optional_input_count, formals, specializers);
    if (!SCM_UNBNDP(def))
        defs = scm_cons(def, defs);
    if (is_method) {
        def = function_define1(method_name, proc, optional_input_count, formals, specializers);
        if (!SCM_UNBNDP(def))
            defs = scm_cons(def, defs);
    }

  end:
    scm_dynwind_end();
    return defs;
}

// Given some function introspection information from a typelib file,
// this procedure creates a SCM wrapper for that procedure in the
// current module.
static SCM
function_define1(const char *public_name, SCM proc, int opt, SCM formals, SCM specializers)
{
    return_val_if_fail(public_name != NULL, SCM_UNDEFINED);

    SCM sym_public_name = scm_from_utf8_symbol(public_name);
    SCM generic = default_definition(sym_public_name);
    if (!scm_is_generic(generic))
        generic = scm_call_2(ensure_generic_proc, generic, sym_public_name);

    SCM t_formals = formals, t_specializers = specializers;

    do {
        SCM mthd = scm_call_7(make_proc,
                              method_type,
                              kwd_specializers, t_specializers,
                              kwd_formals, t_formals,
                              kwd_procedure, proc);

        scm_call_2(add_method_proc, generic, mthd);

        if (scm_is_eq(t_formals, SCM_EOL))
            break;

        t_formals = scm_drop_right_1(t_formals);
        t_specializers = scm_drop_right_1(t_specializers);
    } while (opt-- > 0);

    scm_define(sym_public_name, generic);
    return sym_public_name;
}

static SCM
proc4function(GIFunctionInfo *info, const char *name, SCM self_type,
              int *req, int *opt, SCM *formals, SCM *specializers)
{
    Gsubr *func_gsubr = check_gsubr_cache(info, self_type, req, opt,
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
    Arg_map *amap;

    amap = arg_map_new(name, info);
    if (amap == NULL)
        return SCM_UNDEFINED;

    arg_map_s_input_count(amap, req, opt);
    (*req)++;

    make_formals(info, amap, *req + *opt, self_type, formals, specializers);

    Signal_slot slots[] = { SIGNAL_SLOT_NAME, SIGNAL_SLOT_OUTPUT_MASK };
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
        Arg_map_entry *entry = arg_map_get_input_entry_by_s(amap, i - 1);
        if (entry->is_s_output)
            bits[word_pos] |= mask;
    }
    scm_array_handle_release(&handle);
    arg_map_free(amap);

    SCM signal = make_signal(2, slots, values);

    // check for collisions
    SCM current_definition = current_module_definition(scm_from_utf8_symbol(name));
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

static Gsubr *
check_gsubr_cache(GICallableInfo *function_info, SCM self_type, int *required_input_count,
                  int *optional_input_count, SCM *formals, SCM *specializers)
{
    // Check the cache to see if this function has already been created.
    Function *gfn = keyval_find_entry(function_cache, function_info);

    if (gfn == NULL)
        return NULL;

    arg_map_s_input_count(gfn->amap, required_input_count, optional_input_count);

    if (g_callable_info_is_method(gfn->function_info))
        (*required_input_count)++;

    make_formals(gfn->function_info,
                 gfn->amap,
                 *required_input_count + *optional_input_count, self_type, formals, specializers);

    return gfn->function_ptr;
}

SCM char_type;
SCM list_type;
SCM string_type;
SCM applicable_type;

static SCM
type_specializer(Arg *meta)
{
    switch (meta->payload) {
    case TYPE_UTF8_STRING:
    case TYPE_LOCALE_STRING:
        return string_type;
    case TYPE_GLIST:
    case TYPE_GSLIST:
        return list_type;
    case TYPE_CALLBACK:
        return applicable_type;
    case TYPE_POINTER:
        return SCM_UNDEFINED;
    case TYPE_UNICHAR:
        return char_type;
        // FIXME: use type tags
    default:
        // usual case: refer to the already existing mapping of gtype_t to scheme type
        return get_scheme_type(meta->_gtype);
    }
}

static void
make_formals(GICallableInfo *callable,
             Arg_map *argmap, int n_inputs, SCM self_type, SCM *formals, SCM *specializers)
{
    SCM i_formal, i_specializer;

    i_formal = *formals = scm_make_list(scm_from_int(n_inputs), SCM_BOOL_F);
    i_specializer = *specializers = scm_make_list(scm_from_int(n_inputs), top_type);

    if (g_callable_info_is_method(callable)) {
        scm_set_car_x(i_formal, sym_self);
        scm_set_car_x(i_specializer, self_type);

        i_formal = scm_cdr(i_formal);
        i_specializer = scm_cdr(i_specializer);
        n_inputs--;
    }

    for (int s = 0; s < n_inputs;
         s++, i_formal = scm_cdr(i_formal), i_specializer = scm_cdr(i_specializer)) {
        Arg_map_entry *entry = arg_map_get_input_entry_by_s(argmap, s);
        char *formal = dynfree(gname_to_scm_name(entry->name));
        scm_set_car_x(i_formal, scm_from_utf8_symbol(formal));
        // Don't force types on nullable input, as #f can also be used to represent
        // NULL.
        if (entry->arg.is_nullable)
            continue;

        SCM s_type = type_specializer(&entry->arg);
        if (!SCM_UNBNDP(s_type))
            scm_set_car_x(i_specializer, s_type);
    }
}

static Gsubr *
create_gsubr(GIFunctionInfo *function_info, const char *name, SCM self_type,
             int *required_input_count, int *optional_input_count, SCM *formals, SCM *specializers)
{
    Function *gfn;
    ffi_type *ffi_ret_type;
    Arg_map *amap;

    amap = arg_map_new(name, function_info);
    if (amap == NULL) {
        debug_load("%s - invalid argument map", name);
        return NULL;
    }

    gfn = xcalloc(1, sizeof(Function));
    gfn->function_info = function_info;
    gfn->amap = amap;
    free(gfn->name);
    gfn->name = xstrdup(name);
    g_base_info_ref(function_info);

    arg_map_s_input_count(gfn->amap, required_input_count, optional_input_count);

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
invoke_function(GIFunctionInfo *func_info, Arg_map *amap, const char *name, GObject *self,
                SCM args, GError **error)
{
    Args_store store;
    initialize_args_store(&store, self, amap, args, name);

    // Make the actual call.
    // Use GObject's ffi to call the C function.
    debug_ffi("%s - calling with %d input and %d output arguments",
              name, store.in_len, store.out_len);
    arg_map_dump(name, amap);

    GIArgument return_arg;
    return_arg.v_pointer = NULL;
    
    int ok = g_function_info_invoke(func_info, store.in,
                                    store.in_len,
                                    store.out,
                                    store.out_len,
                                    &return_arg, error);

    SCM ret = get_return_value(&store, self, amap, args, name, &return_arg, ok);

    return ret;

}

SCM
invoke_callable(GICallableInfo *callable_info, void *callable, Arg_map *amap,
                    const char *name, GObject *self, SCM args, GError **error)
{
    Args_store store;
    initialize_args_store(&store, self, amap, args, name);

    GIArgument return_arg;
    int ok;

    // Make the actual call.
    // Use GObject's ffi to call the C function.
    debug_ffi("%s - calling with %d input and %d output arguments",
              name, store.in_len, store.out_len);
    arg_map_dump(name, amap);

    ok = g_callable_info_invoke(callable_info, callable,
                                store.in,
                                store.in_len,
                                store.out,
                                store.out_len,
                                &return_arg,
                                g_callable_info_is_method(callable_info),
                                g_callable_info_can_throw_gerror(callable_info), error);

    SCM ret = get_return_value(&store, self, amap, args, name, &return_arg, ok);

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
    Function *gfn = user_data;
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

    if (scm_is_false(scm_hook_empty_p(before_function_hook)))
        scm_c_run_hook(before_function_hook,
                       scm_list_2(scm_from_utf8_string(gfn->name), s_args));

    if (g_callable_info_is_method(gfn->function_info)) {
        self = peek_object(scm_car(s_args));
        s_args = scm_cdr(s_args);
    }

    // Then invoke the actual function
    GError *err = NULL;
    SCM output = invoke_function(gfn->function_info, gfn->amap, gfn->name, self, s_args, &err);

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

    top_type = scm_c_public_ref("oop goops", "<top>");
    method_type = scm_c_public_ref("oop goops", "<method>");
    char_type = scm_c_public_ref("oop goops", "<char>");
    list_type = scm_c_public_ref("oop goops", "<list>");
    string_type = scm_c_public_ref("oop goops", "<string>");
    applicable_type = scm_c_public_ref("oop goops", "<applicable>");

    ensure_generic_proc = scm_c_public_ref("oop goops", "ensure-generic");
    make_proc = scm_c_public_ref("oop goops", "make");
    add_method_proc = scm_c_public_ref("oop goops", "add-method!");

    kwd_specializers = scm_from_utf8_keyword("specializers");
    kwd_formals = scm_from_utf8_keyword("formals");
    kwd_procedure = scm_from_utf8_keyword("procedure");

    sym_self = scm_from_utf8_symbol("self");

    before_function_hook = scm_permanent_object(scm_make_hook(scm_from_size_t(2)));
    scm_c_define("%before-function-hook", before_function_hook);
    atexit(fini_function);
}

static void
free_function(Function *gfn)
{
    free(gfn->name);
    gfn->name = NULL;

    ffi_closure_free(gfn->closure);
    gfn->closure = NULL;

    g_base_info_unref(gfn->function_info);
    free(gfn->atypes);
    gfn->atypes = NULL;

    arg_map_free(gfn->amap);

    free(gfn);
}

static void
fini_function(void)
{
    debug_init("Freeing functions");
    keyval_free(function_cache, NULL, free_function);
    function_cache = NULL;
}

