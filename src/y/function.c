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
#include "y/arg.h"
#include "y/arg_map.h"
#include "y/guile.h"
#include "y/function.h"
#include "y/util.h"
#include "y/type.h"
#include "y/signal.h"
//#include "function_private.h"

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
static SCM function_invoke(GIFunctionInfo *info, Arg_map *amap, const char *name,
                           GObject *object, SCM args, GError **error);
static SCM convert_output_args(Arg_map *amap, const char *name, GIArgument *in, GIArgument *out,
                               SCM output);
static void object_list_to_c_args(Arg_map *amap, const char *subr,
                                  SCM s_args, GArray *in_args, slist_t **free_list,
                                  GArray *out_args);
static void
store_argument(int invoke_in, int invoke_out, int inout, int inout_free,
               GIArgument *arg, GArray *cinvoke_input_arg_array, slist_t **free_array,
               GArray *cinvoke_output_arg_array);
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
    case TYPE_TAG_UTF8_STRING:
    case TYPE_TAG_LOCALE_STRING:
        return string_type;
    case TYPE_TAG_GLIST:
    case TYPE_TAG_GSLIST:
        return list_type;
    case TYPE_TAG_CALLBACK:
        return applicable_type;
    case TYPE_TAG_POINTER:
        return SCM_UNDEFINED;
    case TYPE_TAG_UNICHAR:
        return char_type;
        // FIXME: use type tags
    default:
        // usual case: refer to the already existing mapping of gtype_t to scheme type
        return get_scheme_type(meta->gtype);
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

static void
callable_prepare_invoke(Arg_map *amap,
                            const char *name,
                            GObject *self,
                            SCM args,
                            GArray **cinvoke_input_arg_array,
                            GArray **cinvoke_output_arg_array,
                            slist_t **free_list, GIArgument **out_args, GIArgument **out_boxes)
{
    *cinvoke_input_arg_array = g_array_new(FALSE, TRUE, sizeof(GIArgument));
    *cinvoke_output_arg_array = g_array_new(FALSE, TRUE, sizeof(GIArgument));

    // Convert the scheme arguments into C.
    object_list_to_c_args(amap, name, args, *cinvoke_input_arg_array,
                          free_list, *cinvoke_output_arg_array);
    // For methods calls, the object gets inserted as the 1st argument.
    if (self) {
        GIArgument self_arg;
        self_arg.v_pointer = self;
        g_array_prepend_val(*cinvoke_input_arg_array, self_arg);
    }

    // Since, in the Guile binding, we're allocating the output
    // parameters in most cases, here's where we make space for
    // immediate return arguments.  There's a trick here.  Sometimes
    // GLib expects to use these out_args directly, and sometimes it
    // expects out_args->v_pointer to point to allocated space.  I
    // allocate space for *all* the output arguments, even when not
    // needed.  It is easier than figuring out which output arguments
    // need allocation.
    *out_args = (GIArgument *)((*cinvoke_output_arg_array)->data);
    *out_boxes = xcalloc((*cinvoke_output_arg_array)->len, sizeof(GIArgument));

    gig_mem_list_add(free_list, *out_boxes);
    for (unsigned i = 0; i < (*cinvoke_output_arg_array)->len; i++)
        if ((*out_args + i)->v_pointer == NULL)
            (*out_args + i)->v_pointer = *out_boxes + i;
}

static SCM
callable_return_value(Arg_map *amap,
                          const char *name,
                          GObject *self,
                          SCM args,
                          int ok,
                          GIArgument *return_arg,
                          GArray *cinvoke_input_arg_array,
                          GArray *cinvoke_output_arg_array,
                          GIArgument *out_args, GIArgument *out_boxes)
{
    SCM output = SCM_EOL;

    // Here is where I check to see if I used the allocated
    // output argument space created above.
    for (unsigned i = 0; i < cinvoke_output_arg_array->len; i++)
        if (out_args[i].v_pointer == &out_boxes[i])
            memcpy(&out_args[i], &out_boxes[i], sizeof(GIArgument));

    if (ok) {
        SCM s_return;
        size_t sz = -1;
        if (amap->return_val.arg.has_size) {
            size_t idx = amap->return_val.child->c_output_pos;
            sz = ((GIArgument *)(cinvoke_output_arg_array->data))[idx].v_size;
        }

        gig_argument_c_to_scm(name, -1, &amap->return_val.arg, return_arg, &s_return, sz);
        if (scm_is_eq(s_return, SCM_UNSPECIFIED))
            output = SCM_EOL;
        else
            output = scm_list_1(s_return);


        if (self)
            output = convert_output_args(amap, name,
                                         (GIArgument *)cinvoke_input_arg_array->data + 1,
                                         (GIArgument *)cinvoke_output_arg_array->data, output);
        else
            output = convert_output_args(amap, name,
                                         (GIArgument *)cinvoke_input_arg_array->data,
                                         (GIArgument *)cinvoke_output_arg_array->data, output);
    }

    g_array_free(cinvoke_input_arg_array, TRUE);
    g_array_free(cinvoke_output_arg_array, TRUE);

    if (!ok)
        return SCM_UNDEFINED;

    switch (scm_c_length(output)) {
    case 0:
        return SCM_UNSPECIFIED;
    case 1:
        return scm_car(output);
    default:
        return scm_values(output);
    }
}

static SCM
function_invoke(GIFunctionInfo *func_info, Arg_map *amap, const char *name, GObject *self,
                SCM args, GError **error)
{
    GArray *cinvoke_input_arg_array;
    slist_t *free_list = NULL;
    GArray *cinvoke_output_arg_array;
    GIArgument *out_args, *out_boxes;

    callable_prepare_invoke(amap, name, self, args,
                                &cinvoke_input_arg_array,
                                &cinvoke_output_arg_array, &free_list, &out_args, &out_boxes);

    // Make the actual call.
    // Use GObject's ffi to call the C function.
    debug_ffi("%s - calling with %d input and %d output arguments",
                  name, cinvoke_input_arg_array->len, cinvoke_output_arg_array->len);
    arg_map_dump(name, amap);

    GIArgument return_arg;
    return_arg.v_pointer = NULL;
    int ok = g_function_info_invoke(func_info, (GIArgument *)(cinvoke_input_arg_array->data),
                                          cinvoke_input_arg_array->len,
                                          (GIArgument *)(cinvoke_output_arg_array->data),
                                          cinvoke_output_arg_array->len, &return_arg, error);
    SCM ret = callable_return_value(amap, name, self, args, ok, &return_arg,
                                        cinvoke_input_arg_array, cinvoke_output_arg_array,
                                        out_args, out_boxes);
    gig_mem_list_free(&free_list);
    return ret;

}

SCM
callable_invoke(GICallableInfo *callable_info, void *callable, Arg_map *amap,
                    const char *name, GObject *self, SCM args, GError **error)
{
    GArray *cinvoke_input_arg_array;
    slist_t *free_list = NULL;
    GArray *cinvoke_output_arg_array;
    GIArgument *out_args, *out_boxes;
    GIArgument return_arg;
    int ok;

    callable_prepare_invoke(amap, name, self, args,
                                &cinvoke_input_arg_array,
                                &cinvoke_output_arg_array, &free_list, &out_args, &out_boxes);

    // Make the actual call.
    // Use GObject's ffi to call the C function.
    debug_ffi("%s - calling with %d input and %d output arguments",
                  name, cinvoke_input_arg_array->len, cinvoke_output_arg_array->len);
    arg_map_dump(name, amap);

    ok = g_callable_info_invoke(callable_info, callable,
                                (GIArgument *)(cinvoke_input_arg_array->data),
                                cinvoke_input_arg_array->len,
                                (GIArgument *)(cinvoke_output_arg_array->data),
                                cinvoke_output_arg_array->len, &return_arg,
                                g_callable_info_is_method(callable_info),
                                g_callable_info_can_throw_gerror(callable_info), error);

    SCM ret = callable_return_value(amap, name, self, args, ok, &return_arg,
                                        cinvoke_input_arg_array, cinvoke_output_arg_array,
                                        out_args, out_boxes);
    gig_mem_list_free(&free_list);
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

static void
object_to_c_arg(Arg_map *amap, int s, const char *name, SCM obj,
                GArray *cinvoke_input_arg_array, slist_t **free_list,
                GArray *cinvoke_output_arg_array)
{
    // Convert an input scheme argument to a C invoke argument
    GIArgument arg;
    Arg_map_entry *entry;
    size_t size;
    int i;
    int c_invoke_in, c_invoke_out, c_child_invoke_in;
    int is_in, is_out;
    int inout, inout_free;

    entry = arg_map_get_input_entry_by_s(amap, s);
    gig_argument_scm_to_c(name, s, &entry->arg, obj, free_list, &arg, &size);

    // Store the converted argument.
    is_in = arg_map_input_s2c(amap, s, &c_invoke_in);
    is_out = arg_map_input_s_2_output_c(amap, s, &c_invoke_out);
    if (!is_in)
        c_invoke_in = -1;
    if (!is_out)
        c_invoke_out = -1;

    // Input/Output arguments have an extra implied level of
    // indirection.
    inout = is_in && is_out;
    if (inout) {
        arg_map_input_s2i(amap, s, &i);
        inout_free = (amap->pdata[i].arg.transfer == GI_TRANSFER_NOTHING);
    }
    else
        inout_free = FALSE;
    store_argument(c_invoke_in, c_invoke_out, inout, inout_free, &arg,
                   cinvoke_input_arg_array, free_list, cinvoke_output_arg_array);

    // If this argument is an array with an associated size, store the
    // array size as well.
    if (arg_map_input_s_2_child_input_c(amap, s, &c_child_invoke_in)) {
        Arg_map_entry *size_entry = entry->child;
        GIArgument size_arg;
        size_t dummy_size;
        int c_child_invoke_out, i_child;

        gig_argument_scm_to_c(name, s, &size_entry->arg, scm_from_size_t(size),
                              free_list, &size_arg, &dummy_size);

        is_in = arg_map_input_c2i(amap, c_child_invoke_in, &i_child);
        is_out = arg_map_output_i2c(amap, i_child, &c_child_invoke_out);
        assert(is_in);
        if (!is_out)
            c_child_invoke_out = -1;

        inout = is_in && is_out;
        if (inout)
            inout_free = (amap->pdata[i_child].arg.transfer == GI_TRANSFER_NOTHING);
        else
            inout_free = FALSE;
        store_argument(c_child_invoke_in, c_child_invoke_out, inout, inout_free, &size_arg,
                       cinvoke_input_arg_array, free_list, cinvoke_output_arg_array);
    }
}

static void
store_argument(int invoke_in, int invoke_out, int inout, int inout_free,
               GIArgument *arg, GArray *cinvoke_input_arg_array, slist_t **free_list,
               GArray *cinvoke_output_arg_array)
{
    GIArgument *parg;

    if (invoke_in >= 0) {
        if (inout) {
            void **dup = xmemdup(arg, sizeof(GIArgument));
            parg = &g_array_index(cinvoke_input_arg_array, GIArgument, invoke_in);
            parg->v_pointer = dup;

            if (inout_free)
                gig_mem_list_add(free_list, dup);

            parg = &g_array_index(cinvoke_output_arg_array, GIArgument, invoke_out);
            parg->v_pointer = 0;
        }
        else {
            parg = &g_array_index(cinvoke_input_arg_array, GIArgument, invoke_in);
            *parg = *arg;
        }
    }
    else if (invoke_out >= 0) {
        parg = &g_array_index(cinvoke_output_arg_array, GIArgument, invoke_out);
        *parg = *arg;
    }
}

static void
object_list_to_c_args(Arg_map *amap,
                      const char *subr, SCM s_args,
                      GArray *cinvoke_input_arg_array,
                      slist_t **free_list, GArray *cinvoke_output_arg_array)
{
    g_assert_nonnull(amap);
    g_assert_nonnull(subr);
    g_assert_nonnull(cinvoke_input_arg_array);
    g_assert_nonnull(cinvoke_output_arg_array);

    int args_count, required, optional;
    if (SCM_UNBNDP(s_args))
        args_count = 0;
    else
        args_count = scm_c_length(s_args);
    arg_map_s_input_count(amap, &required, &optional);
    if (args_count < required || args_count > required + optional)
        scm_error_num_args_subr(subr);

    int input_len, output_len;
    arg_map_c_count(amap, &input_len, &output_len);

    g_array_set_size(cinvoke_input_arg_array, input_len);
    g_array_set_size(cinvoke_output_arg_array, output_len);

    for (int i = 0; i < args_count; i++) {
        SCM obj = scm_c_list_ref(s_args, i);
        object_to_c_arg(amap, i, subr, obj, cinvoke_input_arg_array, free_list,
                        cinvoke_output_arg_array);

    }
    return;
}

GIArgument *
find_output_arg(Arg_map_entry *entry, GIArgument *in, GIArgument *out)
{
    switch (entry->s_direction) {
    case ARG_DIRECTION_INPUT:      // may happen with sizes of preallocated outputs
        return in + entry->c_input_pos;
    case ARG_DIRECTION_INOUT:
        return (in + entry->c_input_pos)->v_pointer;
    case ARG_DIRECTION_PREALLOCATED_OUTPUT:
    case ARG_DIRECTION_OUTPUT:
        return out + entry->c_output_pos;
    default:
        assert_not_reached();
    }
}

static SCM
convert_output_args(Arg_map *amap, const char *func_name, GIArgument *in, GIArgument *out,
                    SCM output)
{
    debug_transfer("%s - convert_output_args", func_name);
    int s_output_pos;

    for (int c_output_pos = 0; c_output_pos < amap->c_output_len; c_output_pos++) {
        if (!arg_map_output_c2s(amap, c_output_pos, &s_output_pos))
            continue;

        Arg_map_entry *entry = arg_map_get_output_entry_by_c(amap, c_output_pos);

        GIArgument *arg;
        arg = find_output_arg(entry, in, out);
        if (arg == NULL)        // an INOUT argument has been eaten
        {
            output = scm_cons(SCM_UNSPECIFIED, output);
            continue;
        }

        SCM obj;
        size_t size = ARG_ARRAY_SIZE_UNKNOWN;

        if (entry->child) {
            // We need to know the size argument before we can process
            // this array argument.
            Arg_map_entry *size_entry = entry->child;
            GIArgument *size_arg = find_output_arg(size_entry, in, out);
            gig_argument_c_to_scm(func_name, size_entry->i, &size_entry->arg, size_arg, &obj, -1);
            size = scm_is_integer(obj) ? scm_to_int(obj) : 0;
        }

        gig_argument_c_to_scm(func_name, c_output_pos, &entry->arg, arg, &obj, size);
        output = scm_cons(obj, output);
    }
    return scm_reverse_x(output, SCM_EOL);
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
