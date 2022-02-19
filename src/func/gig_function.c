// Copyright (C) 2018, 2019, 2020, 2021 Michael L. Gran

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
#include "type.h"
#include "gig_argument.h"
#include "gig_util.h"
#include "gig_arg_map.h"
#include "gig_function.h"
#include "gig_function_private.h"

typedef struct _GigFunction
{
    GIFunctionInfo *function_info;
    ffi_closure *closure;
    ffi_cif cif;
    void *function_ptr;
    char *name;
    ffi_type **atypes;
    GigArgMap *amap;
} GigFunction;

static keyval_t *function_cache = NULL;

SCM top_type;

SCM sym_self;

SCM gig_before_function_hook;

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
static SCM convert_output_args(GigArgMap *amap, const char *name, GIArgument *in, GIArgument *out,
                               SCM output);
static void object_list_to_c_args(GigArgMap *amap, const char *subr,
                                  SCM s_args, GIArgument *in_args, slist_t **free_list,
                                  GIArgument *out_args);
static void
store_argument(int invoke_in, int invoke_out, bool inout, bool inout_free,
               GIArgument *arg, GIArgument *in_args, slist_t **free_list, GIArgument *out_args);
static void function_free(GigFunction *fn);
static void gig_fini_function(void);

static SCM proc4function(GIFunctionInfo *info, const char *name, SCM self_type,
                         int *req, int *opt, SCM *formals, SCM *specs);
static SCM proc4signal(GISignalInfo *info, const char *name, SCM self_type,
                       int *req, int *opt, SCM *formals, SCM *specs);

SCM
gig_function_define(GType type, GICallableInfo *info, const char *_namespace, SCM defs)
{
    scm_dynwind_begin(0);
    SCM def;
    bool is_method = g_callable_info_is_method(info);

    char *function_name = NULL;
    char *method_name = NULL;
    function_name = scm_dynfree(gig_callable_info_make_name(info, _namespace));

    int required_input_count, optional_input_count;
    SCM formals, specializers, self_type = SCM_UNDEFINED;

    gig_debug_load("%s - bound to %s %s%s%s",
                   function_name,
                   (GI_IS_SIGNAL_INFO(info) ? "signal" : "function"),
                   (_namespace ? _namespace : ""), (_namespace ? "." : ""),
                   g_base_info_get_name(info));

    if (is_method) {
        self_type = gig_type_get_scheme_type(type);
        g_return_val_if_fail(!SCM_UNBNDP(self_type), defs);
        method_name = scm_dynfree(gig_callable_info_make_name(info, NULL));
        gig_debug_load("%s - shorthand for %s", method_name, function_name);
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

    def =
        scm_define_methods_from_procedure(function_name, proc, optional_input_count, formals,
                                          specializers);
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
        gig_debug_load("%s - could not create a gsubr", name);
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
                scm_slot_set_x(signal, scm_from_utf8_symbol("procedure"),
                               scm_method_procedure(scm_car(iter)));
                break;
            }

    return signal;
}

static GigGsubr *
check_gsubr_cache(GICallableInfo *function_info, SCM self_type, int *required_input_count,
                  int *optional_input_count, SCM *formals, SCM *specializers)
{
    // Check the cache to see if this function has already been created.
    GigFunction *gfn = (GigFunction *)keyval_find_entry(function_cache, (uintptr_t) function_info);

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

SCM char_type;
SCM list_type;
SCM string_type;

static SCM
type_specializer(GigTypeMeta *meta)
{
    switch (meta->gtype) {
    case G_TYPE_POINTER:
        // special case: POINTER can also mean string, list or callback
        switch (meta->pointer_type) {
        case GIG_DATA_UTF8_STRING:
        case GIG_DATA_LOCALE_STRING:
            return string_type;
        case GIG_DATA_LIST:
        case GIG_DATA_SLIST:
            return list_type;
        case GIG_DATA_CALLBACK:
            return scm_get_applicable_class();
        default:
            return SCM_UNDEFINED;
        }
    case G_TYPE_UINT:
        // special case: Unicode characters
        if (meta->is_unichar)
            return char_type;
        /* fall through */
    default:
        // usual case: refer to the already existing mapping of GType to scheme type
        return gig_type_get_scheme_type(meta->gtype);
    }
}

static void
make_formals(GICallableInfo *callable,
             GigArgMap *argmap, int n_inputs, SCM self_type, SCM *formals, SCM *specializers)
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
        GigArgMapEntry *entry = gig_amap_get_input_entry_by_s(argmap, s);
        char *formal = scm_dynfree(make_scm_name(entry->name));
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
        gig_debug_load("%s - invalid argument map", name);
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

    keyval_add_entry(function_cache, (uintptr_t) function_info, (uintptr_t) gfn);

    return gfn->function_ptr;
}

static void
gig_callable_prepare_invoke(GigArgMap *amap,
                            const char *name,
                            GObject *self,
                            SCM args,
                            GIArgument *in_args,
                            GIArgument *out_args, slist_t **free_list, GIArgument *out_boxes)
{
    // Convert the scheme arguments into C.
    // For methods calls, the object gets inserted as the 1st argument.
    if (self != NULL) {
        in_args[0].v_pointer = self;
        object_list_to_c_args(amap, name, args, &(in_args)[1], free_list, out_args);
    }
    else {
        object_list_to_c_args(amap, name, args, in_args, free_list, out_args);
    }

    // Since, in the Guile binding, we're allocating the output
    // parameters in most cases, here's where we make space for
    // immediate return arguments.  There's a trick here.  Sometimes
    // GLib expects to use these out_args directly, and sometimes it
    // expects out_args->v_pointer to point to allocated space.  I
    // allocate space for *all* the output arguments, even when not
    // needed.  It is easier than figuring out which output arguments
    // need allocation.
    for (unsigned i = 0; i < amap->c_output_len; i++)
        if (out_args[i].v_pointer == NULL)
            out_args[i].v_pointer = &out_boxes[i];
}

static SCM
gig_callable_return_value(GigArgMap *amap,
                          const char *name,
                          GObject *self,
                          SCM args,
                          bool ok,
                          GIArgument *return_arg,
                          GIArgument *in_args,
                          GIArgument *out_args, slist_t **free_list, GIArgument *out_boxes)
{
    SCM output = SCM_EOL;

    // Here is where I check to see if I used the allocated
    // output argument space created above.
    for (unsigned i = 0; i < amap->c_output_len; i++)
        if (out_args[i].v_pointer == &out_boxes[i])
            memcpy(&out_args[i], &out_boxes[i], sizeof(GIArgument));

    if (ok) {
        SCM s_return;
        size_t sz = -1;
        if (amap->return_val.meta.has_size) {
            size_t idx = amap->return_val.child->c_output_pos;
            sz = out_args[idx].v_size;
        }

        gig_argument_c_to_scm(name, -1, &amap->return_val.meta, return_arg, &s_return, sz);
        if (scm_is_eq(s_return, SCM_UNSPECIFIED))
            output = SCM_EOL;
        else
            output = scm_list_1(s_return);


        if (self)
            output = convert_output_args(amap, name, &(in_args[1]), out_args, output);
        else
            output = convert_output_args(amap, name, in_args, out_args, output);
    }

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
function_invoke(GIFunctionInfo *func_info, GigArgMap *amap, const char *name, GObject *self,
                SCM args, GError **error)
{
    GIArgument *in_args;
    slist_t *free_list;
    GIArgument *out_args;
    GIArgument *out_boxes;

    in_args = xcalloc(amap->c_input_len + 1, sizeof(GIArgument));
    out_args = xcalloc(amap->c_output_len, sizeof(GIArgument));
    out_boxes = xcalloc(amap->c_output_len, sizeof(GIArgument));
    free_list = NULL;

    gig_callable_prepare_invoke(amap, name, self, args, in_args, out_args, &free_list, out_boxes);

    // Make the actual call.
    // Use GObject's ffi to call the C function.
    if (self != NULL)
        gig_debug("%s - calling with self plus %d input and %d output arguments",
                  name, amap->c_input_len, amap->c_output_len);
    else
        gig_debug("%s - calling with %d input and %d output arguments",
                  name, amap->c_input_len, amap->c_output_len);
    gig_amap_dump(name, amap);

    GIArgument return_arg;
    return_arg.v_pointer = NULL;
    bool ok = g_function_info_invoke(func_info, in_args,
                                     amap->c_input_len + (self ? 1 : 0),
                                     out_args,
                                     amap->c_output_len, &return_arg, error);
    SCM ret = gig_callable_return_value(amap, name, self, args, ok, &return_arg,
                                        in_args, out_args,
                                        &free_list, out_boxes);
    slist_free(&free_list, free);
    free(out_boxes);
    free(out_args);
    free(in_args);

    return ret;
}

SCM
gig_callable_invoke(GICallableInfo *callable_info, void *callable, GigArgMap *amap,
                    const char *name, GObject *self, SCM args, GError **error)
{
    GIArgument *in_args;
    slist_t *free_list;
    GIArgument *out_args;
    GIArgument *out_boxes;
    GIArgument return_arg;
    bool ok;

    in_args = xcalloc(amap->c_input_len + 1, sizeof(GIArgument));
    out_args = xcalloc(amap->c_output_len, sizeof(GIArgument));
    out_boxes = xcalloc(amap->c_output_len, sizeof(GIArgument));
    free_list = NULL;

    gig_callable_prepare_invoke(amap, name, self, args, in_args, out_args, &free_list, out_boxes);

    // Make the actual call.
    // Use GObject's ffi to call the C function.
    if (self != NULL)
        gig_debug("%s - calling with self plus %d input and %d output arguments",
                  name, amap->c_input_len, amap->c_output_len);
    else
        gig_debug("%s - calling with %d input and %d output arguments",
                  name, amap->c_input_len, amap->c_output_len);
    gig_amap_dump(name, amap);

    ok = g_callable_info_invoke(callable_info, callable,
                                in_args,
                                amap->c_input_len + (self != NULL ? 1 : 0),
                                out_args,
                                amap->c_output_len, &return_arg,
                                g_callable_info_is_method(callable_info),
                                g_callable_info_can_throw_gerror(callable_info), error);

    SCM ret = gig_callable_return_value(amap, name, self, args, ok, &return_arg,
                                        in_args, out_args,
                                        &free_list, out_boxes);
    slist_free(&free_list, free);
    free(out_boxes);
    free(out_args);
    free(in_args);

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

    if (!scm_is_empty_hook(gig_before_function_hook))
        scm_c_activate_hook_2(gig_before_function_hook, scm_from_utf8_string(gfn->name), s_args);

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
object_to_c_arg(GigArgMap *amap, int s, const char *name, SCM obj,
                GIArgument *in_args, slist_t **free_list, GIArgument *out_args)
{
    // Convert an input scheme argument to a C invoke argument
    GIArgument arg;
    GigArgMapEntry *entry;
    size_t size;
    int i;
    int c_invoke_in, c_invoke_out, c_child_invoke_in;
    bool is_in, is_out;
    bool inout, inout_free;

    entry = gig_amap_get_input_entry_by_s(amap, s);
    gig_argument_scm_to_c(name, s, &entry->meta, obj, free_list, &arg, &size);

    // Store the converted argument.
    is_in = gig_amap_input_s2c(amap, s, &c_invoke_in);
    is_out = gig_amap_input_s_2_output_c(amap, s, &c_invoke_out);
    if (!is_in)
        c_invoke_in = -1;
    if (!is_out)
        c_invoke_out = -1;

    // Input/Output arguments have an extra implied level of
    // indirection.
    inout = is_in && is_out;
    if (inout) {
        gig_amap_input_s2i(amap, s, &i);
        inout_free = (amap->pdata[i].meta.transfer == GI_TRANSFER_NOTHING);
    }
    else
        inout_free = false;
    store_argument(c_invoke_in, c_invoke_out, inout, inout_free, &arg,
                   in_args, free_list, out_args);

    // If this argument is an array with an associated size, store the
    // array size as well.
    if (gig_amap_input_s_2_child_input_c(amap, s, &c_child_invoke_in)) {
        GigArgMapEntry *size_entry = entry->child;
        GIArgument size_arg;
        size_t dummy_size;
        int c_child_invoke_out, i_child;

        gig_argument_scm_to_c(name, s, &size_entry->meta, scm_from_size_t(size),
                              free_list, &size_arg, &dummy_size);

        is_in = gig_amap_input_c2i(amap, c_child_invoke_in, &i_child);
        is_out = gig_amap_output_i2c(amap, i_child, &c_child_invoke_out);
        assert(is_in);
        if (!is_out)
            c_child_invoke_out = -1;

        inout = is_in && is_out;
        if (inout)
            inout_free = (amap->pdata[i_child].meta.transfer == GI_TRANSFER_NOTHING);
        else
            inout_free = false;
        store_argument(c_child_invoke_in, c_child_invoke_out, inout, inout_free, &size_arg,
                       in_args, free_list, out_args);
    }
}

static void
store_argument(int invoke_in, int invoke_out, bool inout, bool inout_free,
               GIArgument *arg, GIArgument *in_args, slist_t **free_list, GIArgument *out_args)
{
    if (invoke_in >= 0) {
        if (inout) {
            void **dup = xmemdup(arg, sizeof(GIArgument));
            in_args[invoke_in].v_pointer = dup;

            if (inout_free)
                slist_prepend(free_list, dup);

            out_args[invoke_out].v_pointer = NULL;
        }
        else {
            memcpy(&in_args[invoke_in], arg, sizeof(GIArgument));
        }
    }
    else if (invoke_out >= 0) {
        memcpy(&out_args[invoke_out], arg, sizeof(GIArgument));
    }
}

static void
object_list_to_c_args(GigArgMap *amap,
                      const char *subr, SCM s_args,
                      GIArgument *in_args, slist_t **free_list, GIArgument *out_args)
{
    int args_count, required, optional;
    if (SCM_UNBNDP(s_args))
        args_count = 0;
    else
        args_count = scm_c_length(s_args);
    gig_amap_s_input_count(amap, &required, &optional);
    if (args_count < required || args_count > required + optional)
        scm_error_num_args_subr(subr);

    for (int i = 0; i < args_count; i++) {
        SCM obj = scm_c_list_ref(s_args, i);
        object_to_c_arg(amap, i, subr, obj, in_args, free_list, out_args);

    }
    return;
}

static GIArgument *
find_output_arg(GigArgMapEntry *entry, GIArgument *in, GIArgument *out)
{
    switch (entry->s_direction) {
    case GIG_ARG_DIRECTION_INPUT:      // may happen with sizes of preallocated outputs
        return in + entry->c_input_pos;
    case GIG_ARG_DIRECTION_INOUT:
        return (in + entry->c_input_pos)->v_pointer;
    case GIG_ARG_DIRECTION_PREALLOCATED_OUTPUT:
    case GIG_ARG_DIRECTION_OUTPUT:
        return out + entry->c_output_pos;
    default:
        assert_not_reached();
    }
}

static SCM
convert_output_args(GigArgMap *amap, const char *func_name, GIArgument *in, GIArgument *out,
                    SCM output)
{
    gig_debug_transfer("%s - convert_output_args", func_name);
    int s_output_pos;

    for (int c_output_pos = 0; c_output_pos < amap->c_output_len; c_output_pos++) {
        if (!gig_amap_output_c2s(amap, c_output_pos, &s_output_pos))
            continue;

        GigArgMapEntry *entry = gig_amap_get_output_entry_by_c(amap, c_output_pos);

        GIArgument *arg;
        arg = find_output_arg(entry, in, out);
        if (arg == NULL)        // an INOUT argument has been eaten
        {
            output = scm_cons(SCM_UNSPECIFIED, output);
            continue;
        }

        SCM obj;
        size_t size = GIG_ARRAY_SIZE_UNKNOWN;

        if (entry->child) {
            // We need to know the size argument before we can process
            // this array argument.
            GigArgMapEntry *size_entry = entry->child;
            GIArgument *size_arg = find_output_arg(size_entry, in, out);
            gig_argument_c_to_scm(func_name, size_entry->i, &size_entry->meta, size_arg, &obj, -1);
            size = scm_is_integer(obj) ? scm_to_int(obj) : 0;
        }

        gig_argument_c_to_scm(func_name, c_output_pos, &entry->meta, arg, &obj, size);
        output = scm_cons(obj, output);
    }
    return scm_reverse_x(output, SCM_EOL);
}

void
gig_init_function(void)
{
    function_cache = keyval_new();

    top_type = scm_get_top_class();
    char_type = scm_get_char_class();
    list_type = scm_get_list_class();
    string_type = scm_get_string_class();

    sym_self = scm_from_utf8_symbol("self");

    gig_before_function_hook = scm_permanent_object(scm_make_hook(scm_from_size_t(2)));
    scm_c_define("%before-function-hook", gig_before_function_hook);
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

inline static void
function_free_v(uintptr_t gfn)
{
    function_free((GigFunction *)gfn);
}


static void
gig_fini_function(void)
{
    gig_debug("Freeing functions");
    keyval_free(function_cache, NULL, function_free_v);
    function_cache = NULL;
}
