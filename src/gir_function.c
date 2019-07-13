// Copyright (C) 2018, 2019 Michael L. Gran

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

#include <ffi.h>
#include "gi_function_info.h"
#include "gi_giargument.h"
#include "gi_util.h"
#include "gir_arg_map.h"
#include "gir_function.h"

typedef SCM (*gir_gsubr_t)(void);
typedef struct _GirFunction
{
    GIFunctionInfo *function_info;
    ffi_closure *closure;
    ffi_cif cif;
    void *function_ptr;
    char *name;
    ffi_type **atypes;
    GirArgMap *amap;
} GirFunction;

GSList *function_list = NULL;

static gir_gsubr_t *check_gsubr_cache(GIFunctionInfo *function_info,
                                      int *required_input_count, int *optional_input_count);
static gir_gsubr_t *create_gsubr(GIFunctionInfo *function_info,
                                 const char *name,
                                 int *required_input_count, int *optional_input_count);
static void function_binding(ffi_cif *cif, void *ret, void **ffi_args, void *user_data);

static SCM convert_output_args(GIFunctionInfo *func_info, GirArgMap *amap, const char *name,
                               GArray * out_args);
static void object_list_to_c_args(GIFunctionInfo *func_info, GirArgMap *amap, const char *subr,
                                  SCM s_args, GArray * in_args, GArray * cinvoke_input_free_array,
                                  GArray * out_args);

static void gir_function_free(GirFunction * fn);
static void gir_fini_function(void);


// Given some function introspection information from a typelib file,
// this procedure creates a SCM wrapper for that procedure in the
// current module.
void
gir_function_define_gsubr(GIFunctionInfo *info, const char *prefix)
{
    gir_gsubr_t *func_gsubr;
    char *name;
    int required_input_count, optional_input_count;

    name = gi_function_info_make_name(info, prefix);
    func_gsubr = check_gsubr_cache(info, &required_input_count, &optional_input_count);
    if (!func_gsubr) {
        func_gsubr = create_gsubr(info, name, &required_input_count, &optional_input_count);
        g_debug("dynamically bound %s to %s with %d required and %d optional arguments",
                name, g_base_info_get_name(info), required_input_count, optional_input_count);
    }
    scm_c_define_gsubr(name, required_input_count, optional_input_count, 0, func_gsubr);
    scm_c_export(name, NULL);

    g_free(name);
}

static gir_gsubr_t *
check_gsubr_cache(GIFunctionInfo *function_info, int *required_input_count,
                  int *optional_input_count)
{
    // Check the cache to see if this function has already been created.
    GSList *x = function_list;
    GirFunction *gfn;

    while (x != NULL) {
        gfn = x->data;
        if (gfn->function_info == function_info) {
            gir_arg_map_get_gsubr_args_count(gfn->amap, required_input_count,
                                             optional_input_count);
            return gfn->function_ptr;
        }
        x = x->next;
    }
    return NULL;
}

static gir_gsubr_t *
create_gsubr(GIFunctionInfo *function_info, const char *name, int *required_input_count,
             int *optional_input_count)
{
    GirFunction *gfn;
    ffi_type *ffi_ret_type;

    gfn = g_new0(GirFunction, 1);
    gfn->function_info = function_info;
    gfn->amap = gir_arg_map_new(function_info);
    gfn->name = g_strdup(name);
    g_base_info_ref(function_info);

    gir_arg_map_get_gsubr_args_count(gfn->amap, required_input_count, optional_input_count);

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
    if (*required_input_count + *optional_input_count > 0) {
        gfn->atypes = g_new0(ffi_type *, *required_input_count + *optional_input_count);
    }
    else
        gfn->atypes = NULL;
    // All of our arguments will be SCM, so we use pointer storage.
    for (int i = 0; i < *required_input_count + *optional_input_count; i++)
        gfn->atypes[i] = &ffi_type_pointer;
    // The return type is also SCM, for which we use a pointer.
    ffi_ret_type = &ffi_type_pointer;

    // Initialize the CIF Call Interface Struct.
    ffi_status prep_ok;
    prep_ok = ffi_prep_cif(&(gfn->cif),
                           FFI_DEFAULT_ABI, *required_input_count + *optional_input_count,
                           ffi_ret_type, gfn->atypes);

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

    // We add the allocated structs to a list so we can deallocate
    // nicely later.
    function_list = g_slist_prepend(function_list, gfn);

    return gfn->function_ptr;
}


SCM
gir_function_invoke(GIFunctionInfo *func_info, GirArgMap *amap, const char *name, GObject *self,
                    SCM args, GError **error)
{
    GArray *cinvoke_input_arg_array, *cinvoke_input_free_array, *cinvoke_output_arg_array;
    GIArgument *out_args, *out_boxes;
    GIArgument return_arg;
    SCM output;
    gboolean ok;

    cinvoke_input_arg_array = g_array_new(FALSE, TRUE, sizeof(GIArgument));
    cinvoke_output_arg_array = g_array_new(FALSE, TRUE, sizeof(GIArgument));
    cinvoke_input_free_array = g_array_new(FALSE, TRUE, sizeof(unsigned));

    g_debug("After allocation g %s with %d input and %d output arguments",
            name, cinvoke_input_arg_array->len, cinvoke_output_arg_array->len);

    // Convert the scheme arguments into C.
    object_list_to_c_args(func_info, amap, name, args, cinvoke_input_arg_array,
                          cinvoke_input_free_array, cinvoke_output_arg_array);

    g_debug("Before calling %s with %d input and %d output arguments",
            name, cinvoke_input_arg_array->len, cinvoke_output_arg_array->len);
    // For methods calls, the object gets inserted as the 1st argument.
    if (self) {
        GIArgument self_arg;
        unsigned self_free;
        self_arg.v_pointer = self;
        self_free = GIR_FREE_NONE;
        g_array_prepend_val(cinvoke_input_arg_array, self_arg);
        g_array_prepend_val(cinvoke_input_free_array, self_free);
    }

    // Since, in the Guile binding, we're allocating the output
    // parameters in most cases, here's where we make space for
    // immediate return arguments.  There's a trick here.  Sometimes
    // GLib expects to use these out_args directly, and sometimes
    // it expects out_args->v_pointer to point to allocated space.
    // I allocate
    // space for *all* the output arguments, even when not needed.
    // It is easier than figuring out which output arguments need
    // allocation.
    out_args = (GIArgument *)(cinvoke_output_arg_array->data);
    out_boxes = g_new0(GIArgument, cinvoke_output_arg_array->len);
    for (guint i = 0; i < cinvoke_output_arg_array->len; i++)
        if (out_args[i].v_pointer == NULL)
            out_args[i].v_pointer = &out_boxes[i];

    // Make the actual call.
    // Use GObject's ffi to call the C function.
    g_debug("Calling %s with %d input and %d output arguments",
            name, cinvoke_input_arg_array->len, cinvoke_output_arg_array->len);
    gir_arg_map_dump(amap);
    ok = g_function_info_invoke(func_info, (GIArgument *)(cinvoke_input_arg_array->data),
                                cinvoke_input_arg_array->len,
                                (GIArgument *)(cinvoke_output_arg_array->data),
                                cinvoke_output_arg_array->len, &return_arg, error);

    // Here is where I check to see if I used the allocated
    // output argument space created above.
    for (guint i = 0; i < cinvoke_output_arg_array->len; i++)
        if (out_args[i].v_pointer == &out_boxes[i]) {
            memcpy(&out_args[i], &out_boxes[i], sizeof(GIArgument));
        }
    g_free(out_boxes);

    if (ok) {
        SCM s_return;
        gsize sz = -1;
        if (amap->return_val->array_length_index >= 0) {
            g_assert_cmpint(amap->return_val->array_length_index, <, amap->len);
            gsize idx = amap->pdata[amap->return_val->array_length_index]->cinvoke_output_index;
            sz = ((GIArgument *)(cinvoke_output_arg_array->data))[idx].v_size;
        }

        gig_argument_c_to_scm(name, -1, amap->return_val, &return_arg, NULL, &s_return, sz);
        if (scm_is_eq(s_return, SCM_UNSPECIFIED))
            output = SCM_EOL;
        else
            output = scm_list_1(s_return);

        SCM output2 = convert_output_args(func_info, amap, name, cinvoke_output_arg_array);
        output = scm_append(scm_list_2(output, output2));
    }

    // Sometimes input data transfers ownership to the C side,
    // so we can't free indiscriminately.
    gi_giargument_free_args(cinvoke_input_arg_array->len,
                            (unsigned *)(cinvoke_input_free_array->data),
                            (GIArgument *)(cinvoke_input_arg_array->data));

    g_array_free(cinvoke_input_arg_array, TRUE);
    g_array_free(cinvoke_input_free_array, TRUE);
    g_array_free(cinvoke_output_arg_array, TRUE);

    if (!ok)
        return SCM_UNSPECIFIED;

    switch (scm_to_int(scm_length(output))) {
    case 0:
        return SCM_UNSPECIFIED;
    case 1:
        return scm_car(output);
    default:
        return scm_values(output);
    }
}

// This is the core of a dynamically generated GICallable function wrapper.
// It converts FFI arguments to SCM arguments, converts those
// SCM arguments into GIArguments, calls the C function,
// and returns the results as an SCM packed into an FFI argument.
// Also, it converts GErrors into SCM misc-errors.
static void
function_binding(ffi_cif *cif, void *ret, void **ffi_args, void *user_data)
{
    GirFunction *gfn = user_data;
    SCM s_args = SCM_EOL;

    g_assert(cif != NULL);
    g_assert(ret != NULL);
    g_assert(ffi_args != NULL);
    g_assert(user_data != NULL);

    unsigned int n_args = cif->nargs;
    g_debug("Binding C function %s as %s witn %d args", g_base_info_get_name(gfn->function_info),
            gfn->name, n_args);

    g_assert(n_args < 20);

    // First, convert pointers to SCM
    for (unsigned int i = 0; i < n_args; i++) {
        SCM s_entry = SCM_BOOL_F;

        s_entry = SCM_PACK(*(scm_t_bits *) (ffi_args[i]));
        // I convert unspecified arguments to false because,
        // once you get here, unspecified arguments are shorthand for null
        // pointers
        if (SCM_UNBNDP(s_entry))
            s_args = scm_append(scm_list_2(s_args, scm_list_1(SCM_BOOL_F)));
        else
            s_args = scm_append(scm_list_2(s_args, scm_list_1(s_entry)));
    }

    // Then invoke the actual function
    GError *err = NULL;
    SCM output = gir_function_invoke(gfn->function_info, gfn->amap, gfn->name, NULL, s_args, &err);

    // If there is a GError, write an error and exit.
    if (err) {
        char str[256];
        memset(str, 0, 256);
        strncpy(str, err->message, 255);
        g_error_free(err);

        scm_misc_error(gfn->name, str, SCM_EOL);
        g_return_if_reached();
    }

    *(ffi_arg *) ret = SCM_UNPACK(output);
}

static void
object_to_c_arg(GirArgMap *amap, gint i, const gchar *name, SCM obj,
                GArray * cinvoke_input_arg_array, GArray * cinvoke_input_free_array,
                GArray * cinvoke_output_arg_array)
{
    // Convert an input scheme argument to a C invoke argument
    GIArgument arg;
    GirArgMapEntry *entry;
    unsigned arg_free;
    gsize size;
    gint invoke_in, invoke_out;
    GIArgument *parg;
    guint *pfree;

    entry = gig_arg_map_get_entry(amap, i);
    gig_argument_scm_to_c(name, i, entry, obj, &arg_free, &arg, &size);

    // Store the converted argument.
    gir_arg_map_get_cinvoke_indices(amap, i, &invoke_in, &invoke_out);

    if (invoke_in >= 0) {
        parg = &g_array_index(cinvoke_input_arg_array, GIArgument, invoke_in);
        memcpy(parg, &arg, sizeof(GIArgument));
        pfree = &g_array_index(cinvoke_input_free_array, unsigned, invoke_in);
        memcpy(pfree, &arg_free, sizeof(unsigned));
    }
    if (invoke_out >= 0) {
        parg = &g_array_index(cinvoke_output_arg_array, GIArgument, invoke_out);
        memcpy(parg, &arg, sizeof(GIArgument));
    }

    // If this argument is an array with an associated size, store the
    // array size as well.
    gir_arg_map_get_cinvoke_array_length_indices(amap, i, &invoke_in, &invoke_out);
    arg.v_size = size;
    arg_free = GIR_FREE_NONE;
    if (invoke_in >= 0) {
        parg = &g_array_index(cinvoke_input_arg_array, GIArgument, invoke_in);
        memcpy(parg, &arg, sizeof(GIArgument));
        pfree = &g_array_index(cinvoke_input_free_array, unsigned, invoke_in);
        memcpy(pfree, &arg_free, sizeof(unsigned));
    }
    if (invoke_out >= 0) {
        parg = &g_array_index(cinvoke_output_arg_array, GIArgument, invoke_out);
        memcpy(parg, &arg, sizeof(GIArgument));
    }
}

static void
object_list_to_c_args(GIFunctionInfo *func_info,
                      GirArgMap *amap,
                      const char *subr, SCM s_args,
                      GArray * cinvoke_input_arg_array,
                      GArray * cinvoke_input_free_array, GArray * cinvoke_output_arg_array)
{
    g_assert_nonnull(func_info);
    g_assert_nonnull(amap);
    g_assert_nonnull(subr);
    g_assert_nonnull(cinvoke_input_arg_array);
    g_assert_nonnull(cinvoke_input_free_array);
    g_assert_nonnull(cinvoke_output_arg_array);

    int args_count, required, optional;
    if (SCM_UNBNDP(s_args))
        args_count = 0;
    else
        args_count = scm_to_int(scm_length(s_args));
    gir_arg_map_get_gsubr_args_count(amap, &required, &optional);
    if (args_count < required || args_count > required + optional)
        scm_error_num_args_subr(subr);

    int input_len, output_len;
    gir_arg_map_get_cinvoke_args_count(amap, &input_len, &output_len);

    g_array_set_size(cinvoke_input_arg_array, input_len);
    g_array_set_size(cinvoke_input_free_array, input_len);
    g_array_set_size(cinvoke_output_arg_array, output_len);

    for (int i = 0; i < args_count; i++) {
        SCM obj = scm_list_ref(s_args, scm_from_int(i));
        object_to_c_arg(amap, i, subr, obj, cinvoke_input_arg_array, cinvoke_input_free_array,
                        cinvoke_output_arg_array);
    }
    return;
}

static SCM
convert_output_args(GIFunctionInfo *func_info, GirArgMap *amap,
                    const char *func_name, GArray * out_args)
{
    SCM output = SCM_EOL;
    int gsubr_output_index;

    for (guint cinvoke_output_index = 0; cinvoke_output_index < out_args->len;
         cinvoke_output_index++) {
        if (!gir_arg_map_has_gsubr_output_index(amap, cinvoke_output_index, &gsubr_output_index))
            continue;

        GirArgMapEntry *entry = gig_arg_map_get_output_entry(amap, cinvoke_output_index);
        GIArgument *ob = (GIArgument *)(out_args->data);
        SCM obj;
        int size_index;
        int size = -1;

        if (gir_arg_map_has_output_array_size_index(amap, cinvoke_output_index, &size_index)) {
            if (size_index < 0) {
                g_assert_not_reached();
            }
            // We need to know the size argument before we can process
            // this array argument.
            else {
                // We haven't processed the size argument yet, so
                // let's to that now.
                GirArgMapEntry *size_entry = gig_arg_map_get_output_entry(amap, size_index);
                gig_argument_c_to_scm(func_name, size_index, size_entry, &ob[size_index], NULL,
                                      &obj, -1);
                size = scm_to_int(obj);
            }
        }

        gig_argument_c_to_scm(func_name, cinvoke_output_index, entry, &ob[cinvoke_output_index],
                              NULL, &obj, size);
        output = scm_append(scm_list_2(output, scm_list_1(obj)));
    }
    return output;
}

void
gir_init_function(void)
{
    atexit(gir_fini_function);
}

static void
gir_function_free(GirFunction * gfn)
{
    g_free(gfn->name);
    gfn->name = NULL;

    ffi_closure_free(gfn->closure);
    gfn->closure = NULL;

    g_base_info_unref(gfn->function_info);
    g_free(gfn->atypes);
    gfn->atypes = NULL;

    g_free(gfn);
}

static void
gir_fini_function(void)
{
    g_debug("Freeing functions");
    g_slist_free_full(function_list, (GDestroyNotify) gir_function_free);
    function_list = NULL;
}
