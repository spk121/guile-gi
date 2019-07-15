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
#include "gig_argument.h"
#include "gig_util.h"
#include "gig_arg_map.h"
#include "gig_function.h"
#include "gig_type.h"

typedef struct _GigFunction
{
    GIFunctionInfo *function_info;
    ffi_closure *closure;
    ffi_cif cif;
    gpointer function_ptr;
    gchar *name;
    ffi_type **atypes;
    GigArgMap *amap;
} GigFunction;

GHashTable *function_cache;

static SCM generic_table;

static SCM ensure_generic_proc;
static SCM make_proc;
static SCM add_method_proc;

static SCM top_type;
static SCM method_type;

static SCM kwd_specializers;
static SCM kwd_formals;
static SCM kwd_procedure;


static GigGsubr *check_gsubr_cache(GIFunctionInfo *function_info,
                                   gint *required_input_count, gint *optional_input_count,
                                   SCM *formals, SCM *specializers);
static GigGsubr *create_gsubr(GIFunctionInfo *function_info, const gchar *name,
                              gint *required_input_count, gint *optional_input_count,
                              SCM *formals, SCM *specializers);
static void make_formals(GigFunction *fn, gint n_inputs, SCM *formals, SCM *specializers);
static void function_binding(ffi_cif *cif, gpointer ret, gpointer *ffi_args, gpointer user_data);

static SCM convert_output_args(GIFunctionInfo *func_info, GigArgMap *amap, const gchar *name,
                               GArray *out_args);
static void object_list_to_c_args(GIFunctionInfo *func_info, GigArgMap *amap, const gchar *subr,
                                  SCM s_args, GArray *in_args, GArray *cinvoke_input_free_array,
                                  GArray *out_args);
static void function_free(GigFunction *fn);
static void gig_fini_function(void);

static SCM
default_definition(SCM name)
{
#define LOOKUP_DEFINITION(module)                                       \
    do {                                                                \
        SCM variable = scm_module_variable(module, name);               \
        if (scm_is_true(variable)) return scm_variable_ref(variable);   \
    } while (0)

    LOOKUP_DEFINITION(scm_c_resolve_module("gi"));
    LOOKUP_DEFINITION(scm_c_resolve_module("guile"));
    LOOKUP_DEFINITION(scm_current_module());
    return SCM_BOOL_F;
}

// Given some function introspection information from a typelib file,
// this procedure creates a SCM wrapper for that procedure in the
// current module.
void
gig_function_define_gsubr(GType type, GIFunctionInfo *info, const gchar *prefix)
{
    GigGsubr *func_gsubr;
    gchar *public_name;
    gint required_input_count, optional_input_count;
    SCM formals, specializers, self_type;
    gboolean is_method = (g_function_info_get_flags(info) & GI_FUNCTION_IS_METHOD) != 0;
    gboolean was_generic = FALSE;

    if (is_method) {
        self_type = gig_type_get_scheme_type(type);
        g_return_if_fail(scm_is_true(self_type));
    }

    scm_dynwind_begin(0);

    public_name = scm_dynwind_or_bust("%gir-function-define-gsubr",
                                      gi_function_info_make_name(info, prefix));
    g_return_if_fail(public_name != NULL);

    SCM sym_public_name = scm_from_utf8_symbol(public_name);
    SCM generic = scm_hashq_ref(generic_table, sym_public_name, SCM_BOOL_F);
    if (!scm_is_generic(generic)) {
        generic = default_definition(sym_public_name);
        was_generic = scm_is_generic(generic);
        if (!was_generic)
            generic = scm_call_2(ensure_generic_proc, generic, sym_public_name);
        generic = scm_call_2(ensure_generic_proc,
                             default_definition(sym_public_name), sym_public_name);
        scm_hashq_set_x(generic_table, sym_public_name, generic);
    }

    func_gsubr = check_gsubr_cache(info, &required_input_count, &optional_input_count,
                                   &formals, &specializers);
    if (!func_gsubr)
        func_gsubr = create_gsubr(info, public_name, &required_input_count,
                                  &optional_input_count, &formals, &specializers);

    SCM proc = scm_c_make_gsubr(public_name, 0, 0, 1, func_gsubr);

    if (is_method) {
        scm_set_car_x(formals, scm_from_utf8_symbol("self"));
        scm_set_car_x(specializers, self_type);
    }

    SCM t_formals = formals, t_specializers = specializers;

    gint opt = optional_input_count;
    do {
        SCM mthd = scm_call_7(make_proc,
                              method_type,
                              kwd_specializers, t_specializers,
                              kwd_formals, t_formals,
                              kwd_procedure, proc);

        scm_call_2(add_method_proc, generic, mthd);

        if (scm_is_eq(t_formals, SCM_EOL))
            break;

        t_formals = scm_drop_1(t_formals);
        t_specializers = scm_drop_1(t_specializers);
    } while (opt-- > 0);

    g_debug("dynamically bound %s to %s with %d required and %d optional arguments",
            public_name, g_base_info_get_name(info), required_input_count, optional_input_count);

    if (was_generic)
        scm_c_reexport(public_name, NULL);
    else {
        scm_c_define(public_name, generic);
        scm_c_export(public_name, NULL);
    }

    scm_dynwind_end();
}

static GigGsubr *
check_gsubr_cache(GIFunctionInfo *function_info, gint *required_input_count,
                  gint *optional_input_count, SCM *formals, SCM *specializers)
{
    // Check the cache to see if this function has already been created.
    GigFunction *gfn = g_hash_table_lookup(function_cache, function_info);

    if (gfn == NULL)
        return NULL;

    gig_arg_map_get_gsubr_args_count(gfn->amap, required_input_count, optional_input_count);

    if (g_function_info_get_flags(gfn->function_info) & GI_FUNCTION_IS_METHOD)
        (*required_input_count)++;

    make_formals(gfn, *required_input_count + *optional_input_count, formals, specializers);

    return gfn->function_ptr;
}

static void
make_formals(GigFunction *fn, gint n_inputs, SCM *formals, SCM *specializers)
{
    SCM i_formal, i_specializer;

    i_formal = *formals = scm_make_list(scm_from_int(n_inputs), SCM_BOOL_F);
    i_specializer = *specializers = scm_make_list(scm_from_int(n_inputs), top_type);

    if (g_function_info_get_flags(fn->function_info) & GI_FUNCTION_IS_METHOD) {
        // self argument is handled at top
        i_formal = scm_cdr(i_formal);
        i_specializer = scm_cdr(i_specializer);
        n_inputs--;
    }

    for (gint i = 0; i < n_inputs;
         i++, i_formal = scm_cdr(i_formal), i_specializer = scm_cdr(i_specializer)) {
        gchar *formal = g_strdup_printf("arg%d", i);
        scm_set_car_x(i_formal, scm_from_utf8_symbol(formal));

        GigArgMapEntry *entry = gig_arg_map_get_entry(fn->amap, i);

        // don't force types on nullable input, as #f can also be used to represent
        // NULL.
        if (entry->may_be_null)
            continue;

        if (entry->type_tag == GI_TYPE_TAG_INTERFACE) {
            GIBaseInfo *iface = g_type_info_get_interface(entry->type_info);
            if (!GI_IS_REGISTERED_TYPE_INFO(iface))
                continue;
            GType gtype = g_registered_type_info_get_g_type((GIRegisteredTypeInfo *) iface);
            SCM s_type = gig_type_get_scheme_type(gtype);
            if (scm_is_true(s_type))
                scm_set_car_x(i_specializer, s_type);
        }
    }
}

static GigGsubr *
create_gsubr(GIFunctionInfo *function_info, const gchar *name, gint *required_input_count,
             gint *optional_input_count, SCM *formals, SCM *specializers)
{
    GigFunction *gfn;
    ffi_type *ffi_ret_type;

    gfn = g_new0(GigFunction, 1);
    gfn->function_info = function_info;
    gfn->amap = gig_arg_map_new(function_info);
    gfn->name = g_strdup(name);
    g_base_info_ref(function_info);

    gig_arg_map_get_gsubr_args_count(gfn->amap, required_input_count, optional_input_count);

    if (g_function_info_get_flags(gfn->function_info) & GI_FUNCTION_IS_METHOD)
        (*required_input_count)++;

    make_formals(gfn, *required_input_count + *optional_input_count, formals, specializers);

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
    gint have_args = 0;
    if (*required_input_count + *optional_input_count > 0) {
        gfn->atypes = g_new0(ffi_type *, 1);
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

    g_hash_table_insert(function_cache, function_info, gfn);

    return gfn->function_ptr;
}


SCM
gig_function_invoke(GIFunctionInfo *func_info, GigArgMap *amap, const gchar *name, GObject *self,
                    SCM args, GError **error)
{
    GArray *cinvoke_input_arg_array, *cinvoke_input_free_array, *cinvoke_output_arg_array;
    GIArgument *out_args, *out_boxes;
    GIArgument return_arg;
    SCM output;
    gboolean ok;

    cinvoke_input_arg_array = g_array_new(FALSE, TRUE, sizeof(GIArgument));
    cinvoke_output_arg_array = g_array_new(FALSE, TRUE, sizeof(GIArgument));
    cinvoke_input_free_array = g_array_new(FALSE, TRUE, sizeof(guint));

    g_debug("After allocating %s with %d input and %d output arguments",
            name, cinvoke_input_arg_array->len, cinvoke_output_arg_array->len);

    // Convert the scheme arguments into C.
    object_list_to_c_args(func_info, amap, name, args, cinvoke_input_arg_array,
                          cinvoke_input_free_array, cinvoke_output_arg_array);

    g_debug("Before calling %s with %d input and %d output arguments",
            name, cinvoke_input_arg_array->len, cinvoke_output_arg_array->len);
    // For methods calls, the object gets inserted as the 1st argument.
    if (self) {
        GIArgument self_arg;
        guint self_free;
        self_arg.v_pointer = self;
        self_free = GIG_FREE_NONE;
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
    gig_arg_map_dump(amap);
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
    gig_argument_free_args(cinvoke_input_arg_array->len,
                           (guint *)(cinvoke_input_free_array->data),
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
function_binding(ffi_cif *cif, gpointer ret, gpointer *ffi_args, gpointer user_data)
{
    GigFunction *gfn = user_data;
    GObject *self = NULL;
    SCM s_args = SCM_UNDEFINED;

    g_assert(cif != NULL);
    g_assert(ret != NULL);
    g_assert(ffi_args != NULL);
    g_assert(user_data != NULL);

    guint n_args = cif->nargs;
    g_debug("Binding C function %s as %s witn %d args", g_base_info_get_name(gfn->function_info),
            gfn->name, n_args);

    g_assert(n_args < 20);

    if (n_args)
        s_args = SCM_PACK(*(scm_t_bits *) (ffi_args[0]));

    if (SCM_UNBNDP(s_args))
        s_args = SCM_EOL;

    if (g_function_info_get_flags(gfn->function_info) & GI_FUNCTION_IS_METHOD) {
        self = gig_type_peek_object(scm_car(s_args));
        s_args = scm_cdr(s_args);
    }

    // Then invoke the actual function
    GError *err = NULL;
    SCM output = gig_function_invoke(gfn->function_info, gfn->amap, gfn->name, self, s_args, &err);

    // If there is a GError, write an error and exit.
    if (err) {
        gchar str[256];
        memset(str, 0, 256);
        strncpy(str, err->message, 255);
        g_error_free(err);

        scm_misc_error(gfn->name, str, SCM_EOL);
        g_return_if_reached();
    }

    *(ffi_arg *) ret = SCM_UNPACK(output);
}

static void
object_to_c_arg(GigArgMap *amap, gint i, const gchar *name, SCM obj,
                GArray * cinvoke_input_arg_array, GArray * cinvoke_input_free_array,
                GArray * cinvoke_output_arg_array)
{
    // Convert an input scheme argument to a C invoke argument
    GIArgument arg;
    GigArgMapEntry *entry;
    guint arg_free;
    gsize size;
    gint invoke_in, invoke_out;
    GIArgument *parg;
    guint *pfree;

    entry = gig_arg_map_get_entry(amap, i);
    gig_argument_scm_to_c(name, i, entry, obj, &arg_free, &arg, &size);

    // Store the converted argument.
    gig_arg_map_get_cinvoke_indices(amap, i, &invoke_in, &invoke_out);

    if (invoke_in >= 0) {
        parg = &g_array_index(cinvoke_input_arg_array, GIArgument, invoke_in);
        memcpy(parg, &arg, sizeof(GIArgument));
        pfree = &g_array_index(cinvoke_input_free_array, guint, invoke_in);
        memcpy(pfree, &arg_free, sizeof(guint));
    }
    if (invoke_out >= 0) {
        parg = &g_array_index(cinvoke_output_arg_array, GIArgument, invoke_out);
        memcpy(parg, &arg, sizeof(GIArgument));
    }

    // If this argument is an array with an associated size, store the
    // array size as well.
    gig_arg_map_get_cinvoke_array_length_indices(amap, i, &invoke_in, &invoke_out);
    arg.v_size = size;
    arg_free = GIG_FREE_NONE;
    if (invoke_in >= 0) {
        parg = &g_array_index(cinvoke_input_arg_array, GIArgument, invoke_in);
        memcpy(parg, &arg, sizeof(GIArgument));
        pfree = &g_array_index(cinvoke_input_free_array, guint, invoke_in);
        memcpy(pfree, &arg_free, sizeof(guint));
    }
    if (invoke_out >= 0) {
        parg = &g_array_index(cinvoke_output_arg_array, GIArgument, invoke_out);
        memcpy(parg, &arg, sizeof(GIArgument));
    }
}

static void
object_list_to_c_args(GIFunctionInfo *func_info,
                      GigArgMap *amap,
                      const gchar *subr, SCM s_args,
                      GArray *cinvoke_input_arg_array,
                      GArray *cinvoke_input_free_array, GArray *cinvoke_output_arg_array)
{
    g_assert_nonnull(func_info);
    g_assert_nonnull(amap);
    g_assert_nonnull(subr);
    g_assert_nonnull(cinvoke_input_arg_array);
    g_assert_nonnull(cinvoke_input_free_array);
    g_assert_nonnull(cinvoke_output_arg_array);

    gint args_count, required, optional;
    if (SCM_UNBNDP(s_args))
        args_count = 0;
    else
        args_count = scm_to_int(scm_length(s_args));
    gig_arg_map_get_gsubr_args_count(amap, &required, &optional);
    if (args_count < required || args_count > required + optional)
        scm_error_num_args_subr(subr);

    gint input_len, output_len;
    gig_arg_map_get_cinvoke_args_count(amap, &input_len, &output_len);

    g_array_set_size(cinvoke_input_arg_array, input_len);
    g_array_set_size(cinvoke_input_free_array, input_len);
    g_array_set_size(cinvoke_output_arg_array, output_len);

    for (gint i = 0; i < args_count; i++) {
        SCM obj = scm_list_ref(s_args, scm_from_int(i));
        object_to_c_arg(amap, i, subr, obj, cinvoke_input_arg_array, cinvoke_input_free_array,
                        cinvoke_output_arg_array);
    }
    return;
}

static SCM
convert_output_args(GIFunctionInfo *func_info, GigArgMap *amap,
                    const gchar *func_name, GArray * out_args)
{
    SCM output = SCM_EOL;
    gint gsubr_output_index;

    for (guint cinvoke_output_index = 0; cinvoke_output_index < out_args->len;
         cinvoke_output_index++) {
        if (!gig_arg_map_has_gsubr_output_index(amap, cinvoke_output_index, &gsubr_output_index))
            continue;

        GigArgMapEntry *entry = gig_arg_map_get_output_entry(amap, cinvoke_output_index);
        GIArgument *ob = (GIArgument *)(out_args->data);
        SCM obj;
        gint size_index;
        gint size = -1;

        if (gig_arg_map_has_output_array_size_index(amap, cinvoke_output_index, &size_index)) {
            if (size_index < 0) {
                g_assert_not_reached();
            }
            // We need to know the size argument before we can process
            // this array argument.
            else {
                // We haven't processed the size argument yet, so
                // let's to that now.
                GigArgMapEntry *size_entry = gig_arg_map_get_output_entry(amap, size_index);
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
gig_init_function(void)
{
    generic_table = scm_c_make_hash_table(127);
    function_cache =
        g_hash_table_new_full(g_direct_hash, g_direct_equal, NULL,
                              (GDestroyNotify) function_free);
    top_type = scm_c_public_ref("oop goops", "<top>");
    method_type = scm_c_public_ref("oop goops", "<method>");
    ensure_generic_proc = scm_c_public_ref("oop goops", "ensure-generic");
    make_proc = scm_c_public_ref("oop goops", "make");
    add_method_proc = scm_c_public_ref("oop goops", "add-method!");

    kwd_specializers = scm_from_utf8_keyword("specializers");
    kwd_formals = scm_from_utf8_keyword("formals");
    kwd_procedure = scm_from_utf8_keyword("procedure");

    atexit(gig_fini_function);
}

static void
function_free(GigFunction *gfn)
{
    g_free(gfn->name);
    gfn->name = NULL;

    ffi_closure_free(gfn->closure);
    gfn->closure = NULL;

    g_base_info_unref(gfn->function_info);
    g_free(gfn->atypes);
    gfn->atypes = NULL;

    // TODO: should we free gfn->amap?

    g_free(gfn);
}

static void
gig_fini_function(void)
{
    g_debug("Freeing functions");
    g_hash_table_remove_all(function_cache);
    function_cache = NULL;
}
