// Copyright (C) 2026 Michael L. Gran

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

// Virtual functions.
//
// Calling: each virtual function of a loaded class or interface gets a
// procedure, (CONTAINER:do-NAME IMPLEMENTOR SELF ARG ...), that calls
// IMPLEMENTOR's implementation on SELF.  Passing a type's parent as
// IMPLEMENTOR chains up.
//
// Overriding: register-type takes a list of (NAME . PROCEDURE).  Each
// NAME is looked up in the parent type and its ancestors, then in the
// interfaces the new type implements, and becomes the offset of its
// slot in that class or interface struct and a C closure that calls
// PROCEDURE.  The type system (src/type) stores the closures in the new
// type's class and interface structs as they are initialized.

#include <string.h>
#include <ffi.h>
#include <glib-object.h>
#include "../core.h"
#include "../type.h"
#include "gig_arg_map.h"
#include "gig_callback_priv.h"
#include "gig_function_priv.h"
#include "gig_vfunc.h"

typedef struct
{
    GIVFuncInfo *info;
    GigArgMap *amap;
    GType container;
    char *name;
    ffi_closure *closure;
    ffi_cif cif;
    ffi_type *atypes[1];
    void *ptr;
} GigVFunc;

// Invokers, by "Namespace.Container.vfunc", so that loading a type in
// several modules makes one procedure.
static SCM invokers;

char *
gig_vfunc_make_name(GIVFuncInfo *info, const char *container)
{
    char *prefix = make_scm_name(container);
    char *name = make_scm_name(g_base_info_get_name(info));
    char *full = concatenate4(prefix, ":do-", name, "");
    free(prefix);
    free(name);
    return full;
}

// (CONTAINER:do-NAME IMPLEMENTOR SELF ARG ...)
static void
vfunc_binding(ffi_cif *cif, void *ret, void **ffi_args, void *user_data)
{
    GigVFunc *gvf = user_data;
    SCM args = SCM_EOL;

    (void)cif;
    scm_init_guile();
    if (cif->nargs > 0)
        args = SCM_PACK(*(scm_t_bits *) (ffi_args[0]));
    if (SCM_UNBNDP(args))
        args = SCM_EOL;
    if (scm_c_length(args) < 2)
        scm_error_num_args_subr(gvf->name);

    SCM s_implementor = scm_car(args);
    SCM s_self = scm_cadr(args);
    args = scm_cddr(args);

    GType implementor = scm_to_gtype_full(s_implementor, gvf->name, SCM_ARG1);
    if (!g_type_is_a(implementor, gvf->container))
        scm_misc_error(gvf->name, "~A is not a ~A",
                       scm_list_2(s_implementor,
                                  scm_from_utf8_string(g_type_name(gvf->container))));

    void *self = NULL;
    if (gig_type_check_typed_object(s_self, gig_type_get_scheme_type(implementor)))
        self = gig_type_peek_object(s_self);
    if (self == NULL)
        scm_wrong_type_arg_msg(gvf->name, SCM_ARG2, s_self, g_type_name(implementor));

    GError *err = NULL;
    void *address = g_vfunc_info_get_address(gvf->info, implementor, &err);
    if (address == NULL) {
        SCM message = err ? scm_from_utf8_string(err->message)
            : scm_from_utf8_string("no implementation");
        if (err)
            g_error_free(err);
        scm_misc_error(gvf->name, "~A: ~A", scm_list_2(s_implementor, message));
    }

    SCM output = gig_callable_invoke(gvf->info, address, gvf->amap, gvf->name, self, args, &err);
    if (err) {
        SCM message = scm_from_utf8_string(err->message);
        g_error_free(err);
        scm_misc_error(gvf->name, "~A", scm_list_1(message));
    }
    *(ffi_arg *)ret = SCM_UNPACK(output);
}

static SCM
make_invoker(GIVFuncInfo *info, GType container, const char *name)
{
    GigArgMap *amap = gig_amap_new(name, info);
    if (amap == NULL)
        return SCM_BOOL_F;

    GigVFunc *gvf = xcalloc(1, sizeof(GigVFunc));
    gvf->info = g_base_info_ref(info);
    gvf->amap = amap;
    gvf->container = container;
    gvf->name = xstrdup(name);
    gvf->atypes[0] = &ffi_type_pointer;

    // One argument: the list of all of them.
    gvf->closure = ffi_closure_alloc(sizeof(ffi_closure), &gvf->ptr);
    if (gvf->closure == NULL
        || ffi_prep_cif(&gvf->cif, FFI_DEFAULT_ABI, 1, &ffi_type_pointer, gvf->atypes) != FFI_OK
        || ffi_prep_closure_loc(gvf->closure, &gvf->cif, vfunc_binding, gvf, gvf->ptr) != FFI_OK)
        gig_error("%s: could not make a closure", name);

    return scm_c_make_gsubr(name, 0, 0, 1, gvf->ptr);
}

SCM
gig_vfunc_define(GIVFuncInfo *info, const char *container_name)
{
    GIBaseInfo *container = g_base_info_get_container(info);
    GType container_gtype = g_registered_type_info_get_g_type(container);
    if (container_gtype == G_TYPE_NONE || container_gtype == G_TYPE_INVALID)
        return SCM_EOL;

    char *name = gig_vfunc_make_name(info, container_name);
    char *key = concatenate4(g_base_info_get_namespace(container), ".",
                             g_base_info_get_name(container), ".");
    char *full_key = concatenate(key, g_base_info_get_name(info));
    free(key);
    SCM s_key = scm_from_utf8_string(full_key);
    free(full_key);

    SCM proc = scm_hash_ref(invokers, s_key, SCM_BOOL_F);
    if (scm_is_false(proc)) {
        proc = make_invoker(info, container_gtype, name);
        if (scm_is_false(proc)) {
            gig_debug_load("%s - invalid argument map", name);
            free(name);
            return SCM_EOL;
        }
        scm_hash_set_x(invokers, s_key, proc);
    }
    gig_debug_load("%s - bound to virtual function %s.%s", name,
                   g_base_info_get_name(container), g_base_info_get_name(info));

    SCM sym = scm_from_utf8_symbol(name);
    free(name);
    scm_define(sym, proc);
    return scm_list_1(sym);
}

////////////////////////////////////////////////////////////////
// Overrides
////////////////////////////////////////////////////////////////

// A virtual function's GI name from a symbol or string, written either
// way: size-allocate or size_allocate.
static char *
vfunc_name(SCM key, const char *subr)
{
    char *name;
    if (scm_is_symbol(key))
        name = scm_to_utf8_symbol(key);
    else if (scm_is_string(key))
        name = scm_to_utf8_string(key);
    else
        scm_wrong_type_arg_msg(subr, SCM_ARG3, key, "symbol or string");
    for (char *p = name; *p; p++)
        if (*p == '-')
            *p = '_';
    return name;
}

// The virtual function NAME of PARENT or an ancestor, or of one of
// INTERFACES (a vector of GTypes).  Returns a new reference, or NULL.
static GIVFuncInfo *
find_vfunc(GType parent, GType *interfaces, size_t n_interfaces, const char *name)
{
    for (GType t = parent; t != 0; t = g_type_parent(t)) {
        GIBaseInfo *info = g_irepository_find_by_gtype(NULL, t);
        if (info == NULL)
            continue;           // e.g. a type defined in Scheme
        GIVFuncInfo *vf = NULL;
        if (GI_IS_OBJECT_INFO(info))
            vf = g_object_info_find_vfunc(info, name);
        g_base_info_unref(info);
        if (vf != NULL)
            return vf;
    }
    for (size_t i = 0; i < n_interfaces; i++) {
        GIBaseInfo *info = g_irepository_find_by_gtype(NULL, interfaces[i]);
        GIVFuncInfo *vf = NULL;
        if (info != NULL && GI_IS_INTERFACE_INFO(info))
            vf = g_interface_info_find_vfunc(info, name);
        if (info != NULL)
            g_base_info_unref(info);
        if (vf != NULL)
            return vf;
    }
    return NULL;
}

// The offset of virtual function VF's slot in its container's class
// or interface struct, or -1.
static int
vfunc_offset(GIVFuncInfo *vf)
{
    GIBaseInfo *container = g_base_info_get_container(vf);
    GIStructInfo *klass = NULL;
    if (GI_IS_OBJECT_INFO(container))
        klass = g_object_info_get_class_struct(container);
    else if (GI_IS_INTERFACE_INFO(container))
        klass = g_interface_info_get_iface_struct(container);
    if (klass == NULL)
        return -1;

    int offset = -1;
    int n = g_struct_info_get_n_fields(klass);
    for (int i = 0; i < n && offset < 0; i++) {
        GIFieldInfo *field = g_struct_info_get_field(klass, i);
        if (strcmp(g_base_info_get_name(field), g_base_info_get_name(vf)) == 0)
            offset = g_field_info_get_offset(field);
        g_base_info_unref(field);
    }
    g_base_info_unref(klass);
    return offset;
}

// Virtual functions guile-gi relies on for every type it defines.
static bool
is_reserved(GIVFuncInfo *vf)
{
    GIBaseInfo *container = g_base_info_get_container(vf);
    const char *name = g_base_info_get_name(vf);
    return (strcmp(g_base_info_get_namespace(container), "GObject") == 0
            && strcmp(g_base_info_get_name(container), "Object") == 0
            && (strcmp(name, "set_property") == 0 || strcmp(name, "get_property") == 0
                || strcmp(name, "finalize") == 0 || strcmp(name, "constructor") == 0));
}

// (%vfunc-overrides PARENT INTERFACES OVERRIDES): for each (NAME .
// PROCEDURE) in OVERRIDES, (STRUCT-GTYPE OFFSET . C-POINTER), where
// STRUCT-GTYPE is the class or interface whose struct has the slot.
static SCM
scm_vfunc_overrides(SCM s_parent, SCM s_interfaces, SCM s_overrides)
{
#define FUNC "%vfunc-overrides"
    GType parent = scm_to_gtype_full(s_parent, FUNC, SCM_ARG1);
    SCM_ASSERT_TYPE(scm_is_list(s_interfaces), s_interfaces, SCM_ARG2, FUNC, "list");
    SCM_ASSERT_TYPE(scm_is_list(s_overrides), s_overrides, SCM_ARG3, FUNC, "list");

    scm_dynwind_begin(0);
    size_t n_interfaces = scm_c_length(s_interfaces);
    GType *interfaces = scm_dynfree(xcalloc(n_interfaces + 1, sizeof(GType)));
    for (size_t i = 0; i < n_interfaces; i++, s_interfaces = scm_cdr(s_interfaces))
        interfaces[i] = scm_to_gtype_full(scm_car(s_interfaces), FUNC, SCM_ARG2);

    SCM result = SCM_EOL;
    for (; !scm_is_null(s_overrides); s_overrides = scm_cdr(s_overrides)) {
        SCM entry = scm_car(s_overrides);
        if (!scm_is_pair(entry) || !scm_is_true(scm_procedure_p(scm_cdr(entry))))
            scm_wrong_type_arg_msg(FUNC, SCM_ARG3, entry, "pair of a name and a procedure");

        scm_dynwind_begin(0);
        char *name = scm_dynfree(vfunc_name(scm_car(entry), FUNC));
        GIVFuncInfo *vf = find_vfunc(parent, interfaces, n_interfaces, name);
        if (vf == NULL)
            scm_misc_error(FUNC, "~A has no virtual function ~A, nor do its interfaces",
                           scm_list_2(s_parent, scm_car(entry)));
        scm_dynwind_unwind_handler((void (*)(void *))g_base_info_unref, vf,
                                   SCM_F_WIND_EXPLICITLY);
        if (is_reserved(vf))
            scm_misc_error(FUNC, "guile-gi does not allow overriding ~A",
                           scm_list_1(scm_car(entry)));

        int offset = vfunc_offset(vf);
        if (offset < 0)
            scm_misc_error(FUNC, "the slot of virtual function ~A is unknown",
                           scm_list_1(scm_car(entry)));

        // Check that it can be represented before making a closure:
        // making one for an unrepresentable function is fatal.
        GigArgMap *amap = gig_amap_new(name, vf);
        if (amap == NULL)
            scm_misc_error(FUNC, "virtual function ~A has arguments guile-gi can't represent",
                           scm_list_1(scm_car(entry)));
        gig_amap_free(amap);

        void *closure = gig_callback_to_c(name, vf, scm_cdr(entry));
        GType struct_gtype = g_registered_type_info_get_g_type(g_base_info_get_container(vf));
        result = scm_cons(scm_cons2(scm_from_gtype(struct_gtype), scm_from_int(offset),
                                    scm_from_pointer(closure, NULL)), result);
        scm_dynwind_end();
    }
    scm_dynwind_end();
    return scm_reverse_x(result, SCM_EOL);
#undef FUNC
}

void
gig_init_vfunc(void)
{
    invokers = scm_permanent_object(scm_c_make_hash_table(64));
    scm_c_define_gsubr("%vfunc-overrides", 3, 0, 0, scm_vfunc_overrides);
}
