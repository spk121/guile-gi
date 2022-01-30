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

#include <libguile.h>
#include <girepository.h>
#include "core.h"
#include "gig_type.h"
#include "gig_object.h"
#include "gig_function.h"
#include "gig_constant.h"
#include "gig_flag.h"
#include "gig_repository.h"

static scm_t_bits info_smob_tag = 0;

static SCM
make_info_smob(GIBaseInfo *info)
{
    SCM smob;
    smob = scm_new_smob(info_smob_tag, (size_t)info);
    return smob;
}

static SCM
mark_info_smob(SCM smob)
{
    return SCM_UNSPECIFIED;
}

static size_t
free_info_smob(SCM smob)
{
    GIBaseInfo *info = (GIBaseInfo *)SCM_SMOB_DATA(smob);
    g_base_info_unref(info);
    return 0;
}

static int
print_info_smob(SCM smob, SCM port, scm_print_state * pstate)
{
    GIBaseInfo *info = (GIBaseInfo *)SCM_SMOB_DATA(smob);

    scm_printf(port, "<#RepositoryEntry 0x%x %s>",
               (size_t)info,
               g_base_info_get_name(info));

    /* non-zero means success */
    return 1;
}

static SCM
scm_require(SCM lib, SCM version)
{
    SCM_ASSERT_TYPE(scm_is_string(lib), lib, SCM_ARG1, "require", "string");
    SCM_ASSERT_TYPE(SCM_UNBNDP(version) ||
                    scm_is_string(version), version, SCM_ARG2, "require", "string");

    char *_lib, *_version = NULL;
    GITypelib *tl;
    GError *error = NULL;

    scm_dynwind_begin(0);
    _lib = scm_dynfree(scm_to_utf8_string(lib));
    if (!SCM_UNBNDP(version) && scm_is_true(version))
        _version = scm_dynfree(scm_to_utf8_string(version));

    debug_load("requiring %s-%s", _lib, _version != NULL ? _version : "latest");
    tl = g_irepository_require(NULL, _lib, _version, 0, &error);

    if (tl == NULL) {
        SCM err = scm_from_utf8_string(error->message);
        g_error_free(error);
        scm_misc_error("require", "~A", scm_list_1(err));
    }
    scm_dynwind_end();

    return SCM_UNSPECIFIED;
}

static SCM
scm_get_all_entries(SCM lib)
{
    SCM_ASSERT_TYPE(scm_is_string(lib), lib, SCM_ARG1, "get-all-entries", "string");

    scm_dynwind_begin(0);
    char *_lib = scm_dynfree(scm_to_utf8_string(lib));
    int n = g_irepository_get_n_infos(NULL, _lib);
    SCM infos = SCM_EOL;

    for (int i = 0; i < n; i++) {
        GIBaseInfo *info = g_irepository_get_info(NULL, _lib, i);
        if (g_base_info_is_deprecated(info)) {
            g_base_info_unref(info);
            continue;
        }
        infos = scm_cons(make_info_smob(info), infos);
    }
    scm_dynwind_end();

    return scm_reverse_x(infos, SCM_EOL);
}

typedef enum _LoadFlags
{
    LOAD_INFO_ONLY = 0,
    LOAD_METHODS = 1 << 0,
    LOAD_PROPERTIES = 1 << 1,
    LOAD_SIGNALS = 1 << 2,
    LOAD_EVERYTHING = LOAD_METHODS | LOAD_PROPERTIES | LOAD_SIGNALS
} LoadFlags;

void
gig_repository_nested_infos(GIBaseInfo *base,
                            int *n_methods,
                            GigRepositoryNested *method,
                            int *n_properties,
                            GigRepositoryNested *property,
                            int *n_signals, GigRepositoryNested *signal)
{
    switch (g_base_info_get_type(base)) {
    case GI_INFO_TYPE_STRUCT:
        *n_methods = g_struct_info_get_n_methods(base);
        *method = (GigRepositoryNested)g_struct_info_get_method;
        *n_properties = *n_signals = 0;
        *property = *signal = NULL;
        break;
    case GI_INFO_TYPE_UNION:
        *n_methods = g_union_info_get_n_methods(base);
        *method = (GigRepositoryNested)g_union_info_get_method;
        *n_properties = *n_signals = 0;
        *property = *signal = NULL;
        break;
    case GI_INFO_TYPE_ENUM:
    case GI_INFO_TYPE_FLAGS:
        *n_methods = g_enum_info_get_n_methods(base);
        *method = (GigRepositoryNested)g_enum_info_get_method;
        *n_properties = *n_signals = 0;
        *property = *signal = NULL;
        break;
    case GI_INFO_TYPE_INTERFACE:
        *n_methods = g_interface_info_get_n_methods(base);
        *method = (GigRepositoryNested)g_interface_info_get_method;
        *n_signals = g_interface_info_get_n_signals(base);
        *signal = (GigRepositoryNested)g_interface_info_get_signal;
        *n_properties = g_interface_info_get_n_properties(base);
        *property = (GigRepositoryNested)g_interface_info_get_property;
        {
            GType gtype = g_registered_type_info_get_g_type(base);
            const char *name = g_base_info_get_name(base);
            if (!g_type_is_a(gtype, G_TYPE_OBJECT)) {
                if (*n_properties != 0)
                    warning_load("%s - non-Object interface wants properties", name);
                if (*n_signals != 0)
                    warning_load("%s - non-Object interface wants signals", name);
                *n_properties = *n_signals = 0;
            }
        }
        break;
    case GI_INFO_TYPE_OBJECT:
        *n_methods = g_object_info_get_n_methods(base);
        *method = (GigRepositoryNested)g_object_info_get_method;
        *n_properties = g_object_info_get_n_properties(base);
        *property = (GigRepositoryNested)g_object_info_get_property;
        *n_signals = g_object_info_get_n_signals(base);
        *signal = (GigRepositoryNested)g_object_info_get_signal;
        break;
    default:
        *n_methods = *n_properties = *n_signals = 0;
        *method = *property = *signal = NULL;
    }
}

SCM
load_info(GIBaseInfo *info, LoadFlags flags, SCM defs)
{
    return_val_if_fail(info != NULL, defs);

    GIBaseInfo *parent = g_base_info_get_container(info);
    GType parent_gtype = G_TYPE_INVALID;
    const char *parent_name = NULL;
    if (parent) {
        parent_gtype = g_registered_type_info_get_g_type(parent);
        parent_name = g_base_info_get_name(parent);
    }

    switch (g_base_info_get_type(info)) {
    case GI_INFO_TYPE_CALLBACK:
        debug_load("Unsupported irepository type 'CALLBACK'");
        break;
    case GI_INFO_TYPE_FUNCTION:
    case GI_INFO_TYPE_SIGNAL:
        defs = gig_function_define(parent_gtype, info, parent_name, defs);
        break;
    case GI_INFO_TYPE_PROPERTY:
        defs = gig_property_define(parent_gtype, info, parent_name, defs);
        break;
    case GI_INFO_TYPE_BOXED:
    {
        GType gtype = g_registered_type_info_get_g_type(info);
        if (gtype == G_TYPE_NONE) {
            debug_load("%s - not loading boxed type because is has no GType",
                       g_base_info_get_name(info));
            break;
        }
        defs = gig_type_define(gtype, defs);
        goto recursion;
    }
    case GI_INFO_TYPE_STRUCT:
    {
        GType gtype = g_registered_type_info_get_g_type(info);
        if (gtype == G_TYPE_NONE) {
            debug_load("%s - not loading struct type because is has no GType",
                       g_base_info_get_name(info));
            break;
        }
        defs = gig_type_define(gtype, defs);
        if (g_struct_info_get_size(info) > 0) {
            GQuark size_quark = g_quark_from_string("size");
            g_type_set_qdata(gtype, size_quark, GSIZE_TO_POINTER(g_struct_info_get_size(info)));
        }
        goto recursion;
    }
    case GI_INFO_TYPE_ENUM:
    case GI_INFO_TYPE_FLAGS:
    {
        GType gtype = g_registered_type_info_get_g_type(info);
        if (gtype == G_TYPE_NONE)
            defs = gig_define_enum(info, defs);
        else
            defs = gig_type_define(gtype, defs);
        defs = gig_define_enum_conversions(info, gtype, defs);
        goto recursion;
    }
    case GI_INFO_TYPE_OBJECT:
    {
        GType gtype = g_registered_type_info_get_g_type(info);
        const char *_namespace = g_base_info_get_name(info);
        if (gtype == G_TYPE_INVALID) {
            debug_load("%s - not loading object type because its GType is invalid", _namespace);
            break;
        }
        if (gtype == G_TYPE_NONE) {
            debug_load("%s - not loading object type because is has no GType", _namespace);
            break;
        }

        GIObjectInfo *p = g_object_info_get_parent(info);
        intbool_t has_parent = p ? TRUE : FALSE;
        if (p)
            g_base_info_unref(p);
        if (!has_parent) {
            debug_load("%s:%s - has no parent", _namespace, g_type_name(gtype));
            gig_type_define_fundamental(gtype, SCM_EOL, g_object_ref_sink, g_object_unref);
        }
        defs = gig_type_define(gtype, defs);
        goto recursion;
    }
    case GI_INFO_TYPE_INTERFACE:
    {
        GType gtype = g_registered_type_info_get_g_type(info);
        if (gtype == G_TYPE_NONE) {
            debug_load("%s - not loading interface type because is has no GType",
                       g_base_info_get_name(info));
            break;
        }
        defs = gig_type_define(gtype, defs);
        goto recursion;
    }
    case GI_INFO_TYPE_CONSTANT:
        defs = gig_constant_define(info, defs);
        break;
    case GI_INFO_TYPE_UNION:
    {
        GType gtype = g_registered_type_info_get_g_type(info);
        if (gtype == G_TYPE_NONE) {
            debug_load("%s - not loading union type because is has no GType",
                       g_base_info_get_name(info));
            break;
        }
        defs = gig_type_define(gtype, defs);
        if (g_union_info_get_size(info) > 0) {
            GQuark size_quark = g_quark_from_string("size");
            g_type_set_qdata(gtype, size_quark, GSIZE_TO_POINTER(g_union_info_get_size(info)));
        }
        goto recursion;
    }
    case GI_INFO_TYPE_VALUE:
        critical_load("Unsupported irepository type 'VALUE'");
        break;
    case GI_INFO_TYPE_VFUNC:
        critical_load("Unsupported irepository type 'VFUNC'");
        break;
    case GI_INFO_TYPE_FIELD:
        critical_load("Unsupported irepository type 'FIELD'");
        break;
    case GI_INFO_TYPE_ARG:
        critical_load("Unsupported irepository type 'ARG'");
        break;
    case GI_INFO_TYPE_TYPE:
        critical_load("Unsupported irepository type 'TYPE'");
        break;
    case GI_INFO_TYPE_INVALID:
    case GI_INFO_TYPE_INVALID_0:
    default:
        critical_load("Unsupported irepository type %d '%s'", g_base_info_get_type(info),
                      g_base_info_get_name(info));
        break;
    }

  end:
    return defs;

  recursion:
    {
#define LOAD_NESTED(F, N, I)                                    \
        do {                                                    \
            if (flags & F)                                      \
                for (int i = 0; i < N; i++) {                  \
                    GIBaseInfo *nested_info = I(info, i);       \
                    defs = load_info(nested_info, flags, defs); \
                    g_base_info_unref(nested_info);             \
                }                                               \
        } while (0)

        int n_methods, n_properties, n_signals;
        GigRepositoryNested method, property, nested_signal;

        gig_repository_nested_infos(info, &n_methods, &method, &n_properties, &property,
                                    &n_signals, &nested_signal);

        LOAD_NESTED(LOAD_METHODS, n_methods, method);
        LOAD_NESTED(LOAD_PROPERTIES, n_properties, property);
        LOAD_NESTED(LOAD_SIGNALS, n_signals, nested_signal);
#undef LOAD_NESTED
        goto end;
    }

}

static SCM
scm_load_entry(SCM info, SCM flags)
{
    SCM_ASSERT_TYPE(SCM_UNBNDP(flags) ||
                    scm_is_unsigned_integer(flags, 0, LOAD_EVERYTHING), flags, SCM_ARG2,
                    "load-entry", "integer");

    LoadFlags load_flags;
    if (SCM_UNBNDP(flags))
        load_flags = LOAD_EVERYTHING;
    else
        load_flags = scm_to_uint(flags);

    GIBaseInfo *base_info = (GIBaseInfo *)SCM_SMOB_DATA(info);

    return load_info(base_info, load_flags, SCM_EOL);
}

static SCM
scm_find_entry_by_name(SCM lib, SCM name)
{
    SCM_ASSERT_TYPE(scm_is_string(lib), lib, SCM_ARG1, "find-entry-by-name", "string");
    SCM_ASSERT_TYPE(scm_is_string(name), name, SCM_ARG2, "find-entry-by-name", "string");

    char *_lib, *_name;
    GIBaseInfo *info;
    scm_dynwind_begin(0);
    _lib = scm_dynfree(scm_to_utf8_string(lib));
    _name = scm_dynfree(scm_to_utf8_string(name));

    info = g_irepository_find_by_name(NULL, _lib, _name);
    if (info == NULL)
        scm_misc_error("find-entry-by-name",
                       "could not load ~A from ~A, did you forget to require or perhaps misspell?",
                       scm_list_2(name, lib));
    scm_dynwind_end();

    return make_info_smob(info);
}

static SCM
scm_get_search_path(void)
{
    GSList *slist = g_irepository_get_search_path();
    SCM entry;
    SCM output = SCM_EOL;

    if (slist == NULL)
        return SCM_EOL;
    do {
        entry = scm_from_utf8_string(slist->data);
        output = scm_cons(entry, output);
    } while ((slist = slist->next) != NULL);
    return scm_reverse_x(output, SCM_EOL);
}

static SCM
scm_prepend_search_path_x(SCM s_dir)
{
    char *dir;

    SCM_ASSERT_TYPE(scm_is_string(s_dir), s_dir, SCM_ARG1, "prepend-search-path!", "string");

    dir = scm_to_utf8_string(s_dir);
    g_irepository_prepend_search_path(dir);
    free(dir);

    return SCM_UNSPECIFIED;
}

static SCM
scm_get_dependencies(SCM namespace)
{
    char *_namespace;
    char **_dependencies;
    int i;
    SCM dependencies = SCM_EOL;

    SCM_ASSERT_TYPE(scm_is_string(namespace), namespace, SCM_ARG1, "get-dependencies", "string");
    _namespace = scm_to_utf8_string(namespace);
    _dependencies = g_irepository_get_dependencies(NULL, _namespace);
    free(_namespace);
    if (_dependencies == NULL)
        return SCM_EOL;
    i = 0;
    while (_dependencies[i] != NULL) {
        SCM entry = scm_from_utf8_string(_dependencies[i]);
        dependencies = scm_cons(entry, dependencies);
        free(_dependencies[i]);
        i++;
    }
    free(_dependencies);
    return scm_reverse_x(dependencies, SCM_EOL);
}

void
gig_init_repository()
{
    info_smob_tag = scm_make_smob_type("RepositoryEntry", sizeof(GIBaseInfo *));
    scm_set_smob_mark(info_smob_tag, mark_info_smob);
    scm_set_smob_free(info_smob_tag, free_info_smob);
    scm_set_smob_print(info_smob_tag, print_info_smob);

    scm_c_define_gsubr("%require", 1, 1, 0, scm_require);
    scm_c_define_gsubr("%get-all-entries", 1, 0, 0, scm_get_all_entries);
    scm_c_define_gsubr("%find-entry-by-name", 2, 0, 0, scm_find_entry_by_name);
    scm_c_define_gsubr("%load-entry", 1, 1, 0, scm_load_entry);
    scm_c_define_gsubr("%get-search-path", 0, 0, 0, scm_get_search_path);
    scm_c_define_gsubr("%prepend-search-path!", 1, 0, 0, scm_prepend_search_path_x);
    scm_c_define_gsubr("%get-dependencies", 1, 0, 0, scm_get_dependencies);

#define D(x) scm_permanent_object(scm_c_define(#x, scm_from_uint(x)))

    D(LOAD_INFO_ONLY);
    D(LOAD_METHODS);
    D(LOAD_PROPERTIES);
    D(LOAD_SIGNALS);
    D(LOAD_EVERYTHING);
}
