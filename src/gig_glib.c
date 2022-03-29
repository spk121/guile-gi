#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <dlfcn.h>
#include <glib-object.h>
#include "gig_glib.h"

//#define DYNAMIC_GLIB

struct vtable G;

void
xdlsym(void **p, void *handle, const char *name, int *successes, int *attempts)
{
    if (*p == NULL)
        *p = dlsym(handle, name);
    *attempts = *attempts + 1;
    if (*p != NULL)
        *successes = *successes + 1;
}

bool
gig_init_dlsyms(void *handle)
{
    int i = 0, n = 0;

#ifdef DYNAMIC_GLIB
#define D(name) \
    xdlsym(&G.name, handle, "g_" # name, &i, &n)
#else
#define D(name) \
    G.name = g_ ## name
#endif

#ifdef DYNAMIC_GLIB
    handle = dlopen("libgobject-2.0.so", RTLD_NOW);
#endif

    D(array_free);
    D(array_new);
    D(array_set_clear_func);
    D(boxed_copy);
    D(boxed_free);
    D(byte_array_free);
    D(byte_array_new_take);
    D(closure_add_invalidate_notifier);
    D(closure_get_type);
    D(closure_invoke);
    D(closure_new_simple);
    D(closure_ref);
    D(closure_set_marshal);
    D(closure_sink);
    D(direct_equal);
    D(direct_hash);
    D(double_equal);
    D(double_hash);
    D(error_free);
    D(gstring_get_type);
    D(hash_table_insert);
    D(hash_table_iter_init);
    D(hash_table_iter_next);
    D(hash_table_new_full);
    D(hash_table_size);
    D(hash_table_unref);
    D(int64_equal);
    D(int64_hash);
    D(list_free);
    D(object_class_find_property);
    D(object_class_install_property);
    D(object_class_list_properties);
    D(object_get_property);
    D(object_get_qdata);
    D(object_interface_find_property);
    D(object_new_with_properties);
    D(object_ref);
    D(object_ref_sink);
    D(object_set_property);
    D(object_set_qdata);
    D(object_unref);
    D(once_init_enter);
    D(once_init_leave);
    D(param_spec_get_default_value);
    D(param_spec_ref_sink);
    D(param_spec_types);
    D(param_spec_unref);
    D(param_value_convert);
    D(propagate_error);
    D(ptr_array_add);
    D(ptr_array_free);
    D(ptr_array_new);
    D(quark_from_static_string);
    D(quark_from_string);
    D(signal_accumulator_first_wins);
    D(signal_accumulator_true_handled);
    D(signal_connect_closure_by_id);
    D(signal_emitv);
    D(signal_lookup);
    D(signal_name);
    D(signal_newv);
    D(signal_query);
    D(slist_free);
    D(slist_length);
    D(str_equal);
    D(str_hash);
    D(type_check_class_cast);
    D(type_check_instance_cast);
    D(type_check_instance_is_a);
    D(type_check_instance_is_fundamentally_a);
    D(type_check_value);
    D(type_check_value_holds);
    D(type_children);
    D(type_class_ref);
    D(type_class_unref);
    D(type_default_interface_ref);
    D(type_default_interface_unref);
    D(type_depth);
    D(type_from_name);
    D(type_fundamental);
    D(type_interface_prerequisites);
    D(type_interfaces);
    D(type_is_a);
    D(type_name);
    D(type_parent);
    D(type_query);
    D(type_register_static);
    D(type_test_flags);
    D(value_copy);
    D(value_get_boolean);
    D(value_get_boxed);
    D(value_get_double);
    D(value_get_enum);
    D(value_get_flags);
    D(value_get_float);
    D(value_get_int);
    D(value_get_int64);
    D(value_get_long);
    D(value_get_object);
    D(value_get_param);
    D(value_get_pointer);
    D(value_get_schar);
    D(value_get_string);
    D(value_get_type);
    D(value_get_uchar);
    D(value_get_uint);
    D(value_get_uint64);
    D(value_get_ulong);
    D(value_get_variant);
    D(value_init);
    D(value_set_boolean);
    D(value_set_boxed);
    D(value_set_double);
    D(value_set_enum);
    D(value_set_flags);
    D(value_set_float);
    D(value_set_int);
    D(value_set_int64);
    D(value_set_long);
    D(value_set_object);
    D(value_set_pointer);
    D(value_set_schar);
    D(value_set_uchar);
    D(value_set_uint);
    D(value_set_uint64);
    D(value_set_ulong);
    D(value_take_string);
    D(value_transform);
    D(value_unset);
    D(variant_ref);
    D(variant_ref_sink);
    D(variant_unref);
    return (i == n);
}
