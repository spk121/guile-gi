#ifndef GIG_GLIB_H
#define GIG_GLIB_H

#include <stdbool.h>
#include <glib-object.h>

bool gig_init_dlsyms(void *handle);

struct vtable
{
    gchar *(*array_free)(GArray *, gboolean);
    GArray *(*array_new)(gboolean, gboolean, guint);
    void (*array_set_clear_func)(GArray *, void(*)(void *));
    void *(*boxed_copy)(GType, const void *);
    void (*boxed_free)(GType, void *);
    guint8 *(*byte_array_free)(GByteArray *, gboolean);
    GByteArray *(*byte_array_new_take)(guint8 *, gsize);
    void (*closure_add_invalidate_notifier)(GClosure *, void *, void(*)(void *, GClosure *));
    GType (*closure_get_type)(void);
    void (*closure_invoke)(GClosure *, GValue *, guint, const GValue *, void *);
    GClosure *(*closure_new_simple)(guint, void *);
    GClosure *(*closure_ref)(GClosure *);
    void (*closure_set_marshal)(GClosure *,
                                void(*)(GClosure *, GValue *, guint, const GValue *, void *,
                                        void *));
    void (*closure_sink)(GClosure *);
    gboolean (*direct_equal)(const void *, const void *);
    guint (*direct_hash)(const void *);
    gboolean (*double_equal)(const void *, const void *);
    guint (*double_hash)(const void *);
    void (*error_free)(GError *);
    GType (*gstring_get_type)(void);
    gboolean (*hash_table_insert)(GHashTable *, void *, void *);
    void (*hash_table_iter_init)(GHashTableIter *, GHashTable *);
    gboolean (*hash_table_iter_next)(GHashTableIter *, void **, void **);
    GHashTable *(*hash_table_new_full)(guint(*)(const void *),
                                       gboolean(*)(const void *, const void *), void(*)(void *),
                                       void(*)(void *));
    guint (*hash_table_size)(GHashTable *);
    void (*hash_table_unref)(GHashTable *);
    gboolean (*int64_equal)(const void *, const void *);
    guint (*int64_hash)(const void *);
    void (*list_free)(GList *);
    GParamSpec *(*object_class_find_property)(GObjectClass *, const gchar *);
    void (*object_class_install_property)(GObjectClass *, guint, GParamSpec *);
    GParamSpec **(*object_class_list_properties)(GObjectClass *, guint *);
    void (*object_get_property)(GObject *, const gchar *, GValue *);
    void *(*object_get_qdata)(GObject *, GQuark);
    GParamSpec *(*object_interface_find_property)(void *, const gchar *);
    GObject *(*object_new_with_properties)(GType, guint, const char **, const GValue *);
    void *(*object_ref)(void *);
    void *(*object_ref_sink)(void *);
    void (*object_set_property)(GObject *, const gchar *, const GValue *);
    void (*object_set_qdata)(GObject *, GQuark, void *);
    void (*object_unref)(void *);
    gboolean (*once_init_enter)(volatile void *);
    void (*once_init_leave)(volatile void *, gsize);
    const GValue *(*param_spec_get_default_value)(GParamSpec *);
    GParamSpec *(*param_spec_ref_sink)(GParamSpec *);
    GType *param_spec_types;
    void (*param_spec_unref)(GParamSpec *);
    gboolean (*param_value_convert)(GParamSpec *, const GValue *, GValue *, gboolean);
    void (*propagate_error)(GError **, GError *);
    void (*ptr_array_add)(GPtrArray *, void *);
    void **(*ptr_array_free)(GPtrArray *, gboolean);
    GPtrArray *(*ptr_array_new)(void);
    GQuark (*quark_from_static_string)(const gchar *);
    GQuark (*quark_from_string)(const gchar *);
    gboolean (*signal_accumulator_first_wins)(GSignalInvocationHint *, GValue *, const GValue *,
                                              void *);
    gboolean (*signal_accumulator_true_handled)(GSignalInvocationHint *, GValue *, const GValue *,
                                                void *);
    gulong (*signal_connect_closure_by_id)(void *, guint, GQuark, GClosure *, gboolean);
    void (*signal_emitv)(const GValue *, guint, GQuark, GValue *);
    guint (*signal_lookup)(const char *, GType);
    const gchar *(*signal_name)(guint);
    guint (*signal_newv)(const gchar *, GType, GSignalFlags, GClosure *,
                         gboolean(*)(GSignalInvocationHint *, GValue *, const GValue *, void *),
                         void *, void(*)(GClosure *, GValue *, guint, const GValue *, void *,
                                         void *), GType, guint, GType *);
    void (*signal_query)(guint, GSignalQuery *);
    void (*slist_free)(GSList *);
    guint (*slist_length)(GSList *);
    gboolean (*str_equal)(const void *, const void *);
    guint (*str_hash)(const void *);
    GTypeClass *(*type_check_class_cast)(GTypeClass *, GType);
    GTypeInstance *(*type_check_instance_cast)(GTypeInstance *, GType);
    gboolean (*type_check_instance_is_a)(GTypeInstance *, GType);
    gboolean (*type_check_instance_is_fundamentally_a)(GTypeInstance *, GType);
    gboolean (*type_check_value)(const GValue *);
    gboolean (*type_check_value_holds)(const GValue *, GType);
    GType *(*type_children)(GType, guint *);
    void *(*type_class_ref)(GType);
    void (*type_class_unref)(void *);
    void *(*type_default_interface_ref)(GType);
    void (*type_default_interface_unref)(void *);
    guint (*type_depth)(GType);
    GType (*type_from_name)(const gchar *);
    GType (*type_fundamental)(GType);
    GType *(*type_interface_prerequisites)(GType, guint *);
    GType *(*type_interfaces)(GType, guint *);
    gboolean (*type_is_a)(GType, GType);
    const gchar *(*type_name)(GType);
    GType (*type_parent)(GType);
    void (*type_query)(GType, GTypeQuery *);
    GType (*type_register_static)(GType, const gchar *, const GTypeInfo *, GTypeFlags);
    gboolean (*type_test_flags)(GType, guint);
    void (*value_copy)(const GValue *, GValue *);
    gboolean (*value_get_boolean)(const GValue *);
    void *(*value_get_boxed)(const GValue *);
    gdouble (*value_get_double)(const GValue *);
    int (*value_get_enum)(const GValue *);
    guint (*value_get_flags)(const GValue *);
    gfloat (*value_get_float)(const GValue *);
    gint (*value_get_int)(const GValue *);
    gint64 (*value_get_int64)(const GValue *);
    glong (*value_get_long)(const GValue *);
    void *(*value_get_object)(const GValue *);
    GParamSpec *(*value_get_param)(const GValue *);
    void *(*value_get_pointer)(const GValue *);
    gint8 (*value_get_schar)(const GValue *);
    const gchar *(*value_get_string)(const GValue *);
    GType (*value_get_type)(void);
    guchar (*value_get_uchar)(const GValue *);
    guint (*value_get_uint)(const GValue *);
    guint64 (*value_get_uint64)(const GValue *);
    gulong (*value_get_ulong)(const GValue *);
    GVariant *(*value_get_variant)(const GValue *);
    GValue *(*value_init)(GValue *, GType);
    void (*value_set_boolean)(GValue *, gboolean);
    void (*value_set_boxed)(GValue *, const void *);
    void (*value_set_double)(GValue *, gdouble);
    void (*value_set_enum)(GValue *, gint);
    void (*value_set_flags)(GValue *, guint);
    void (*value_set_float)(GValue *, gfloat);
    void (*value_set_int)(GValue *, gint);
    void (*value_set_int64)(GValue *, gint64);
    void (*value_set_long)(GValue *, glong);
    void (*value_set_object)(GValue *, void *);
    void (*value_set_pointer)(GValue *, void *);
    void (*value_set_schar)(GValue *, gint8);
    void (*value_set_uchar)(GValue *, guint8);
    void (*value_set_uint)(GValue *, guint);
    void (*value_set_uint64)(GValue *, guint64);
    void (*value_set_ulong)(GValue *, gulong);
    void (*value_take_string)(GValue *, gchar *);
    gboolean (*value_transform)(const GValue *, GValue *);
    void (*value_unset)(GValue *);
    GVariant *(*variant_ref)(GVariant *);
    GVariant *(*variant_ref_sink)(GVariant *);
    void (*variant_unref)(GVariant *);
};

extern struct vtable G;

#endif
