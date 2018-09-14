#pragma once
#include <libguile.h>
#include <glib.h>
#include <glib-object.h>
#include <girepository.h>

extern SCM GuGInterface_Type; // a Type
extern SCM GuGIBaseInfo_Type;
extern SCM GuType_Type; // a type
extern SCM GuGBoxed_Type;
extern SCM GuGPointer_Type;
extern SCM GuGParamSpec_Type;
extern SCM GuGEnum_Type;
extern SCM GuGFlags_Type;

typedef struct _PyGICallableCache PyGICallableCache;

SCM
gug_enum_add (SCM module,
	      const char * typename,
	      const char * strip_prefix,
	      GType        gtype);

SCM
gug_flags_add (SCM module,
	      const char * typename,
	      const char * strip_prefix,
	      GType        gtype);
SCM gug_value_as_scm(const GValue *value, gboolean copy_boxed);
SCM GuObject_CallFunction(SCM callable, const char *format, ...);
int       gug_value_from_scm(GValue *value, SCM obj);

SCM
gugi_call_do_get_property       (SCM instance,
                                 GParamSpec *pspec);

SCM gug_param_spec_new (GParamSpec *pspec);
const GInterfaceInfo *gug_lookup_interface_info(GType gtype);
const char *GUGLIB_GuUnicode_AsString(SCM val);
const char *gug_foreign_object_type_get_name (SCM x /* a type */);
GType gug_foreign_object_type_get_gtype (SCM x /* a type */);
SCM gug_foreign_object_type_get_dict(SCM x /* a type */);
SCM gug_foreign_object_type_get_bases(SCM class);
SCM gug_foreign_object_type_get_base(SCM class);
//SCM gug_type_wrapper_new (GType type);
typedef struct {
  //PyObject_HEAD
  int op_refcnt;
    GIBaseInfo *info;
    SCM inst_weakreflist;
    PyGICallableCache *cache;
} GuGIBaseInfo;
void gug_register_interface_info(GType gtype, const
				 GInterfaceInfo *info);
struct _GuGICallableCache
{
    const gchar *name;
    const gchar *container_name;
    const gchar *namespace;

  #if 0
    GuGICallingContext calling_context;

    GuGIArgCache *return_cache;
    GPtrArray *args_cache;
    GSList *to_gu_args;
    GSList *arg_name_list; /* for keyword arg matching */
    GHashTable *arg_name_hash;
    gboolean throws;

    /* Index of user_data arg passed to a callable. */
    gssize user_data_index;

    /* Index of user_data arg that can eat variable args passed to a callable. */
    gssize user_data_varargs_index;

    /* Number of args already added */
    gssize args_offset;

    /* Number of out args passed to g_function_info_invoke.
     * This is used for the length of GuGIInvokeState.out_values */
    gssize n_to_gu_args;

    /* If the callable return value gets used */
    gboolean has_return;

    /* The type used for returning multiple values or NULL */
    GuTypeObject* resulttuple_type;

    /* Number of out args for g_function_info_invoke that will be skipped
     * when marshaling to Guthon due to them being implicitly available
     * (list/array length).
     */
    gssize n_to_gu_child_args;

    /* Number of Guthon arguments expected for invoking the gi function. */
    gssize n_gu_args;

    /* Minimum number of args required to call the callable from Guthon.
     * This count does not include args with defaults. */
    gssize n_gu_required_args;

    void     (*deinit)              (GuGICallableCache *callable_cache);

    gboolean (*generate_args_cache) (GuGICallableCache *callable_cache,
                                     GICallableInfo *callable_info);
  #endif
};
typedef struct _GuGICallableCache GuGICallableCache;

typedef GuGICallableCache GuGIClosureCache;

#define    Gu_RETURN_NONE return SCM_UNSPECIFIED;
typedef struct _GuGICClosure
{
    GICallableInfo *info;
    SCM function;

  #if 0
    ffi_closure *closure;
    ffi_cif cif;
  #else
    void *closure;
    int cif;
  #endif

    GIScopeType scope;

    SCM user_data;
#if 0
    GuGIClosureCache *cache;
#else
  void *cache;
#endif
} GuGICClosure;

GuGIClosureCache *
gugi_closure_cache_new      (GICallableInfo *info)
;
GuGICClosure* _gugi_make_native_closure (GICallableInfo* info,
                                         GuGIClosureCache *cache,
                                         GIScopeType scope,
                                         SCM function,
                                         gpointer user_data);
SCM gugi_boxed_new (SCM type,
		    gpointer      boxed,
		    gboolean      free_on_dealloc,
		    gsize         allocated_slice);


SCM GUGLIB_GuBytes_FromString(const char *str);
SCM GUGLIB_GuBytes_FromStringAndSize(const char *str, ssize_t len);
ssize_t GUGLIB_GuBytes_Size(SCM x);
int GUGLIB_GuBytes_Resize(SCM bytes, ssize_t newsize);
char *GUGLIB_GuBytes_AsString(SCM x);
gboolean gugi_error_check (GError **error);
void gug_foreign_object_type_set_type(SCM type, SCM metatype);
SCM gugi_source_new (SCM self, SCM args);
SCM gugi_source_set_callback (SCM self, SCM args);
typedef SCM (*GuGIArgOverrideToGIArgumentFunc)   (SCM value,
                                                         GIInterfaceInfo *interface_info,
                                                         GITransfer       transfer,
                                                         GIArgument      *arg);
typedef SCM (*GuGIArgOverrideFromGIArgumentFunc) (GIInterfaceInfo *interface_info,
                                                         GITransfer       transfer,
                                                         gpointer         data);
typedef SCM (*GuGIArgOverrideReleaseFunc)        (GITypeInfo *type_info,
                                                         gpointer  struct_);


struct GuGI_API {
    void (*register_foreign_struct) (const char* namespace_,
                                     const char* name,
                                     GuGIArgOverrideToGIArgumentFunc to_func,
                                     GuGIArgOverrideFromGIArgumentFunc from_func,
                                     GuGIArgOverrideReleaseFunc release_func);
};


void gugi_register_foreign_struct (const char* namespace_,
                                   const char* name,
                                   GuGIArgOverrideToGIArgumentFunc to_func,
                                   GuGIArgOverrideFromGIArgumentFunc from_func,
                                   GuGIArgOverrideReleaseFunc release_func);

SCM gugi_require_foreign    (SCM self, SCM args, SCM kwargs);
SCM guglib_spawn_async    (SCM self, SCM args, SCM kwargs);
void gug_register_interface(SCM dict,
			    const gchar *class_name,
			    GType gtype,
			    SCM type);
void       gugi_register_gboxed (SCM dict, const gchar *class_name,
                                 GType boxed_type, SCM type);
SCM gugi_gboxed_new      (GType boxed_type, gpointer boxed,
                                 gboolean copy_boxed, gboolean own_ref);
void       gug_register_pointer (SCM dict, const gchar *class_name,
                                 GType pointer_type, SCM type);

SCM gug_pointer_new      (GType pointer_type, gpointer pointer);
SCM gug_type_wrapper_new (GType type);

#define                            GUGOBJECT_MAJOR_VERSION 0
#define                            GUGOBJECT_MINOR_VERSION 0
#define                            GUGOBJECT_MICRO_VERSION 0
#define GUGLIB_MODULE_START(a,b) SCM func(void); SCM func(void) { SCM module = SCM_EOL;
#define GUGLIB_MODULE_END return module; }
#define GUGLIB_MODULE_ERROR_RETURN -1
