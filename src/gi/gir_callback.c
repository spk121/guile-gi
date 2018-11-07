#include <libguile.h>
#include <ffi.h>
#include "gir_callback.h"
#include "gi_giargument.h"

SCM gir_callback_type;
SCM gir_callback_type_store;

GSList *callback_list = NULL;

static ffi_type *
type_info_to_ffi_type(GITypeInfo *type_info);

// This is the core of a dynamically generated callback funcion.
// It converts FFI arguments to SCM arguments, calls a SCM function
// and then returns the result.
void callback_binding(ffi_cif *cif, void *ret, void **ffi_args,
                      void *user_data)
{
    GirCallback *gcb = user_data;
    SCM s_args = SCM_EOL;
    SCM s_ret;

    g_debug("in callback C->SCM binding");
    unsigned int n_args = cif->nargs;

    for (unsigned int i = 0; i < n_args; i ++)
    {
        SCM s_entry = SCM_BOOL_F;
        GIArgument giarg;
        GIArgInfo *arg_info;

        // Did I need this block? Or can I just 
        // do giarg.v_pointer = ffi_args[i] for all cases?

        if (cif->arg_types[i] == &ffi_type_pointer)
            giarg.v_pointer = ffi_args[i];
        else if (cif->arg_types[i] == &ffi_type_void)
            giarg.v_pointer = ffi_args[i];
        else if (cif->arg_types[i] == &ffi_type_sint)
            giarg.v_int = (int) ffi_args[i];
        else if (cif->arg_types[i] == &ffi_type_sint8)
            giarg.v_int8 = (gint8) ffi_args[i];
        else if (cif->arg_types[i] == &ffi_type_uint8)
            giarg.v_uint8 = (guint8) ffi_args[i];
        else if (cif->arg_types[i] == &ffi_type_sint16)
            giarg.v_int16 = (gint16) ffi_args[i];
        else if (cif->arg_types[i] == &ffi_type_uint16)
            giarg.v_uint16 = (guint16) ffi_args[i];
        else if (cif->arg_types[i] == &ffi_type_sint32)
            giarg.v_int32 = (gint32) ffi_args[i];
        else if (cif->arg_types[i] == &ffi_type_uint32)
            giarg.v_uint32 = (guint32) ffi_args[i];
        else if (cif->arg_types[i] == &ffi_type_sint64)
            giarg.v_int64 = (gint64) ffi_args[i];
        else if (cif->arg_types[i] == &ffi_type_uint64)
            giarg.v_uint64 = (guint64) ffi_args[i];
        else if (cif->arg_types[i] == &ffi_type_float)
        {
            float val;
            val = *(float *)ffi_args[i];
            giarg.v_float = val;
        }
        else if (cif->arg_types[i] == &ffi_type_double)
        {
            float val;
            val = *(double *)ffi_args[i];
            giarg.v_double = val;
        }
        else
        {
            g_critical ("Unhandled FFI type in %s: %d", __FILE__, __LINE__);
            giarg.v_pointer = ffi_args[i];
        }

        arg_info = g_callable_info_get_arg(gcb->callback_info, i);
        gi_giargument_convert_arg_to_object(&giarg, arg_info, &s_entry);
        s_args = scm_append(scm_list_2(s_args, scm_list_1(s_entry)));
        g_base_info_unref (arg_info);
    }

    s_ret = scm_apply_0(gcb->s_func, s_args);

    GIArgument giarg;
    GITypeInfo *ret_type_info = g_callable_info_get_return_type (gcb->callback_info);
    GIArgumentStatus ret2;
    ret2 = gi_giargument_convert_return_type_object_to_arg(s_ret,
             ret_type_info,
             g_callable_info_get_caller_owns (gcb->callback_info),
             g_callable_info_may_return_null (gcb->callback_info),
             g_callable_info_skip_return(gcb->callback_info),
             &giarg);
    g_base_info_unref (ret_type_info);

    // I'm pretty sure I don't need a big type case/switch block here.
    // I'll try brutally coercing the data, and see what happens.
    *(ffi_arg *)ret = giarg.v_uint64;
}

// We use the CALLBACK_INFO to create a dynamic FFI closure
// to use as an entry point to the scheme procedure S_FUNC.
GirCallback *gir_callback_new(GICallbackInfo *callback_info, SCM s_func)
{
    GirCallback *gir_callback = g_new0(GirCallback, 1);
    ffi_type **ffi_args = NULL;
    ffi_type *ffi_ret_type;
    gint n_args = g_callable_info_get_n_args(callback_info);

    gir_callback->s_func = s_func;
    gir_callback->callback_info = callback_info;
    g_base_info_ref(callback_info);

    // STEP 1
    // Allocate the block of memory that FFI uses to hold a closure object,
    // and set a pointer to the corresponding executable address.
    gir_callback->closure = ffi_closure_alloc(sizeof(ffi_closure), &(gir_callback->callback_ptr));
    g_return_val_if_fail (gir_callback->closure != NULL, NULL);
    g_return_val_if_fail (gir_callback->callback_ptr != NULL, NULL);

    // STEP 2
    // Next, we begin to construct an FFI_CIF to describe the function call.

    // Initialize the argument info vectors */
    if (n_args > 0)
        ffi_args = g_new0(ffi_type *, n_args);
    for (int i = 0; i < n_args; i++)
    {
        GIArgInfo *callback_arg_info = g_callable_info_get_arg(callback_info, i);
        GITypeInfo *type_info = g_arg_info_get_type(callback_arg_info);
        ffi_args[i] = type_info_to_ffi_type(type_info);
        g_base_info_unref(callback_arg_info);
        g_base_info_unref(type_info);
    }
    GITypeInfo *ret_type_info = g_callable_info_get_return_type(callback_info);
    ffi_ret_type = type_info_to_ffi_type(ret_type_info);
    g_base_info_unref(ret_type_info);

    // Initialize the cif
    ffi_status prep_ok;
    prep_ok = ffi_prep_cif(&(gir_callback->cif), // pointer to the call interface struct
        FFI_DEFAULT_ABI,
        n_args,
        ffi_ret_type,
        ffi_args);

    if (prep_ok != FFI_OK)
        scm_misc_error ("gir-callback-new",
        "closure call interface preparation error #~A", scm_list_1(scm_from_int (prep_ok)));                                    
    //g_free(ffi_args);

    // STEP 3
    // Initialize the closure
    ffi_status closure_ok;
    closure_ok = ffi_prep_closure_loc(gir_callback->closure,    // Address of FFI closure object
        &(gir_callback->cif),                                   // the CIF describing the function params
        callback_binding,                                       // The function to be called when the closure is invoked
        gir_callback,                                           // The 'user-data' passed to the function
        gir_callback->callback_ptr);                            // The executable address returned by ffi_closure_alloc
    
    if (closure_ok != FFI_OK)
        scm_misc_error ("gir-callback-new",
        "closure location preparation error #~A", scm_list_1(scm_from_int (closure_ok)));

#ifdef DEBUG_CALLBACKS
    gir_callback->callback_info_ptr_as_uint = GPOINTER_TO_UINT(gir_callback->callback_ptr);
    gir_callback->closure_ptr_as_uint = GPOINTER_TO_UINT(gir_callback->closure);
    gir_callback->callback_ptr_as_uint = GPOINTER_TO_UINT (gir_callback->closure);
#endif  

    return gir_callback;
}

GirCallback *gir_callback_cache(GICallbackInfo *callback_info, SCM s_func)
{
    g_assert (callback_info != NULL);
    g_assert (scm_is_true (scm_procedure_p (s_func)));

    // Lookup s_func in the callback cache.
    GSList *x = callback_list;
    GirCallback *gcb;
    while (x != NULL)
    {
        gcb = x->data;
        if (scm_is_eq(gcb->s_func, s_func) && (g_base_info_get_type (callback_info) == g_base_info_get_type(gcb->callback_info)))
            return gcb;
        x = x->next;
    }

    // Create a new entry if necessary.
    gcb = gir_callback_new (callback_info, s_func);
    callback_list = g_slist_prepend(callback_list, gcb);
    return gcb;
}

SCM gir_callback_lookup_by_pointer(gpointer callback_ptr)
{
    g_assert (callback_ptr != NULL);

    GSList *x = callback_list;
    GirCallback *gcb;
    while (x != NULL)
    {
        gcb = x->data;
        if (gcb->callback_ptr == callback_ptr)
        {
            return scm_make_foreign_object_1 (gir_callback_type, gcb);
        }
        x = x->next;
    }
    return SCM_BOOL_F;
}

void *
gir_callback_get_func (SCM s_gcb)
{
    g_assert (SCM_IS_A_P (s_gcb, gir_callback_type));
    GirCallback *gcb = scm_foreign_object_ref(s_gcb, 0);
    return gcb->callback_ptr;
}

static ffi_type *
type_info_to_ffi_type(GITypeInfo *type_info)
{
    GITypeTag type_tag = g_type_info_get_tag(type_info);
    gboolean is_ptr = g_type_info_is_pointer(type_info);

    ffi_type *rettype = NULL;
    if (is_ptr)
        return &ffi_type_pointer;
    else
    {
        switch (type_tag)
        {
        case GI_TYPE_TAG_VOID:
            rettype = &ffi_type_void;
            break;
        case GI_TYPE_TAG_BOOLEAN:
            rettype = &ffi_type_sint;
            break;
        case GI_TYPE_TAG_INT8:
            rettype = &ffi_type_sint8;
            break;
        case GI_TYPE_TAG_UINT8:
            rettype = &ffi_type_uint8;
            break;
        case GI_TYPE_TAG_INT16:
            rettype = &ffi_type_sint16;
            break;
        case GI_TYPE_TAG_UINT16:
            rettype = &ffi_type_uint16;
            break;
        case GI_TYPE_TAG_INT32:
            rettype = &ffi_type_sint32;
            break;
        case GI_TYPE_TAG_UINT32:
            rettype = &ffi_type_uint32;
            break;
        case GI_TYPE_TAG_INT64:
            rettype = &ffi_type_sint64;
            break;
        case GI_TYPE_TAG_UINT64:
            rettype = &ffi_type_uint64;
            break;
        case GI_TYPE_TAG_FLOAT:
            rettype = &ffi_type_float;
            break;
        case GI_TYPE_TAG_DOUBLE:
            rettype = &ffi_type_double;
            break;
        case GI_TYPE_TAG_GTYPE:
            if (sizeof(GType) == sizeof(guint32))
                rettype = &ffi_type_sint32;
            else
                rettype = &ffi_type_sint64;
            break;
        case GI_TYPE_TAG_UTF8:
        case GI_TYPE_TAG_FILENAME:
        case GI_TYPE_TAG_ARRAY:
            g_critical("Unhandled FFI type in %s: %d", __FILE__, __LINE__);
            g_abort();
            break;
        case GI_TYPE_TAG_INTERFACE:
        {
            GIBaseInfo *base_info = g_type_info_get_interface(type_info);
            GIInfoType base_info_type = g_base_info_get_type(base_info);
            if (base_info_type == GI_INFO_TYPE_ENUM)
                rettype = &ffi_type_sint;
            else if (base_info_type == GI_INFO_TYPE_FLAGS)
                rettype = &ffi_type_uint;
            else
            {
                g_critical("Unhandled FFI type in %s: %d", __FILE__, __LINE__);
                g_abort();
            }
            g_base_info_unref(base_info);
            break;
        }
        case GI_TYPE_TAG_GLIST:
        case GI_TYPE_TAG_GSLIST:
        case GI_TYPE_TAG_GHASH:
        case GI_TYPE_TAG_ERROR:
            g_critical("Unhandled FFI type in %s: %d", __FILE__, __LINE__);
            g_abort();
            break;
        case GI_TYPE_TAG_UNICHAR:
            if (sizeof(gunichar) == sizeof(guint32))
                rettype = &ffi_type_uint32;
            else
            {
                g_critical("Unhandled FFI type in %s: %d", __FILE__, __LINE__);
                g_abort();
            }
            break;
        default:
            g_critical("Unhandled FFI type in %s: %d", __FILE__, __LINE__);
            g_abort();
        }
    }

    return rettype;
}

#if 0
static void
value_from_ffi_type(GValue *gvalue, gpointer *value)
{
    ffi_arg *int_val = (ffi_arg *)value;
    switch (g_type_fundamental(G_VALUE_TYPE(gvalue)))
    {
    case G_TYPE_INT:
        g_value_set_int(gvalue, (gint)*int_val);
        break;
    case G_TYPE_FLOAT:
        g_value_set_float(gvalue, *(gfloat *)value);
        break;
    case G_TYPE_DOUBLE:
        g_value_set_double(gvalue, *(gdouble *)value);
        break;
    case G_TYPE_BOOLEAN:
        g_value_set_boolean(gvalue, (gboolean)*int_val);
        break;
    case G_TYPE_STRING:
        g_value_take_string(gvalue, *(gchar **)value);
        break;
    case G_TYPE_CHAR:
        g_value_set_schar(gvalue, (gint8)*int_val);
        break;
    case G_TYPE_UCHAR:
        g_value_set_uchar(gvalue, (guchar)*int_val);
        break;
    case G_TYPE_UINT:
        g_value_set_uint(gvalue, (guint)*int_val);
        break;
    case G_TYPE_POINTER:
        g_value_set_pointer(gvalue, *(gpointer *)value);
        break;
    case G_TYPE_LONG:
        g_value_set_long(gvalue, (glong)*int_val);
        break;
    case G_TYPE_ULONG:
        g_value_set_ulong(gvalue, (gulong)*int_val);
        break;
    case G_TYPE_INT64:
        g_value_set_int64(gvalue, (gint64)*int_val);
        break;
    case G_TYPE_UINT64:
        g_value_set_uint64(gvalue, (guint64)*int_val);
        break;
    case G_TYPE_BOXED:
        g_value_take_boxed(gvalue, *(gpointer *)value);
        break;
    case G_TYPE_ENUM:
        g_value_set_enum(gvalue, (gint)*int_val);
        break;
    case G_TYPE_FLAGS:
        g_value_set_flags(gvalue, (guint)*int_val);
        break;
    case G_TYPE_PARAM:
        g_value_take_param(gvalue, *(gpointer *)value);
        break;
    case G_TYPE_OBJECT:
        g_value_take_object(gvalue, *(gpointer *)value);
        break;
    case G_TYPE_VARIANT:
        g_value_take_variant(gvalue, *(gpointer *)value);
        break;
    default:
        g_warning("value_from_ffi_type: Unsupported fundamental type: %s",
                  g_type_name(g_type_fundamental(G_VALUE_TYPE(gvalue))));
   }
}
#endif

static SCM
scm_gir_callback_new (SCM s_callback_info, SCM s_proc)
{
    GirCallback *gcb;
    
    gcb = gir_callback_cache(scm_to_pointer(s_callback_info), s_proc);
    return scm_make_foreign_object_1(gir_callback_type, gcb);
}

static void
gir_callback_finalizer (SCM callback)
{
    GirCallback *gcb = scm_foreign_object_ref(callback, 0);
    //if (gcb != NULL)
    //{
    //    if (gcb->closure != NULL)
    //        ffi_closure_free(gcb->closure);
    //   gcb->closure = NULL;
    //   gcb->s_func = SCM_BOOL_F;
    //}
}

static void gir_callback_hash_key_destroy (gpointer data)
{
    // Destroy the scm procedure.
}

void
gir_init_callback(void)
{
    SCM name, slots;
    name = scm_from_utf8_symbol("<GCallback>");
    slots = scm_list_n(
		       scm_from_utf8_symbol ("ptr"),
		       SCM_UNDEFINED);
    gir_callback_type = scm_make_foreign_object_type (name, slots, gir_callback_finalizer);
    gir_callback_type_store = scm_c_define ("<GCallback>", gir_callback_type);
    scm_c_define_gsubr("gir-callback-new", 2, 0, 0, scm_gir_callback_new);
}
