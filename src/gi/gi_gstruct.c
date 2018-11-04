/* -*- Mode: C; c-basic-offset: 4 -*- */
#include "gi_gstruct.h"
#include "gi_gtype.h"

SCM gir_gbox_type;
SCM gir_gbox_type_store;

static void gir_sptr_destroy (GirSmartPtr *sptr);

void gir_sptr_add_ref (GirSmartPtr *sptr)
{
    g_atomic_int_inc (&(sptr->count));
}

void gir_sptr_release (GirSmartPtr *sptr)
{
    g_assert (g_atomic_int_get (&(sptr->count)) >= 0);
    if (g_atomic_int_dec_and_test (&(sptr->count)))
    {
        gir_sptr_destroy (sptr);
    }
}

static void gir_sptr_destroy (GirSmartPtr *sptr)
{
    if (sptr->ptr)
    {
        if (sptr->dealloc == SPTR_DEFAULT_FREE_FUNC)
        {
	    if (sptr->holds == SPTR_HOLDS_GBOXED)
		g_boxed_free (sptr->type, sptr->ptr);
	    else
		g_free(sptr->ptr);
        }
        else if (sptr->dealloc == SPTR_C_FREE_FUNC)
        {
            sptr->c_free_func(sptr->ptr);
            sptr->c_free_func = NULL;
        }
        else if (sptr->dealloc == SPTR_SCM_FREE_FUNC)
        {
            SCM scm_ptr = scm_from_pointer(sptr->ptr, NULL);
            scm_call_1(sptr->scm_free_func, scm_ptr);
            sptr->scm_free_func = SCM_BOOL_F;
        }
        sptr->ptr = NULL;
    }
}

SCM
gir_new_gbox (GirPointerContents holds, GType gtype, gpointer ptr, gboolean use_default_free)
{
    GirSmartPtr *sptr = scm_gc_malloc(sizeof (GirSmartPtr), "box");
    memset (sptr, 0, sizeof(GirSmartPtr));
    sptr->holds = holds;
    sptr->ptr = ptr;
    sptr->type = gtype;
    g_atomic_int_set(&(sptr->count), 1);
    if (use_default_free)
        sptr->dealloc = SPTR_DEFAULT_FREE_FUNC;
    else
        sptr->dealloc = SPTR_NO_FREE_FUNC;

    SCM obj = scm_make_foreign_object_1(gir_gbox_type, sptr);
    return obj;
}

void
gir_gbox_connect_c_free_func (SCM box, void (*free_func)(gpointer ptr))
{
    g_assert (free_func != NULL);

    GirSmartPtr *sptr = scm_foreign_object_ref (box, 0);
    sptr->dealloc = SPTR_C_FREE_FUNC;
    sptr->c_free_func = free_func;
}

void
gir_gbox_connect_scm_free_func (SCM box, SCM free_func)
{
    g_assert (scm_is_true (scm_procedure_p (free_func)));

    GirSmartPtr *sptr = scm_foreign_object_ref (box, 0);
    sptr->dealloc = SPTR_SCM_FREE_FUNC;
    sptr->scm_free_func = free_func;
}

SCM
gir_new_struct_gbox (GType type, void *ptr, gboolean free_on_dealloc)
{
    if (!ptr)
        return SCM_BOOL_F;

    return gir_new_gbox (SPTR_HOLDS_STRUCT, type, ptr, free_on_dealloc);
}

SCM
gir_new_union_gbox (GType type, void *ptr, gboolean free_on_dealloc)
{
    if (!ptr)
        return SCM_BOOL_F;

    return gir_new_gbox (SPTR_HOLDS_UNION, type, ptr, free_on_dealloc);
}

static SCM
scm_make_gbox (GirPointerContents contains, SCM s_type, SCM s_pointer, SCM s_free_on_dealloc)
{
    GirPointerFreeAction free_type;

    if (SCM_UNBNDP(s_free_on_dealloc) || scm_is_false (s_free_on_dealloc))
        free_type = SPTR_NO_FREE_FUNC;
    else if (scm_is_eq (s_free_on_dealloc, SCM_BOOL_T))
        free_type = SPTR_DEFAULT_FREE_FUNC;
    else if (scm_is_true (scm_procedure_p (s_free_on_dealloc)))
        free_type = SPTR_SCM_FREE_FUNC;
    else
        g_assert_not_reached ();

    if (free_type == SPTR_NO_FREE_FUNC)
        return gir_new_gbox (contains, gi_gtype_get_type (s_type), scm_to_pointer (s_pointer), FALSE);
    else if (free_type == SPTR_DEFAULT_FREE_FUNC)
        return gir_new_gbox (contains, gi_gtype_get_type (s_type), scm_to_pointer (s_pointer), TRUE);
    else {
        SCM obj = gir_new_gbox (contains, gi_gtype_get_type (s_type), scm_to_pointer (s_pointer), FALSE);
        gir_gbox_connect_scm_free_func (obj, s_free_on_dealloc);
        return obj;
    }

    g_return_val_if_reached (SCM_BOOL_F);
}

#define SCM_MAKE_XXX_GBOX(SCM_FUNC_NAME, C_FUNC_NAME, CONTENTS)                                                                                                                           \
    static SCM                                                                                                                                                                            \
    C_FUNC_NAME(SCM s_type, SCM s_pointer, SCM s_free_on_dealloc)                                                                                                                         \
    {                                                                                                                                                                                     \
        GirPointerContents free_type;                                                                                                                                                     \
        if (!SCM_IS_A_P(s_type, gi_gtype_type))                                                                                                                                           \
            scm_wrong_type_arg_msg(SCM_FUNC_NAME, SCM_ARG1, s_type, "GType");                                                                                                             \
        if (!SCM_POINTER_P(s_pointer))                                                                                                                                                    \
            scm_wrong_type_arg_msg(SCM_FUNC_NAME, SCM_ARG2, s_pointer, "pointer");                                                                                                        \
        if (!SCM_UNBNDP(s_free_on_dealloc) && !(scm_is_eq(s_free_on_dealloc, SCM_BOOL_F) || scm_is_eq(s_free_on_dealloc, SCM_BOOL_T) || scm_is_true(scm_procedure_p(s_free_on_dealloc)))) \
            scm_wrong_type_arg_msg(SCM_FUNC_NAME, SCM_ARG3, s_free_on_dealloc, "boolean or procedure");                                                                               \
        return scm_make_gbox(CONTENTS, s_type, s_pointer, s_free_on_dealloc);                                                                                                              \
    }

SCM_MAKE_XXX_GBOX("make-struct-gbox", scm_make_struct_gbox, SPTR_HOLDS_STRUCT)
SCM_MAKE_XXX_GBOX("make-union-gbox", scm_make_union_gbox, SPTR_HOLDS_UNION)
SCM_MAKE_XXX_GBOX("make-pointer-gbox", scm_make_pointer_gbox, SPTR_HOLDS_POINTER)

void *
gi_gbox_peek_pointer (SCM self)
{
    if (!SCM_IS_A_P (self, gir_gbox_type))
        scm_wrong_type_arg_msg("gbox-peek-pointer", SCM_ARG1, self, "GBox");
    
    GirSmartPtr *sptr = scm_foreign_object_ref(self, 0);
    if (sptr)
        return sptr->ptr;
    else
        return NULL;
}

static SCM
scm_gbox_peek_pointer (SCM self)
{
    if (!SCM_IS_A_P (self, gir_gbox_type))
        scm_wrong_type_arg_msg("gbox-peek-pointer", SCM_ARG1, self, "GBox");
    
    GirSmartPtr *sptr = scm_foreign_object_ref(self, 0);
    if (sptr)
    {
        void *ptr = sptr->ptr;
        return scm_from_pointer (ptr, NULL);
    }
    return SCM_BOOL_F;
}

GType gi_gbox_get_type (SCM self)
{
    GirSmartPtr *sptr = scm_foreign_object_ref(self, 0);
    if (sptr)
        return sptr->type;
    return G_TYPE_NONE;    
}

static SCM
scm_gbox_get_gtype (SCM self)
{
    if (!SCM_IS_A_P(self, gir_gbox_type))
        scm_wrong_type_arg_msg ("gbox-get-gtype", SCM_ARG1, self, "GBox");
        
    GirSmartPtr *sptr = scm_foreign_object_ref(self, 0);
    if (sptr)
    {
        return gi_gtype_c2g (sptr->type);
    }
    return SCM_BOOL_F;
}

static void
gi_gbox_finalizer(SCM self)
{
    GirSmartPtr *sptr = scm_foreign_object_ref (self, 0);
    if (sptr)
        gir_sptr_release(sptr);
    scm_foreign_object_set_x (self, 0, NULL);
}

static SCM scm_gbox_get_refcount (SCM self)
{
    if (!SCM_IS_A_P(self, gir_gbox_type))
        scm_wrong_type_arg_msg ("gbox-get-refcount", SCM_ARG1, self, "GBox");
        
    GirSmartPtr *sptr = scm_foreign_object_ref(self, 0);
    if (sptr)
	return scm_from_int (g_atomic_int_get (&(sptr->count)));
    return scm_from_int (0);
}

static SCM scm_gbox_p (SCM self)
{
  return scm_from_bool (SCM_IS_A_P (self, gir_gbox_type));
}

/* A procedure suitable as a record-type printer. */
SCM
scm_gbox_printer (SCM self, SCM port)
{
    if (!SCM_IS_A_P(self, gir_gbox_type))
	scm_simple_format (port,
			   scm_from_utf8_string("NOT A GBOX TYPE"),
			   SCM_EOL);
    else
	{
	    GirSmartPtr *sptr = scm_foreign_object_ref(self, 0);
	    
	    scm_simple_format (port,
			       scm_from_utf8_string("~s [~s] <~s>"),
			       scm_list_3 (scm_from_utf8_string (g_type_name (sptr->type)),
					   scm_from_int (sptr->type),
					   scm_from_uintmax ((uintmax_t)sptr->ptr)));
	}
    return SCM_UNSPECIFIED;
}


void
gi_init_gbox (void)
{
    SCM name, slots;
    name = scm_from_utf8_symbol("<GBox>");
    slots = scm_list_n(
		       scm_from_utf8_symbol ("sptr"),
		       SCM_UNDEFINED);
    gir_gbox_type = scm_make_foreign_object_type (name, slots, gi_gbox_finalizer);
    gir_gbox_type_store = scm_c_define ("<GBox>", gir_gbox_type);
    scm_c_define_gsubr ("gbox?", 1, 0, 0, scm_gbox_p);
    scm_c_export ("<GBox>", "gbox?", NULL);

    scm_c_define_gsubr("make-struct-gbox", 2, 1, 0, scm_make_struct_gbox);
    scm_c_define_gsubr("make-union-gbox", 2, 1, 0, scm_make_union_gbox);
    scm_c_define_gsubr("make-pointer-gbox", 2, 1, 0, scm_make_pointer_gbox);
    scm_c_define_gsubr("gbox-peek-pointer", 1, 0, 0, scm_gbox_peek_pointer);
    scm_c_define_gsubr("gbox-get-gtype", 1, 0, 0, scm_gbox_get_gtype);
    scm_c_define_gsubr("%gbox-get-refcount", 1, 0, 0, scm_gbox_get_refcount);
    scm_c_define_gsubr("gbox-printer", 2, 0, 0, scm_gbox_printer);
}
