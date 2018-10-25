#include "gi_gstruct.h"
#include "gi_gtype.h"

SCM
gi_gstruct_new_from_gtype (GType type, void *ptr, gboolean free_on_dealloc)
{
    SCM stype;
    SCM obj;

    if (!ptr)
        return SCM_BOOL_F;

    stype = gi_gtype_c2g(type);

    obj = scm_make_foreign_object_0(gi_gstruct_type);
    gi_gstruct_set_type (obj, type);
    gi_gstruct_set_ptr (obj, ptr);
    gi_gstruct_set_free_on_dealloc (obj, free_on_dealloc);
    return obj;
}

static SCM
scm_make_gstruct (SCM s_type, SCM s_pointer, SCM s_free_on_dealloc)
{
    if (!SCM_IS_A_P (s_type, gi_gtype_type))
        scm_wrong_type_arg_msg("make-gstruct", SCM_ARG1, s_type, "GType");
    if (!SCM_POINTER_P (s_pointer))
        scm_wrong_type_arg_msg("make-gstruct", SCM_ARG2, s_pointer, "pointer");
    if (SCM_UNBNDP(s_free_on_dealloc))
        s_free_on_dealloc = SCM_BOOL_F;
    if (!scm_is_bool(s_free_on_dealloc))
        scm_wrong_type_arg_msg("make-gstruct", SCM_ARG3, s_free_on_dealloc, "boolean");

    return gi_gstruct_new_from_gtype (gi_gtype_get_type(s_type), scm_to_pointer(s_pointer), scm_is_true(s_free_on_dealloc));
}

static SCM
scm_gstruct_to_pointer (SCM self)
{
    if (!SCM_IS_A_P (self, gi_gstruct_type))
        scm_wrong_type_arg_msg("gstruct->pointer", SCM_ARG1, self, "GStruct");
    
    void *ptr = gi_gstruct_get_ptr (self);
    if (gi_gstruct_get_free_on_dealloc (self))
        return scm_from_pointer (ptr, gi_gstruct_pointer_finalizer);
    return scm_from_pointer (ptr, NULL);
}

static SCM
scm_gstruct_to_gtype (SCM self)
{
    if (!SCM_IS_A_P(self, gi_gstruct_type))
        scm_wrong_type_arg_msg ("gstruct->gtype", SCM_ARG1, self, "GStruct");
    return gi_gtype_c2g (gi_gstruct_get_type(self));
}

void
gi_gstruct_finalizer(SCM self)
{
    if (gi_gstruct_get_free_on_dealloc(self))
        gi_gstruct_pointer_finalizer (gi_gstruct_get_ptr (self));
    gi_gstruct_set_ptr(self, NULL);
    gi_gstruct_set_type(self, G_TYPE_NONE);
}

void
gi_gstruct_pointer_finalizer(void *ptr)
{
    g_free (ptr);
}

void
gi_init_gstruct (void)
{
    scm_c_define_gsubr("make-gstruct", 2, 1, 0, scm_make_gstruct);
    scm_c_define_gsubr("gstruct->pointer", 1, 0, 0, scm_gstruct_to_pointer);
    scm_c_define_gsubr("gstruct->gtype", 1, 0, 0, scm_gstruct_to_gtype);
}