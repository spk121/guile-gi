

SCM
gi_struct_new (SCM foreign_object_type,
	       gpointer pointer,
	       gboolean free_on_dealloc)
{
  SCM obj = scm_make_foreign_object (foreign_object_type);
  

void
gi_init_struct (void)
{
  /* register GI struct foreign object type */
}
