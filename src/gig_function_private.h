#ifndef GIG_FUNCTION_PRIVATE_H
#define GIG_FUNCTION_PRIVATE_H

SCM ensure_generic_proc;
SCM make_proc;
SCM add_method_proc;

SCM top_type;
SCM method_type;
SCM char_type;
SCM list_type;

SCM kwd_specializers;
SCM kwd_formals;
SCM kwd_procedure;

SCM sym_self;

SCM default_definition(SCM name);

#endif
