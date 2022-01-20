#include "hooks.h"

static SCM before_c_callback_hook = SCM_BOOL_F;
static SCM before_callback_hook = SCM_BOOL_F;
static SCM before_function_hook = SCM_BOOL_F;

void run_before_callback_hook(SCM name, SCM func, SCM args)
{
    if (scm_is_true(before_callback_hook) && !scm_is_empty_hook(before_callback_hook))
        scm_c_run_hook(before_callback_hook,
                       scm_list_3(name, func, args));
}

void run_before_c_callback_hook(SCM name, SCM func, SCM args)
{
    if (scm_is_true(before_c_callback_hook) && !scm_is_empty_hook(before_c_callback_hook))
        scm_c_run_hook(before_c_callback_hook,
                       scm_list_3(name, func, args));
}

void run_before_function_hook(SCM name, SCM args)
{
    if (scm_is_true(before_function_hook) && !scm_is_empty_hook(before_function_hook))
        scm_c_run_hook(before_function_hook,
                       scm_list_2(name, args));
}

void
init_core_hooks(void)
{
    static int first = 1;
    if (first == 1) {
        first = 0;
        before_callback_hook = scm_permanent_object(scm_make_hook(scm_from_size_t(3)));
        before_c_callback_hook = scm_permanent_object(scm_make_hook(scm_from_size_t(3)));
        before_function_hook = scm_permanent_object(scm_make_hook(scm_from_size_t(2)));
        scm_c_define("%before-callback-hook", before_callback_hook);
        scm_c_define("%before-c-callback-hook", before_c_callback_hook);
        scm_c_define("%before-function-hook", before_function_hook);
        scm_c_export("%before-callback-hook",
                     "%before-c-callback-hook"
                     "%before-function-hook",
                     NULL);
    }
}
