#include <stdarg.h>
#include <stdio.h>
#include "guile.h"

SCM sym_self;

int
scm_is_boolean(SCM x)
{
    return scm_is_eq(x, SCM_BOOL_T) || scm_is_eq(x, SCM_BOOL_F);
}

int
scm_is_bytevector(SCM x)
{
    return scm_is_true(scm_bytevector_p(x));
}

int
scm_is_char(SCM x)
{
    return SCM_CHARP(x);
}

int
scm_is_class(SCM x)
{
    return SCM_CLASSP(x);
}

int
scm_is_empty_hook(SCM x)
{
    return scm_is_true(scm_hook_empty_p(x));
}

int
scm_is_keyword(SCM x)
{
    return scm_is_true(scm_keyword_p(x));
}

int
scm_is_list(SCM x)
{
    return scm_is_true(scm_list_p(x));
}

int
scm_is_pointer(SCM x)
{
    return SCM_POINTER_P(x);
}

int
scm_is_procedure(SCM x)
{
    return scm_is_true(scm_procedure_p(x));
}

int
scm_is_real(SCM x)
{
    return scm_is_true(scm_real_p(x));
}

size_t
scm_c_length(SCM x)
{
    return scm_to_size_t(scm_length(x));
}

SCM
scm_c_list_ref(SCM lst, size_t i)
{
    return scm_list_ref(lst, scm_from_size_t(i));
}

SCM
scm_current_module_definition(SCM name)
{
    SCM variable;
    variable = scm_module_variable(scm_current_module(), name);
    if (scm_is_true(variable))
        return scm_variable_ref(variable);
    return SCM_BOOL_F;
}

void *
scm_dynfree(void *x)
{
    scm_dynwind_free(x);
    return x;
}

int
scm_is_equal(SCM a, SCM b)
{
    return scm_is_true(scm_equal_p(a, b));
}

void
scm_printf(SCM port, const char *fmt, ...)
{
#define SCM_PRINTF_MAX_LINE_LEN (1024)
    char _message[SCM_PRINTF_MAX_LINE_LEN];
    va_list args;
    va_start(args, fmt);
    vsnprintf(_message, SCM_PRINTF_MAX_LINE_LEN, fmt, args);
    va_end(args);
    SCM message = scm_from_utf8_string(_message);
    scm_display(message, port);
#undef SCM_PRINTF_MAX_LINE_LEN
}

SCM
scm_sad_quit(void)
{
    scm_c_eval_string("(quit EXIT_FAILURE)");
}

char *
scm_write_to_utf8_stringn(SCM x, size_t max_len)
{
    static int first = 1;
    static SCM format;
    static SCM ellipses;
    if (first) {
        format = scm_from_utf8_string("~S");
        ellipses = scm_from_utf8_string("...");
        first = 0;
    }

    SCM args_str = scm_simple_format(SCM_BOOL_F, format, scm_list_1(x));
    char *cstr;
    if (scm_c_string_length(args_str) > max_len) {
        SCM truncated_args_str =
            scm_string_append(scm_list_2(scm_c_substring(args_str, 0, max_len), ellipses));
        cstr = scm_to_utf8_string(truncated_args_str);
    }
    else
        cstr = scm_to_utf8_string(args_str);

    return cstr;
}

SCM
scm_keyword_to_string(SCM x)
{
    return scm_symbol_to_string(scm_keyword_to_symbol(x));
}

char *
scm_to_utf8_symbol(SCM sym)
{
    return scm_to_utf8_string(scm_symbol_to_string(sym));
}

SCM
guile_get_default_definition(SCM name)
{
    SCM variable;
    variable = scm_module_variable(scm_current_module(), name);
    if (scm_is_true(variable))
        return scm_variable_ref(variable);
    variable = scm_module_variable(scm_c_resolve_module("gi"), name);
    if (scm_is_true(variable))
        return scm_variable_ref(variable);
    variable = scm_module_variable(scm_c_resolve_module("guile"), name);
    if (scm_is_true(variable))
        return scm_variable_ref(variable);
    return SCM_BOOL_F;
}

void
init_core_guile(void)
{
    static int first = 1;
    if (first) {
        first = 0;
        sym_self = scm_from_utf8_symbol("self");
    }
}
