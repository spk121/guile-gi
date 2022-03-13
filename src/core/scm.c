// Copyright (C) 2022 Michael L. Gran

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <stdarg.h>
#include <stdio.h>
#include "scm.h"

static SCM srfi1_drop_right_proc = SCM_UNDEFINED;

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
scm_is_defined(SCM x)
{
    return scm_is_true(scm_defined_p(x, scm_current_module()));
}

#if HAVE_SCM_HOOKS
int
scm_is_empty_hook(SCM x)
{
    return scm_is_true(scm_hook_empty_p(x));
}
#endif

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

SCM
scm_append2(SCM a, SCM b)
{
    if (scm_is_null(a))
        return b;
    if (scm_is_null(b))
        return a;
    return scm_append(scm_list_2(a, b));
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

#if HAVE_SCM_HOOKS
void
scm_c_activate_hook_2(SCM hook, SCM a, SCM b)
{
    if (scm_is_true(hook) && !scm_is_empty_hook(hook))
        scm_c_run_hook(hook, scm_list_2(a, b));
}

void
scm_c_activate_hook_3(SCM hook, SCM a, SCM b, SCM c)
{
    if (scm_is_true(hook) && !scm_is_empty_hook(hook))
        scm_c_run_hook(hook, scm_list_3(a, b, c));
}
#endif

SCM
scm_current_module_definition(SCM name)
{
    SCM variable;
    variable = scm_module_variable(scm_current_module(), name);
    if (scm_is_true(variable))
        return scm_variable_ref(variable);
    return SCM_BOOL_F;
}

SCM
scm_drop_right_1(SCM lst)
{
    if (SCM_UNBNDP(srfi1_drop_right_proc))
        srfi1_drop_right_proc = scm_c_public_ref("srfi srfi-1", "drop-right");
    return scm_call_2(srfi1_drop_right_proc, lst, scm_from_int(1));
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
    return scm_c_eval_string("(quit EXIT_FAILURE)");
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
scm_default_definition(SCM name)
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
