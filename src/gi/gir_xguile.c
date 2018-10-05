/* -*- Mode: C; c-basic-offset: 4 -*- */
#ifndef XGUILE_H
#define XGUILE_H

#include <libguile.h>
#include <stdarg.h>
#include <stdbool.h>
#include <ctype.h>
#include <glib.h>
#include "gir_xguile.h"

SCM GuNone_Type = SCM_BOOL_F;
SCM SCM_NONE = SCM_BOOL_F;
SCM SCM_NONE_Store = SCM_BOOL_F;
static SCM var_make_standard_class = SCM_BOOL_F;
static SCM class_class;

void
scm_add_int_constant (const char *name, long val)
{
    SCM var = scm_c_define (name, scm_from_int (val));
    scm_permanent_object (var);
    scm_c_export (name, NULL);
}

void
scm_add_string_constant (const char *name, const char *val)
{
    SCM var = scm_c_define (name, scm_from_utf8_string (val));
    scm_permanent_object (var);
    scm_c_export (name, NULL);
}

SCM scm_c_list_ref (SCM list, size_t k)
{
    return scm_list_ref (list, scm_from_size_t (k));
}

/* I think this test is "necessary but not complete". */
int
scm_is_foreign_object_type (SCM type)
{
    SCM layout;
    size_t i;
    size_t n;
    char *layout_chars;

    if (scm_is_false (scm_struct_vtable_p (type)))
	return 0;

    layout = scm_struct_ref (type, scm_from_int (0));

    if (!scm_is_symbol (layout))
	return 0;

    layout_chars = scm_to_utf8_string (layout);
    n = strlen(layout_chars) / 2;
    for (i = 0; i < n; i ++)
	if (layout_chars[i * 2] != 'u') {
	    free (layout_chars);
	    return 0;
	}

    free (layout_chars);
    return 1;
}

int
scm_is_gobject (SCM x)
{
    SCM ref = scm_slot_ref (x, scm_from_latin1_symbol("ob_type"));
    return scm_is_true (ref);
}

int
scm_is_hash_table (SCM x)
{
    return scm_is_true (scm_hash_table_p (x));
}

int
scm_is_list (SCM x)
{
    return scm_is_true (scm_list_p (x));
}

int
scm_is_none (SCM x)
{
    return x == SCM_NONE;
}

int
scm_is_procedure (SCM x)
{
    return scm_is_true (scm_procedure_p (x));
}

int
scm_is_struct (SCM x)
{
    return scm_is_true (scm_struct_p (x));
}

SCM
scm_list_of_utf8_symbols(const char *sym, ...)
{
    SCM list, entry;
    va_list args;
    const char *str;
    list = SCM_EOL;

    if (!sym)
	return SCM_EOL;
    list = scm_list_1 (scm_from_utf8_symbol (sym));
    va_start(args, sym);
    while ((str = va_arg(args, const char *)) != NULL) {
	entry = scm_from_utf8_symbol (str);
	list = scm_append (scm_list_2 (list, scm_list_1 (entry)));
    }
    va_end(args);
    return list;
}

SCM scm_make_class (SCM name, SCM dsupers, SCM dslots)
{
  return scm_call_4 (scm_variable_ref (var_make_standard_class),
                     class_class, name, dsupers, dslots);
}

void
scm_module_add_int_constant (SCM module, const char *name, long val)
{
    SCM var = scm_c_module_define (module, name, scm_from_int (val));
    scm_permanent_object (var);
    scm_c_export (name, NULL);
}

void
scm_module_add_string_constant (SCM module, const char *name, const char *val)
{
    SCM var = scm_c_module_define (module, name, scm_from_utf8_string (val));
    scm_permanent_object (var);
    scm_c_export (name, NULL);
}

static char parse_error_str[256];

const char *scm_get_parse_error (void)
{
    return parse_error_str;
}

/* This is a serious simplification to the Python function.  It
   only works for format strings that are simple types, and most
   special parsing is ignored.  */
/* re PyArg_ParseTuple */
int
scm_parse_list (SCM args, const char *format, ...)
{
    va_list va;
    size_t n_args_provided, n_args_required;
    size_t i_arg, i_fmt;
    size_t len;
    const char *bar, *colon, *name;
    int opt;
    static const char *arg = "arguments";
    SCM prev = SCM_BOOL_F;

    memset (parse_error_str, 0, 256);
    n_args_provided = scm_to_size_t (scm_length (args));
    len = strlen (format);
    colon = strchr(format, ':');
    if (colon)
	len = colon - format;

    bar = strchr(format, '|');
    if (bar)
	n_args_required = bar - format;
    else if (colon)
	n_args_required = colon - format;
    else
	n_args_required = strlen(format);

    if (!colon)
	name = arg;
    else
	name = colon + 1;

    if (n_args_provided < n_args_required) {
	sprintf(parse_error_str, "%s: expected %zu arguments, but, received %zu",
		name, n_args_required, n_args_provided);
	return false;
    }

    va_start(va, format);

    /* Typechecking */
    i_arg = 0;
    i_fmt = 0;
    opt = false;
    for (i_fmt = 0; i_fmt < len; i_fmt ++) {
	if (i_arg >= n_args_provided)
	    break;
	if (format[i_fmt] == '|')
	    opt = true;
	if (isalpha (format[i_fmt]) || format[i_fmt] == '!') {
	    void *ptr = va_arg(va, void *);
	    SCM entry = scm_list_ref (args, scm_from_int (i_arg));
	    if (scm_is_none (entry) && opt)
		continue;
	    switch (format[i_fmt]) {
	    case 'c':
		if (!scm_is_true (scm_char_p (entry)) && !scm_is_exact_integer (entry)) {
		    sprintf(parse_error_str, "%s: arg %zu, expected a char", name, i_fmt);
		    return false;
		}
		break;
	    case 's':
		if (!scm_is_string (entry)) {
		    sprintf(parse_error_str, "%s: arg %zu, expected a string", name, i_fmt);
		    return false;
		}
		break;
	    case 'z':
		if (!scm_is_string (entry) && !scm_is_none (entry)) {
		    sprintf(parse_error_str, "%s: arg %zu, expected a string or NONE", name, i_fmt);
		    return false;
		}
		break;
	    case 'i':
	    case 'I':
	    case 'k':
	    case 'K':
	    case 'l':
	    case 'L':
		if (!scm_is_exact_integer (entry)) {
		    sprintf(parse_error_str, "%s: arg %zu, expected an exact integer", name, i_fmt);
		    return false;
		}
		break;
	    case 'f':
	    case 'd':
		if (!scm_is_real (entry)) {
		    sprintf(parse_error_str, "%s: arg %zu, expected a real number", name, i_fmt);
		    return false;
		}
		break;
	    case 'O':
		if (format[i_fmt + 1] == '!') {
		    if (!SCM_IS_A_P(entry, (SCM) ptr)) {
			sprintf(parse_error_str, "%s: arg %zu, expected a matching type", name, i_fmt);
			return false;
		    }
		}
		break;
	    case '!':
		/* We don't consume an argument here. */
		continue;

	    default:
		g_return_val_if_reached (false);
		break;
	    }

	    i_arg ++;
	}
    }

    /* Marshaling */
    i_arg = 0;
    i_fmt = 0;
    for (i_fmt = 0; i_fmt < len; i_fmt ++) {
	if (isalpha (format[i_fmt]) || format[i_fmt] == '!') {
	    void *ptr = va_arg(va, void *);
	    SCM entry;
	    if (i_arg < n_args_provided)
		entry = scm_list_ref (args, scm_from_size_t (i_arg));
	    else
		entry = SCM_NONE;
	    switch (format[i_fmt]) {
	    case 'c':
		if (scm_is_none (entry))
		    *(char *)ptr = '\0';
		else if (scm_is_true (scm_char_p (entry)))
		    *(char *)ptr = SCM_CHAR (entry);
		else if (scm_is_exact_integer (entry))
		    *(char *)ptr = scm_to_char (entry);
		break;
	    case 's':
	    case 'z':
		if (scm_is_none (entry))
		    ptr = NULL;
		else
		    ptr = scm_to_utf8_string (entry);
		break;
	    case 'i':
		if (scm_is_none (entry))
		    *(int *) ptr = 0;
		else
		    *(int *) ptr = scm_to_int (entry);
		break;
	    case 'I':
		if (scm_is_none (entry))
		    *(unsigned int *)ptr = 0;
		else
		    *(unsigned int *)ptr = scm_to_uint (entry);
		break;
	    case 'k':
		if (scm_is_none (entry))
		    *(unsigned long *)ptr = 0;
		else
		    *(unsigned long *)ptr = scm_to_ulong (entry);
		break;
	    case 'K':
		if (scm_is_none (entry))
		    *(unsigned long long *)ptr = 0;
		else
		    *(unsigned long long *)ptr = scm_to_ulong_long (entry);
		break;
	    case 'l':
		if (scm_is_none (entry))
		    *(long *)ptr = 0;
		else
		    *(long *)ptr = scm_to_long (entry);
		break;
	    case 'L':
		if (scm_is_none (entry))
		    *(long long *)ptr = 0;
		else
		    *(long long *)ptr = scm_to_long_long (entry);
		break;
	    case 'f':
		if (scm_is_none (entry))
		    *(float *)ptr = 0.0f;
		else
		    *(float *)ptr = scm_to_double (entry);
		break;
	    case 'd':
		if (scm_is_none (entry))
		    *(double *)ptr = 0.0;
		else
		    *(double *)ptr = scm_to_double (entry);
		break;
	    case 'O':
		if (format[i_fmt + 1] != '!')
		    *(scm_t_bits *)ptr = SCM_UNPACK (entry);
		break;
	    case '!':
		*(scm_t_bits *)ptr = SCM_UNPACK (prev);
		/* We don't consume an argument here. */
		continue;

	    default:
		g_return_val_if_reached (false);
		break;
	    }
	    prev = entry;
	    i_arg ++;
	}
    }
    va_end(va);
    return true;
}

SCM
scm_build_list (const char *format, ...)
{
    // s = utf8 string
    // l = long
    // N = object (but do no increase reference count)
    // i = integer
    // c = 8-bit char
    // parens and brackets = make a list
    // space and comma are ignored
    g_return_val_if_fail (format != NULL && strlen(format) > 0, SCM_BOOL_F);
    g_return_val_if_reached (SCM_BOOL_F);
}

SCM
scm_sublist (SCM list, size_t start, size_t end)
{
    SCM output, entry;
    output = SCM_EOL;
    for (size_t i = start; i < end; i ++) {
	entry = scm_c_list_ref (list, i);
	list = scm_append(scm_list_2(output, scm_list_1(entry)));
    }
    return list;
}

void
gir_init_xguile (void)
{
    GuNone_Type = scm_make_foreign_object_type (scm_from_utf8_symbol ("$NONE"),
						SCM_EOL, NULL);
    SCM_NONE = scm_permanent_object (scm_make_foreign_object_0 (GuNone_Type));
    SCM_NONE_Store = scm_c_define("$NONE", SCM_NONE);
    // var_make_standard_class = scm_c_lookup ("make-standard-class");
    // class_class = scm_variable_ref (scm_c_lookup ("<class>"));

    scm_c_export ("$NONE",
		  NULL);
}




#endif
