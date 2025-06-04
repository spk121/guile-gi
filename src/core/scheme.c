// Copyright (C) 2022 Michael L. Gran
// Modernized 2025 - Enhanced GNU Guile integration with safety, rich API, and error handling

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
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <errno.h>
#include <time.h>

#include "scheme.h"
#include "mem.h"
#include "logging.h"
#include "str.h"

// ============================================================================
// Global State and Caching
// ============================================================================

// Cached procedure references for performance
static struct {
    SCM srfi1_drop_right_proc;
    SCM srfi1_filter_proc;
    SCM srfi1_remove_proc;
    SCM srfi1_find_proc;
    bool procedures_loaded;
} g_scm_cache = {
    .srfi1_drop_right_proc = SCM_UNDEFINED,
    .srfi1_filter_proc = SCM_UNDEFINED,
    .srfi1_remove_proc = SCM_UNDEFINED,
    .srfi1_find_proc = SCM_UNDEFINED,
    .procedures_loaded = false
};

// Error state
static ScmErrorType g_last_error = SCM_ERROR_NONE;

// Timing for performance monitoring
static struct timespec g_start_time;
static bool g_timing_active = false;

// ============================================================================
// Helper Functions
// ============================================================================

static void set_error(ScmErrorType error) {
    g_last_error = error;
}

static void load_cached_procedures(void) {
    if (g_scm_cache.procedures_loaded) return;
    
    // Load SRFI-1 procedures with error handling
    SCM srfi1_module = scm_c_resolve_module("srfi srfi-1");
    if (scm_is_true(srfi1_module)) {
        g_scm_cache.srfi1_drop_right_proc = scm_c_module_lookup(srfi1_module, "drop-right");
        g_scm_cache.srfi1_filter_proc = scm_c_module_lookup(srfi1_module, "filter");
        g_scm_cache.srfi1_remove_proc = scm_c_module_lookup(srfi1_module, "remove");
        g_scm_cache.srfi1_find_proc = scm_c_module_lookup(srfi1_module, "find");
    }
    
    g_scm_cache.procedures_loaded = true;
}

static SCM safe_call_with_error_handling(SCM proc, SCM args) {
    if (!scm_is_procedure(proc)) {
        set_error(SCM_ERROR_WRONG_TYPE);
        return SCM_BOOL_F;
    }
    
    SCM result = SCM_BOOL_F;
    
    // Use Guile's exception handling
    SCM catch_key = scm_from_utf8_symbol("system-error");
    
    // TODO: Implement proper exception handling here
    // For now, use direct call with basic error checking
    result = scm_apply_0(proc, args);
    
    set_error(SCM_ERROR_NONE);
    return result;
}

// ============================================================================
// Enhanced Type Predicates (original functions modernized)
// ============================================================================

bool scm_is_boolean(SCM x) {
    return scm_is_eq(x, SCM_BOOL_T) || scm_is_eq(x, SCM_BOOL_F);
}

#if 0
bool scm_is_bytevector(SCM x) {
#if HAVE_SCM_BYTEVECTORS
    return scm_is_true(scm_bytevector_p(x));
#else
    return false;
#endif
}
#endif

// Additional type predicates
bool scm_is_port(SCM x) {
    return scm_is_true(scm_port_p(x));
}

bool scm_is_input_port(SCM x) {
    return scm_is_true(scm_input_port_p(x));
}

bool scm_is_output_port(SCM x) {
    return scm_is_true(scm_output_port_p(x));
}

bool scm_is_file_port(SCM x) {
    return scm_is_true(scm_file_port_p(x));
}

bool scm_is_hash_table(SCM x) {
    return scm_is_true(scm_hash_table_p(x));
}


bool scm_is_module(SCM x) {
    return SCM_MODULEP(x);
}

#if 0
bool scm_is_exact_integer(SCM x) {
    return scm_is_true(scm_exact_integer_p(x));
}
#endif

bool scm_is_finite(SCM x) {
    return scm_is_true(scm_finite_p(x));
}

bool scm_is_infinite(SCM x) {
    return scm_is_true(scm_inf_p(x));
}

bool scm_is_nan(SCM x) {
    return scm_is_true(scm_nan_p(x));
}

// ============================================================================
// Enhanced List Operations (original functions modernized)
// ============================================================================

SCM scm_append2(SCM a, SCM b) {
    if (scm_is_null(a)) return b;
    if (scm_is_null(b)) return a;
    return scm_append(scm_list_2(a, b));
}

size_t scm_c_length(SCM x) {
    if (!scm_is_list(x)) {
        set_error(SCM_ERROR_WRONG_TYPE);
        return 0;
    }
    return scm_to_size_t(scm_length(x));
}

SCM scm_c_list_ref(SCM lst, size_t i) {
    if (!scm_is_list(lst)) {
        set_error(SCM_ERROR_WRONG_TYPE);
        return SCM_BOOL_F;
    }
    
    size_t length = scm_c_length(lst);
    if (i >= length) {
        set_error(SCM_ERROR_OUT_OF_RANGE);
        return SCM_BOOL_F;
    }
    
    return scm_list_ref(lst, scm_from_size_t(i));
}

SCM scm_drop_right_1(SCM lst) {
    if (!scm_is_list(lst)) {
        set_error(SCM_ERROR_WRONG_TYPE);
        return SCM_BOOL_F;
    }
    
    load_cached_procedures();
    
    if (SCM_UNBNDP(g_scm_cache.srfi1_drop_right_proc)) {
        // Fallback implementation
        if (scm_is_null(lst) || scm_is_null(scm_cdr(lst))) {
            return SCM_EOL;
        }
        
        SCM result = SCM_EOL;
        SCM current = lst;
        
        while (!scm_is_null(scm_cdr(current))) {
            result = scm_cons(scm_car(current), result);
            current = scm_cdr(current);
        }
        
        return scm_reverse(result);
    }
    
    return scm_call_2(g_scm_cache.srfi1_drop_right_proc, lst, scm_from_int(1));
}

// Enhanced list operations
SCM scm_c_make_list(size_t length, SCM fill) {
    return scm_make_list(scm_from_size_t(length), fill);
}

SCM scm_c_list_set_x(SCM lst, size_t i, SCM value) {
    if (!scm_is_list(lst)) {
        set_error(SCM_ERROR_WRONG_TYPE);
        return SCM_BOOL_F;
    }
    
    size_t length = scm_c_length(lst);
    if (i >= length) {
        set_error(SCM_ERROR_OUT_OF_RANGE);
        return SCM_BOOL_F;
    }
    
    return scm_list_set_x(lst, scm_from_size_t(i), value);
}

SCM scm_c_list_slice(SCM lst, size_t start, size_t end) {
    if (!scm_is_list(lst)) {
        set_error(SCM_ERROR_WRONG_TYPE);
        return SCM_BOOL_F;
    }
    
    size_t length = scm_c_length(lst);
    if (start > end || end > length) {
        set_error(SCM_ERROR_OUT_OF_RANGE);
        return SCM_BOOL_F;
    }
    
    // Drop 'start' elements from the beginning
    SCM current = lst;
    for (size_t i = 0; i < start && !scm_is_null(current); i++) {
        current = scm_cdr(current);
    }
    
    // Take 'end - start' elements
    SCM result = SCM_EOL;
    for (size_t i = start; i < end && !scm_is_null(current); i++) {
        result = scm_cons(scm_car(current), result);
        current = scm_cdr(current);
    }
    
    return scm_reverse(result);
}

ssize_t scm_c_list_index(SCM lst, SCM item) {
    if (!scm_is_list(lst)) {
        set_error(SCM_ERROR_WRONG_TYPE);
        return -1;
    }
    
    SCM current = lst;
    size_t index = 0;
    
    while (!scm_is_null(current)) {
        if (scm_is_equal(scm_car(current), item)) {
            return (ssize_t)index;
        }
        current = scm_cdr(current);
        index++;
    }
    
    return -1;
}

size_t scm_c_list_count(SCM lst, SCM item) {
    if (!scm_is_list(lst)) {
        set_error(SCM_ERROR_WRONG_TYPE);
        return 0;
    }
    
    SCM current = lst;
    size_t count = 0;
    
    while (!scm_is_null(current)) {
        if (scm_is_equal(scm_car(current), item)) {
            count++;
        }
        current = scm_cdr(current);
    }
    
    return count;
}

// ============================================================================
// Enhanced Hook Operations
// ============================================================================

#if HAVE_SCM_HOOKS
void scm_c_activate_hook_2(SCM hook, SCM a, SCM b) {
    if (scm_is_true(hook) && scm_is_hook(hook) && !scm_is_empty_hook(hook)) {
        scm_c_run_hook(hook, scm_list_2(a, b));
    }
}

void scm_c_activate_hook_3(SCM hook, SCM a, SCM b, SCM c) {
    if (scm_is_true(hook) && scm_is_hook(hook) && !scm_is_empty_hook(hook)) {
        scm_c_run_hook(hook, scm_list_3(a, b, c));
    }
}

void scm_c_activate_hook_4(SCM hook, SCM a, SCM b, SCM c, SCM d) {
    if (scm_is_true(hook) && scm_is_hook(hook) && !scm_is_empty_hook(hook)) {
        scm_c_run_hook(hook, scm_list_4(a, b, c, d));
    }
}

void scm_c_activate_hook_many(SCM hook, SCM *args, size_t count) {
    if (!scm_is_true(hook) || !scm_is_hook(hook) || scm_is_empty_hook(hook)) {
        return;
    }
    
    SCM arg_list = SCM_EOL;
    for (size_t i = count; i > 0; i--) {
        arg_list = scm_cons(args[i-1], arg_list);
    }
    
    scm_c_run_hook(hook, arg_list);
}

SCM scm_c_make_hook(size_t arity) {
    return scm_make_hook(scm_from_size_t(arity));
}

size_t scm_c_hook_size(SCM hook) {
    if (!scm_is_hook(hook)) {
        set_error(SCM_ERROR_WRONG_TYPE);
        return 0;
    }
    
    SCM procedures = scm_hook_to_list(hook);
    return scm_c_length(procedures);
}
#endif

// ============================================================================
// Enhanced Module and Definition Operations (original functions modernized)
// ============================================================================

SCM scm_current_module_definition(SCM name) {
    if (!scm_is_symbol(name)) {
        set_error(SCM_ERROR_WRONG_TYPE);
        return SCM_BOOL_F;
    }
    
    SCM variable = scm_module_variable(scm_current_module(), name);
    if (scm_is_true(variable) && scm_is_true(scm_variable_bound_p(variable))) {
        return scm_variable_ref(variable);
    }
    return SCM_BOOL_F;
}

SCM scm_default_definition(SCM name) {
    if (!scm_is_symbol(name)) {
        set_error(SCM_ERROR_WRONG_TYPE);
        return SCM_BOOL_F;
    }
    
    // Try current module first
    SCM result = scm_current_module_definition(name);
    if (!scm_is_false(result)) {
        return result;
    }
    
    // Try 'gi' module
    SCM gi_module = scm_c_resolve_module("gi");
    if (scm_is_true(gi_module)) {
        SCM variable = scm_module_variable(gi_module, name);
        if (scm_is_true(variable) && scm_is_true(scm_variable_bound_p(variable))) {
            return scm_variable_ref(variable);
        }
    }
    
    // Try 'guile' module as fallback
    SCM guile_module = scm_c_resolve_module("guile");
    if (scm_is_true(guile_module)) {
        SCM variable = scm_module_variable(guile_module, name);
        if (scm_is_true(variable) && scm_is_true(scm_variable_bound_p(variable))) {
            return scm_variable_ref(variable);
        }
    }
    
    return SCM_BOOL_F;
}

SCM scm_safe_module_definition(SCM module, SCM name) {
    if (!scm_is_module(module) || !scm_is_symbol(name)) {
        set_error(SCM_ERROR_WRONG_TYPE);
        return SCM_BOOL_F;
    }
    
    SCM variable = scm_module_variable(module, name);
    if (scm_is_true(variable) && scm_is_true(scm_variable_bound_p(variable))) {
        return scm_variable_ref(variable);
    }
    return SCM_BOOL_F;
}

bool scm_module_has_definition(SCM module, SCM name) {
    if (!scm_is_module(module) || !scm_is_symbol(name)) {
        set_error(SCM_ERROR_WRONG_TYPE);
        return false;
    }
    
    SCM variable = scm_module_variable(module, name);
    return scm_is_true(variable) && scm_is_true(scm_variable_bound_p(variable));
}

SCM scm_resolve_module_safe(const char *module_name) {
    if (!module_name) {
        set_error(SCM_ERROR_WRONG_TYPE);
        return SCM_BOOL_F;
    }
    
    SCM module_symbol = scm_from_utf8_symbol(module_name);
    return scm_resolve_module(scm_list_1(module_symbol));
}

SCM scm_c_public_ref_safe(const char *module_name, const char *name) {
    if (!module_name || !name) {
        set_error(SCM_ERROR_WRONG_TYPE);
        return SCM_BOOL_F;
    }
    
    SCM module = scm_resolve_module_safe(module_name);
    if (scm_is_false(module)) {
        return SCM_BOOL_F;
    }
    
    SCM name_symbol = scm_from_utf8_symbol(name);
    return scm_safe_module_definition(module, name_symbol);
}

bool _scm_c_use_module(const char *module_name) {
    if (!module_name) {
        set_error(SCM_ERROR_WRONG_TYPE);
        return false;
    }
    
    SCM module = scm_resolve_module_safe(module_name);
    if (scm_is_false(module)) {
        return false;
    }
    
    scm_set_current_module(module);
    set_error(SCM_ERROR_NONE);
    return true;
}

// ============================================================================
// Enhanced String and Symbol Operations (original functions modernized)
// ============================================================================

char *scm_write_to_utf8_stringn(SCM x, size_t max_len) {
    static bool first = true;
    static SCM format;
    static SCM ellipses;
    
    if (first) {
        format = scm_from_utf8_string("~S");
        ellipses = scm_from_utf8_string("...");
        scm_gc_protect_object(format);
        scm_gc_protect_object(ellipses);
        first = false;
    }
    
    SCM args_str = scm_simple_format(SCM_BOOL_F, format, scm_list_1(x));
    size_t str_len = scm_c_string_length(args_str);
    
    char *result;
    if (str_len > max_len) {
        SCM truncated = scm_string_append(
            scm_list_2(scm_c_substring(args_str, 0, max_len), ellipses));
        result = scm_to_utf8_string(truncated);
    } else {
        result = scm_to_utf8_string(args_str);
    }
    
    return result;
}

SCM scm_keyword_to_string(SCM x) {
    if (!scm_is_keyword(x)) {
        set_error(SCM_ERROR_WRONG_TYPE);
        return SCM_BOOL_F;
    }
    
    return scm_symbol_to_string(scm_keyword_to_symbol(x));
}

char *scm_to_utf8_symbol(SCM sym) {
    if (!scm_is_symbol(sym)) {
        set_error(SCM_ERROR_WRONG_TYPE);
        return NULL;
    }
    
    return scm_to_utf8_string(scm_symbol_to_string(sym));
}

char *scm_to_utf8_string_safe(SCM str) {
    if (!scm_is_string(str)) {
        set_error(SCM_ERROR_WRONG_TYPE);
        return NULL;
    }
    
    return scm_to_utf8_string(str);
}

SCM scm_from_utf8_string_safe(const char *str) {
    if (!str) {
        set_error(SCM_ERROR_WRONG_TYPE);
        return SCM_BOOL_F;
    }
    
    return scm_from_utf8_string(str);
}

char *_scm_object_to_string(SCM obj, size_t max_len) {
    return scm_write_to_utf8_stringn(obj, max_len);
}

char *scm_object_to_display_string(SCM obj, size_t max_len) {
    SCM port = scm_open_output_string();
    scm_display(obj, port);
    SCM str = scm_get_output_string(port);
    scm_close_port(port);
    
    size_t str_len = scm_c_string_length(str);
    if (str_len > max_len) {
        str = scm_c_substring(str, 0, max_len);
    }
    
    return scm_to_utf8_string(str);
}

SCM scm_gensym_safe(const char *prefix) {
    SCM prefix_scm = prefix ? scm_from_utf8_string(prefix) : SCM_UNDEFINED;
    return scm_gensym(prefix_scm);
}

bool scm_symbol_exists_p(const char *name) {
    if (!name) return false;
    
    SCM symbol = scm_from_utf8_symbol(name);
    return scm_is_defined(symbol);
}

// ============================================================================
// Enhanced I/O and Formatting (original functions modernized)
// ============================================================================

void scm_printf(SCM port, const char *fmt, ...) {
    if (!fmt) return;
    
    char message[2048];  // Increased buffer size
    va_list args;
    
    va_start(args, fmt);
    int result = vsnprintf(message, sizeof(message), fmt, args);
    va_end(args);
    
    if (result < 0) {
        set_error(SCM_ERROR_SYSTEM);
        return;
    }
    
    if ((size_t)result >= sizeof(message)) {
        // Handle truncation
        strcpy(message + sizeof(message) - 12, "...[TRUNC]");
    }
    
    SCM message_scm = scm_from_utf8_string(message);
    scm_display(message_scm, port);
}

void scm_vprintf(SCM port, const char *fmt, va_list args) {
    if (!fmt) return;
    
    char message[2048];
    int result = vsnprintf(message, sizeof(message), fmt, args);
    
    if (result < 0) {
        set_error(SCM_ERROR_SYSTEM);
        return;
    }
    
    if ((size_t)result >= sizeof(message)) {
        strcpy(message + sizeof(message) - 12, "...[TRUNC]");
    }
    
    SCM message_scm = scm_from_utf8_string(message);
    scm_display(message_scm, port);
}

void scm_eprintf(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    scm_vprintf(scm_current_error_port(), fmt, args);
    va_end(args);
}

void scm_dprintf(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    scm_vprintf(scm_current_output_port(), fmt, args);
    va_end(args);
}

SCM scm_sprintf(const char *fmt, ...) {
    if (!fmt) return SCM_BOOL_F;
    
    char message[2048];
    va_list args;
    
    va_start(args, fmt);
    int result = vsnprintf(message, sizeof(message), fmt, args);
    va_end(args);
    
    if (result < 0) {
        set_error(SCM_ERROR_SYSTEM);
        return SCM_BOOL_F;
    }
    
    return scm_from_utf8_string(message);
}

// ============================================================================
// Enhanced Memory Management (original functions modernized)
// ============================================================================

void *scm_dynfree(void *x) {
    if (x) {
        scm_dynwind_free(x);
    }
    return x;
}

void *scm_dynwind_malloc(size_t size) {
    void *ptr = xmalloc(size);
    scm_dynwind_free(ptr);
    return ptr;
}

char *scm_dynwind_strdup(const char *str) {
    if (!str) return NULL;
    
    char *copy = xstrdup(str);
    scm_dynwind_free(copy);
    return copy;
}

// ============================================================================
// Enhanced Error Handling and Exceptions
// ============================================================================

SCM scm_sad_quit(void) {
    return scm_c_eval_string("(quit 1)");
}

ScmErrorType scm_get_last_error(void) {
    return g_last_error;
}

const char *scm_error_type_string(ScmErrorType error) {
    switch (error) {
        case SCM_ERROR_NONE:
            return "No error";
        case SCM_ERROR_WRONG_TYPE:
            return "Wrong type argument";
        case SCM_ERROR_OUT_OF_RANGE:
            return "Argument out of range";
        case SCM_ERROR_UNBOUND_VARIABLE:
            return "Unbound variable";
        case SCM_ERROR_SYNTAX:
            return "Syntax error";
        case SCM_ERROR_SYSTEM:
            return "System error";
        case SCM_ERROR_MISC:
            return "Miscellaneous error";
        default:
            return "Unknown error";
    }
}

void scm_throw_error(const char *key, const char *message, ...) {
    if (!key || !message) return;
    
    char formatted_message[1024];
    va_list args;
    
    va_start(args, message);
    vsnprintf(formatted_message, sizeof(formatted_message), message, args);
    va_end(args);
    
    SCM key_scm = scm_from_utf8_symbol(key);
    SCM message_scm = scm_from_utf8_string(formatted_message);
    
    scm_throw(key_scm, scm_list_1(message_scm));
}

void scm_wrong_type_arg(const char *func, int pos, SCM actual) {
    scm_throw_error("wrong-type-arg", 
                   "Wrong type argument in position %d to %s: ~S", 
                   pos, func ? func : "unknown");
}

void scm_out_of_range(const char *func, SCM value) {
    scm_throw_error("out-of-range",
                   "Argument out of range in %s: ~S",
                   func ? func : "unknown");
}

// ============================================================================
// Enhanced Debugging and Introspection
// ============================================================================

// GDB helpers (original function modernized)
void pg(SCM x) {
    scm_write(x, scm_current_output_port());
    scm_newline(scm_current_output_port());
}

void pd(SCM x) {
    scm_display(x, scm_current_output_port());
    scm_newline(scm_current_output_port());
}

void pw(SCM x) {
    scm_write(x, scm_current_output_port());
    scm_newline(scm_current_output_port());
}

void pp(SCM x) {
    SCM pretty_print = scm_c_public_ref_safe("ice-9 pretty-print", "pretty-print");
    if (scm_is_procedure(pretty_print)) {
        scm_call_1(pretty_print, x);
    } else {
        scm_write(x, scm_current_output_port());
        scm_newline(scm_current_output_port());
    }
}

void pi(SCM x) {
    scm_printf(scm_current_output_port(), "Object: %s\n", scm_object_type_name(x));
    scm_printf(scm_current_output_port(), "Value: ");
    scm_write(x, scm_current_output_port());
    scm_newline(scm_current_output_port());
}

const char *scm_object_type_name(SCM obj) {
    if (scm_is_null(obj)) return "null";
    if (scm_is_boolean(obj)) return "boolean";
    if (scm_is_char(obj)) return "character";
    if (scm_is_integer(obj)) return "integer";
    if (scm_is_real(obj)) return "real";
    if (scm_is_complex(obj)) return "complex";
    if (scm_is_string(obj)) return "string";
    if (scm_is_symbol(obj)) return "symbol";
    if (scm_is_keyword(obj)) return "keyword";
    if (scm_is_pair(obj)) return "pair";
    if (scm_is_list(obj)) return "list";
    if (scm_is_vector(obj)) return "vector";
    if (scm_is_bytevector(obj)) return "bytevector";
    if (scm_is_procedure(obj)) return "procedure";
    if (scm_is_port(obj)) return "port";
    if (scm_is_hash_table(obj)) return "hash-table";
    if (scm_is_module(obj)) return "module";
    return "unknown";
}

// Performance monitoring
void scm_start_timing(void) {
    clock_gettime(CLOCK_MONOTONIC, &g_start_time);
    g_timing_active = true;
}

double scm_end_timing(void) {
    if (!g_timing_active) return 0.0;
    
    struct timespec end_time;
    clock_gettime(CLOCK_MONOTONIC, &end_time);
    
    double elapsed = (end_time.tv_sec - g_start_time.tv_sec) +
                    (end_time.tv_nsec - g_start_time.tv_nsec) / 1000000000.0;
    
    g_timing_active = false;
    return elapsed;
}

void scm_print_stats(FILE *stream) {
    if (!stream) stream = stderr;
    
    // Print basic Guile statistics
    fprintf(stream, "Scheme Statistics:\n");
    fprintf(stream, "  Guile version: %s\n", scm_to_utf8_string(scm_version()));
    
    // Add GC stats if available
    SCM gc_stats = scm_gc_stats();
    if (scm_is_true(gc_stats)) {
        fprintf(stream, "  GC statistics available\n");
    }
}

bool scm_is_char(SCM x) {
    return SCM_CHARP(x);
}

#if 0
bool scm_is_class(SCM x) {
    return SCM_CLASSP(x);
}
#endif

bool scm_is_defined(SCM x) {
    if (!scm_is_symbol(x)) return false;
    
    SCM var = scm_module_variable(scm_current_module(), x);
    return scm_is_true(var) && scm_is_true(scm_variable_bound_p(var));
}

#if 0
bool scm_is_keyword(SCM x) {
    return scm_is_true(scm_keyword_p(x));
}
#endif

bool scm_is_list(SCM x) {
    return scm_is_true(scm_list_p(x));
}

bool scm_is_pointer(SCM x) {
    return SCM_POINTER_P(x);
}

bool scm_is_procedure(SCM x) {
    return scm_is_true(scm_procedure_p(x));
}

#if 0
bool scm_is_real(SCM x) {
    return scm_is_true(scm_real_p(x));
}
#endif

bool scm_is_equal(SCM a, SCM b) {
    return scm_is_true(scm_equal_p(a, b));
}

#if HAVE_SCM_HOOKS
bool scm_is_empty_hook(SCM x) {
    return scm_is_true(scm_hook_empty_p(x));
}

bool scm_is_hook(SCM x) {
    return scm_is_true(scm_hook_p(x));
}
#endif
