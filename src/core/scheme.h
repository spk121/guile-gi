// Copyright (C) 2022 Michael L. Gran
// Modernized 2025 - Enhanced GNU Guile integration with safety, convenience, and rich API

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

#ifndef CORE_SCM_H
#define CORE_SCM_H

#include <libguile.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include "visibility.h"

#ifdef __cplusplus
extern "C" {
#endif

// ============================================================================
// Guile Version Detection and Feature Macros
// ============================================================================

// Enhanced version detection for fine-grained feature control
#define GUILE_VERSION_CHECK(major, minor, micro) \
    (SCM_MAJOR_VERSION > (major) || \
     (SCM_MAJOR_VERSION == (major) && SCM_MINOR_VERSION > (minor)) || \
     (SCM_MAJOR_VERSION == (major) && SCM_MINOR_VERSION == (minor) && SCM_MICRO_VERSION >= (micro)))

// Feature availability macros
#define HAVE_SCM_HOOKS GUILE_VERSION_CHECK(3, 0, 7)
#define HAVE_SCM_EXCEPTIONS GUILE_VERSION_CHECK(3, 0, 0)
#define HAVE_SCM_BYTEVECTORS GUILE_VERSION_CHECK(2, 0, 0)
#define HAVE_SCM_DYNAMIC_STATE GUILE_VERSION_CHECK(2, 0, 0)
#define HAVE_SCM_R6RS_PORTS GUILE_VERSION_CHECK(2, 0, 0)

// Backward compatibility for older Guile versions
#if !GUILE_VERSION_CHECK(3, 0, 4)
#define scm_c_bitvector_count(x) scm_to_size_t(scm_bit_count(SCM_BOOL_T, (x)))
#endif

#if !GUILE_VERSION_CHECK(2, 2, 0)
// Provide fallbacks for missing functions in older Guile
#define scm_is_bytevector(x) scm_is_true(scm_bytevector_p(x))
#endif

// ============================================================================
// Enhanced Type Predicates (original functions modernized)
// ============================================================================

GIG_API bool scm_is_boolean(SCM x) GIG_PURE;
// GIG_API bool scm_is_bytevector(SCM x) GIG_PURE;
GIG_API bool scm_is_char(SCM x) GIG_PURE;
// GIG_API bool scm_is_class(SCM x) GIG_PURE;
GIG_API bool scm_is_defined(SCM x) GIG_PURE;
// GIG_API bool scm_is_keyword(SCM x) GIG_PURE;
GIG_API bool scm_is_list(SCM x) GIG_PURE;
GIG_API bool scm_is_pointer(SCM x) GIG_PURE;
GIG_API bool scm_is_procedure(SCM x) GIG_PURE;
// GIG_API bool scm_is_real(SCM x) GIG_PURE;
GIG_API bool scm_is_equal(SCM a, SCM b) GIG_PURE;

#if HAVE_SCM_HOOKS
GIG_API bool scm_is_empty_hook(SCM x) GIG_PURE;
GIG_API bool scm_is_hook(SCM x) GIG_PURE;
#endif

// Additional type predicates
GIG_API bool scm_is_port(SCM x) GIG_PURE;
GIG_API bool scm_is_input_port(SCM x) GIG_PURE;
GIG_API bool scm_is_output_port(SCM x) GIG_PURE;
GIG_API bool scm_is_file_port(SCM x) GIG_PURE;
GIG_API bool scm_is_string_port(SCM x) GIG_PURE;
GIG_API bool scm_is_hash_table(SCM x) GIG_PURE;
GIG_API bool scm_is_weak_hash_table(SCM x) GIG_PURE;
GIG_API bool scm_is_module(SCM x) GIG_PURE;
GIG_API bool scm_is_syntax(SCM x) GIG_PURE;
GIG_API bool scm_is_promise(SCM x) GIG_PURE;
GIG_API bool scm_is_continuation(SCM x) GIG_PURE;

// Type checking with better names
// GIG_API bool scm_is_exact_integer(SCM x) GIG_PURE;
GIG_API bool scm_is_inexact_real(SCM x) GIG_PURE;
// GIG_API bool scm_is_complex(SCM x) GIG_PURE;
// GIG_API bool scm_is_rational(SCM x) GIG_PURE;
GIG_API bool scm_is_finite(SCM x) GIG_PURE;
GIG_API bool scm_is_infinite(SCM x) GIG_PURE;
GIG_API bool scm_is_nan(SCM x) GIG_PURE;

// ============================================================================
// Enhanced List Operations (original functions modernized)
// ============================================================================

GIG_API SCM scm_append2(SCM a, SCM b) GIG_MALLOC;
GIG_API size_t scm_c_length(SCM x) GIG_PURE;
GIG_API SCM scm_c_list_ref(SCM lst, size_t i) GIG_PURE;
GIG_API SCM scm_drop_right_1(SCM lst) GIG_MALLOC;

// Enhanced list operations
GIG_API SCM scm_append_many(SCM first, ...) GIG_MALLOC;
GIG_API SCM scm_appendv(SCM *lists, size_t count) GIG_MALLOC;
GIG_API SCM scm_c_make_list(size_t length, SCM fill) GIG_MALLOC;
GIG_API SCM scm_c_list_set_x(SCM lst, size_t i, SCM value);
GIG_API SCM scm_c_list_slice(SCM lst, size_t start, size_t end) GIG_MALLOC;
GIG_API SCM scm_c_list_reverse_slice(SCM lst, size_t start, size_t end) GIG_MALLOC;

// List searching and filtering
GIG_API SCM scm_c_list_find(SCM lst, SCM predicate) GIG_PURE;
GIG_API ssize_t scm_c_list_index(SCM lst, SCM item) GIG_PURE;
GIG_API size_t scm_c_list_count(SCM lst, SCM item) GIG_PURE;
GIG_API SCM scm_c_list_filter(SCM lst, SCM predicate) GIG_MALLOC;
GIG_API SCM scm_c_list_remove(SCM lst, SCM item) GIG_MALLOC;

// ============================================================================
// Enhanced Hook Operations
// ============================================================================

#if HAVE_SCM_HOOKS
GIG_API void scm_c_activate_hook_2(SCM hook, SCM a, SCM b);
GIG_API void scm_c_activate_hook_3(SCM hook, SCM a, SCM b, SCM c);
GIG_API void scm_c_activate_hook_4(SCM hook, SCM a, SCM b, SCM c, SCM d);
GIG_API void scm_c_activate_hook_many(SCM hook, SCM *args, size_t count);

// Hook management
GIG_API SCM scm_c_make_hook(size_t arity) GIG_MALLOC;
// GIG_API bool scm_c_hook_add(SCM hook, SCM proc);
// GIG_API bool scm_c_hook_remove(SCM hook, SCM proc);
GIG_API void scm_c_hook_clear(SCM hook);
GIG_API size_t scm_c_hook_size(SCM hook) GIG_PURE;
#endif

// ============================================================================
// Enhanced Module and Definition Operations (original functions modernized)
// ============================================================================

GIG_API SCM scm_current_module_definition(SCM name) GIG_PURE;
GIG_API SCM scm_default_definition(SCM name) GIG_PURE;

// Enhanced module operations
GIG_API SCM scm_safe_module_definition(SCM module, SCM name) GIG_PURE;
GIG_API bool scm_module_has_definition(SCM module, SCM name) GIG_PURE;
GIG_API SCM scm_module_export_list(SCM module) GIG_PURE;
GIG_API SCM scm_module_import_list(SCM module) GIG_PURE;

// Module path operations
GIG_API SCM scm_resolve_module_safe(const char *module_name) GIG_PURE;
GIG_API SCM scm_c_public_ref_safe(const char *module_name, const char *name) GIG_PURE;
GIG_API bool scm_c_module_define_safe(const char *module_name, const char *name, SCM value);

// Dynamic module loading
// GIG_API bool scm_c_use_module(const char *module_name);
GIG_API SCM scm_c_load_file_safe(const char *filename);
GIG_API SCM scm_c_eval_string_safe(const char *expr);

// ============================================================================
// Enhanced String and Symbol Operations (original functions modernized)
// ============================================================================

GIG_API char *scm_write_to_utf8_stringn(SCM x, size_t max_len) GIG_MALLOC;
GIG_API SCM scm_keyword_to_string(SCM x) GIG_MALLOC;
GIG_API char *scm_to_utf8_symbol(SCM sym) GIG_MALLOC;

// Enhanced string operations
GIG_API char *scm_to_utf8_string_safe(SCM str) GIG_MALLOC;
GIG_API SCM scm_from_utf8_string_safe(const char *str) GIG_MALLOC;
// GIG_API char *scm_object_to_string(SCM obj, size_t max_len) GIG_MALLOC;
GIG_API char *scm_object_to_display_string(SCM obj, size_t max_len) GIG_MALLOC;

// String port operations
GIG_API SCM scm_open_input_string_safe(const char *str) GIG_MALLOC;
GIG_API SCM scm_open_output_string_safe(void) GIG_MALLOC;
GIG_API char *scm_get_output_string_utf8(SCM port) GIG_MALLOC;

// Symbol operations
GIG_API SCM scm_gensym_safe(const char *prefix) GIG_MALLOC;
GIG_API bool scm_symbol_exists_p(const char *name) GIG_PURE;
GIG_API SCM scm_string_to_symbol_safe(const char *str) GIG_MALLOC;

// ============================================================================
// Enhanced I/O and Formatting (original functions modernized)
// ============================================================================

GIG_API void scm_printf(SCM port, const char *fmt, ...) GIG_PRINTF(2, 3);
GIG_API void scm_vprintf(SCM port, const char *fmt, va_list args) GIG_PRINTF(2, 0);

// Enhanced formatting
GIG_API void scm_eprintf(const char *fmt, ...) GIG_PRINTF(1, 2);  // Error output
GIG_API void scm_dprintf(const char *fmt, ...) GIG_PRINTF(1, 2);  // Debug output
GIG_API SCM scm_sprintf(const char *fmt, ...) GIG_PRINTF(1, 2) GIG_MALLOC;
GIG_API char *scm_asprintf(const char *fmt, ...) GIG_PRINTF(1, 2) GIG_MALLOC;

// Port operations
GIG_API SCM scm_open_file_safe(const char *filename, const char *mode) GIG_MALLOC;
GIG_API bool scm_close_port_safe(SCM port);
GIG_API SCM scm_read_safe(SCM port);
GIG_API bool scm_write_safe(SCM obj, SCM port);
GIG_API bool scm_display_safe(SCM obj, SCM port);

// ============================================================================
// Enhanced Memory Management (original functions modernized)
// ============================================================================

GIG_API void *scm_dynfree(void *x);

// Enhanced dynamic wind operations
GIG_API void *scm_dynwind_malloc(size_t size);
GIG_API char *scm_dynwind_strdup(const char *str);
GIG_API void scm_dynwind_unwind_handler_safe(void (*handler)(void *), void *data);
GIG_API void scm_dynwind_rewind_handler_safe(void (*handler)(void *), void *data);

// Garbage collection helpers
GIG_API void scm_gc_protect_object_safe(SCM obj);
GIG_API void scm_gc_unprotect_object_safe(SCM obj);
GIG_API size_t scm_gc_total_allocated(void) GIG_PURE;
// GIG_API void scm_gc_stats(size_t *total, size_t *free, size_t *used);

// ============================================================================
// Enhanced Error Handling and Exceptions
// ============================================================================

    GIG_API SCM scm_sad_quit(void);

// Exception handling
typedef enum {
    SCM_ERROR_NONE = 0,
    SCM_ERROR_WRONG_TYPE,
    SCM_ERROR_OUT_OF_RANGE,
    SCM_ERROR_UNBOUND_VARIABLE,
    SCM_ERROR_SYNTAX,
    SCM_ERROR_SYSTEM,
    SCM_ERROR_MISC
} ScmErrorType;

GIG_API ScmErrorType scm_get_last_error(void);
GIG_API const char *scm_error_type_string(ScmErrorType error);
GIG_API SCM scm_make_error_safe(const char *key, const char *message, SCM irritants);

// Safe exception handling
GIG_API SCM scm_catch_safe(SCM key, SCM (*thunk)(void *), void *thunk_data,
                          SCM (*handler)(void *, SCM, SCM), void *handler_data);
GIG_API bool scm_with_exception_handler_safe(SCM handler, SCM (*thunk)(void *), void *data);

// Error throwing
GIG_API void scm_throw_error(const char *key, const char *message, ...) GIG_PRINTF(2, 3) GIG_NORETURN;
GIG_API void scm_wrong_type_arg(const char *func, int pos, SCM actual) GIG_NORETURN;
GIG_API void scm_out_of_range(const char *func, SCM value) GIG_NORETURN;

// ============================================================================
// Enhanced Conversion Functions
// ============================================================================

// Safe conversion with error handling
GIG_API bool scm_to_bool_safe(SCM x, bool *result);
GIG_API bool scm_to_int_safe(SCM x, int *result);
GIG_API bool scm_to_long_safe(SCM x, long *result);
GIG_API bool scm_to_size_t_safe(SCM x, size_t *result);
GIG_API bool scm_to_double_safe(SCM x, double *result);

// Array conversions
GIG_API int *scm_to_int_array(SCM lst, size_t *length) GIG_MALLOC;
GIG_API double *scm_to_double_array(SCM lst, size_t *length) GIG_MALLOC;
GIG_API char **scm_to_string_array(SCM lst, size_t *length) GIG_MALLOC;
GIG_API void scm_free_string_array(char **array, size_t length);

GIG_API SCM scm_from_int_array(const int *array, size_t length) GIG_MALLOC;
GIG_API SCM scm_from_double_array(const double *array, size_t length) GIG_MALLOC;
GIG_API SCM scm_from_string_array(const char * const *array, size_t length) GIG_MALLOC;

// ============================================================================
// Enhanced Hash Table Operations
// ============================================================================

// GIG_API SCM scm_c_make_hash_table(size_t size) GIG_MALLOC;
GIG_API SCM scm_c_hash_ref_safe(SCM table, SCM key, SCM default_value) GIG_PURE;
GIG_API bool scm_c_hash_set_safe(SCM table, SCM key, SCM value);
GIG_API bool scm_c_hash_remove_safe(SCM table, SCM key);
GIG_API size_t scm_c_hash_size(SCM table) GIG_PURE;
GIG_API SCM scm_c_hash_keys(SCM table) GIG_MALLOC;
GIG_API SCM scm_c_hash_values(SCM table) GIG_MALLOC;

// ============================================================================
// Enhanced Vector Operations
// ============================================================================

GIG_API SCM scm_c_make_vector_safe(size_t length, SCM fill) GIG_MALLOC;
GIG_API SCM scm_c_vector_ref_safe(SCM vector, size_t index) GIG_PURE;
GIG_API bool scm_c_vector_set_safe(SCM vector, size_t index, SCM value);
GIG_API size_t scm_c_vector_length_safe(SCM vector) GIG_PURE;
GIG_API SCM scm_vector_slice(SCM vector, size_t start, size_t end) GIG_MALLOC;

// ============================================================================
// Enhanced Procedure Operations
// ============================================================================

GIG_API SCM scm_c_call_safe(SCM proc, SCM *args, size_t nargs);
GIG_API SCM scm_c_apply_safe(SCM proc, SCM args);
GIG_API size_t scm_c_procedure_arity(SCM proc) GIG_PURE;
GIG_API bool scm_c_procedure_accepts_n_args(SCM proc, size_t n) GIG_PURE;

// Lambda and closure creation
GIG_API SCM scm_c_make_procedure(SCM (*func)(SCM *args, size_t nargs), 
                                size_t required_args, size_t optional_args) GIG_MALLOC;

// ============================================================================
// Enhanced Debugging and Introspection
// ============================================================================

// GDB helpers (original function modernized)
GIG_API void pg(SCM x);

// Enhanced debugging
GIG_API void pd(SCM x);  // Display
GIG_API void pw(SCM x);  // Write  
GIG_API void pp(SCM x);  // Pretty print
GIG_API void pi(SCM x);  // Inspect

// Object introspection
GIG_API SCM scm_object_type(SCM obj) GIG_PURE;
GIG_API const char *scm_object_type_name(SCM obj) GIG_PURE;
GIG_API size_t scm_object_size(SCM obj) GIG_PURE;
GIG_API bool scm_object_has_property(SCM obj, SCM property) GIG_PURE;

// Performance monitoring
GIG_API void scm_start_timing(void);
GIG_API double scm_end_timing(void);
GIG_API void scm_print_stats(FILE *stream);

// ============================================================================
// Enhanced Utility Macros (original macros modernized)
// ============================================================================

// Safe undefined to false conversion
#define SCM_UNBND_TO_BOOL_F(obj) \
    do { \
        if (SCM_UNBNDP(obj)) \
            (obj) = SCM_BOOL_F; \
    } while (0)

// Enhanced type checking macros
#define MYSCM_ASSERT_TYPE(pred, obj, pos, func) \
    do { \
        if (!(pred(obj))) \
            scm_wrong_type_arg(func, pos, obj); \
    } while (0)

#define MYSCM_ASSERT_RANGE(obj, condition, func) \
    do { \
        if (!(condition)) \
            scm_out_of_range(func, obj); \
    } while (0)

// Safe accessor macros
#define SCM_SAFE_CALL_0(proc) \
    (scm_is_procedure(proc) ? scm_call_0(proc) : SCM_BOOL_F)

#define SCM_SAFE_CALL_1(proc, arg1) \
    (scm_is_procedure(proc) ? scm_call_1(proc, arg1) : SCM_BOOL_F)

#define SCM_SAFE_CALL_2(proc, arg1, arg2) \
    (scm_is_procedure(proc) ? scm_call_2(proc, arg1, arg2) : SCM_BOOL_F)

#define SCM_SAFE_CALL_3(proc, arg1, arg2, arg3) \
    (scm_is_procedure(proc) ? scm_call_3(proc, arg1, arg2, arg3) : SCM_BOOL_F)

// List creation macros
#define SCM_LIST(...) scm_list_n(__VA_ARGS__, SCM_UNDEFINED)
#define SCM_VECTOR(...) scm_c_vector(__VA_ARGS__, SCM_UNDEFINED)

// Error handling in C functions
#define SCM_TRY_BEGIN \
    do { \
        SCM _scm_error_key = SCM_BOOL_F; \
        SCM _scm_error_args = SCM_BOOL_F;

#define SCM_TRY_END \
    } while (0)

#define SCM_CATCH(key_var, args_var) \
    if (scm_is_true(_scm_error_key)) { \
        SCM key_var = _scm_error_key; \
        SCM args_var = _scm_error_args;

// Convenience macros for common operations
#define SCM_IS_TRUE(x) scm_is_true(x)
#define SCM_IS_FALSE(x) scm_is_false(x)
#define SCM_IS_NULL(x) scm_is_null(x)
#define SCM_IS_PAIR(x) scm_is_pair(x)

// ============================================================================
// Deprecated Aliases (for backward compatibility)
// ============================================================================

// Legacy int-returning functions (now return bool)
GIG_DEPRECATED_API int scm_is_boolean_legacy(SCM x);
GIG_DEPRECATED_API int scm_is_bytevector_legacy(SCM x);
GIG_DEPRECATED_API int scm_is_equal_legacy(SCM a, SCM b);

#define scm_is_boolean_legacy(x) ((int)scm_is_boolean(x))
#define scm_is_bytevector_legacy(x) ((int)scm_is_bytevector(x))
#define scm_is_equal_legacy(x, y) ((int)scm_is_equal(x, y))

#ifdef __cplusplus
}
#endif

#endif // CORE_SCM_H
