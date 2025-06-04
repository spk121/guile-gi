// Copyright (C) 2022 Michael L. Gran
// Modernized 2025 - Enhanced GOOPS integration with safety, rich API, and modern C features

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

#ifndef CORE_OOP_H
#define CORE_OOP_H

#include <libguile.h>
#include <stdbool.h>
#include <stddef.h>
#include "visibility.h"
#include "scheme.h"

#ifdef __cplusplus
extern "C" {
#endif

// ============================================================================
// GOOPS System Initialization and Status
// ============================================================================

// Initialization (enhanced from original)
GIG_API void init_core_oop(void);
GIG_API bool oop_is_initialized(void) GIG_PURE;
GIG_API void oop_cleanup(void);

// System status and diagnostics
GIG_API bool oop_goops_available(void) GIG_PURE;
GIG_API const char *oop_goops_version(void) GIG_PURE;
GIG_API void oop_print_class_hierarchy(SCM class, FILE *stream);

// ============================================================================
// Built-in Class Accessors (original functions modernized)
// ============================================================================

// Core type classes
GIG_API SCM scm_get_top_class(void) GIG_PURE;
GIG_API SCM scm_get_object_class(void) GIG_PURE;
GIG_API SCM scm_get_class_class(void) GIG_PURE;
GIG_API SCM scm_get_generic_class(void) GIG_PURE;
GIG_API SCM scm_get_method_class(void) GIG_PURE;

// Primitive type classes
GIG_API SCM scm_get_boolean_class(void) GIG_PURE;
GIG_API SCM scm_get_char_class(void) GIG_PURE;
GIG_API SCM scm_get_integer_class(void) GIG_PURE;
GIG_API SCM scm_get_real_class(void) GIG_PURE;
GIG_API SCM scm_get_complex_class(void) GIG_PURE;
GIG_API SCM scm_get_number_class(void) GIG_PURE;
GIG_API SCM scm_get_string_class(void) GIG_PURE;
GIG_API SCM scm_get_symbol_class(void) GIG_PURE;
GIG_API SCM scm_get_keyword_class(void) GIG_PURE;

// Collection type classes
GIG_API SCM scm_get_list_class(void) GIG_PURE;
GIG_API SCM scm_get_pair_class(void) GIG_PURE;
GIG_API SCM scm_get_null_class(void) GIG_PURE;
GIG_API SCM scm_get_vector_class(void) GIG_PURE;
GIG_API SCM scm_get_bytevector_class(void) GIG_PURE;
GIG_API SCM scm_get_hashtable_class(void) GIG_PURE;

// Function and procedure classes
GIG_API SCM scm_get_procedure_class(void) GIG_PURE;
GIG_API SCM scm_get_applicable_class(void) GIG_PURE;
GIG_API SCM scm_get_applicable_struct_class(void) GIG_PURE;
GIG_API SCM scm_get_applicable_struct_with_setter_class(void) GIG_PURE;

// I/O and system classes
GIG_API SCM scm_get_port_class(void) GIG_PURE;
GIG_API SCM scm_get_input_port_class(void) GIG_PURE;
GIG_API SCM scm_get_output_port_class(void) GIG_PURE;
GIG_API SCM scm_get_foreign_class(void) GIG_PURE;
GIG_API SCM scm_get_unknown_class(void) GIG_PURE;

// ============================================================================
// Enhanced Class Operations
// ============================================================================

// Class creation and manipulation (original functions enhanced)
GIG_API SCM scm_make_class_with_name(SCM supers, SCM slots, SCM name) GIG_MALLOC;
GIG_API SCM scm_make_class_simple(SCM super, const char *name) GIG_MALLOC;
GIG_API SCM scm_make_class_with_slots(SCM super, const char *name, 
                                     const char **slot_names, size_t slot_count) GIG_MALLOC;

// Enhanced class operations
GIG_API SCM scm_define_class(const char *name, SCM supers, SCM slots) GIG_MALLOC;
GIG_API bool scm_class_has_slot(SCM class, SCM slot_name) GIG_PURE;
GIG_API SCM scm_class_get_slots(SCM class) GIG_PURE;
GIG_API SCM scm_class_get_direct_slots(SCM class) GIG_PURE;
GIG_API SCM scm_class_get_all_slots(SCM class) GIG_PURE;

// Class hierarchy operations
GIG_API bool scm_class_is_subclass_of(SCM class, SCM potential_super) GIG_PURE;
GIG_API SCM scm_class_get_precedence_list(SCM class) GIG_PURE;
GIG_API SCM scm_class_get_direct_supers(SCM class) GIG_PURE;
GIG_API SCM scm_class_get_direct_subclasses(SCM class) GIG_PURE;

// Class introspection
GIG_API const char *scm_class_get_name_string(SCM class) GIG_PURE;
GIG_API size_t scm_class_get_instance_size(SCM class) GIG_PURE;
GIG_API bool scm_class_is_abstract(SCM class) GIG_PURE;
GIG_API bool scm_class_is_metaclass(SCM class) GIG_PURE;

// ============================================================================
// Enhanced Instance Operations (original functions modernized)
// ============================================================================

// Instance creation
GIG_API SCM scm_make_with_value(SCM type, SCM value) GIG_MALLOC;
GIG_API SCM scm_make_instance(SCM class, ...) GIG_MALLOC;
GIG_API SCM scm_make_instance_from_alist(SCM class, SCM init_alist) GIG_MALLOC;

// Slot access (original functions enhanced)
GIG_API SCM scm_get_value_slot(SCM instance) GIG_PURE;
GIG_API bool scm_instance_has_slot(SCM instance, SCM slot_name) GIG_PURE;
GIG_API SCM scm_slot_ref_safe(SCM instance, SCM slot_name) GIG_PURE;
GIG_API bool scm_slot_set_safe(SCM instance, SCM slot_name, SCM value);
GIG_API bool scm_slot_is_bound(SCM instance, SCM slot_name) GIG_PURE;
GIG_API bool scm_slot_exists(SCM instance, SCM slot_name) GIG_PURE;

// Instance introspection
GIG_API SCM scm_instance_get_class(SCM instance) GIG_PURE;
GIG_API bool scm_instance_is_a(SCM instance, SCM class) GIG_PURE;
GIG_API SCM scm_instance_get_slots(SCM instance) GIG_PURE;
GIG_API size_t scm_instance_get_slot_count(SCM instance) GIG_PURE;

// ============================================================================
// Enhanced Class Slot Operations (original functions modernized)
// ============================================================================

// Class slot access (enhanced from original)
GIG_API SCM scm_get_class_ref_slot(SCM cls) GIG_PURE;
GIG_API SCM scm_get_class_size_slot(SCM cls) GIG_PURE;
GIG_API SCM scm_get_class_unref_slot(SCM cls) GIG_PURE;

GIG_API bool scm_set_class_ref_slot(SCM cls, SCM func);
GIG_API bool scm_set_class_size_slot(SCM cls, SCM func);
GIG_API bool scm_set_class_unref_slot(SCM cls, SCM func);
GIG_API bool scm_set_class_obarray_slot(SCM cls, SCM obarray);

// Enhanced class slot operations
GIG_API SCM scm_class_slot_ref_safe(SCM cls, SCM slot) GIG_PURE;
GIG_API bool scm_class_slot_set_safe(SCM cls, SCM slot, SCM value);
GIG_API bool scm_class_slot_is_bound(SCM cls, SCM slot) GIG_PURE;

// ============================================================================
// Enhanced Generic Function and Method System (original functions modernized)
// ============================================================================

// Generic function creation and management
GIG_API SCM scm_make_generic(const char *name) GIG_MALLOC;
GIG_API SCM scm_ensure_generic(SCM proc, const char *name) GIG_MALLOC;
// GIG_API bool scm_is_generic(SCM obj) GIG_PURE;
// GIG_API bool scm_is_method(SCM obj) GIG_PURE;

// Method creation (original function enhanced)
GIG_API SCM scm_make_method(SCM specializers, SCM formals, SCM proc) GIG_MALLOC;
GIG_API SCM scm_make_method_simple(SCM *specializers, size_t spec_count, SCM proc) GIG_MALLOC;

// Method dispatch and management (original function enhanced)
GIG_API bool scm_add_method(SCM target, SCM method);
GIG_API bool scm_remove_method(SCM generic, SCM method);
GIG_API SCM scm_find_method(SCM generic, SCM *args, size_t arg_count) GIG_PURE;
GIG_API SCM scm_compute_applicable_methods(SCM generic, SCM args) GIG_PURE;

// Enhanced method operations
GIG_API SCM scm_define_methods_from_procedure(const char *public_name, SCM proc, int opt, 
                                             SCM formals, SCM specializers);
GIG_API SCM scm_define_method(const char *name, SCM specializers, SCM formals, SCM proc);
GIG_API SCM scm_define_method_simple(const char *name, SCM *specializers, size_t spec_count, SCM proc);

// Generic function introspection
GIG_API SCM scm_generic_get_methods(SCM generic) GIG_PURE;
GIG_API size_t scm_generic_get_method_count(SCM generic) GIG_PURE;
GIG_API const char *scm_generic_get_name_string(SCM generic) GIG_PURE;

// Method introspection
GIG_API SCM scm_method_get_specializers(SCM method) GIG_PURE;
GIG_API SCM scm_method_get_formals(SCM method) GIG_PURE;
GIG_API SCM scm_method_get_procedure(SCM method) GIG_PURE;
GIG_API SCM scm_method_get_generic(SCM method) GIG_PURE;

// ============================================================================
// Enhanced Accessor System
// ============================================================================

// Accessor creation (original function enhanced)
GIG_API SCM scm_ensure_accessor_with_name(SCM proc, SCM name) GIG_MALLOC;
GIG_API SCM scm_make_accessor(const char *name) GIG_MALLOC;
GIG_API SCM scm_make_slot_accessor(SCM class, SCM slot_name) GIG_MALLOC;

// Setter operations (original function enhanced)
GIG_API bool scm_set_procedure_slot(SCM instance, SCM proc);
GIG_API SCM scm_procedure_with_setter(SCM getter, SCM setter) GIG_MALLOC;
GIG_API bool scm_is_procedure_with_setter(SCM obj) GIG_PURE;
GIG_API SCM scm_get_procedure_setter(SCM proc) GIG_PURE;

// ============================================================================
// Enhanced Type System Integration
// ============================================================================

// Type checking (original function enhanced)
GIG_API bool scm_is_unknown_class(SCM type) GIG_PURE;
GIG_API bool scm_is_instance(SCM obj) GIG_PURE;
GIG_API bool scm_is_class(SCM obj) GIG_PURE;

// Type conversion and coercion
GIG_API SCM scm_class_of(SCM obj) GIG_PURE;
GIG_API SCM scm_primitive_class_of(SCM obj) GIG_PURE;
GIG_API bool scm_object_is_instance_of(SCM obj, SCM class) GIG_PURE;

// Enhanced type operations
GIG_API SCM scm_find_common_superclass(SCM class1, SCM class2) GIG_PURE;
GIG_API SCM scm_compute_class_precedence_list(SCM class) GIG_PURE;
GIG_API bool scm_classes_compatible_p(SCM class1, SCM class2) GIG_PURE;

// ============================================================================
// Advanced GOOPS Features
// ============================================================================

// Metaclass support
GIG_API SCM scm_make_metaclass(const char *name, SCM supers, SCM slots) GIG_MALLOC;
GIG_API bool scm_is_metaclass(SCM obj) GIG_PURE;
GIG_API SCM scm_class_get_metaclass(SCM class) GIG_PURE;

// Multiple inheritance utilities
GIG_API SCM scm_linearize_class_hierarchy(SCM class) GIG_PURE;
GIG_API bool scm_has_multiple_inheritance(SCM class) GIG_PURE;
GIG_API SCM scm_resolve_method_conflict(SCM methods) GIG_PURE;

// Protocol and interface support
GIG_API SCM scm_define_protocol(const char *name, SCM methods) GIG_MALLOC;
GIG_API bool scm_class_implements_protocol(SCM class, SCM protocol) GIG_PURE;
GIG_API SCM scm_get_protocol_methods(SCM protocol) GIG_PURE;

// Dynamic class modification
GIG_API bool scm_class_add_slot(SCM class, SCM slot_spec);
GIG_API bool scm_class_remove_slot(SCM class, SCM slot_name);
GIG_API bool scm_class_add_superclass(SCM class, SCM super);
GIG_API bool scm_class_remove_superclass(SCM class, SCM super);

// ============================================================================
// C Integration Helpers
// ============================================================================

// C type to GOOPS class mapping
typedef struct {
    const char *c_type_name;
    size_t size;
    SCM goops_class;
    void (*destructor)(void *);
    void *(*constructor)(void);
} CTypeInfo;

GIG_API bool scm_register_c_type(const CTypeInfo *type_info);
GIG_API SCM scm_get_c_type_class(const char *c_type_name) GIG_PURE;
GIG_API SCM scm_wrap_c_object(void *ptr, const char *c_type_name) GIG_MALLOC;
GIG_API void *scm_unwrap_c_object(SCM obj, const char *c_type_name) GIG_PURE;

// Foreign function interface helpers
GIG_API SCM scm_make_foreign_class(const char *name, size_t size,
                                  void (*finalizer)(void *)) GIG_MALLOC;
GIG_API SCM scm_make_foreign_instance(SCM foreign_class, void *ptr) GIG_MALLOC;
GIG_API void *scm_foreign_instance_get_ptr(SCM instance) GIG_PURE;

// ============================================================================
// Performance and Optimization
// ============================================================================

// Method cache management
GIG_API void scm_clear_method_cache(void);
GIG_API void scm_optimize_generic(SCM generic);
GIG_API size_t scm_get_method_cache_size(void) GIG_PURE;

// Class compilation and optimization
GIG_API bool scm_compile_class(SCM class);
GIG_API bool scm_is_class_compiled(SCM class) GIG_PURE;
GIG_API void scm_precompute_class_hierarchy(SCM class);

// ============================================================================
// Debugging and Introspection
// ============================================================================

// Class debugging
GIG_API void scm_print_class_info(SCM class, FILE *stream);
GIG_API void scm_print_instance_info(SCM instance, FILE *stream);
GIG_API void scm_print_method_info(SCM method, FILE *stream);

// Method dispatch debugging
GIG_API void scm_trace_method_dispatch(bool enable);
GIG_API bool scm_is_method_dispatch_traced(void) GIG_PURE;
GIG_API void scm_print_method_dispatch_trace(SCM generic, SCM args, FILE *stream);

// System introspection
GIG_API SCM scm_get_all_classes(void) GIG_PURE;
GIG_API SCM scm_get_all_generics(void) GIG_PURE;
GIG_API size_t scm_get_class_count(void) GIG_PURE;
GIG_API size_t scm_get_generic_count(void) GIG_PURE;

// ============================================================================
// Error Handling and Validation
// ============================================================================

// Error types specific to GOOPS
typedef enum {
    OOP_ERROR_NONE = 0,
    OOP_ERROR_NOT_INITIALIZED,
    OOP_ERROR_INVALID_CLASS,
    OOP_ERROR_INVALID_INSTANCE,
    OOP_ERROR_SLOT_NOT_FOUND,
    OOP_ERROR_SLOT_UNBOUND,
    OOP_ERROR_METHOD_NOT_FOUND,
    OOP_ERROR_DISPATCH_FAILED,
    OOP_ERROR_INHERITANCE_CONFLICT,
    OOP_ERROR_METACLASS_CONFLICT
} OopError;

GIG_API OopError oop_get_last_error(void);
GIG_API const char *oop_error_string(OopError error) GIG_PURE;
GIG_API void oop_clear_error(void);

// Validation functions
GIG_API bool oop_validate_class(SCM class);
GIG_API bool oop_validate_instance(SCM instance);
GIG_API bool oop_validate_method(SCM method);
GIG_API bool oop_validate_generic(SCM generic);

// ============================================================================
// Utility Macros
// ============================================================================

// Type checking macros
#define OOP_ASSERT_CLASS(obj) \
    do { if (!scm_is_class(obj)) { \
        scm_wrong_type_arg(__func__, 1, obj); \
    } } while(0)

#define OOP_ASSERT_INSTANCE(obj) \
    do { if (!scm_is_instance(obj)) { \
        scm_wrong_type_arg(__func__, 1, obj); \
    } } while(0)

#define OOP_ASSERT_GENERIC(obj) \
    do { if (!scm_is_generic(obj)) { \
        scm_wrong_type_arg(__func__, 1, obj); \
    } } while(0)

// Safe slot access macros
#define OOP_SLOT_REF(instance, slot) \
    scm_slot_ref_safe((instance), scm_from_utf8_symbol(slot))

#define OOP_SLOT_SET(instance, slot, value) \
    scm_slot_set_safe((instance), scm_from_utf8_symbol(slot), (value))

// Class definition macros
#define OOP_DEFINE_CLASS(name, super) \
    scm_define_class(name, scm_list_1(super), SCM_EOL)

#define OOP_DEFINE_CLASS_WITH_SLOTS(name, super, ...) \
    scm_define_class(name, scm_list_1(super), \
                    scm_list_n(__VA_ARGS__, SCM_UNDEFINED))

// Method definition macros
#define OOP_DEFINE_METHOD_0(name, proc) \
    scm_define_method_simple(name, NULL, 0, proc)

#define OOP_DEFINE_METHOD_1(name, spec1, proc) \
    do { \
        SCM _specs[] = {spec1}; \
        scm_define_method_simple(name, _specs, 1, proc); \
    } while(0)

#define OOP_DEFINE_METHOD_2(name, spec1, spec2, proc) \
    do { \
        SCM _specs[] = {spec1, spec2}; \
        scm_define_method_simple(name, _specs, 2, proc); \
    } while(0)

// ============================================================================
// Legacy Compatibility (deprecated but maintained)
// ============================================================================

// Deprecated function aliases for backward compatibility
GIG_DEPRECATED_API SCM scm_add_method_legacy(SCM target, SCM method);
GIG_DEPRECATED_API _Bool scm_is_unknown_class_legacy(SCM type);

#define scm_add_method_legacy(target, method) \
    (scm_add_method(target, method) ? (target) : SCM_BOOL_F)

#define scm_is_unknown_class_legacy(type) \
    scm_is_unknown_class(type)

#ifdef __cplusplus
}
#endif

#endif // CORE_OOP_H
