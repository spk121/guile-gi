// Copyright (C) 2025 Michael L. Gran
// Modernized GIRepository interface with enhanced core library integration

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

#ifndef GIG_GIREPOSITORY_H
#define GIG_GIREPOSITORY_H

#include <libguile.h>
#include <girepository.h>
#include <stdint.h>
#include <stdbool.h>
#include "visibility.h"
#include "scheme.h"
#include "oop.h"
#include "logging.h"
#include "mem.h"
#include "keyval.h"
#include "slist.h"

#ifdef __cplusplus
extern "C" {
#endif

// ============================================================================
// Type System and Caching
// ============================================================================

// Forward declarations
typedef struct gig_repository GigRepository;
typedef struct gig_type_cache GigTypeCache;
typedef struct gig_info_wrapper GigInfoWrapper;

// Enhanced GIBaseInfo wrapper with metadata
typedef struct gig_info_wrapper {
    GIBaseInfo *info;
    GIInfoType info_type;
    char *namespace_name;
    char *name;
    SCM scm_class;          // Associated GOOPS class
    bool is_cached;
    uint32_t ref_count;
    time_t cache_time;
} GigInfoWrapper;

// Repository state management
typedef struct gig_repository {
    HashMap *namespace_cache;     // namespace -> info list
    HashMap *type_cache;         // type name -> GigInfoWrapper
    HashMap *gtype_cache;        // GType -> GigInfoWrapper
    StrMap *symbol_cache;        // C symbol -> info
    SList *loaded_namespaces;
    bool auto_load_dependencies;
    size_t cache_max_size;
} GigRepository;

// ============================================================================
// Initialization and Management
// ============================================================================

GIG_API void gig_init_girepository(void);
GIG_API void gig_cleanup_girepository(void);
GIG_API GigRepository *gig_get_default_repository(void) GIG_PURE;

// Configuration
GIG_API void gig_repository_set_auto_load_dependencies(bool enable);
GIG_API void gig_repository_set_cache_max_size(size_t max_size);
GIG_API void gig_repository_clear_cache(void);
GIG_API void gig_repository_get_stats(size_t *cached_types, size_t *memory_used);

// ============================================================================
// Enhanced GIBaseInfo Management (using your core library)
// ============================================================================

// GIBaseInfo foreign object type
extern SCM gig_gibaseinfo_type;

// Creation and conversion with enhanced memory management
GIG_API SCM gig_gibaseinfo_from_c(GIBaseInfo *info) GIG_MALLOC;
GIG_API GIBaseInfo *gig_gibaseinfo_to_c(SCM scm_info) GIG_PURE;
GIG_API bool gig_is_gibaseinfo(SCM obj) GIG_PURE;

// Enhanced wrapper management
GIG_API GigInfoWrapper *gig_info_wrapper_new(GIBaseInfo *info) GIG_MALLOC;
GIG_API void gig_info_wrapper_free(GigInfoWrapper *wrapper);
GIG_API GigInfoWrapper *gig_info_wrapper_ref(GigInfoWrapper *wrapper);
GIG_API void gig_info_wrapper_unref(GigInfoWrapper *wrapper);

// ============================================================================
// GOOPS Integration (using your oop.h)
// ============================================================================

// Base classes for GI types
extern SCM gig_gibaseinfo_class;
extern SCM gig_callable_info_class;
extern SCM gig_function_info_class;
extern SCM gig_object_info_class;
extern SCM gig_interface_info_class;
extern SCM gig_struct_info_class;
extern SCM gig_enum_info_class;
extern SCM gig_union_info_class;
extern SCM gig_constant_info_class;
extern SCM gig_field_info_class;
extern SCM gig_property_info_class;
extern SCM gig_arg_info_class;
extern SCM gig_type_info_class;

// GOOPS class creation and management
GIG_API void gig_init_gibaseinfo_classes(void);
GIG_API SCM gig_get_class_for_info_type(GIInfoType info_type) GIG_PURE;
GIG_API SCM gig_create_info_instance(GIBaseInfo *info) GIG_MALLOC;

// ============================================================================
// Repository Operations (enhanced with caching)
// ============================================================================

// Namespace management
GIG_API SCM gig_irepository_get_loaded_namespaces(void) GIG_PURE;
GIG_API SCM gig_irepository_get_dependencies(SCM namespace) GIG_PURE;
GIG_API SCM gig_irepository_get_immediate_dependencies(SCM namespace) GIG_PURE;
GIG_API SCM gig_irepository_require(SCM namespace, SCM version, SCM flags);
GIG_API SCM gig_irepository_require_private(SCM typelib_dir, SCM namespace, 
                                           SCM version, SCM flags);

// Enhanced info retrieval with caching
GIG_API SCM gig_irepository_get_info(SCM namespace, SCM index) GIG_PURE;
GIG_API SCM gig_irepository_get_n_infos(SCM namespace) GIG_PURE;
GIG_API SCM gig_irepository_find_by_name(SCM namespace, SCM name) GIG_PURE;
GIG_API SCM gig_irepository_find_by_gtype(SCM gtype) GIG_PURE;
GIG_API SCM gig_irepository_find_by_error_domain(SCM error_domain) GIG_PURE;

// Bulk operations
GIG_API SCM gig_irepository_get_all_infos(SCM namespace) GIG_MALLOC;
GIG_API SCM gig_irepository_get_infos_by_type(SCM namespace, SCM info_type) GIG_MALLOC;

// ============================================================================
// Enhanced String Operations (using your str.h)
// ============================================================================

// Safe string conversion with memory management
GIG_API char *gig_scm_to_utf8_string_safe(SCM str) GIG_MALLOC;
GIG_API SCM gig_utf8_string_to_scm_safe(const char *str) GIG_MALLOC;
GIG_API SCM gig_c_string_list_to_scm(char **strings) GIG_MALLOC;

// ============================================================================
// Error Handling (using your logging.h)
// ============================================================================

typedef enum {
    GIG_ERROR_NONE = 0,
    GIG_ERROR_REPOSITORY_NOT_FOUND,
    GIG_ERROR_NAMESPACE_NOT_LOADED,
    GIG_ERROR_INFO_NOT_FOUND,
    GIG_ERROR_TYPE_MISMATCH,
    GIG_ERROR_CACHE_FULL,
    GIG_ERROR_INVALID_ARGUMENT
} GigError;

GIG_API GigError gig_get_last_error(void);
GIG_API const char *gig_error_string(GigError error) GIG_PURE;
GIG_API void gig_clear_error(void);

// Error throwing with enhanced messages
GIG_API void gig_throw_gerror(GError *error, const char *function) GIG_NORETURN;
GIG_API void gig_throw_error(GigError error, const char *format, ...) 
    GIG_PRINTF(2, 3) GIG_NORETURN;

// ============================================================================
// Memory Management Hooks
// ============================================================================

// Custom allocators for GI objects
GIG_API void gig_set_gi_allocator(const MemAllocator *allocator);
GIG_API void gig_track_gi_memory(bool enable);
GIG_API void gig_get_gi_memory_stats(MemStats *stats);

// ============================================================================
// Debugging and Introspection
// ============================================================================

GIG_API void gig_print_repository_stats(FILE *stream);
GIG_API void gig_print_info_details(SCM info, FILE *stream);
GIG_API bool gig_validate_repository_state(void);

// Debug logging
#define gig_debug_gi(format, ...) \
    gig_debug_load("GI: " format, ##__VA_ARGS__)

#define gig_warning_gi(format, ...) \
    gig_warning_load("GI: " format, ##__VA_ARGS__)

// ============================================================================
// Performance Optimization
// ============================================================================

// Batch operations for better performance
GIG_API SCM gig_irepository_preload_namespace(SCM namespace);
GIG_API void gig_irepository_warm_cache(SCM namespaces);
GIG_API void gig_irepository_optimize_cache(void);

// ============================================================================
// Constants and Enums (enhanced with better organization)
// ============================================================================

// Repository load flags
extern SCM gig_IREPOSITORY_LOAD_FLAG_LAZY;

// Version info
extern SCM gig_MAJOR_VERSION;
extern SCM gig_MINOR_VERSION; 
extern SCM gig_MICRO_VERSION;

// Type tags
extern SCM gig_TYPE_TAG_VOID;
extern SCM gig_TYPE_TAG_BOOLEAN;
extern SCM gig_TYPE_TAG_INT8;
extern SCM gig_TYPE_TAG_UINT8;
extern SCM gig_TYPE_TAG_INT16;
extern SCM gig_TYPE_TAG_UINT16;
extern SCM gig_TYPE_TAG_INT32;
extern SCM gig_TYPE_TAG_UINT32;
extern SCM gig_TYPE_TAG_INT64;
extern SCM gig_TYPE_TAG_UINT64;
extern SCM gig_TYPE_TAG_FLOAT;
extern SCM gig_TYPE_TAG_DOUBLE;
extern SCM gig_TYPE_TAG_GTYPE;
extern SCM gig_TYPE_TAG_UTF8;
extern SCM gig_TYPE_TAG_FILENAME;
extern SCM gig_TYPE_TAG_ARRAY;
extern SCM gig_TYPE_TAG_INTERFACE;
extern SCM gig_TYPE_TAG_GLIST;
extern SCM gig_TYPE_TAG_GSLIST;
extern SCM gig_TYPE_TAG_GHASH;
extern SCM gig_TYPE_TAG_ERROR;
extern SCM gig_TYPE_TAG_UNICHAR;

// Info types
extern SCM gig_INFO_TYPE_INVALID;
extern SCM gig_INFO_TYPE_FUNCTION;
extern SCM gig_INFO_TYPE_CALLBACK;
extern SCM gig_INFO_TYPE_STRUCT;
extern SCM gig_INFO_TYPE_BOXED;
extern SCM gig_INFO_TYPE_ENUM;
extern SCM gig_INFO_TYPE_FLAGS;
extern SCM gig_INFO_TYPE_OBJECT;
extern SCM gig_INFO_TYPE_INTERFACE;
extern SCM gig_INFO_TYPE_CONSTANT;
extern SCM gig_INFO_TYPE_UNION;
extern SCM gig_INFO_TYPE_VALUE;
extern SCM gig_INFO_TYPE_SIGNAL;
extern SCM gig_INFO_TYPE_VFUNC;
extern SCM gig_INFO_TYPE_PROPERTY;
extern SCM gig_INFO_TYPE_FIELD;
extern SCM gig_INFO_TYPE_ARG;
extern SCM gig_INFO_TYPE_TYPE;
extern SCM gig_INFO_TYPE_UNRESOLVED;

// Function flags
extern SCM gig_FUNCTION_IS_METHOD;
extern SCM gig_FUNCTION_IS_CONSTRUCTOR;
extern SCM gig_FUNCTION_IS_GETTER;
extern SCM gig_FUNCTION_IS_SETTER;
extern SCM gig_FUNCTION_WRAPS_VFUNC;
extern SCM gig_FUNCTION_THROWS;

// Transfer modes
extern SCM gig_TRANSFER_NOTHING;
extern SCM gig_TRANSFER_CONTAINER;
extern SCM gig_TRANSFER_EVERYTHING;

// Argument directions
extern SCM gig_DIRECTION_IN;
extern SCM gig_DIRECTION_OUT;
extern SCM gig_DIRECTION_INOUT;

// ============================================================================
// Utility Macros
// ============================================================================

// Safe GI info access
#define GIG_CHECK_GIBASEINFO(obj, func) \
    do { \
        if (!gig_is_gibaseinfo(obj)) { \
            scm_wrong_type_arg(func, 1, obj); \
        } \
    } while(0)

// Memory-safe string operations
#define GIG_SCM_TO_UTF8_STRING(str) \
    gig_scm_to_utf8_string_safe(str)

#define GIG_UTF8_STRING_TO_SCM(str) \
    gig_utf8_string_to_scm_safe(str)

// Error handling macros
#define GIG_TRY_BEGIN \
    do { \
        GError *_gig_error = NULL;

#define GIG_TRY_END(function) \
        if (_gig_error != NULL) { \
            gig_throw_gerror(_gig_error, function); \
        } \
    } while(0)

// Cache management helpers
#define GIG_CACHE_KEY(namespace, name) \
    str_printf("%s::%s", (namespace), (name))

#ifdef __cplusplus
}
#endif

#endif // GIG_GIREPOSITORY_H
