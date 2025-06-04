// Copyright (C) 2025 Michael L. Gran
// Modernized GIRepository implementation using enhanced core library

#include "gig_girepository.h"
#include <time.h>
#include "str.h"

// ============================================================================
// Global State
// ============================================================================

static GigRepository *default_repository = NULL;
static GigError last_error = GIG_ERROR_NONE;
static bool initialized = false;

// GOOPS classes for GI types
SCM gig_gibaseinfo_class = SCM_BOOL_F;
SCM gig_callable_info_class = SCM_BOOL_F;
SCM gig_function_info_class = SCM_BOOL_F;
SCM gig_object_info_class = SCM_BOOL_F;
SCM gig_interface_info_class = SCM_BOOL_F;
SCM gig_struct_info_class = SCM_BOOL_F;
SCM gig_enum_info_class = SCM_BOOL_F;
SCM gig_union_info_class = SCM_BOOL_F;
SCM gig_constant_info_class = SCM_BOOL_F;
SCM gig_field_info_class = SCM_BOOL_F;
SCM gig_property_info_class = SCM_BOOL_F;
SCM gig_arg_info_class = SCM_BOOL_F;
SCM gig_type_info_class = SCM_BOOL_F;

// Foreign object type for GIBaseInfo
SCM gig_gibaseinfo_type = SCM_BOOL_F;

// Constants (defined later in init function)
SCM gig_MAJOR_VERSION = SCM_BOOL_F;
SCM gig_MINOR_VERSION = SCM_BOOL_F;
SCM gig_MICRO_VERSION = SCM_BOOL_F;

// ... (other constants will be defined similarly)

// ============================================================================
// Error Handling
// ============================================================================

static const char *error_messages[] = {
    [GIG_ERROR_NONE] = "No error",
    [GIG_ERROR_REPOSITORY_NOT_FOUND] = "Repository not found",
    [GIG_ERROR_NAMESPACE_NOT_LOADED] = "Namespace not loaded",
    [GIG_ERROR_INFO_NOT_FOUND] = "Info not found",
    [GIG_ERROR_TYPE_MISMATCH] = "Type mismatch",
    [GIG_ERROR_CACHE_FULL] = "Cache is full",
    [GIG_ERROR_INVALID_ARGUMENT] = "Invalid argument"
};

GIG_API GigError 
gig_get_last_error(void)
{
    return last_error;
}

GIG_API const char *
gig_error_string(GigError error)
{
    if (error >= 0 && error < sizeof(error_messages)/sizeof(error_messages[0])) {
        return error_messages[error];
    }
    return "Unknown error";
}

GIG_API void 
gig_clear_error(void)
{
    last_error = GIG_ERROR_NONE;
}

GIG_API void 
gig_throw_gerror(GError *error, const char *function)
{
    gig_error_load("GError in %s: %s", function, error->message);
    
    SCM err_msg = scm_from_utf8_string(error->message);
    g_error_free(error);
    
    scm_misc_error(function, "~A", scm_list_1(err_msg));
}

GIG_API void 
gig_throw_error(GigError error, const char *format, ...)
{
    last_error = error;
    
    va_list args;
    va_start(args, format);
    char *message = str_vprintf(format, args);
    va_end(args);
    
    gig_error_load("GIG Error: %s", message);
    
    SCM err_msg = scm_from_utf8_string(message);
    xfree(message);
    
    scm_misc_error("gig-error", "~A", scm_list_1(err_msg));
}

// ============================================================================
// Memory Management
// ============================================================================

static void
gig_info_wrapper_finalizer(GigInfoWrapper *wrapper)
{
    if (wrapper) {
        if (wrapper->info) {
            g_base_info_unref(wrapper->info);
        }
        xfree(wrapper->namespace_name);
        xfree(wrapper->name);
        xfree(wrapper);
    }
}

GIG_API GigInfoWrapper *
gig_info_wrapper_new(GIBaseInfo *info)
{
    gig_return_val_if_fail(info != NULL, NULL);
    
    GigInfoWrapper *wrapper = XNEW0(GigInfoWrapper);
    wrapper->info = g_base_info_ref(info);
    wrapper->info_type = g_base_info_get_type(info);
    wrapper->namespace_name = xstrdup(g_base_info_get_namespace(info));
    wrapper->name = xstrdup(g_base_info_get_name(info));
    wrapper->ref_count = 1;
    wrapper->cache_time = time(NULL);
    wrapper->scm_class = SCM_BOOL_F;
    wrapper->is_cached = false;
    
    return wrapper;
}

GIG_API void 
gig_info_wrapper_free(GigInfoWrapper *wrapper)
{
    if (wrapper && --wrapper->ref_count == 0) {
        gig_info_wrapper_finalizer(wrapper);
    }
}

GIG_API GigInfoWrapper *
gig_info_wrapper_ref(GigInfoWrapper *wrapper)
{
    if (wrapper) {
        wrapper->ref_count++;
    }
    return wrapper;
}

GIG_API void 
gig_info_wrapper_unref(GigInfoWrapper *wrapper)
{
    gig_info_wrapper_free(wrapper);
}

// ============================================================================
// Foreign Object Management
// ============================================================================

static void
gc_free_gibaseinfo(SCM scm_info)
{
    GigInfoWrapper *wrapper = scm_foreign_object_ref(scm_info, 0);
    if (wrapper) {
        gig_info_wrapper_unref(wrapper);
        scm_foreign_object_set_x(scm_info, 0, NULL);
    }
}

static SCM
gibaseinfo_equalp(SCM x, SCM y)
{
    GigInfoWrapper *wrapper1 = scm_foreign_object_ref(x, 0);
    GigInfoWrapper *wrapper2 = scm_foreign_object_ref(y, 0);
    
    if (!wrapper1 || !wrapper2) {
        return SCM_BOOL_F;
    }
    
    return scm_from_bool(g_base_info_equal(wrapper1->info, wrapper2->info));
}

GIG_API SCM 
gig_gibaseinfo_from_c(GIBaseInfo *info)
{
    gig_return_val_if_fail(info != NULL, SCM_BOOL_F);
    
    GigInfoWrapper *wrapper = gig_info_wrapper_new(info);
    return scm_make_foreign_object_1(gig_gibaseinfo_type, wrapper);
}

GIG_API GIBaseInfo *
gig_gibaseinfo_to_c(SCM scm_info)
{
    gig_return_val_if_fail(gig_is_gibaseinfo(scm_info), NULL);
    
    GigInfoWrapper *wrapper = scm_foreign_object_ref(scm_info, 0);
    return wrapper ? wrapper->info : NULL;
}

GIG_API bool 
gig_is_gibaseinfo(SCM obj)
{
    return SCM_IS_A_P(obj, gig_gibaseinfo_type) && 
           scm_foreign_object_ref(obj, 0) != NULL;
}

// ============================================================================
// Repository Management
// ============================================================================

static GigRepository *
gig_repository_new(void)
{
    GigRepository *repo = XNEW0(GigRepository);
    
    // Initialize hash maps with appropriate configurations
    HashMapConfig string_config = hashmap_config_string_keys();
    repo->namespace_cache = hashmap_create_string_keys(sizeof(SList*), &string_config);
    repo->type_cache = hashmap_create_string_keys(sizeof(GigInfoWrapper*), &string_config);
    repo->gtype_cache = hashmap_create_int_keys(sizeof(GigInfoWrapper*), NULL);
    
    repo->symbol_cache = strmap_create();
    repo->loaded_namespaces = slist_create_simple();
    repo->auto_load_dependencies = true;
    repo->cache_max_size = 10000;
    
    return repo;
}

static void
gig_repository_destroy(GigRepository *repo)
{
    if (repo) {
        hashmap_destroy(repo->namespace_cache);
        hashmap_destroy(repo->type_cache);
        hashmap_destroy(repo->gtype_cache);
        strmap_destroy(repo->symbol_cache);
        slist_destroy(repo->loaded_namespaces);
        xfree(repo);
    }
}

GIG_API GigRepository *
gig_get_default_repository(void)
{
    if (!default_repository) {
        default_repository = gig_repository_new();
    }
    return default_repository;
}

// ============================================================================
// Enhanced String Operations
// ============================================================================

GIG_API char *
gig_scm_to_utf8_string_safe(SCM str)
{
    if (!scm_is_string(str)) {
        return NULL;
    }
    
    return scm_to_utf8_string(str);
}

GIG_API SCM 
gig_utf8_string_to_scm_safe(const char *str)
{
    if (!str) {
        return SCM_BOOL_F;
    }
    
    return scm_from_utf8_string(str);
}

GIG_API SCM 
gig_c_string_list_to_scm(char **strings)
{
    if (!strings) {
        return SCM_EOL;
    }
    
    SCM result = SCM_EOL;
    for (int i = 0; strings[i] != NULL; i++) {
        result = scm_cons(scm_from_utf8_string(strings[i]), result);
        xfree(strings[i]);
    }
    xfree(strings);
    
    return scm_reverse(result);
}

// ============================================================================
// GOOPS Integration
// ============================================================================

static SCM
make_info_class(const char *name, SCM parent)
{
    return scm_make_class_simple(parent, name);
}

GIG_API void 
gig_init_gibaseinfo_classes(void)
{
    // Create the base class hierarchy
    gig_gibaseinfo_class = make_info_class("<gi-base-info>", scm_get_object_class());
    
    // Callable hierarchy
    gig_callable_info_class = make_info_class("<gi-callable-info>", gig_gibaseinfo_class);
    gig_function_info_class = make_info_class("<gi-function-info>", gig_callable_info_class);
    
    // Type hierarchy
    gig_object_info_class = make_info_class("<gi-object-info>", gig_gibaseinfo_class);
    gig_interface_info_class = make_info_class("<gi-interface-info>", gig_gibaseinfo_class);
    gig_struct_info_class = make_info_class("<gi-struct-info>", gig_gibaseinfo_class);
    gig_enum_info_class = make_info_class("<gi-enum-info>", gig_gibaseinfo_class);
    gig_union_info_class = make_info_class("<gi-union-info>", gig_gibaseinfo_class);
    
    // Other info types
    gig_constant_info_class = make_info_class("<gi-constant-info>", gig_gibaseinfo_class);
    gig_field_info_class = make_info_class("<gi-field-info>", gig_gibaseinfo_class);
    gig_property_info_class = make_info_class("<gi-property-info>", gig_gibaseinfo_class);
    gig_arg_info_class = make_info_class("<gi-arg-info>", gig_gibaseinfo_class);
    gig_type_info_class = make_info_class("<gi-type-info>", gig_gibaseinfo_class);
    
    gig_debug_gi("Initialized GOOPS class hierarchy for GI types");
}

GIG_API SCM 
gig_get_class_for_info_type(GIInfoType info_type)
{
    switch (info_type) {
        case GI_INFO_TYPE_FUNCTION:
            return gig_function_info_class;
        case GI_INFO_TYPE_OBJECT:
            return gig_object_info_class;
        case GI_INFO_TYPE_INTERFACE:
            return gig_interface_info_class;
        case GI_INFO_TYPE_STRUCT:
        case GI_INFO_TYPE_BOXED:
            return gig_struct_info_class;
        case GI_INFO_TYPE_ENUM:
        case GI_INFO_TYPE_FLAGS:
            return gig_enum_info_class;
        case GI_INFO_TYPE_UNION:
            return gig_union_info_class;
        case GI_INFO_TYPE_CONSTANT:
            return gig_constant_info_class;
        case GI_INFO_TYPE_FIELD:
            return gig_field_info_class;
        case GI_INFO_TYPE_PROPERTY:
            return gig_property_info_class;
        case GI_INFO_TYPE_ARG:
            return gig_arg_info_class;
        case GI_INFO_TYPE_TYPE:
            return gig_type_info_class;
        default:
            return gig_gibaseinfo_class;
    }
}

GIG_API SCM 
gig_create_info_instance(GIBaseInfo *info)
{
    gig_return_val_if_fail(info != NULL, SCM_BOOL_F);
    
    GIInfoType info_type = g_base_info_get_type(info);
    SCM info_class = gig_get_class_for_info_type(info_type);
    SCM foreign_obj = gig_gibaseinfo_from_c(info);
    
    // Create GOOPS instance with the foreign object as the underlying data
    return scm_make_instance_from_alist(info_class, 
                                       scm_list_1(scm_cons(scm_from_utf8_symbol("ptr"), 
                                                          foreign_obj)));
}

// ============================================================================
// Repository Operations with Caching
// ============================================================================

GIG_API SCM 
gig_irepository_require(SCM namespace, SCM version, SCM flags)
{
    char *c_namespace = gig_scm_to_utf8_string_safe(namespace);
    char *c_version = scm_is_false(version) ? NULL : gig_scm_to_utf8_string_safe(version);
    int c_flags = scm_to_int(flags);
    GITypelib *typelib;
    
    GIG_TRY_BEGIN;
    
    typelib = g_irepository_require(NULL, c_namespace, c_version, 
				    c_flags, &_gig_error);
    
    GIG_TRY_END("irepository-require");
    
    // Add to loaded namespaces cache
    GigRepository *repo = gig_get_default_repository();
    if (!slist_contains(repo->loaded_namespaces, c_namespace)) {
        slist_push_back(repo->loaded_namespaces, xstrdup(c_namespace));
        gig_debug_gi("Loaded namespace: %s", c_namespace);
    }
    
    xfree(c_namespace);
    xfree(c_version);
    
    return typelib ? SCM_BOOL_T : SCM_BOOL_F;
}

GIG_API SCM 
gig_irepository_find_by_name(SCM namespace, SCM name)
{
    char *c_namespace = gig_scm_to_utf8_string_safe(namespace);
    char *c_name = gig_scm_to_utf8_string_safe(name);
    
    if (!c_namespace || !c_name) {
        xfree(c_namespace);
        xfree(c_name);
        return SCM_BOOL_F;
    }
    
    // Check cache first
    GigRepository *repo = gig_get_default_repository();
    char *cache_key = str_printf("%s::%s", c_namespace, c_name);
    GigInfoWrapper *cached_wrapper = NULL;
    
    if (hashmap_get_string(repo->type_cache, cache_key, &cached_wrapper)) {
        xfree(cache_key);
        xfree(c_namespace);
        xfree(c_name);
        return gig_create_info_instance(cached_wrapper->info);
    }
    
    // Not in cache, query GIRepository
    GIBaseInfo *info = g_irepository_find_by_name(NULL, c_namespace, c_name);
    
    if (info) {
        // Add to cache
        GigInfoWrapper *wrapper = gig_info_wrapper_new(info);
        wrapper->is_cached = true;
        hashmap_insert_string(repo->type_cache, cache_key, &wrapper);
        
        gig_debug_gi("Cached info for %s::%s", c_namespace, c_name);
        
        SCM result = gig_create_info_instance(info);
        
        xfree(cache_key);
        xfree(c_namespace);
        xfree(c_name);
        return result;
    }
    
    xfree(cache_key);
    xfree(c_namespace);
    xfree(c_name);
    return SCM_BOOL_F;
}

GIG_API SCM 
gig_irepository_get_loaded_namespaces(void)
{
    char **c_namespaces = g_irepository_get_loaded_namespaces(NULL);
    return gig_c_string_list_to_scm(c_namespaces);
}

GIG_API SCM 
gig_irepository_get_dependencies(SCM namespace)
{
    char *c_namespace = gig_scm_to_utf8_string_safe(namespace);
    if (!c_namespace) {
        return SCM_EOL;
    }
    
    char **c_dependencies = g_irepository_get_dependencies(NULL, c_namespace);
    xfree(c_namespace);
    
    return gig_c_string_list_to_scm(c_dependencies);
}

GIG_API SCM 
gig_irepository_get_info(SCM namespace, SCM index)
{
    char *c_namespace = gig_scm_to_utf8_string_safe(namespace);
    int c_index = scm_to_int(index);
    
    if (!c_namespace) {
        return SCM_BOOL_F;
    }
    
    GIBaseInfo *info = g_irepository_get_info(NULL, c_namespace, c_index);
    xfree(c_namespace);
    
    if (info) {
        return gig_create_info_instance(info);
    }
    
    return SCM_BOOL_F;
}

GIG_API SCM 
gig_irepository_get_all_infos(SCM namespace)
{
    char *c_namespace = gig_scm_to_utf8_string_safe(namespace);
    if (!c_namespace) {
        return SCM_EOL;
    }
    
    int n_infos = g_irepository_get_n_infos(NULL, c_namespace);
    SCM result = SCM_EOL;
    
    for (int i = 0; i < n_infos; i++) {
        GIBaseInfo *info = g_irepository_get_info(NULL, c_namespace, i);
        if (info) {
            result = scm_cons(gig_create_info_instance(info), result);
        }
    }
    
    xfree(c_namespace);
    return scm_reverse(result);
}

// ============================================================================
// Enhanced Info Operations
// ============================================================================

static SCM
gig_base_info_get_name(SCM scm_info)
{
    GIBaseInfo *info = gig_gibaseinfo_to_c(scm_info);
    gig_return_val_if_fail(info != NULL, SCM_BOOL_F);
    
    const char *name = g_base_info_get_name(info);
    return name ? gig_utf8_string_to_scm_safe(name) : SCM_BOOL_F;
}

static SCM
gig_base_info_get_namespace(SCM scm_info)
{
    GIBaseInfo *info = gig_gibaseinfo_to_c(scm_info);
    gig_return_val_if_fail(info != NULL, SCM_BOOL_F);
    
    const char *namespace = g_base_info_get_namespace(info);
    return namespace ? gig_utf8_string_to_scm_safe(namespace) : SCM_BOOL_F;
}

static SCM
gig_base_info_get_type(SCM scm_info)
{
    GIBaseInfo *info = gig_gibaseinfo_to_c(scm_info);
    gig_return_val_if_fail(info != NULL, SCM_BOOL_F);
    
    GIInfoType info_type = g_base_info_get_type(info);
    return scm_from_int(info_type);
}

static SCM
gig_base_info_is_deprecated(SCM scm_info)
{
    GIBaseInfo *info = gig_gibaseinfo_to_c(scm_info);
    gig_return_val_if_fail(info != NULL, SCM_BOOL_F);
    
    return scm_from_bool(g_base_info_is_deprecated(info));
}

static SCM
gig_base_info_equal(SCM info1, SCM info2)
{
    GIBaseInfo *c_info1 = gig_gibaseinfo_to_c(info1);
    GIBaseInfo *c_info2 = gig_gibaseinfo_to_c(info2);
    
    if (!c_info1 || !c_info2) {
        return SCM_BOOL_F;
    }
    
    return scm_from_bool(g_base_info_equal(c_info1, c_info2));
}

static SCM
gig_base_info_get_attributes(SCM scm_info)
{
    GIBaseInfo *info = gig_gibaseinfo_to_c(scm_info);
    gig_return_val_if_fail(info != NULL, SCM_EOL);
    
    GIAttributeIter iter = { 0, };
    char *name, *value;
    SCM result = SCM_EOL;
    
    while (g_base_info_iterate_attributes(info, &iter, &name, &value)) {
        SCM pair = scm_cons(gig_utf8_string_to_scm_safe(name),
                           gig_utf8_string_to_scm_safe(value));
        result = scm_cons(pair, result);
    }
    
    return scm_reverse(result);
}

// ============================================================================
// Configuration and Management
// ============================================================================

GIG_API void 
gig_repository_set_auto_load_dependencies(bool enable)
{
    GigRepository *repo = gig_get_default_repository();
    repo->auto_load_dependencies = enable;
    gig_debug_gi("Auto-load dependencies: %s", enable ? "enabled" : "disabled");
}

GIG_API void 
gig_repository_set_cache_max_size(size_t max_size)
{
    GigRepository *repo = gig_get_default_repository();
    repo->cache_max_size = max_size;
    gig_debug_gi("Cache max size set to: %zu", max_size);
}

GIG_API void 
gig_repository_clear_cache(void)
{
    GigRepository *repo = gig_get_default_repository();
    
    hashmap_clear(repo->namespace_cache);
    hashmap_clear(repo->type_cache);
    hashmap_clear(repo->gtype_cache);
    strmap_destroy(repo->symbol_cache);
    repo->symbol_cache = strmap_create();
    
    gig_debug_gi("Repository cache cleared");
}

GIG_API void 
gig_repository_get_stats(size_t *cached_types, size_t *memory_used)
{
    GigRepository *repo = gig_get_default_repository();
    
    if (cached_types) {
        *cached_types = hashmap_size(repo->type_cache);
    }
    
    if (memory_used) {
        HashMapStats stats;
        hashmap_get_stats(repo->type_cache, &stats);
        *memory_used = stats.memory_used;
    }
}

// ============================================================================
// Debugging and Validation
// ============================================================================

GIG_API void 
gig_print_repository_stats(FILE *stream)
{
    GigRepository *repo = gig_get_default_repository();
    
    fprintf(stream, "=== GIG Repository Statistics ===\n");
    fprintf(stream, "Loaded namespaces: %zu\n", slist_size(repo->loaded_namespaces));
    fprintf(stream, "Cached types: %zu\n", hashmap_size(repo->type_cache));
    fprintf(stream, "Cached GTypes: %zu\n", hashmap_size(repo->gtype_cache));
    fprintf(stream, "Symbol cache: %zu\n", strmap_size(repo->symbol_cache));
    fprintf(stream, "Auto-load dependencies: %s\n", 
            repo->auto_load_dependencies ? "yes" : "no");
    fprintf(stream, "Cache max size: %zu\n", repo->cache_max_size);
    
    HashMapStats stats;
    hashmap_get_stats(repo->type_cache, &stats);
    fprintf(stream, "Type cache memory: %zu bytes\n", stats.memory_used);
    fprintf(stream, "Type cache load factor: %.2f\n", stats.load_factor);
    fprintf(stream, "Type cache collisions: %zu\n", stats.collisions);
}

GIG_API void 
gig_print_info_details(SCM info, FILE *stream)
{
    GIBaseInfo *c_info = gig_gibaseinfo_to_c(info);
    if (!c_info) {
        fprintf(stream, "Invalid GIBaseInfo\n");
        return;
    }
    
    fprintf(stream, "=== GI Info Details ===\n");
    fprintf(stream, "Name: %s\n", g_base_info_get_name(c_info));
    fprintf(stream, "Namespace: %s\n", g_base_info_get_namespace(c_info));
    fprintf(stream, "Type: %s\n", g_info_type_to_string(g_base_info_get_type(c_info)));
    fprintf(stream, "Deprecated: %s\n", g_base_info_is_deprecated(c_info) ? "yes" : "no");
}

// ============================================================================
// Constants Definition Function
// ============================================================================

static SCM
define_constant(const char *name, int value)
{
    return scm_permanent_object(scm_c_define(name, scm_from_int(value)));
}

// ============================================================================
// Initialization
// ============================================================================

GIG_API void 
gig_init_girepository(void)
{
    if (initialized) {
        return;
    }
    
    gig_debug_gi("Initializing GIRepository integration");
    
    // Initialize GOOPS if not already done
    if (!oop_is_initialized()) {
        init_core_oop();
    }
    
    // Create foreign object type for GIBaseInfo
    gig_gibaseinfo_type = scm_make_foreign_object_type(
        scm_from_utf8_symbol("<gi-base-info>"),
        scm_list_1(scm_from_utf8_symbol("ptr")),
        gc_free_gibaseinfo
    );
    
    // Initialize GOOPS class hierarchy
    gig_init_gibaseinfo_classes();
    
    // Initialize default repository
    default_repository = gig_repository_new();
    
    // Define Scheme procedures
    scm_c_define_gsubr("gi-base-info?", 1, 0, 0, 
                       (scm_t_subr)gig_is_gibaseinfo);
    scm_c_define_gsubr("gi-base-info=?", 2, 0, 0, 
                       (scm_t_subr)gibaseinfo_equalp);
    
    // Repository operations
    scm_c_define_gsubr("irepository-require", 3, 0, 0,
                       (scm_t_subr)gig_irepository_require);
    scm_c_define_gsubr("irepository-get-loaded-namespaces", 0, 0, 0,
                       (scm_t_subr)gig_irepository_get_loaded_namespaces);
    scm_c_define_gsubr("irepository-get-dependencies", 1, 0, 0,
                       (scm_t_subr)gig_irepository_get_dependencies);
    scm_c_define_gsubr("irepository-find-by-name", 2, 0, 0,
                       (scm_t_subr)gig_irepository_find_by_name);
    scm_c_define_gsubr("irepository-get-info", 2, 0, 0,
                       (scm_t_subr)gig_irepository_get_info);
    scm_c_define_gsubr("irepository-get-all-infos", 1, 0, 0,
                       (scm_t_subr)gig_irepository_get_all_infos);
    
    // Base info operations
    scm_c_define_gsubr("gi-base-info-get-name", 1, 0, 0,
                       (scm_t_subr)gig_base_info_get_name);
    scm_c_define_gsubr("gi-base-info-get-namespace", 1, 0, 0,
                       (scm_t_subr)gig_base_info_get_namespace);
    scm_c_define_gsubr("gi-base-info-get-type", 1, 0, 0,
                       (scm_t_subr)gig_base_info_get_type);
    scm_c_define_gsubr("gi-base-info-is-deprecated?", 1, 0, 0,
                       (scm_t_subr)gig_base_info_is_deprecated);
    scm_c_define_gsubr("gi-base-info-equal?", 2, 0, 0,
                       (scm_t_subr)gig_base_info_equal);
    scm_c_define_gsubr("gi-base-info-get-attributes", 1, 0, 0,
                       (scm_t_subr)gig_base_info_get_attributes);
    
    // Configuration functions
    scm_c_define_gsubr("gi-repository-set-auto-load-dependencies!", 1, 0, 0,
                       (scm_t_subr)gig_repository_set_auto_load_dependencies);
    scm_c_define_gsubr("gi-repository-clear-cache!", 0, 0, 0,
                       (scm_t_subr)gig_repository_clear_cache);
    
    // Define constants
    gig_MAJOR_VERSION = define_constant("GI-MAJOR-VERSION", GI_MAJOR_VERSION);
    gig_MINOR_VERSION = define_constant("GI-MINOR-VERSION", GI_MINOR_VERSION);
    gig_MICRO_VERSION = define_constant("GI-MICRO-VERSION", GI_MICRO_VERSION);
    
    // Type tags
    define_constant("GI-TYPE-TAG-VOID", GI_TYPE_TAG_VOID);
    define_constant("GI-TYPE-TAG-BOOLEAN", GI_TYPE_TAG_BOOLEAN);
    define_constant("GI-TYPE-TAG-INT8", GI_TYPE_TAG_INT8);
    define_constant("GI-TYPE-TAG-UINT8", GI_TYPE_TAG_UINT8);
    define_constant("GI-TYPE-TAG-INT16", GI_TYPE_TAG_INT16);
    define_constant("GI-TYPE-TAG-UINT16", GI_TYPE_TAG_UINT16);
    define_constant("GI-TYPE-TAG-INT32", GI_TYPE_TAG_INT32);
    define_constant("GI-TYPE-TAG-UINT32", GI_TYPE_TAG_UINT32);
    define_constant("GI-TYPE-TAG-INT64", GI_TYPE_TAG_INT64);
    define_constant("GI-TYPE-TAG-UINT64", GI_TYPE_TAG_UINT64);
    define_constant("GI-TYPE-TAG-FLOAT", GI_TYPE_TAG_FLOAT);
    define_constant("GI-TYPE-TAG-DOUBLE", GI_TYPE_TAG_DOUBLE);
    define_constant("GI-TYPE-TAG-GTYPE", GI_TYPE_TAG_GTYPE);
    define_constant("GI-TYPE-TAG-UTF8", GI_TYPE_TAG_UTF8);
    define_constant("GI-TYPE-TAG-FILENAME", GI_TYPE_TAG_FILENAME);
    define_constant("GI-TYPE-TAG-ARRAY", GI_TYPE_TAG_ARRAY);
    define_constant("GI-TYPE-TAG-INTERFACE", GI_TYPE_TAG_INTERFACE);
    define_constant("GI-TYPE-TAG-GLIST", GI_TYPE_TAG_GLIST);
    define_constant("GI-TYPE-TAG-GSLIST", GI_TYPE_TAG_GSLIST);
    define_constant("GI-TYPE-TAG-GHASH", GI_TYPE_TAG_GHASH);
    define_constant("GI-TYPE-TAG-ERROR", GI_TYPE_TAG_ERROR);
    define_constant("GI-TYPE-TAG-UNICHAR", GI_TYPE_TAG_UNICHAR);
    
    // Info types
    define_constant("GI-INFO-TYPE-INVALID", GI_INFO_TYPE_INVALID);
    define_constant("GI-INFO-TYPE-FUNCTION", GI_INFO_TYPE_FUNCTION);
    define_constant("GI-INFO-TYPE-CALLBACK", GI_INFO_TYPE_CALLBACK);
    define_constant("GI-INFO-TYPE-STRUCT", GI_INFO_TYPE_STRUCT);
    define_constant("GI-INFO-TYPE-BOXED", GI_INFO_TYPE_BOXED);
    define_constant("GI-INFO-TYPE-ENUM", GI_INFO_TYPE_ENUM);
    define_constant("GI-INFO-TYPE-FLAGS", GI_INFO_TYPE_FLAGS);
    define_constant("GI-INFO-TYPE-OBJECT", GI_INFO_TYPE_OBJECT);
    define_constant("GI-INFO-TYPE-INTERFACE", GI_INFO_TYPE_INTERFACE);
    define_constant("GI-INFO-TYPE-CONSTANT", GI_INFO_TYPE_CONSTANT);
    define_constant("GI-INFO-TYPE-UNION", GI_INFO_TYPE_UNION);
    define_constant("GI-INFO-TYPE-VALUE", GI_INFO_TYPE_VALUE);
    define_constant("GI-INFO-TYPE-SIGNAL", GI_INFO_TYPE_SIGNAL);
    define_constant("GI-INFO-TYPE-VFUNC", GI_INFO_TYPE_VFUNC);
    define_constant("GI-INFO-TYPE-PROPERTY", GI_INFO_TYPE_PROPERTY);
    define_constant("GI-INFO-TYPE-FIELD", GI_INFO_TYPE_FIELD);
    define_constant("GI-INFO-TYPE-ARG", GI_INFO_TYPE_ARG);
    define_constant("GI-INFO-TYPE-TYPE", GI_INFO_TYPE_TYPE);
    define_constant("GI-INFO-TYPE-UNRESOLVED", GI_INFO_TYPE_UNRESOLVED);
    
    // Function flags
    define_constant("GI-FUNCTION-IS-METHOD", GI_FUNCTION_IS_METHOD);
    define_constant("GI-FUNCTION-IS-CONSTRUCTOR", GI_FUNCTION_IS_CONSTRUCTOR);
    define_constant("GI-FUNCTION-IS-GETTER", GI_FUNCTION_IS_GETTER);
    define_constant("GI-FUNCTION-IS-SETTER", GI_FUNCTION_IS_SETTER);
    define_constant("GI-FUNCTION-WRAPS-VFUNC", GI_FUNCTION_WRAPS_VFUNC);
    define_constant("GI-FUNCTION-THROWS", GI_FUNCTION_THROWS);
    
    // Transfer modes
    define_constant("GI-TRANSFER-NOTHING", GI_TRANSFER_NOTHING);
    define_constant("GI-TRANSFER-CONTAINER", GI_TRANSFER_CONTAINER);
    define_constant("GI-TRANSFER-EVERYTHING", GI_TRANSFER_EVERYTHING);
    
    // Argument directions
    define_constant("GI-DIRECTION-IN", GI_DIRECTION_IN);
    define_constant("GI-DIRECTION-OUT", GI_DIRECTION_OUT);
    define_constant("GI-DIRECTION-INOUT", GI_DIRECTION_INOUT);
    
    // Repository load flags
    define_constant("GI-IREPOSITORY-LOAD-FLAG-LAZY", G_IREPOSITORY_LOAD_FLAG_LAZY);
    
    initialized = true;
    gig_debug_gi("GIRepository integration initialized successfully");
}

GIG_API void 
gig_cleanup_girepository(void)
{
    if (!initialized) {
        return;
    }
    
    gig_debug_gi("Cleaning up GIRepository integration");
    
    if (default_repository) {
        gig_repository_destroy(default_repository);
        default_repository = NULL;
    }
    
    initialized = false;
}

// ============================================================================
// Performance Optimization
// ============================================================================

GIG_API SCM 
gig_irepository_preload_namespace(SCM namespace)
{
    char *c_namespace = gig_scm_to_utf8_string_safe(namespace);
    if (!c_namespace) {
        return SCM_BOOL_F;
    }
    
    gig_debug_gi("Preloading namespace: %s", c_namespace);
    
    int n_infos = g_irepository_get_n_infos(NULL, c_namespace);
    GigRepository *repo = gig_get_default_repository();
    
    for (int i = 0; i < n_infos; i++) {
        GIBaseInfo *info = g_irepository_get_info(NULL, c_namespace, i);
        if (info) {
            const char *name = g_base_info_get_name(info);
            if (name) {
                char *cache_key = str_printf("%s::%s", c_namespace, name);
                
                GigInfoWrapper *wrapper = gig_info_wrapper_new(info);
                wrapper->is_cached = true;
                hashmap_insert_string(repo->type_cache, cache_key, &wrapper);
                
                xfree(cache_key);
            }
        }
    }
    
    gig_debug_gi("Preloaded %d infos for namespace %s", n_infos, c_namespace);
    xfree(c_namespace);
    
    return scm_from_int(n_infos);
}
