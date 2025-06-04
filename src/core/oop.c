// Copyright (C) 2022 Michael L. Gran
// Modernized 2025 - Enhanced GOOPS integration with comprehensive OOP support

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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>

#include "scheme.h"
#include "oop.h"
#include "mem.h"
#include "logging.h"
#include "keyval.h"

// ============================================================================
// Global State and Caching
// ============================================================================

// Cached GOOPS classes for performance
static struct {
    // Core hierarchy
    SCM top_type;
    SCM object_type;
    SCM class_type;
    SCM generic_type;
    SCM method_type;
    
    // Primitive types
    SCM boolean_type;
    SCM char_type;
    SCM integer_type;
    SCM real_type;
    SCM complex_type;
    SCM number_type;
    SCM string_type;
    SCM symbol_type;
    SCM keyword_type;
    
    // Collection types
    SCM list_type;
    SCM pair_type;
    SCM null_type;
    SCM vector_type;
    SCM bytevector_type;
    SCM hashtable_type;
    
    // Function types
    SCM procedure_type;
    SCM applicable_type;
    SCM applicable_struct_type;
    SCM applicable_struct_with_setter_type;
    
    // I/O and system types
    SCM port_type;
    SCM input_port_type;
    SCM output_port_type;
    SCM foreign_type;
    SCM unknown_type;
    
    bool types_loaded;
} g_oop_types = { .types_loaded = false };

// Cached GOOPS procedures
static struct {
    SCM add_method_proc;
    SCM remove_method_proc;
    SCM class_slot_ref_proc;
    SCM class_slot_set_proc;
    SCM ensure_accessor_proc;
    SCM ensure_generic_proc;
    SCM make_proc;
    SCM make_class_proc;
    SCM find_method_proc;
    SCM compute_applicable_methods_proc;
    
    bool procedures_loaded;
} g_oop_procs = { .procedures_loaded = false };

// Cached symbols and keywords
static struct {
    // Keywords
    SCM name_kwd;
    SCM value_kwd;
    SCM formals_kwd;
    SCM procedure_kwd;
    SCM specializers_kwd;
    SCM supers_kwd;
    SCM slots_kwd;
    
    // Symbols
    SCM obarray_sym;
    SCM procedure_sym;
    SCM ref_sym;
    SCM size_sym;
    SCM unref_sym;
    SCM value_sym;
    SCM name_sym;
    
    bool symbols_loaded;
} g_oop_symbols = { .symbols_loaded = false };

// C type registry
static HashMap *g_c_type_registry = NULL;

// Global state
static struct {
    bool initialized;
    bool goops_available;
    bool method_dispatch_traced;
    OopError last_error;
} g_oop_state = {
    .initialized = false,
    .goops_available = false,
    .method_dispatch_traced = false,
    .last_error = OOP_ERROR_NONE
};

// ============================================================================
// Helper Functions
// ============================================================================

static void set_oop_error(OopError error) {
    g_oop_state.last_error = error;
}

static void clear_oop_error(void) {
    g_oop_state.last_error = OOP_ERROR_NONE;
}

static bool ensure_oop_initialized(void) {
    if (!g_oop_state.initialized) {
        set_oop_error(OOP_ERROR_NOT_INITIALIZED);
        return false;
    }
    return true;
}

static void load_goops_types(void) {
    if (g_oop_types.types_loaded) return;
    
    // Core hierarchy
    g_oop_types.top_type = scm_c_public_ref_safe("oop goops", "<top>");
    g_oop_types.object_type = scm_c_public_ref_safe("oop goops", "<object>");
    g_oop_types.class_type = scm_c_public_ref_safe("oop goops", "<class>");
    g_oop_types.generic_type = scm_c_public_ref_safe("oop goops", "<generic>");
    g_oop_types.method_type = scm_c_public_ref_safe("oop goops", "<method>");
    
    // Primitive types
    g_oop_types.boolean_type = scm_c_public_ref_safe("oop goops", "<boolean>");
    g_oop_types.char_type = scm_c_public_ref_safe("oop goops", "<char>");
    g_oop_types.integer_type = scm_c_public_ref_safe("oop goops", "<integer>");
    g_oop_types.real_type = scm_c_public_ref_safe("oop goops", "<real>");
    g_oop_types.complex_type = scm_c_public_ref_safe("oop goops", "<complex>");
    g_oop_types.number_type = scm_c_public_ref_safe("oop goops", "<number>");
    g_oop_types.string_type = scm_c_public_ref_safe("oop goops", "<string>");
    g_oop_types.symbol_type = scm_c_public_ref_safe("oop goops", "<symbol>");
    g_oop_types.keyword_type = scm_c_public_ref_safe("oop goops", "<keyword>");
    
    // Collection types
    g_oop_types.list_type = scm_c_public_ref_safe("oop goops", "<list>");
    g_oop_types.pair_type = scm_c_public_ref_safe("oop goops", "<pair>");
    g_oop_types.null_type = scm_c_public_ref_safe("oop goops", "<null>");
    g_oop_types.vector_type = scm_c_public_ref_safe("oop goops", "<vector>");
    g_oop_types.bytevector_type = scm_c_public_ref_safe("oop goops", "<bytevector>");
    g_oop_types.hashtable_type = scm_c_public_ref_safe("oop goops", "<hashtable>");
    
    // Function types
    g_oop_types.procedure_type = scm_c_public_ref_safe("oop goops", "<procedure>");
    g_oop_types.applicable_type = scm_c_public_ref_safe("oop goops", "<applicable>");
    g_oop_types.applicable_struct_type = scm_c_public_ref_safe("oop goops", "<applicable-struct>");
    g_oop_types.applicable_struct_with_setter_type = scm_c_public_ref_safe("oop goops", "<applicable-struct-with-setter>");
    
    // I/O and system types
    g_oop_types.port_type = scm_c_public_ref_safe("oop goops", "<port>");
    g_oop_types.input_port_type = scm_c_public_ref_safe("oop goops", "<input-port>");
    g_oop_types.output_port_type = scm_c_public_ref_safe("oop goops", "<output-port>");
    g_oop_types.foreign_type = scm_c_public_ref_safe("oop goops", "<foreign>");
    g_oop_types.unknown_type = scm_c_public_ref_safe("oop goops", "<unknown>");
    
    g_oop_types.types_loaded = true;
}

static void load_goops_procedures(void) {
    if (g_oop_procs.procedures_loaded) return;
    
    g_oop_procs.add_method_proc = scm_c_public_ref_safe("oop goops", "add-method!");
    g_oop_procs.remove_method_proc = scm_c_public_ref_safe("oop goops", "remove-method!");
    g_oop_procs.class_slot_ref_proc = scm_c_public_ref_safe("oop goops", "class-slot-ref");
    g_oop_procs.class_slot_set_proc = scm_c_public_ref_safe("oop goops", "class-slot-set!");
    g_oop_procs.ensure_accessor_proc = scm_c_public_ref_safe("oop goops", "ensure-accessor");
    g_oop_procs.ensure_generic_proc = scm_c_public_ref_safe("oop goops", "ensure-generic");
    g_oop_procs.make_proc = scm_c_public_ref_safe("oop goops", "make");
    g_oop_procs.make_class_proc = scm_c_public_ref_safe("oop goops", "make-class");
    g_oop_procs.find_method_proc = scm_c_public_ref_safe("oop goops", "find-method");
    g_oop_procs.compute_applicable_methods_proc = scm_c_public_ref_safe("oop goops", "compute-applicable-methods");
    
    g_oop_procs.procedures_loaded = true;
}

static void load_goops_symbols(void) {
    if (g_oop_symbols.symbols_loaded) return;
    
    // Keywords
    g_oop_symbols.name_kwd = scm_from_utf8_keyword("name");
    g_oop_symbols.value_kwd = scm_from_utf8_keyword("value");
    g_oop_symbols.formals_kwd = scm_from_utf8_keyword("formals");
    g_oop_symbols.procedure_kwd = scm_from_utf8_keyword("procedure");
    g_oop_symbols.specializers_kwd = scm_from_utf8_keyword("specializers");
    g_oop_symbols.supers_kwd = scm_from_utf8_keyword("supers");
    g_oop_symbols.slots_kwd = scm_from_utf8_keyword("slots");
    
    // Symbols
    g_oop_symbols.obarray_sym = scm_from_utf8_symbol("obarray");
    g_oop_symbols.procedure_sym = scm_from_utf8_symbol("procedure");
    g_oop_symbols.ref_sym = scm_from_utf8_symbol("ref");
    g_oop_symbols.size_sym = scm_from_utf8_symbol("size");
    g_oop_symbols.unref_sym = scm_from_utf8_symbol("unref");
    g_oop_symbols.value_sym = scm_from_utf8_symbol("value");
    g_oop_symbols.name_sym = scm_from_utf8_symbol("name");
    
    // Protect symbols from GC
    scm_gc_protect_object_safe(g_oop_symbols.name_kwd);
    scm_gc_protect_object_safe(g_oop_symbols.value_kwd);
    scm_gc_protect_object_safe(g_oop_symbols.formals_kwd);
    scm_gc_protect_object_safe(g_oop_symbols.procedure_kwd);
    scm_gc_protect_object_safe(g_oop_symbols.specializers_kwd);
    
    g_oop_symbols.symbols_loaded = true;
}

// ============================================================================
// System Initialization and Status (enhanced from original)
// ============================================================================

void init_core_oop(void) {
    if (g_oop_state.initialized) return;
    
    gig_debug("Initializing GOOPS core OOP system");
    
    // Initialize Guile if not already done
    scm_init_guile();
    
    // Load GOOPS
    scm_load_goops();
    g_oop_state.goops_available = true;
    
    // Load cached objects
    load_goops_types();
    load_goops_procedures();
    load_goops_symbols();
    
    // Initialize C type registry
    if (!g_c_type_registry) {
        HashMapConfig config = hashmap_config_string_keys();
        g_c_type_registry = hashmap_create_string_keys(sizeof(CTypeInfo), &config);
    }
    
    g_oop_state.initialized = true;
    clear_oop_error();
    
    gig_debug("GOOPS core OOP system initialized successfully");
}

bool oop_is_initialized(void) {
    return g_oop_state.initialized;
}

void oop_cleanup(void) {
    if (!g_oop_state.initialized) return;
    
    gig_debug("Cleaning up GOOPS core OOP system");
    
    // Cleanup C type registry
    if (g_c_type_registry) {
        hashmap_destroy(g_c_type_registry);
        g_c_type_registry = NULL;
    }
    
    // Unprotect symbols from GC
    if (g_oop_symbols.symbols_loaded) {
        scm_gc_unprotect_object_safe(g_oop_symbols.name_kwd);
        scm_gc_unprotect_object_safe(g_oop_symbols.value_kwd);
        scm_gc_unprotect_object_safe(g_oop_symbols.formals_kwd);
        scm_gc_unprotect_object_safe(g_oop_symbols.procedure_kwd);
        scm_gc_unprotect_object_safe(g_oop_symbols.specializers_kwd);
    }
    
    // Reset state
    memset(&g_oop_types, 0, sizeof(g_oop_types));
    memset(&g_oop_procs, 0, sizeof(g_oop_procs));
    memset(&g_oop_symbols, 0, sizeof(g_oop_symbols));
    
    g_oop_state.initialized = false;
    g_oop_state.goops_available = false;
    
    gig_debug("GOOPS core OOP system cleanup complete");
}

bool oop_goops_available(void) {
    return g_oop_state.goops_available;
}

const char *oop_goops_version(void) {
    if (!oop_goops_available()) return "unavailable";
    
    SCM version_proc = scm_c_public_ref_safe("oop goops", "goops-version");
    if (scm_is_procedure(version_proc)) {
        SCM version = scm_call_0(version_proc);
        if (scm_is_string(version)) {
            return scm_to_utf8_string_safe(version);
        }
    }
    
    return "unknown";
}

// ============================================================================
// Built-in Class Accessors (original functions modernized)
// ============================================================================

SCM scm_get_top_class(void) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    return g_oop_types.top_type;
}

SCM scm_get_object_class(void) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    return g_oop_types.object_type;
}

SCM scm_get_class_class(void) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    return g_oop_types.class_type;
}

SCM scm_get_generic_class(void) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    return g_oop_types.generic_type;
}

SCM scm_get_method_class(void) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    return g_oop_types.method_type;
}

SCM scm_get_boolean_class(void) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    return g_oop_types.boolean_type;
}

SCM scm_get_char_class(void) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    return g_oop_types.char_type;
}

SCM scm_get_integer_class(void) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    return g_oop_types.integer_type;
}

SCM scm_get_real_class(void) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    return g_oop_types.real_type;
}

SCM scm_get_complex_class(void) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    return g_oop_types.complex_type;
}

SCM scm_get_string_class(void) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    return g_oop_types.string_type;
}

SCM scm_get_list_class(void) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    return g_oop_types.list_type;
}

SCM scm_get_hashtable_class(void) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    return g_oop_types.hashtable_type;
}

SCM scm_get_applicable_class(void) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    return g_oop_types.applicable_type;
}

SCM scm_get_applicable_struct_class(void) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    return g_oop_types.applicable_struct_type;
}

SCM scm_get_applicable_struct_with_setter_class(void) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    return g_oop_types.applicable_struct_with_setter_type;
}

SCM scm_get_foreign_class(void) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    return g_oop_types.foreign_type;
}

SCM scm_get_unknown_class(void) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    return g_oop_types.unknown_type;
}

// ============================================================================
// Enhanced Class Operations (original functions modernized)
// ============================================================================

SCM scm_make_class_with_name(SCM supers, SCM slots, SCM name) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    
    if (!scm_is_list(supers)) {
        set_oop_error(OOP_ERROR_INVALID_CLASS);
        return SCM_BOOL_F;
    }
    
    if (!scm_is_list(slots)) {
        set_oop_error(OOP_ERROR_INVALID_CLASS);
        return SCM_BOOL_F;
    }
    
    SCM result = scm_call_4(g_oop_procs.make_class_proc, supers, slots, 
                           g_oop_symbols.name_kwd, name);
    
    if (scm_is_true(result)) {
        clear_oop_error();
        gig_debug("Created class: %s", scm_to_utf8_string_safe(name));
    } else {
        set_oop_error(OOP_ERROR_INVALID_CLASS);
    }
    
    return result;
}

SCM scm_make_class_simple(SCM super, const char *name) {
    if (!name) {
        set_oop_error(OOP_ERROR_INVALID_CLASS);
        return SCM_BOOL_F;
    }
    
    SCM name_scm = scm_from_utf8_string_safe(name);
    SCM supers = super ? scm_list_1(super) : SCM_EOL;
    
    return scm_make_class_with_name(supers, SCM_EOL, name_scm);
}

SCM scm_make_class_with_slots(SCM super, const char *name, 
                             const char **slot_names, size_t slot_count) {
    if (!name || !slot_names) {
        set_oop_error(OOP_ERROR_INVALID_CLASS);
        return SCM_BOOL_F;
    }
    
    SCM slots = SCM_EOL;
    for (size_t i = slot_count; i > 0; i--) {
        SCM slot_name = scm_from_utf8_symbol(slot_names[i-1]);
        slots = scm_cons(slot_name, slots);
    }
    
    SCM name_scm = scm_from_utf8_string_safe(name);
    SCM supers = super ? scm_list_1(super) : SCM_EOL;
    
    return scm_make_class_with_name(supers, slots, name_scm);
}

bool scm_class_has_slot(SCM class, SCM slot_name) {
    if (!ensure_oop_initialized()) return false;
    
    if (!scm_is_class(class) || !scm_is_symbol(slot_name)) {
        set_oop_error(OOP_ERROR_INVALID_CLASS);
        return false;
    }
    
    SCM slots = scm_class_slots(class);
    SCM slot_names = scm_map(scm_c_public_ref_safe("oop goops", "slot-definition-name"), 
			     scm_car(slots), scm_cdr(slots));
    
    bool found = scm_c_list_index(slot_names, slot_name) >= 0;
    clear_oop_error();
    return found;
}

bool scm_class_is_subclass_of(SCM class, SCM potential_super) {
    if (!ensure_oop_initialized()) return false;
    
    if (!scm_is_class(class) || !scm_is_class(potential_super)) {
        set_oop_error(OOP_ERROR_INVALID_CLASS);
        return false;
    }
    
    SCM cpl = scm_class_precedence_list(class);
    bool is_subclass = scm_c_list_index(cpl, potential_super) >= 0;
    
    clear_oop_error();
    return is_subclass;
}

// ============================================================================
// Enhanced Instance Operations (original functions modernized)
// ============================================================================

SCM scm_make_with_value(SCM type, SCM value) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    
    if (!scm_is_class(type)) {
        set_oop_error(OOP_ERROR_INVALID_CLASS);
        return SCM_BOOL_F;
    }
    
    SCM result = scm_call_3(g_oop_procs.make_proc, type, 
                           g_oop_symbols.value_kwd, value);
    
    if (scm_is_true(result)) {
        clear_oop_error();
    } else {
        set_oop_error(OOP_ERROR_INVALID_INSTANCE);
    }
    
    return result;
}

SCM scm_make_instance(SCM class, ...) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    
    if (!scm_is_class(class)) {
        set_oop_error(OOP_ERROR_INVALID_CLASS);
        return SCM_BOOL_F;
    }
    
    // Build initialization list from variadic arguments
    va_list args;
    va_start(args, class);
    
    SCM init_list = SCM_EOL;
    SCM key;
    
    while ((key = va_arg(args, SCM)) != SCM_UNDEFINED) {
        SCM value = va_arg(args, SCM);
        init_list = scm_cons(key, scm_cons(value, init_list));
    }
    
    va_end(args);
    
    init_list = scm_reverse(init_list);
    SCM result = scm_apply_0(g_oop_procs.make_proc, scm_cons(class, init_list));
    
    if (scm_is_true(result)) {
        clear_oop_error();
    } else {
        set_oop_error(OOP_ERROR_INVALID_INSTANCE);
    }
    
    return result;
}

SCM scm_get_value_slot(SCM instance) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    
    if (!scm_is_instance(instance)) {
        set_oop_error(OOP_ERROR_INVALID_INSTANCE);
        return SCM_BOOL_F;
    }
    
    return scm_slot_ref_safe(instance, g_oop_symbols.value_sym);
}

bool scm_instance_has_slot(SCM instance, SCM slot_name) {
    if (!ensure_oop_initialized()) return false;
    
    if (!scm_is_instance(instance) || !scm_is_symbol(slot_name)) {
        set_oop_error(OOP_ERROR_INVALID_INSTANCE);
        return false;
    }
    
    return scm_slot_exists(instance, slot_name);
}

SCM scm_slot_ref_safe(SCM instance, SCM slot_name) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    
    if (!scm_is_instance(instance) || !scm_is_symbol(slot_name)) {
        set_oop_error(OOP_ERROR_INVALID_INSTANCE);
        return SCM_BOOL_F;
    }
    
    if (!scm_slot_exists(instance, slot_name)) {
        set_oop_error(OOP_ERROR_SLOT_NOT_FOUND);
        return SCM_BOOL_F;
    }
    
    if (!scm_slot_is_bound(instance, slot_name)) {
        set_oop_error(OOP_ERROR_SLOT_UNBOUND);
        return SCM_BOOL_F;
    }
    
    clear_oop_error();
    return scm_slot_ref(instance, slot_name);
}

bool scm_slot_set_safe(SCM instance, SCM slot_name, SCM value) {
    if (!ensure_oop_initialized()) return false;
    
    if (!scm_is_instance(instance) || !scm_is_symbol(slot_name)) {
        set_oop_error(OOP_ERROR_INVALID_INSTANCE);
        return false;
    }
    
    if (!scm_slot_exists(instance, slot_name)) {
        set_oop_error(OOP_ERROR_SLOT_NOT_FOUND);
        return false;
    }
    
    scm_slot_set_x(instance, slot_name, value);
    clear_oop_error();
    return true;
}

// ============================================================================
// Enhanced Class Slot Operations (original functions modernized)
// ============================================================================

SCM scm_get_class_ref_slot(SCM cls) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    return scm_class_slot_ref_safe(cls, g_oop_symbols.ref_sym);
}

SCM scm_get_class_size_slot(SCM cls) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    return scm_class_slot_ref_safe(cls, g_oop_symbols.size_sym);
}

SCM scm_get_class_unref_slot(SCM cls) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    return scm_class_slot_ref_safe(cls, g_oop_symbols.unref_sym);
}

bool scm_set_class_ref_slot(SCM cls, SCM func) {
    if (!ensure_oop_initialized()) return false;
    return scm_class_slot_set_safe(cls, g_oop_symbols.ref_sym, func);
}

bool scm_set_class_size_slot(SCM cls, SCM func) {
    if (!ensure_oop_initialized()) return false;
    return scm_class_slot_set_safe(cls, g_oop_symbols.size_sym, func);
}

bool scm_set_class_unref_slot(SCM cls, SCM func) {
    if (!ensure_oop_initialized()) return false;
    return scm_class_slot_set_safe(cls, g_oop_symbols.unref_sym, func);
}

bool scm_set_class_obarray_slot(SCM cls, SCM obarray) {
    if (!ensure_oop_initialized()) return false;
    return scm_class_slot_set_safe(cls, g_oop_symbols.obarray_sym, obarray);
}

SCM scm_class_slot_ref_safe(SCM cls, SCM slot) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    
    if (!scm_is_class(cls) || !scm_is_symbol(slot)) {
        set_oop_error(OOP_ERROR_INVALID_CLASS);
        return SCM_BOOL_F;
    }
    
    SCM result = scm_call_2(g_oop_procs.class_slot_ref_proc, cls, slot);
    clear_oop_error();
    return result;
}

bool scm_class_slot_set_safe(SCM cls, SCM slot, SCM value) {
    if (!ensure_oop_initialized()) return false;
    
    if (!scm_is_class(cls) || !scm_is_symbol(slot)) {
        set_oop_error(OOP_ERROR_INVALID_CLASS);
        return false;
    }
    
    scm_call_3(g_oop_procs.class_slot_set_proc, cls, slot, value);
    clear_oop_error();
    return true;
}

// ============================================================================
// Enhanced Generic Function and Method System (original functions modernized)
// ============================================================================

bool scm_add_method(SCM target, SCM method) {
    if (!ensure_oop_initialized()) return false;
    
    if (!scm_is_generic(target) || !scm_is_method(method)) {
        set_oop_error(OOP_ERROR_INVALID_CLASS);
        return false;
    }
    
    scm_call_2(g_oop_procs.add_method_proc, target, method);
    clear_oop_error();
    return true;
}

SCM scm_make_method(SCM specializers, SCM formals, SCM proc) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    
    if (!scm_is_list(specializers) || !scm_is_list(formals) || !scm_is_procedure(proc)) {
        set_oop_error(OOP_ERROR_INVALID_CLASS);
        return SCM_BOOL_F;
    }
    
    SCM result = scm_call_7(g_oop_procs.make_proc, g_oop_types.method_type,
                           g_oop_symbols.specializers_kwd, specializers,
                           g_oop_symbols.formals_kwd, formals,
                           g_oop_symbols.procedure_kwd, proc);
    
    if (scm_is_true(result)) {
        clear_oop_error();
    } else {
        set_oop_error(OOP_ERROR_INVALID_CLASS);
    }
    
    return result;
}

SCM scm_define_methods_from_procedure(const char *public_name, SCM proc, int opt, 
                                     SCM formals, SCM specializers) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    
    if (!public_name || !scm_is_procedure(proc) || 
        !scm_is_list(formals) || !scm_is_list(specializers)) {
        set_oop_error(OOP_ERROR_INVALID_CLASS);
        return SCM_BOOL_F;
    }
    
    SCM sym_public_name = scm_from_utf8_symbol(public_name);
    SCM generic = scm_default_definition(sym_public_name);
    
    if (!scm_is_generic(generic)) {
        generic = scm_call_2(g_oop_procs.ensure_generic_proc, generic, sym_public_name);
    }
    
    SCM t_formals = formals;
    SCM t_specializers = specializers;
    
    do {
        SCM method = scm_make_method(t_specializers, t_formals, proc);
        if (scm_is_false(method)) {
            return SCM_BOOL_F;
        }
        
        if (!scm_add_method(generic, method)) {
            return SCM_BOOL_F;
        }
        
        if (scm_is_eq(t_formals, SCM_EOL)) {
            break;
        }
        
        t_formals = scm_drop_right_1(t_formals);
        t_specializers = scm_drop_right_1(t_specializers);
    } while (opt-- > 0);
    
    scm_define(sym_public_name, generic);
    clear_oop_error();
    return sym_public_name;
}

SCM scm_make_generic(const char *name) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    
    SCM name_scm = name ? scm_from_utf8_string_safe(name) : SCM_BOOL_F;
    SCM result = scm_call_2(g_oop_procs.ensure_generic_proc, SCM_BOOL_F, name_scm);
    
    if (scm_is_true(result)) {
        clear_oop_error();
    } else {
        set_oop_error(OOP_ERROR_DISPATCH_FAILED);
    }
    
    return result;
}

SCM scm_make_method_simple(SCM *specializers, size_t spec_count, SCM proc) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    
    if (!specializers || spec_count == 0 || !scm_is_procedure(proc)) {
        set_oop_error(OOP_ERROR_INVALID_CLASS);
        return SCM_BOOL_F;
    }
    
    SCM spec_list = SCM_EOL;
    for (size_t i = spec_count; i > 0; i--) {
        spec_list = scm_cons(specializers[i-1], spec_list);
    }
    
    // Create formals list (arg0, arg1, ...)
    SCM formals = SCM_EOL;
    for (size_t i = spec_count; i > 0; i--) {
        char arg_name[32];
        snprintf(arg_name, sizeof(arg_name), "arg%zu", i-1);
        SCM arg_sym = scm_from_utf8_symbol(arg_name);
        formals = scm_cons(arg_sym, formals);
    }
    
    return scm_make_method(spec_list, formals, proc);
}

bool scm_remove_method(SCM generic, SCM method) {
    if (!ensure_oop_initialized()) return false;
    
    if (!scm_is_generic(generic) || !scm_is_method(method)) {
        set_oop_error(OOP_ERROR_INVALID_CLASS);
        return false;
    }
    
    scm_call_2(g_oop_procs.remove_method_proc, generic, method);
    clear_oop_error();
    return true;
}

SCM scm_find_method(SCM generic, SCM *args, size_t arg_count) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    
    if (!scm_is_generic(generic) || !args) {
        set_oop_error(OOP_ERROR_DISPATCH_FAILED);
        return SCM_BOOL_F;
    }
    
    SCM arg_list = SCM_EOL;
    for (size_t i = arg_count; i > 0; i--) {
        arg_list = scm_cons(args[i-1], arg_list);
    }
    
    SCM result = scm_call_2(g_oop_procs.find_method_proc, generic, arg_list);
    
    if (scm_is_true(result)) {
        clear_oop_error();
    } else {
        set_oop_error(OOP_ERROR_METHOD_NOT_FOUND);
    }
    
    return result;
}

// ============================================================================
// Enhanced Accessor System (original functions modernized)
// ============================================================================

SCM scm_ensure_accessor_with_name(SCM proc, SCM name) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    
    if (!scm_is_procedure(proc)) {
        set_oop_error(OOP_ERROR_INVALID_CLASS);
        return SCM_BOOL_F;
    }
    
    SCM result = scm_call_2(g_oop_procs.ensure_accessor_proc, proc, name);
    clear_oop_error();
    return result;
}

SCM scm_make_accessor(const char *name) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    
    SCM name_scm = name ? scm_from_utf8_string_safe(name) : SCM_BOOL_F;
    return scm_ensure_accessor_with_name(SCM_BOOL_F, name_scm);
}

bool scm_set_procedure_slot(SCM instance, SCM proc) {
    if (!ensure_oop_initialized()) return false;
    
    if (!scm_is_instance(instance) || !scm_is_procedure(proc)) {
        set_oop_error(OOP_ERROR_INVALID_INSTANCE);
        return false;
    }
    
    return scm_slot_set_safe(instance, g_oop_symbols.procedure_sym, proc);
}

// ============================================================================
// Enhanced Type System Integration (original functions modernized)
// ============================================================================

bool scm_is_unknown_class(SCM type) {
    if (!ensure_oop_initialized()) return false;
    return scm_is_equal(type, g_oop_types.unknown_type);
}

bool scm_is_instance(SCM obj) {
    if (!ensure_oop_initialized()) return false;
    return scm_is_true(scm_instance_p(obj));
}

bool scm_is_class(SCM obj) {
    if (!ensure_oop_initialized()) return false;
    //return scm_is_true(scm_class_p(obj));
    return SCM_CLASSP(obj);
}

#if 0
bool scm_is_generic(SCM obj) {
    if (!ensure_oop_initialized()) return false;
    return scm_is_true(scm_generic_p(obj));
}
#endif

#if 0
bool scm_is_method(SCM obj) {
    if (!ensure_oop_initialized()) return false;
    return scm_is_true(scm_method_p(obj));
}
#endif

SCM scm_class_of(SCM obj) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    return scm_class_of(obj);
}

bool scm_object_is_instance_of(SCM obj, SCM class) {
    if (!ensure_oop_initialized()) return false;
    
    if (!scm_is_class(class)) {
        set_oop_error(OOP_ERROR_INVALID_CLASS);
        return false;
    }
    
    SCM obj_class = scm_class_of(obj);
    bool result = scm_class_is_subclass_of(obj_class, class);
    
    clear_oop_error();
    return result;
}

// ============================================================================
// Error Handling and Validation
// ============================================================================

OopError oop_get_last_error(void) {
    return g_oop_state.last_error;
}

const char *oop_error_string(OopError error) {
    switch (error) {
        case OOP_ERROR_NONE:
            return "No error";
        case OOP_ERROR_NOT_INITIALIZED:
            return "OOP system not initialized";
        case OOP_ERROR_INVALID_CLASS:
            return "Invalid class";
        case OOP_ERROR_INVALID_INSTANCE:
            return "Invalid instance";
        case OOP_ERROR_SLOT_NOT_FOUND:
            return "Slot not found";
        case OOP_ERROR_SLOT_UNBOUND:
            return "Slot is unbound";
        case OOP_ERROR_METHOD_NOT_FOUND:
            return "Method not found";
        case OOP_ERROR_DISPATCH_FAILED:
            return "Method dispatch failed";
        case OOP_ERROR_INHERITANCE_CONFLICT:
            return "Inheritance conflict";
        case OOP_ERROR_METACLASS_CONFLICT:
            return "Metaclass conflict";
        default:
            return "Unknown error";
    }
}

void oop_clear_error(void) {
    clear_oop_error();
}

bool oop_validate_class(SCM class) {
    if (!ensure_oop_initialized()) return false;
    
    if (!scm_is_class(class)) {
        set_oop_error(OOP_ERROR_INVALID_CLASS);
        return false;
    }
    
    // Additional validation checks
    SCM cpl = scm_class_precedence_list(class);
    if (!scm_is_list(cpl) || scm_is_null(cpl)) {
        set_oop_error(OOP_ERROR_INVALID_CLASS);
        return false;
    }
    
    // Check that the class is in its own precedence list
    if (scm_c_list_index(cpl, class) < 0) {
        set_oop_error(OOP_ERROR_INVALID_CLASS);
        return false;
    }
    
    clear_oop_error();
    return true;
}

bool oop_validate_instance(SCM instance) {
    if (!ensure_oop_initialized()) return false;
    
    if (!scm_is_instance(instance)) {
        set_oop_error(OOP_ERROR_INVALID_INSTANCE);
        return false;
    }
    
    SCM class = scm_class_of(instance);
    if (!oop_validate_class(class)) {
        return false;
    }
    
    clear_oop_error();
    return true;
}

bool oop_validate_method(SCM method) {
    if (!ensure_oop_initialized()) return false;
    
    if (!scm_is_method(method)) {
        set_oop_error(OOP_ERROR_METHOD_NOT_FOUND);
        return false;
    }
    
    // Check that method has required components
    SCM specializers = scm_method_specializers(method);
    SCM procedure = scm_method_procedure(method);
    
    if (!scm_is_list(specializers) || !scm_is_procedure(procedure)) {
        set_oop_error(OOP_ERROR_METHOD_NOT_FOUND);
        return false;
    }
    
    clear_oop_error();
    return true;
}

bool oop_validate_generic(SCM generic) {
    if (!ensure_oop_initialized()) return false;
    
    if (!scm_is_generic(generic)) {
        set_oop_error(OOP_ERROR_DISPATCH_FAILED);
        return false;
    }
    
    clear_oop_error();
    return true;
}

// ============================================================================
// Debugging and Introspection
// ============================================================================

void scm_print_class_info(SCM class, FILE *stream) {
    if (!stream) stream = stderr;
    if (!oop_validate_class(class)) return;
    
    fprintf(stream, "Class Information:\n");
    
    // Class name
    SCM name = scm_class_name(class);
    if (scm_is_true(name)) {
        char *name_str = scm_to_utf8_string_safe(name);
        fprintf(stream, "  Name: %s\n", name_str ? name_str : "<unnamed>");
        if (name_str) free(name_str);
    }
    
    // Class precedence list
    SCM cpl = scm_class_precedence_list(class);
    fprintf(stream, "  Precedence list length: %zu\n", scm_c_length(cpl));
    
    // Direct superclasses
    SCM supers = scm_class_direct_supers(class);
    fprintf(stream, "  Direct superclasses: %zu\n", scm_c_length(supers));
    
    // Slots
    SCM slots = scm_class_slots(class);
    fprintf(stream, "  Total slots: %zu\n", scm_c_length(slots));
    
    SCM direct_slots = scm_class_direct_slots(class);
    fprintf(stream, "  Direct slots: %zu\n", scm_c_length(direct_slots));
}

void scm_print_instance_info(SCM instance, FILE *stream) {
    if (!stream) stream = stderr;
    if (!oop_validate_instance(instance)) return;
    
    fprintf(stream, "Instance Information:\n");
    
    SCM class = scm_class_of(instance);
    SCM class_name = scm_class_name(class);
    
    if (scm_is_true(class_name)) {
        char *name_str = scm_to_utf8_string_safe(class_name);
        fprintf(stream, "  Class: %s\n", name_str ? name_str : "<unnamed>");
        if (name_str) free(name_str);
    }
    
    // Count bound slots
    SCM slots = scm_class_slots(class);
    size_t total_slots = scm_c_length(slots);
    
#if 0    
    size_t bound_slots = 0;
    SCM current = slots;
    while (!scm_is_null(current)) {
        SCM slot_def = scm_car(current);
        SCM slot_name = scm_slot_definition_name(slot_def);
        
        if (scm_slot_is_bound(instance, slot_name)) {
            bound_slots++;
        }
        
        current = scm_cdr(current);
    }
    
    fprintf(stream, "  Bound slots: %zu/%zu\n", bound_slots, total_slots);
#else
    fprintf(stream, "  Total slots: %zu\n", total_slots);
#endif
}

void scm_print_method_info(SCM method, FILE *stream) {
    if (!stream) stream = stderr;
    if (!oop_validate_method(method)) return;
    
    fprintf(stream, "Method Information:\n");
    
    SCM specializers = scm_method_specializers(method);
    fprintf(stream, "  Specializers: %zu\n", scm_c_length(specializers));

#if 0    
    SCM formals = scm_method_formals(method);
    if (scm_is_true(formals)) {
        fprintf(stream, "  Formals: %zu\n", scm_c_length(formals));
    }
#endif
    
    SCM generic = scm_method_generic_function(method);
    if (scm_is_true(generic)) {
        SCM generic_name = scm_generic_function_name(generic);
        if (scm_is_true(generic_name)) {
            char *name_str = scm_to_utf8_string_safe(generic_name);
            fprintf(stream, "  Generic function: %s\n", name_str ? name_str : "<unnamed>");
            if (name_str) free(name_str);
        }
    }
}

static void print_hierarchy_recursive(SCM cls, int depth, FILE *stream) {
    for (int i = 0; i < depth; i++) {
        fprintf(stream, "  ");
    }
    
    SCM name = scm_class_name(cls);
    if (scm_is_true(name)) {
        char *name_str = scm_to_utf8_string_safe(name);
        fprintf(stream, "%s\n", name_str ? name_str : "<unnamed>");
        if (name_str) free(name_str);
    } else {
        fprintf(stream, "<unnamed class>\n");
    }
    
    SCM direct_subs = scm_class_direct_subclasses(cls);
    SCM current = direct_subs;
    
    while (!scm_is_null(current)) {
        print_hierarchy_recursive(scm_car(current), depth + 1, stream);
        current = scm_cdr(current);
    }
}

void oop_print_class_hierarchy(SCM class, FILE *stream) {
    if (!stream) stream = stderr;
    if (!oop_validate_class(class)) return;
    
    fprintf(stream, "Class Hierarchy:\n");
    print_hierarchy_recursive(class, 0, stream);
}

void scm_trace_method_dispatch(bool enable) {
    g_oop_state.method_dispatch_traced = enable;
    
    if (enable) {
        gig_debug("Method dispatch tracing enabled");
    } else {
        gig_debug("Method dispatch tracing disabled");
    }
}

bool scm_is_method_dispatch_traced(void) {
    return g_oop_state.method_dispatch_traced;
}

// ============================================================================
// C Integration Helpers
// ============================================================================

bool scm_register_c_type(const CTypeInfo *type_info) {
    if (!ensure_oop_initialized() || !type_info || !type_info->c_type_name) {
        set_oop_error(OOP_ERROR_INVALID_CLASS);
        return false;
    }
    
    if (!g_c_type_registry) {
        set_oop_error(OOP_ERROR_NOT_INITIALIZED);
        return false;
    }
    
    // Store a copy of the type info
    CTypeInfo *info_copy = xmalloc(sizeof(CTypeInfo));
    *info_copy = *type_info;
    info_copy->c_type_name = xstrdup(type_info->c_type_name);
    
    bool success = hashmap_insert_string(g_c_type_registry, type_info->c_type_name, info_copy);
    
    if (success) {
        gig_debug("Registered C type: %s", type_info->c_type_name);
        clear_oop_error();
    } else {
        xfree((void*)info_copy->c_type_name);
        xfree(info_copy);
        set_oop_error(OOP_ERROR_INVALID_CLASS);
    }
    
    return success;
}

SCM scm_get_c_type_class(const char *c_type_name) {
    if (!ensure_oop_initialized() || !c_type_name) {
        set_oop_error(OOP_ERROR_INVALID_CLASS);
        return SCM_BOOL_F;
    }
    
    if (!g_c_type_registry) {
        set_oop_error(OOP_ERROR_NOT_INITIALIZED);
        return SCM_BOOL_F;
    }
    
    CTypeInfo *info;
    if (hashmap_get_string(g_c_type_registry, c_type_name, &info)) {
        clear_oop_error();
        return info->goops_class;
    }
    
    set_oop_error(OOP_ERROR_INVALID_CLASS);
    return SCM_BOOL_F;
}

SCM scm_wrap_c_object(void *ptr, const char *c_type_name) {
    if (!ptr || !c_type_name) {
        set_oop_error(OOP_ERROR_INVALID_INSTANCE);
        return SCM_BOOL_F;
    }
    
    SCM class = scm_get_c_type_class(c_type_name);
    if (scm_is_false(class)) {
        return SCM_BOOL_F;
    }
    
    SCM foreign_obj = scm_from_pointer(ptr, NULL);
    return scm_make_with_value(class, foreign_obj);
}

void *scm_unwrap_c_object(SCM obj, const char *c_type_name) {
    if (!scm_is_instance(obj) || !c_type_name) {
        set_oop_error(OOP_ERROR_INVALID_INSTANCE);
        return NULL;
    }
    
    SCM expected_class = scm_get_c_type_class(c_type_name);
    if (scm_is_false(expected_class)) {
        return NULL;
    }
    
    if (!scm_object_is_instance_of(obj, expected_class)) {
        set_oop_error(OOP_ERROR_INVALID_INSTANCE);
        return NULL;
    }
    
    SCM value = scm_get_value_slot(obj);
    if (scm_is_false(value)) {
        return NULL;
    }
    
    clear_oop_error();
    return scm_to_pointer(value);
}

// ============================================================================
// Performance and Optimization
// ============================================================================

void scm_clear_method_cache(void) {
    if (!ensure_oop_initialized()) return;
    
    SCM clear_cache_proc = scm_c_public_ref_safe("oop goops", "clear-method-cache!");
    if (scm_is_procedure(clear_cache_proc)) {
        scm_call_0(clear_cache_proc);
        gig_debug("Method cache cleared");
    }
}

void scm_optimize_generic(SCM generic) {
    if (!ensure_oop_initialized() || !scm_is_generic(generic)) return;
    
    // This is a placeholder for generic function optimization
    // In a real implementation, this might:
    // - Precompute method dispatch tables
    // - Optimize method ordering
    // - Cache frequently used method combinations
    
    gig_debug("Optimized generic function");
}

size_t scm_get_method_cache_size(void) {
    if (!ensure_oop_initialized()) return 0;
    
    SCM cache_size_proc = scm_c_public_ref_safe("oop goops", "method-cache-size");
    if (scm_is_procedure(cache_size_proc)) {
        SCM size_scm = scm_call_0(cache_size_proc);
        if (scm_is_integer(size_scm)) {
            return scm_to_size_t(size_scm);
        }
    }
    
    return 0;
}

// ============================================================================
// System Introspection
// ============================================================================

SCM scm_get_all_classes(void) {
    if (!ensure_oop_initialized()) return SCM_BOOL_F;
    
    SCM all_classes_proc = scm_c_public_ref_safe("oop goops", "all-classes");
    if (scm_is_procedure(all_classes_proc)) {
        return scm_call_0(all_classes_proc);
    }
    
    return SCM_EOL;
}

size_t scm_get_class_count(void) {
    SCM all_classes = scm_get_all_classes();
    return scm_is_list(all_classes) ? scm_c_length(all_classes) : 0;
}

size_t scm_get_generic_count(void) {
    if (!ensure_oop_initialized()) return 0;
    
    SCM all_generics_proc = scm_c_public_ref_safe("oop goops", "all-generic-functions");
    if (scm_is_procedure(all_generics_proc)) {
        SCM all_generics = scm_call_0(all_generics_proc);
        return scm_is_list(all_generics) ? scm_c_length(all_generics) : 0;
    }
    
    return 0;
}
