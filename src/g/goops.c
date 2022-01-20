#include "guile.h"
#include "goops.h"

static SCM applicable_type;
static SCM applicable_struct_type;
static SCM applicable_struct_with_setter_type;
static SCM char_type;
static SCM hashtable_type;
static SCM list_type;
static SCM method_type;
static SCM integer_type;
static SCM real_type;
static SCM string_type;
static SCM top_type;

static SCM add_method_proc;
static SCM class_slot_set_proc;
static SCM class_slot_ref_proc;
static SCM ensure_accessor_proc;
static SCM ensure_generic_proc;
static SCM make_proc;
static SCM make_class_proc;
static SCM srfi1_drop_right_proc;

static SCM name_kwd;
static SCM value_kwd;
static SCM formals_kwd;
static SCM procedure_kwd;
static SCM specializers_kwd;

static SCM obarray_sym;
static SCM procedure_sym;
static SCM ref_sym;
static SCM size_sym;
static SCM unref_sym;
static SCM value_sym;

SCM
scm_get_applicable_class(void)
{
    return applicable_type;
}

SCM
scm_get_applicable_struct_class(void)
{
    return applicable_struct_type;
}

SCM
scm_get_applicable_struct_with_setter_class(void)
{
    return applicable_struct_with_setter_type;
}

SCM
scm_get_char_class(void)
{
    return char_type;
}

SCM
scm_get_hashtable_class(void)
{
    return hashtable_type;
}

SCM
scm_get_integer_class(void)
{
    return integer_type;
}

SCM
scm_get_list_class(void)
{
    return list_type;
}

SCM
scm_get_method_class(void)
{
    return method_type;
}

SCM
scm_get_real_class(void)
{
    return real_type;
}

SCM
scm_get_string_class(void)
{
    return string_type;
}

SCM
scm_get_top_class(void)
{
    return top_type;
}

SCM
scm_add_method(SCM target, SCM method)
{
    return scm_call_2(add_method_proc, target, method);
}

static SCM
scm_drop_right_1(SCM lst)
{
    return scm_call_2(srfi1_drop_right_proc, lst, scm_from_int(1));
}


SCM
scm_define_methods_from_procedure(const char *public_name, SCM proc, int opt, SCM formals,
                                  SCM specializers)
{
    if (public_name == NULL)
        return SCM_UNDEFINED;

    SCM sym_public_name = scm_from_utf8_symbol(public_name);
    SCM generic = guile_get_default_definition(sym_public_name);
    if (!scm_is_generic(generic))
        generic = scm_call_2(ensure_generic_proc, generic, sym_public_name);

    SCM t_formals = formals, t_specializers = specializers;

    do {
        SCM mthd = scm_call_7(make_proc,
                              method_type,
                              specializers_kwd, t_specializers,
                              formals_kwd, t_formals,
                              procedure_kwd, proc);

        scm_call_2(add_method_proc, generic, mthd);

        if (scm_is_eq(t_formals, SCM_EOL))
            break;

        t_formals = scm_drop_right_1(t_formals);
        t_specializers = scm_drop_right_1(t_specializers);
    } while (opt-- > 0);

    scm_define(sym_public_name, generic);
    return sym_public_name;
}

SCM
scm_ensure_accessor_with_name(SCM proc, SCM name)
{
    return scm_call_2(ensure_accessor_proc, proc, name);
}

static SCM
scm_class_slot_ref(SCM cls, SCM slot)
{
    return scm_call_2(class_slot_ref_proc, cls, slot);
}

SCM
scm_get_class_ref_slot(SCM cls)
{
    return scm_class_slot_ref(cls, ref_sym);
}

SCM
scm_get_class_size_slot(SCM cls)
{
    return scm_class_slot_ref(cls, size_sym);
}

SCM
scm_get_class_unref_slot(SCM cls)
{
    return scm_class_slot_ref(cls, unref_sym);
}

SCM
scm_get_value_slot(SCM instance)
{
    return scm_slot_ref(instance, value_sym);
}

SCM
scm_make_class_with_name(SCM supers, SCM slots, SCM name)
{
    return scm_call_4(make_class_proc, supers, slots, name_kwd, name);
}

SCM
scm_make_with_value(SCM type, SCM value)
{
    return scm_call_3(make_proc, type, value_kwd, value);
}

SCM
scm_make_method(SCM specializers, SCM formals, SCM proc)
{
    return scm_call_7(make_proc, method_type,
                      specializers_kwd, specializers, formals_kwd, formals, procedure_kwd, proc);
}

static SCM
scm_set_class_slot(SCM cls, SCM slot, SCM val)
{
    return scm_call_3(class_slot_set_proc, cls, slot, val);
}


SCM
scm_set_class_obarray_slot(SCM cls, SCM obarray)
{
    return scm_set_class_slot(cls, obarray_sym, obarray);
}

SCM
scm_set_class_ref_slot(SCM cls, SCM func)
{
    return scm_set_class_slot(cls, ref_sym, func);
}

SCM
scm_set_class_size_slot(SCM cls, SCM func)
{
    return scm_set_class_slot(cls, size_sym, func);
}

SCM
scm_set_class_unref_slot(SCM cls, SCM func)
{
    return scm_set_class_slot(cls, unref_sym, func);
}

SCM
scm_set_procedure_slot(SCM instance, SCM proc)
{
    scm_slot_set_x(instance, procedure_sym, proc);
}

void
init_core_goops(void)
{
    static int first = 1;
    if (first) {
        first = 0;
        obarray_sym = scm_from_utf8_symbol("obarray");
        procedure_sym = scm_from_utf8_symbol("procedure");
        ref_sym = scm_from_utf8_symbol("ref");
        size_sym = scm_from_utf8_symbol("size");
        unref_sym = scm_from_utf8_symbol("unref");
        value_sym = scm_from_utf8_symbol("value");

        name_kwd = scm_from_utf8_keyword("name");
        value_kwd = scm_from_utf8_keyword("value");
        specializers_kwd = scm_from_utf8_keyword("specializers");
        formals_kwd = scm_from_utf8_keyword("formals");
        procedure_kwd = scm_from_utf8_keyword("procedure");



        applicable_type = scm_c_public_ref("oop goops", "<applicable>");
        applicable_struct_type = scm_c_public_ref("oop goops", "<applicable-struct>");
        applicable_struct_with_setter_type =
            scm_c_public_ref("oop goops", "<applicable-struct-with-setter>");
        char_type = scm_c_public_ref("oop goops", "<char>");
        integer_type = scm_c_public_ref("oop goops", "<integer>");
        real_type = scm_c_public_ref("oop goops", "<real>");
        hashtable_type = scm_c_public_ref("oop goops", "<hashtable>");
        list_type = scm_c_public_ref("oop goops", "<list>");
        method_type = scm_c_public_ref("oop goops", "<method>");
        string_type = scm_c_public_ref("oop goops", "<string>");
        top_type = scm_c_public_ref("oop goops", "<top>");

        add_method_proc = scm_c_public_ref("oop goops", "add-method!");
        ensure_accessor_proc = scm_c_public_ref("oop goops", "ensure-accessor");
        ensure_generic_proc = scm_c_public_ref("oop goops", "ensure-generic");
        make_proc = scm_c_public_ref("oop goops", "make");
        make_class_proc = scm_c_public_ref("oop goops", "make-class");
        srfi1_drop_right_proc = scm_c_public_ref("srfi srfi-1", "drop-right");

        class_slot_ref_proc = scm_c_public_ref("oop goops", "class-slot-ref");
        class_slot_set_proc = scm_c_public_ref("oop goops", "class-slot-set!");
    }
}
