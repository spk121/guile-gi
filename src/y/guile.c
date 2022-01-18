#include <stdio.h>
#include <libguile.h>

void *dynfree(void *mem)
{
    if (mem)
        scm_dynwind_free(mem);
    else {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    return mem;
}

SCM
scm_class_set_x(SCM cls, SCM slot, SCM val)
{
    static SCM class_set_proc = SCM_BOOL_F;
    if (scm_is_false(class_set_proc))
        class_set_proc = scm_c_public_ref("oop goops", "class-slot-set!");
    return scm_call_3(class_set_proc, cls, slot, val);
}

SCM
scm_c_list_ref(SCM list, size_t k)
{
    return scm_list_ref(list, scm_from_size_t(k));
}

size_t
scm_c_length(SCM list)
{
    return scm_to_size_t(scm_length(list));
}

int
scm_is_list(SCM obj)
{
    return scm_is_true(scm_list_p(obj));
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
scm_class_ref(SCM cls, SCM slot)
{
    static SCM class_ref_proc = SCM_UNDEFINED;
    if (SCM_UNBNDP(class_ref_proc))
        class_ref_proc = scm_c_public_ref("oop goops", "class-slot-ref");

    return scm_call_2(class_ref_proc, cls, slot);
}

int
scm_is_equal(SCM a, SCM b)
{
    return scm_is_true(scm_equal_p(a,b));
}

SCM
scm_drop_right_1(SCM lst)
{
    static SCM srfi1_drop_right_proc = SCM_UNSPECIFIED;
    if (SCM_UNBNDP(srfi1_drop_right_proc))
        srfi1_drop_right_proc = scm_c_public_ref("srfi srfi-1", "drop-right");
    return scm_call_2(srfi1_drop_right_proc, lst, scm_from_int(1));
}
