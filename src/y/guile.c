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
    static SCM class_set_proc = SCM_UNSPECIFIED;
    if (SCM_UNBNDP(class_set_proc))
        class_set_proc = scm_c_public_ref("oop goops", "class-slot-set!");
    return scm_call_3(class_set_proc, cls, slot, val);
}
