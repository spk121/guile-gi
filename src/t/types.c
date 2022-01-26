#include <girepository.h>
#include <libguile.h>



void init_t_types(void)
{
    static int first = 1;
    if (first) {
        first = 0;
        scm_c_define ("%object-ref-sink", scm_from_pointer(g_object_ref_sink, NULL));
        scm_c_define ("%object-unref", scm_from_pointer(g_object_unref, NULL));
        scm_c_define ("%param-spec-ref-sink", scm_from_pointer(g_param_spec_ref_sink, NULL));
        scm_c_define ("%param-spec-unref", scm_from_pointer(g_param_spec_unref, NULL));
        scm_c_define ("%variant-ref-sink", scm_from_pointer(g_variant_ref_sink, NULL));
        scm_c_define ("%variant-unref", scm_from_pointer(g_variant_unref, NULL));
    }
}
