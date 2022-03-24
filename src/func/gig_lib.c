#include "../core.h"
#include "gig_lib.h"
#include <dlfcn.h>
#include <string.h>

typedef struct GigSharedLib_
{
    int n;
    char **files;
    void **handles;
} GigSharedLib;

static strval_t *lib_cache = NULL;
static char errmsg[256];

SCM
gig_il_library(SCM s_namespace_, SCM s_version, SCM s_solist)
{
#define FUNC_NAME "^library"
    char *namespace_;
    int i, n;
    GigSharedLib *shlib;
    uint64_t val;
    SCM entry;

    SCM_ASSERT_TYPE(scm_is_string(s_namespace_), s_namespace_, SCM_ARG1, FUNC_NAME, "string");
    SCM_ASSERT_TYPE(scm_is_string(s_version), s_version, SCM_ARG2, FUNC_NAME, "string");
    SCM_ASSERT_TYPE(scm_is_list(s_solist), s_solist, SCM_ARG3, FUNC_NAME, "list");

    if (scm_is_true(scm_string_null_p(s_namespace_)))
        scm_wrong_type_arg(FUNC_NAME, SCM_ARG1, s_namespace_);
    n = scm_c_length(s_solist);
    if (n == 0)
        scm_wrong_type_arg(FUNC_NAME, SCM_ARG3, s_solist);
    for (i = 0; i < n; i++) {
        entry = scm_c_list_ref(s_solist, i);
        if (!scm_is_string(entry))
            scm_wrong_type_arg(FUNC_NAME, SCM_ARG3, s_solist);
        if (scm_is_true(scm_string_null_p(entry)))
            scm_wrong_type_arg(FUNC_NAME, SCM_ARG3, s_solist);
    }

    if (lib_cache == NULL)
        lib_cache = strval_new();

    namespace_ = scm_to_utf8_string(s_namespace_);
    val = strval_find_entry(lib_cache, namespace_);
    if (val != 0) {
        gig_warning_load("library '%s' is already loaded", namespace_);
        free(namespace_);
        return SCM_BOOL_F;
    }
    shlib = xcalloc(1, sizeof(GigSharedLib));
    shlib->n = n;
    shlib->files = xcalloc(n, sizeof(char *));
    shlib->handles = xcalloc(n, sizeof(void *));
    for (i = 0; i < n; i++)
        shlib->files[i] = scm_to_utf8_string(scm_c_list_ref(s_solist, i));
    for (i = 0; i < n; i++) {
        shlib->handles[i] = dlopen(shlib->files[i], RTLD_NOW);
        if (shlib->handles[i] == NULL)
            goto err;
    }
    strval_add_entry(lib_cache, namespace_, (uintptr_t) shlib);
    free(namespace_);
    return SCM_BOOL_T;

  err:
    memset(errmsg, 0, 256);
    strncpy(errmsg, dlerror(), 255);
    for (i = 0; i < n; i++) {
        if (shlib->handles[i])
            dlclose(shlib->handles[i]);
        free(shlib->files[i]);
    }
    free(shlib);
    free(namespace_);
    scm_syserror_msg(FUNC_NAME, "dlopen ~a ~a - ~a",
                     scm_list_3(s_namespace_, s_version, scm_from_utf8_string(errmsg)), 0);
    return SCM_BOOL_F;
#undef FUNC_NAME
}

void *
gig_lib_lookup(const char *namespace_, const char *symbol)
{
    GigSharedLib *shlib;
    void *handle;
    int i;

    if (!lib_cache)
        return NULL;
    shlib = (GigSharedLib *) strval_find_entry(lib_cache, namespace_);
    if (!shlib)
        return NULL;
    i = 0;
    while (i < shlib->n) {
        handle = dlsym(shlib->handles[i], symbol);
        dlerror();
        if (handle)
            return handle;
        i++;
    }
    return NULL;
}

void
gig_lib_init(void)
{
    scm_c_define_gsubr("^library", 3, 0, 0, gig_il_library);
}
