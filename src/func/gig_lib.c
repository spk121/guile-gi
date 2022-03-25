#include "../core.h"
#include "gig_lib.h"
#include <dlfcn.h>
#include <string.h>

typedef struct GigSharedLib_
{
    int n;
    char **filenames;
    void **handles;
} GigSharedLib;

static GigSharedLib *allocate_shlib(int n);
static void free_shlib(GigSharedLib * shlib);
static bool is_nonempty_list_of_strings(SCM lst);

static strval_t *lib_cache = NULL;
static char errmsg[256];

SCM
gig_il_library(SCM s_namespace_, SCM s_path_list)
{
#define FUNC_NAME "^library"
    char *namespace_;
    int i, n;
    GigSharedLib *shlib;

    SCM_ASSERT_TYPE(scm_is_string(s_namespace_), s_namespace_, SCM_ARG1, FUNC_NAME, "string");
    SCM_ASSERT_TYPE(scm_is_list(s_path_list), s_path_list, SCM_ARG2, FUNC_NAME, "list");

    if (scm_is_true(scm_string_null_p(s_namespace_)))
        scm_wrong_type_arg(FUNC_NAME, SCM_ARG1, s_namespace_);
    if (!is_nonempty_list_of_strings(s_path_list))
        scm_wrong_type_arg(FUNC_NAME, SCM_ARG2, s_path_list);

    if (lib_cache == NULL)
        lib_cache = strval_new();

    namespace_ = scm_to_utf8_string(s_namespace_);
    if (strval_find_entry(lib_cache, namespace_) != 0) {
        gig_debug_load("namespace '%s' is already dynamically loaded", namespace_);
        free(namespace_);
        return SCM_BOOL_F;
    }
    n = scm_c_length(s_path_list);
    shlib = allocate_shlib(n);
    for (i = 0; i < n; i++)
        shlib->filenames[i] = scm_to_locale_string(scm_c_list_ref(s_path_list, i));
    for (i = 0; i < n; i++) {
        shlib->handles[i] = dlopen(shlib->filenames[i], RTLD_NOW);
        if (shlib->handles[i] == NULL)
            goto err;
    }
    strval_add_entry(lib_cache, namespace_, (uintptr_t) shlib);
    gig_debug_load("dynamically loaded namespace '%s'", namespace_);
    free(namespace_);
    return SCM_BOOL_T;

  err:
    memset(errmsg, 0, 256);
    strncpy(errmsg, dlerror(), 255);
    free_shlib(shlib);
    free(namespace_);
    scm_syserror_msg(FUNC_NAME, "failed to dynamically load namespace '~a' - ~a",
                     scm_list_2(s_namespace_, scm_from_utf8_string(errmsg)), 0);
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

static GigSharedLib *
allocate_shlib(int n)
{
    GigSharedLib *shlib;
    shlib = xcalloc(1, sizeof(GigSharedLib));
    shlib->n = n;
    shlib->filenames = xcalloc(n, sizeof(char *));
    shlib->handles = xcalloc(n, sizeof(void *));
    return shlib;
}

static void
free_shlib(GigSharedLib * shlib)
{
    int n = shlib->n;
    for (int i = 0; i < n; i++) {
        if (shlib->handles[i])
            dlclose(shlib->handles[i]);
        free(shlib->filenames[i]);
    }
    free(shlib);
}

static bool
is_nonempty_list_of_strings(SCM lst)
{
    int n;
    SCM entry;

    n = scm_c_length(lst);
    if (n == 0)
        return false;
    for (int i = 0; i < n; i++) {
        entry = scm_c_list_ref(lst, i);
        if (!scm_is_string(entry))
            return false;
        if (scm_is_true(scm_string_null_p(entry)))
            return false;
    }
    return true;
}

void
gig_lib_init(void)
{
    scm_c_define_gsubr("^library", 2, 0, 0, gig_il_library);
}
