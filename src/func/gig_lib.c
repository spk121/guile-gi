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

void
gig_lib_add(const char *namespace_, const char *version, const char **so_list, size_t n)
{
    int i;
    GigSharedLib *shlib;

    if (lib_cache == NULL)
        lib_cache = strval_new();

    shlib = xcalloc(1, sizeof(GigSharedLib));
    shlib->n = n;
    shlib->files = xcalloc(n, sizeof(char *));
    shlib->handles = xcalloc(n, sizeof(void *));
    for (i = 0; i < n; i++)
        shlib->files[i] = xstrdup(so_list[i]);
    for (i = 0; i < n; i++) {
        shlib->handles[i] = dlopen(shlib->files[i], RTLD_NOW);
        if (shlib->handles[i] == NULL)
            goto err;
    }
    strval_add_entry(lib_cache, namespace_, (uintptr_t) shlib);
    return;

  err:
    memset(errmsg, 0, 256);
    strncpy(errmsg, dlerror(), 255);
    for (i = 0; i < n; i++) {
        if (shlib->handles[i])
            dlclose(shlib->handles[i]);
        free(shlib->files[i]);
    }
    free(shlib);
    scm_syserror_msg("%gig-lib-add", "dlopen ~a ~a - ~a",
                     scm_list_3(scm_from_utf8_string(namespace_),
                                scm_from_utf8_string(version), scm_from_utf8_string(errmsg)), 0);
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
