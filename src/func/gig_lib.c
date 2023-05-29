#include "../core.h"
#include "gig_lib.h"
#include <string.h>
#ifdef __MINGW32__
#include <windows.h>
#else
#include <dlfcn.h>
#endif

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

#ifdef __MINGW32__
static const char *
dlerror ()
{
    static char msg1[256], msg2[256];
    DWORD dw = GetLastError ();
    FormatMessage (FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
		   NULL,
		   dw,
		   MAKELANGID (LANG_NEUTRAL, SUBLANG_DEFAULT),
		   (LPTSTR) msg1, 256, NULL);
    if (dw == 0)
	return NULL;

    snprintf (msg2, 255, "error %ld: %s", (long) dw, msg1);
    return msg2;
}
#endif

static SCM
gig_il_library(SCM s_namespace_, SCM s_version, SCM s_path_list)
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
        return SCM_EOL;
    }
    n = scm_c_length(s_path_list);
    shlib = allocate_shlib(n);
    for (i = 0; i < n; i++)
        shlib->filenames[i] = scm_to_locale_string(scm_c_list_ref(s_path_list, i));
    for (i = 0; i < n; i++) {
#ifdef __MINGW32__
        shlib->handles[i] = LoadLibrary(shlib->filenames[i]);
#else
        shlib->handles[i] = dlopen(shlib->filenames[i], RTLD_NOW);
#endif
        if (shlib->handles[i] == NULL)
            goto err;
    }
    strval_add_entry(lib_cache, namespace_, (uintptr_t) shlib);
    gig_debug_load("dynamically loaded namespace '%s'", namespace_);
    free(namespace_);
    return SCM_EOL;

  err:
    memset(errmsg, 0, 256);
#ifdef __MINGW32__
    {
#define DLERROR_LEN 80
	static char dlerror_str[DLERROR_LEN + 1];
	DWORD dw = GetLastError ();
	FormatMessage (FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
		       NULL,
		       dw,
		       MAKELANGID (LANG_NEUTRAL, SUBLANG_DEFAULT),
		       (LPTSTR) dlerror_str, DLERROR_LEN, NULL);
	if (dw == 0)
	    snprintf (errmsg, 255, "No error");
	else
	    snprintf (errmsg, 255, "error %ld: %s", (long) dw, dlerror_str);
    }
#else    
    strncpy(errmsg, dlerror(), 255);
#endif
    free_shlib(shlib);
    free(namespace_);
    scm_syserror_msg(FUNC_NAME, "failed to dynamically load namespace '~a' - ~a",
                     scm_list_2(s_namespace_, scm_from_utf8_string(errmsg)), 0);
    return SCM_EOL;
#undef FUNC_NAME
}

void *
gig_lib_lookup(const char *namespace_, const char *symbol)
{
    GigSharedLib *shlib;
    void *address;
    int i;

    assert(namespace_ != NULL);
    assert(strlen(namespace_) > 0);
    assert(symbol != NULL);
    assert(strlen(symbol) > 0);

    if (!lib_cache)
        return NULL;
    shlib = (GigSharedLib *) strval_find_entry(lib_cache, namespace_);
    if (!shlib)
        return NULL;
    i = 0;
    while (i < shlib->n) {
#ifdef __MINGW32__
	address = GetProcAddress (shlib->handles[i], symbol);
#else
        address = dlsym(shlib->handles[i], symbol);
#endif
        dlerror();
        if (address)
            return address;
        i++;
    }
    return NULL;
}

void **
gig_lib_all_handles()
{
    void **handles;
    int i, j, n;

    if (lib_cache == NULL)
        return NULL;

    n = 0;
    for (j = 0; j < lib_cache->len; j++) {
        GigSharedLib *shlib = (GigSharedLib *) lib_cache->entries[j].val;
        n += shlib->n;
    }
    handles = (void **)xcalloc(n + 1, sizeof(void *));
    n = 0;
    for (j = 0; j < lib_cache->len; j++) {
        GigSharedLib *shlib = (GigSharedLib *) lib_cache->entries[j].val;
        for (i = 0; i < shlib->n; i++) {
            handles[n++] = shlib->handles[i];
        }
    }
    return handles;
}

static GigSharedLib *
allocate_shlib(int n)
{
    GigSharedLib *shlib;
    if (n == 0)
        return NULL;

    shlib = xcalloc(1, sizeof(GigSharedLib));
    shlib->n = n;
    shlib->filenames = xcalloc(n, sizeof(char *));
    shlib->handles = xcalloc(n, sizeof(void *));
    return shlib;
}

static void
free_shlib(GigSharedLib * shlib)
{
    if (shlib == NULL)
        return;

    int n = shlib->n;
    for (int i = 0; i < n; i++) {
        if (shlib->handles[i])
#ifdef __MINGW32__
            FreeLibrary(shlib->handles[i]);
#else
            dlclose(shlib->handles[i]);
#endif
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
gig_init_lib(void)
{
    scm_c_define_gsubr("^library", 3, 0, 0, gig_il_library);
}
