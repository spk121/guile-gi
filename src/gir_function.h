#ifndef _GIR_FUNCTION_H_
#define _GIR_FUNCTION_H_

#include <glib.h>
#include <girepository.h>
#include <ffi.h>
#include <libguile.h>

extern SCM gir_function_type;
typedef SCM (*gir_gsubr_t)(void);

typedef struct _GirFunction
{
    GIFunctionInfo *function_info;
    ffi_closure *closure;
    ffi_cif cif;
    void *function_ptr;
    int n_required;
    int n_optional;
    char *name;
} GirFunction;

void gir_function_define_gsubr(const char *namespace_, const char *parent, GIFunctionInfo *info);
void gir_init_function(void);
#endif