#pragma once

#include <libguile.h>
#include <glib-object.h>

typedef SCM (* fromvaluefunc)(const GValue *value);
typedef int (*tovaluefunc)(GValue *value, SCM obj);

typedef struct {
    fromvaluefunc fromvalue;
    tovaluefunc tovalue;
} GirGTypeMarshal;

extern SCM gir_NONE;

void gir_register_gtype_custom(GType gtype,
                               fromvaluefunc from_func,
                               tovaluefunc to_func);
void gir_register_NONE_sigil (void);


