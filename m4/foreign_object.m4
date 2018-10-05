include(`foreach.m4')
define(`LOWERCASE', `gboxed')
define(`CAMELCASE', `GBoxed')
define(`SLOTS', `(gboxed, pofinalizer)')
define(`DECLARE_TYPE_STORE', dnl
`static SCM gi_$1_type;
static SCM gi_$1_type_store;')

define(`INIT_FUNC',
`void
gi_init_$1 (void)
{
    SCM name, slots;
    slots = scm_list_of_utf8_symbols( foreach(`x', SLOTS, `"x", ') NULL);
    name = scm_from_utf8_symbol("$2");
    scm_c_export("$2");
}')

#include <libguile.h>
#include <glib.h>
#include <glib-object.h>
#include "gir_xguile.h"

DECLARE_TYPE_STORE(LOWERCASE)

INIT_FUNC(LOWERCASE, CAMELCASE)
