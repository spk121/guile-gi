#pragma once

#include <libguile.h>
#include <girepository.h>

gboolean _gir_is_r5rs_keyword (const gchar *name);
SCM _gir_info_new (GIBaseInfo *info);
void gir_info_register_types (void);
