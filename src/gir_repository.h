#pragma once

#include <libguile.h>
#include <glib-object.h>
// #include "_gir_repository.h"

G_BEGIN_DECLS

extern SCM ScmGiRepository_type;

// extern SCM ScmGiRepositoryError;

void gir_repository_register_types (void);

G_END_DECLS
