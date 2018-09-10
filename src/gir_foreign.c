#include <libguile.h>
#include <glib-object.h>

static GPtrArray *foreign_structs = NULL;

static void
init_foreign_structs (void)
{
  foreign_structs = g_ptr_array_new();
}

void
gir_foreign_init (void)
{
  init_foreign_structs();
}
