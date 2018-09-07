#include "GirtestTriv.h"

struct _GirtestTriv
{
  GObject parent_instance;
};

G_DEFINE_TYPE (GirtestTriv, girtest_triv, G_TYPE_OBJECT);

static void
girtest_triv_class_init (GirtestTrivClass *klass)
{
}

static void
girtest_triv_init (GirtestTriv *self)
{
}

static int calls;
void
girtest_triv_func_void__void (void)
{
  calls ++;
}

void
girtest_triv_func_void__int (int x)
{
  calls ++;
}
