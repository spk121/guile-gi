#ifndef __GIRTEST_TRIV_H__
#define __GIRTEST_TRIV_H__

#include <glib-object.h>
G_BEGIN_DECLS
#define GIRTEST_TYPE_TRIV girtest_triv_get_type()

G_DECLARE_FINAL_TYPE (GirtestTriv, girtest_triv, GIRTEST, TRIV, GObject)

GirtestTriv *girtest_triv_new (void);

/**
 * SECTION: simple_funcs
 *
 * Functions of basic c types with no pointers.
 */

/**
 * girtest_triv_func_void__void
 *
 * Test func with no params and no returns
 */
void
girtest_triv_func_void__void (void);

/**
 * girtest_triv_func_void__int
 *
 * Test func with one integer params and no returns
 */
void
girtest_triv_func_void__int (int x);

G_END_DECLS

#endif
