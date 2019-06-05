#include <glib-object.h>
#include <libguile.h>

/*
Say you want to make a SCM function callable as a closure.

Make a GClosure *closure;
GClosure must be a G_REAL_CLOSURE (?)
set GRealClosure *real_closure = G_REAL_CLOSURE(closure);
make sure closure->is_invalid is FALSE
set real_closure->meta_marshal to a conversion function
set real_closure->meta_marshal_data to a generic pointer of extra data

Then g_closure_invoke will call your meta_marshall.

void meta_marshall (GClosure *self,
	GValue *return_value,
	Guint n_param_values,
	const GValue *param_values
	gpointer invocation_hint,
	gpointer marshal_data)

meta_marshal should
 - convert the GValues to SCM
 - call a scheme procedure
 - capture the return value
 - convert the return value to a GValue
*/

struct _GScmClosurePrivate {
  SCM s_func;
};

void scm_marshaller (GClosure *closure,
		     GValue *return_value,
		     guint n_param_values,
		     const GValue *param_values,
		     gpointer invocation_hint,
		     gpointer marshal_data)
{
  SCM func = ((GScmClosurePrivate *) closure)->s_func;

  /* Wrap this in a catch */
  /* Check for parameter count */
  /* Convert the params. */
  /* Apply params to the scheme func */
  /* Get the reutrn value, and convert it back to a GValue */

  /* If we caught an error, do something. */
}

GClosure *make_scheme_closure (SCM func, char *extra_data)
{
  GScmClosurePrivate *priv = g_new0(GScmClosurePrivate, 1);
  priv->s_func = func;
  
  GClosure *clo = g_closure_new_simple (sizeof(GScmClosurePrivate),
					priv);
  g_closure_set_meta_marshall (clo, extra_data, g_scm_meta_marshall);

  return clo;
}


static void callback1 (int x)
{
  printf("In callback1.  received %d\n", x);
  fflush(stdout);
}

static void callback2(int x, char *user_data)
{
  printf("In callback2.  Received %d '%s'\n", x, user_data); 
  fflush(stdout);
  callback1 (x);
}

static void callback_destroy_notify (gpointer data,
				     GClosure *closure)
{
  printf("in callback_destroy_notify. Received %s and %p",
	 (char *)data,
	 closure);
  fflush(stdout);
  free (data);
}

int main (int argc, char **argv)
{
  GValue ret = G_VALUE_INIT;
  GValue *params;
  
  char *str = strdup("hello");
  params = g_new0(GValue, 2);
  
  scm_init_guile();

  GCClosure *closure = g_cclosure_new (callback1,
				      str,
				      callback_destroy_notify);
  void (*cb_func)(int x) = closure->callback;
  cb_func(10);

  int intval = 11;
  
  g_value_init (&(params[0]), G_TYPE_POINTER);
  g_value_set_pointer (&(params[0]), &intval);
  g_value_init (&(params[1]), G_TYPE_INT);
  g_value_set_int (&(params[1]), 11);
  g_value_init (&ret, G_TYPE_POINTER);
  g_closure_set_marshal (closure, g_cclosure_marshal_VOID__INT);
  g_closure_invoke(closure, &ret, 2, &(params[0]), NULL);
  //scm_shell(argc, argv);
  return 0;
  
}
