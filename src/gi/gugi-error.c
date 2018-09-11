#include <libguile.h>
#include <glib-object.h>
#include "gugi-type.h"

SCM ScmGError = SCM_UNSPECIFIED;

SCM scm_glib_error_key;

/**
 * gir_error_marshal_to_py:
 * @error: a pointer to the GError.
 *
 * Checks to see if @error has been set.  If @error has been set, then 
 * the information necessary to throw a Guile error is returned (but not raised)
 *
 * Returns: a list of (key subr message args data) or SCM_BOOL_F
 */
SCM
gir_error_marshal_to_scm (GError **error)
{
  // PyGILState_STATE state;
  //PyObject *exc_type;
  //PyObject *exc_instance;
    const char *domain = NULL;

    g_return_val_if_fail(error != NULL, NULL);

    if (*error == NULL)
        return SCM_BOOL_F;

    // state = PyGILState_Ensure();

    // exc_type = PyGError;

    if ((*error)->domain) {
        domain = g_quark_to_string ((*error)->domain);
    }

    /* exc_instance = PyObject_CallFunction (exc_type, "ssi", */
    /*                                       (*error)->message, */
    /*                                       domain, */
    /*                                       (*error)->code); */

    /* PyGILState_Release(state); */

    return scm_list_5(scm_glib_error_key,
		      SCM_BOOL_F,
		      scm_from_utf8_string((*error)->message),
		      SCM_BOOL_F,
		      scm_list_2 (scm_from_utf8_string (domain), scm_from_int ((*error)->code)));
}

/**
 * scm_error_marshal_from_scm:
 * @scmerr: #f or a 5-element list of (key subr message args data)
 * @error: a standard GLib GError ** output parameter
 *
 * Converts from the contents of a Guile implemented scm_error into a GError.
 *
 * Returns: TRUE, otherwise a misc-error is thrown
 */
gboolean
gir_error_marshal_from_scm (SCM scmerr, GError **error)
{
    gint code;
    gchar *message = NULL;
    gchar *domain = NULL;
    gboolean res = FALSE;

    if (!scm_is_true (scm_list_p (scmerr))
	|| scm_to_int (scm_length (scmerr)) != 5
	|| scm_is_false (scm_eqv_p (scm_car(scmerr), scm_glib_error_key)))
	scm_misc_error ("error marshalling", "error must be a Glib error, not ~A",
			scm_list_1 (scm_car (scmerr)));

    SCM msg = scm_list_ref (scmerr, scm_from_int (2));
    if (!scm_is_string (msg))
	scm_misc_error ("error marshalling", "Glib errors must have a 'message' string", SCM_EOL);
	
    SCM data = scm_list_ref (scmerr, scm_from_int (4));
    if (!scm_is_true (scm_list_p (data))
	|| scm_to_int (scm_length (data)) != 2)
      scm_misc_error ("error marshalling", "Glib error instances must have a 2-element data list", SCM_EOL);

    if (!scm_is_string (scm_car (data)))
      scm_misc_error ("error marshalling", "Glib error instances must have a 'domain' string", SCM_EOL);

    if (!scm_is_exact_integer (scm_cadr (data)))
      scm_misc_error ("error marshalling", "Glib error instances must have a ''code' int attribute", SCM_EOL);

    domain = scm_to_utf8_string (scm_car (data));
    code = scm_to_int (scm_cadr (data));
    message = scm_to_utf8_string (msg);
    g_set_error_literal (error,
                         g_quark_from_string (domain),
                         code,
                         message);
    g_free (domain);
    g_free (message);
    return TRUE;
}


static SCM
scmgerror_from_gvalue (const GValue *value)
{
    GError *gerror = (GError *) g_value_get_boxed (value);
    SCM scmerr = gir_error_marshal_to_scm (&gerror);
    if (scmerr == SCM_BOOL_F) {
      return gir_NONE;
    } else {
      return scmerr;
    }
}

static int
scmgerror_to_gvalue (GValue *value, SCM scmerror)
{
    GError *gerror = NULL;

    if (gir_error_marshal_from_scm (scmerror, &gerror)) {
        g_value_take_boxed (value, gerror);
        return 0;
    }

    return -1;
}

void gir_error_register_types(void)
{
  ScmGError = scm_c_public_ref ("gir _error", "GError");
  scm_glib_error_key = scm_permanent_object (scm_from_utf8_symbol ("glib-error"));
  gir_register_gtype_custom (G_TYPE_ERROR,
			     scmgerror_from_gvalue,
			     scmgerror_to_gvalue);
}
