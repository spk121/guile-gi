#ifndef XGUILE_H
#define XGUILE_H

#include <libguile.h>

typedef SCM SCM_OBJECT;
typedef SCM SCM_TYPE_OBJECT;


/* I think this test is "necessary but not complete". */
int
scm_is_foreign_object_type (SCM type)
{
  SCM layout;
  size_t i;
  size_t n;
  char *layout_chars;
  
  if (scm_is_false (scm_struct_vtable_p (v)))
    return FALSE;

  layout = scm_struct_ref (type, scm_from_int (0));

  if (!scm_is_symbol (layout))
    return FALSE;

  layout_chars = scm_to_utf8_symbol (layout);
  n = strlen(layout_chars) / 2;
  for (i = 0; i < n; i ++)
    if (layout_chars[i * 2] != 'u') {
      free (layout_chars);
      return FALSE;
    }

  free (layout_chars);
  return TRUE;
}

int
scm_is_struct (SCM x)
{
  return scm_is_true (scm_struct_p (x));
}

#endif
