/* -*- Mode: C; c-basic-offset: 4 -*- */
#ifndef XGUILE_H
#define XGUILE_H

#include <libguile.h>

SCM Gu_None_Type;
SCM SCM_NONE;

/* I think this test is "necessary but not complete". */
int
scm_is_foreign_object_type (SCM type)
{
  SCM layout;
  size_t i;
  size_t n;
  char *layout_chars;
  
  if (scm_is_false (scm_struct_vtable_p (type)))
    return 0;

  layout = scm_struct_ref (type, scm_from_int (0));

  if (!scm_is_symbol (layout))
    return 0;

  layout_chars = scm_to_utf8_string (layout);
  n = strlen(layout_chars) / 2;
  for (i = 0; i < n; i ++)
    if (layout_chars[i * 2] != 'u') {
      free (layout_chars);
      return 0;
    }

  free (layout_chars);
  return 1;
}

int
scm_is_list (SCM x)
{
    return scm_is_true (scm_list_p (x));
}

int
scm_is_struct (SCM x)
{
  return scm_is_true (scm_struct_p (x));
}

int
scm_is_gobject (SCM x)
{
  
  SCM ref = scm_slot_ref (x, scm_from_latin1_symbol("ob_type"));
  return scm_is_true (ref);
}

void
gir_init_xguile (void)
{
  Gu_None_Type = scm_make_foreign_object_type (scm_from_utf8_string ("$NONE"),
						SCM_EOL, NULL);
  SCM_NONE = scm_permanent_object (scm_make_foreign_object_0 (Gu_None_Type));
}




#endif
