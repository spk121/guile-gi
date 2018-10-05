#include <stdio.h>
#include <glib.h>

FILE *fp;

static char *
getter (const char *self, const char *type, int n)
{
  if ((strlen (type) > 2 && type[strlen(type)-1] == '*')
      || (!strcmp (type, "gpointer")))
    return g_strdup_printf ("(%s) scm_foreign_object_ref (%s, %d)", type, self, n);
  else if (!strcmp (type, "GType") || !strcmp (type, "ssize_t"))
    return g_strdup_printf ("(%s) scm_to_ssize_t (scm_foreign_object_ref (%s, %d))",
			    type, self, n);
  else if (!strcmp (type, "gboolean") || !strcmp (type, "bool"))
    return g_strdup_printf ("(%s) scm_to_bool (scm_foreign_object_ref (%s, %d))",
			    type, self, n);
  else if (!strcmp (type, "gint") || !strcmp (type, "int"))
    return g_strdup_printf ("(%s) scm_to_int (scm_foreign_object_ref (%s, %d))",
			    type, self, n);
  else if (!strcmp (type, "SCM"))
    return g_strdup_printf ("scm_foreign_object_ref (%s, %d)", self, n);
  else {
    fprintf (stderr, "UNKNOWN TYPE %s\n", type);
    return g_strdup_printf ("scm_foreign_object_ref (%s, %d)", self, n);
  }
}

static char *
setter (const char *self, const char *var, const char *type, int n)
{
  if ((strlen (type) > 2 && type[strlen(type)-1] == '*')
      || (!strcmp (type, "gpointer")))
    return g_strdup_printf ("scm_foreign_object_set_x (%s, %d, %s)", self, n, var);
  else if (!strcmp (type, "GType") || !strcmp (type, "ssize_t"))
    return g_strdup_printf ("scm_foreign_object_set_x (%s, %d, scm_from_ssize_t ((ssize_t) %s))",
			    self, n, var);
  else if (!strcmp (type, "gboolean") || !strcmp (type, "bool"))
    return g_strdup_printf ("scm_foreign_object_set_x (%s, %d, scm_from_bool ((size_t) %s))",
			    self, n, var);
  else if (!strcmp (type, "gint") || !strcmp (type, "int"))
    return g_strdup_printf ("scm_foreign_object_set_x (%s, %d, scm_from_int ((int) %s))",
			    self, n, var);
  else if (!strcmp (type, "SCM"))
    return g_strdup_printf ("scm_foreign_object_set_x (%s, %d, %s)",
			    self, n, var);
  else {
    fprintf (stderr, "UNKNOWN TYPE %s\n", type);
    
    return g_strdup_printf ("scm_foreign_object_set_x (%s, %d, %s)", self, n, var);
  }
}

static void
do_includes (char *name)
{
  gchar *lower = g_ascii_strdown(name, -1);
  char *filename = g_strdup_printf("__gi_%s.h", lower);
  
  fprintf(fp, "#include <libguile.h>\n");
  fprintf(fp, "#include <glib.h>\n");
  fprintf(fp, "#include <glib-object.h>\n");
  fprintf(fp, "#include \"%s\"\n", filename);
  fprintf(fp, "\n");
  g_free (filename);
  g_free (lower);
}

static void
do_header_includes (char *name)
{
  fprintf(fp, "#ifndef ___GI_%s_H_\n", name);
  fprintf(fp, "#define ___GI_%s_H_\n", name);
  fprintf(fp, "#include <libguile.h>\n");
  fprintf(fp, "#include <glib.h>\n");
  fprintf(fp, "#include <glib-object.h>\n");
  fprintf(fp, "\n");
}

static void
do_declaration (const gchar *name, gsize n, gchar **fields, gboolean finalizer)
{
  gchar *lower = g_ascii_strdown(name, -1);
  gchar *upper = g_ascii_strup(name, -1);
  fprintf(fp, "SCM gi_%s_type;\n", lower);
  fprintf(fp, "SCM gi_%s_type_store;\n", lower);
  fprintf(fp, "\n");

  for (gsize i = 0; i < n; i ++)
    {
      gchar *field_upper = g_ascii_strup(fields[i], -1);
      fprintf(fp, "#define GI_%s_%s_SLOT (%zu)\n", upper, field_upper, i);
      g_free (field_upper);
    }
  
  fprintf(fp, "\n");
  g_free (lower);
  g_free (upper);
}

static void
do_header_declaration (const gchar *name, gsize n, gchar **fields, gboolean finalizer)
{
  gchar *lower = g_ascii_strdown(name, -1);
  gchar *upper = g_ascii_strup(name, -1);
  fprintf(fp, "extern SCM gi_%s_type;\n", lower);
  fprintf(fp, "extern SCM gi_%s_type_store;\n", lower);
  if (finalizer)
    fprintf(fp, "void gi_%s_finalizer (SCM self);\n", lower);
  fprintf(fp, "\n");
  g_free (lower);
  g_free (upper);
}

static void
do_predicate (const gchar *name)
{
  gchar *lower = g_ascii_strdown (name, -1);
  fprintf (fp, "SCM gi_%s_p (SCM self)\n", lower);
  fprintf (fp, "{\n");
  fprintf (fp, "  return scm_from_bool (SCM_IS_A_P (self, gi_%s_type));\n",
	   lower);
  fprintf (fp, "}\n");
}

static void
do_header_predicate (const gchar *name)
{
  gchar *lower = g_ascii_strdown (name, -1);
  fprintf (fp, "SCM gi_%s_p (SCM self);\n", lower);
}

static void
do_getters (const gchar *name, gsize n, gchar **fields, gchar **types)
{
  gchar *lower = g_ascii_strdown(name, -1);
  for (gsize i = 0; i < n; i ++)
    {
      gchar *field_lower = g_ascii_strdown (fields[i], -1);
      gchar *func_name = g_strdup_printf("gi_%s_get_%s", lower, field_lower);
      gchar *conv = getter("self", types[i], i);
      fprintf (fp, "%s\n", types[i]);
      fprintf (fp, "%s (SCM %s)\n", func_name, "self");
      fprintf (fp, "{\n");
      fprintf (fp, "\treturn %s;\n", conv); 
      fprintf (fp, "}\n");
      fprintf (fp, "\n");
      g_free (conv);
      g_free (func_name);
      g_free (field_lower);
    }
  g_free (lower);
}

static void
do_header_getters (const gchar *name, gsize n, gchar **fields, gchar **types)
{
  gchar *lower = g_ascii_strdown(name, -1);
  for (gsize i = 0; i < n; i ++)
    {
      gchar *field_lower = g_ascii_strdown (fields[i], -1);
      gchar *func_name = g_strdup_printf("gi_%s_get_%s", lower, field_lower);
      gchar *conv = getter("self", types[i], i);
      fprintf (fp, "%s", types[i]);
      fprintf (fp, "\t%s (SCM %s);\n", func_name, "self");
      g_free (conv);
      g_free (func_name);
      g_free (field_lower);
    }
  g_free (lower);
}

static void
do_setters (const gchar *name, gsize n, gchar **fields, gchar **types)
{
  gchar *lower = g_ascii_strdown(name, -1);
  for (gsize i = 0; i < n; i ++)
    {
      gchar *field_lower = g_ascii_strdown (fields[i], -1);
      gchar *func_name = g_strdup_printf("gi_%s_set_%s", lower, field_lower);
      gchar *conv = setter("self", "val", types[i], i);
      fprintf (fp, "void\n");
      fprintf (fp, "%s (SCM self, %s val)\n", func_name, types[i]);
      fprintf (fp, "{\n");
      fprintf (fp, "\t%s;\n", conv); 
      fprintf (fp, "}\n");
      fprintf (fp, "\n");
      g_free (conv);
      g_free (func_name);
      g_free (field_lower);
    }
  g_free (lower);
}

static void
do_header_setters (const gchar *name, gsize n, gchar **fields, gchar **types)
{
  gchar *lower = g_ascii_strdown(name, -1);
  for (gsize i = 0; i < n; i ++)
    {
      gchar *field_lower = g_ascii_strdown (fields[i], -1);
      gchar *func_name = g_strdup_printf("gi_%s_set_%s", lower, field_lower);
      gchar *conv = setter("self", "val", types[i], i);
      fprintf (fp, "void");
      fprintf (fp, "\t%s (SCM self, %s val);\n", func_name, types[i]);
      g_free (conv);
      g_free (func_name);
      g_free (field_lower);
    }
  g_free (lower);
}

static void
do_init (const gchar *name, gsize n, gchar **fields, gboolean finalizer)
{
  gchar *lower = g_ascii_strdown(name, -1);
  gchar *func_name = g_strdup_printf("gi_init_%s_type", lower);
  fprintf (fp, "void\n");
  fprintf (fp, "%s (void)\n", func_name);
  fprintf (fp, "{\n");
  fprintf (fp, "\tSCM name, slots;\n");
  fprintf (fp, "\tname = scm_from_utf8_symbol(\"<%s>\");\n", name);
  fprintf (fp, "\tslots = scm_list_n(\n");
  for (gsize i = 0; i < n; i ++)
    {
      fprintf(fp, "\t\tscm_from_utf8_symbol (\"%s\"),\n", fields[i]);
    }
  fprintf (fp, "\t\tSCM_UNDEFINED);\n");
  fprintf (fp, "\tgi_%s_type = scm_make_foreign_object_type (name, slots, ", lower);
  if (finalizer)
    fprintf (fp, "gi_%s_finalizer", lower);
  else
    fprintf (fp, "NULL");
  fprintf (fp, ");\n");
  fprintf (fp, "\tgi_%s_type_store = scm_c_define (\"<%s>\", gi_%s_type);\n", lower, name, lower);
  fprintf (fp, "\tscm_c_define_gsubr (\"%s?\", 1, 0, 0, gi_%s_p);\n",
	   lower, lower);
  fprintf (fp, "\tscm_c_export (\"%s\", \"%s?\", NULL);\n", name, lower);
  fprintf (fp, "}\n");
  g_free (lower);
  g_free (func_name);
}

static void
do_header_init (const gchar *name, gsize n, gchar **fields, gboolean finalizer)
{
  gchar *lower = g_ascii_strdown(name, -1);
  gchar *func_name = g_strdup_printf("gi_init_%s_type", lower);
  fprintf (fp, "void");
  fprintf (fp, "\t%s (void);\n", func_name);
  fprintf (fp, "#endif\n");
  g_free (lower);
  g_free (func_name);
}

int main(int argc, char **argv)
{
  GKeyFile *key_file = g_key_file_new();
  GError *error = NULL;
  gchar *name;
  gchar **names;
  gsize n_names;
  gchar *lowercase;
  gchar **fields;
  gchar **types;
  gboolean finalizer;
  gsize n_fields;
  gsize n_types;
  
  if (argc < 3)
    {
      return 1;
    }
  if (!g_key_file_load_from_file (key_file, argv[1], G_KEY_FILE_NONE, &error))
    {
      if (!g_error_matches (error, G_FILE_ERROR, G_FILE_ERROR_NOENT))
	{
	  g_warning ("Error loading key file %s", error->message);
	  g_error_free (error);
	  return 1;
	}
    }
    names = g_key_file_get_string_list (key_file, "Foreign Objects", "Names", &n_names, &error);
    for (gsize n = 0; n < n_names; n ++)
    {
      char *filename;
      char *filepath;
      name = g_key_file_get_string (key_file, names[n], "Name", &error);
      fields = g_key_file_get_string_list (key_file, names[n], "Fields", &n_fields, &error);
      types = g_key_file_get_string_list (key_file, names[n], "Types", &n_types, &error);
      lowercase = g_ascii_strdown (name, -1);
      error = NULL;
      finalizer = g_key_file_get_boolean (key_file, names[n], "Finalizer", &error);
      if (!finalizer)
	g_error_free (error);

      filename = g_strdup_printf("__gi_%s.c", lowercase);
      filepath = g_build_filename(argv[2], filename, NULL);
      fp = fopen(filepath, "wt");
    
      do_includes(name);
      do_declaration(name, n_fields, fields, finalizer);
      do_getters(name, n_fields, fields, types);
      do_setters(name, n_fields, fields, types);
      do_predicate(name);
      do_init(name, n_fields, fields, finalizer);

      fclose (fp);
      g_free (filename);
      g_free (filepath);

      filename = g_strdup_printf("__gi_%s.h", lowercase);
      filepath = g_build_filename(argv[2], filename, NULL);
      fp = fopen(filepath, "wt");

      do_header_includes(name);
      do_header_declaration(name, n_fields, fields, finalizer);
      do_header_getters(name, n_fields, fields, types);
      do_header_setters(name, n_fields, fields, types);
      do_header_predicate(name);
      do_header_init(name, n_fields, fields, finalizer);

      fclose (fp);
      g_free (filename);
      g_free (filepath);

      g_free (lowercase);
      g_free (name);
      g_strfreev (fields);
      g_strfreev (types);
    }

    g_key_file_free (key_file);
    return 0;
}
  
