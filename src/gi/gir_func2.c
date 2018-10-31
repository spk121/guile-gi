#include <libguile.h>
#include <girepository.h>
#include "gi_giargument.h"
#include "gir_type.h"
#include "gir_func2.h"
#include "gi_gobject.h"
#include "gi_gtype.h"


#define MAX_GERROR_MSG 100
static char gerror_msg[MAX_GERROR_MSG];

static void
store_gerror_message (const char *msg)
{
  memset (gerror_msg, 0, MAX_GERROR_MSG);
  strncpy (gerror_msg, msg, MAX_GERROR_MSG - 1);
  if (strlen (msg) >= MAX_GERROR_MSG - 1) {
    gerror_msg[MAX_GERROR_MSG-2] = '.';
    gerror_msg[MAX_GERROR_MSG-3] = '.';
    gerror_msg[MAX_GERROR_MSG-4] = '.';
  }
}

static GHashTable *gi_constants = NULL;
static GHashTable *gi_enums = NULL;
static GHashTable *gi_flags = NULL;
static GHashTable *gi_functions = NULL;
static GHashTable *gi_methods = NULL;
static GHashTable *gi_callbacks = NULL;
static GHashTable *gi_structs = NULL;
static GHashTable *gi_unions = NULL;
static GHashTable *gi_objects = NULL;
static GHashTable *gi_interfaces = NULL;
static GHashTable *gi_signals = NULL;
#define FIGURE_OUT_ALL_ARG_TYPES
#ifdef FIGURE_OUT_ALL_ARG_TYPES
struct _arg_info_func_name {
    GIArgInfo *ai;
    char *name;
};
static GPtrArray *gi_arg_infos = NULL;
#endif

#if 0
static guint
hash_key_func (gconstpointer v)
{
  guint hash = 5381;
  int i = 0;
  unsigned char *p = v;
  while (*p) {
    hash = hash * 33 + *p;
    p++;
    if (i++ > 100)
      break;
  }
  g_debug("string hash of %s is %u", (char *)v, hash);
  return hash;
}

static gboolean
hash_equal_func(gconstpointer a, gconstpointer b)
{
  return a == b;
}
#endif

/* The method table is different from other hash tables in that
 * its VALUE is itself a hashtable mapping a GTYPE to a
 * FUNC_INFO.
 * This is because many methods have the same name but operate
 * on different GTypes */
static gboolean
insert_into_method_table(GType type,
                         GIFunctionInfo *info,
                         gboolean *is_new_method)
{
    *is_new_method = FALSE;

    if (!gi_methods)
    {
        g_debug("Creating the methods hash table");
        gi_methods = g_hash_table_new_full(g_str_hash,
                                           g_str_equal,
                                           g_free,
                                           (GDestroyNotify)g_hash_table_destroy);
    }
    const gchar *full_name = g_base_info_get_name(info);
    GHashTable *subhash = g_hash_table_lookup(gi_methods,
                                              full_name);
    if (!subhash)
    {
        g_debug("Inserted '%s' into the methods table", full_name);
        *is_new_method = TRUE;
        subhash = g_hash_table_new_full(g_direct_hash,
                                        g_direct_equal,
                                        NULL,
                                        (GDestroyNotify)g_base_info_unref);
        g_hash_table_insert(gi_methods,
                            g_strdup(full_name),
                            subhash);
    }
    if (g_hash_table_contains(subhash, GINT_TO_POINTER(type)))
    {
        g_critical("Did not overwrite method '%s' for type '%s'",
                   full_name,
                   g_type_name(type));
        return FALSE;
    }
    g_hash_table_insert(subhash,
                        GINT_TO_POINTER(type),
                        info);
    g_debug("Inserted '%s' for type '%s' %lu into methods table",
            full_name,
            g_type_name(type),
            (unsigned long)type);
    return TRUE;
}

static gboolean
insert_into_hash_table (const char *category,
            const char *namespace_,
            const char *parent,
            GHashTable **p_hash_table,
            GIBaseInfo *info)
{
  if (!*p_hash_table) {
    g_debug ("Creating %s hash table", category);
    *p_hash_table = g_hash_table_new_full (g_str_hash,
                       g_str_equal,
                       g_free,
                       (GDestroyNotify) g_base_info_unref);
  }
  gchar *full_name;
#ifdef PREFIX_NAME_IN_HASH
  if (parent)
    full_name = g_strdup_printf ("%s-%s-%s", namespace_, parent, g_base_info_get_name (info));
  else
    full_name = g_strdup_printf ("%s-%s", namespace_, g_base_info_get_name (info));
#else
  if (parent)
    full_name = g_strdup_printf ("%s-%s", parent, g_base_info_get_name (info));
  else
    full_name = g_strdup_printf ("%s", g_base_info_get_name (info));
#endif
  if (g_hash_table_contains (*p_hash_table, full_name)) {
    g_critical ("Did not overwrite %s in %s hash table.", full_name, category);
    g_free (full_name);
    return FALSE;
  }

  /* The hash table keeps the key string. */
  g_hash_table_replace (*p_hash_table,
            full_name,
            info);
  g_debug ("Inserted %s into %s hash table. %u entries.",
       full_name,
       category,
       g_hash_table_size (*p_hash_table));
  return TRUE;
}

/* Convert the type of names that GTK uses into Guile-like names */
static char *
gname_to_scm_name(const char *gname)
{
  size_t len = strlen (gname);
  GString *str = g_string_new(NULL);
  gboolean was_lower = FALSE;

  for (size_t i = 0; i < len; i ++) {
    if (g_ascii_islower(gname[i])) {
      g_string_append_c (str, gname[i]);
      was_lower = TRUE;
    }
    else if (gname[i] == '_' || gname[i] == '-') {
      g_string_append_c (str, '-');
      was_lower = FALSE;
    }
    else if (g_ascii_isdigit(gname[i])) {
      g_string_append_c (str, gname[i]);
      was_lower = FALSE;
    }
    else if (g_ascii_isupper(gname[i])) {
      if (was_lower)
    g_string_append_c (str, '-');
      g_string_append_c (str, g_ascii_tolower (gname[i]));
      was_lower = FALSE;
    }
  }
  return g_string_free (str, FALSE);
}

static char *
gname_to_scm_constant_name(const char *gname)
{
  size_t len = strlen (gname);
  GString *str = g_string_new(NULL);
  gboolean was_lower = FALSE;

  for (size_t i = 0; i < len; i ++) {
    if (g_ascii_islower(gname[i])) {
      g_string_append_c (str, g_ascii_toupper (gname[i]));
      was_lower = TRUE;
    }
    else if (gname[i] == '_' || gname[i] == '-') {
      g_string_append_c (str, '_');
      was_lower = FALSE;
    }
    else if (g_ascii_isdigit(gname[i])) {
      g_string_append_c (str, gname[i]);
      was_lower = FALSE;
    }
    else if (g_ascii_isupper(gname[i])) {
      if (was_lower)
    g_string_append_c (str, '_');
      g_string_append_c (str, gname[i]);
      was_lower = FALSE;
    }
  }

  char *fptr = strstr(str->str, "_FLAGS");
  if (fptr) {
    memcpy(fptr, fptr + 6, str->len - (fptr - str->str) - 6);
    memset(str->str + str->len - 6, 0, 6);
    str->len -= 6;
  }

  return g_string_free (str, FALSE);
}


static void
export_type_info (GString **export, const char *parent, GIRegisteredTypeInfo *info)
{
  char *name = gname_to_scm_name(parent);
  g_string_append_printf(*export, "(define <%s>\n  (gi-lookup-type \"%s\"))\n\n", parent, parent);
  free (name);
}

static void
export_constant_info (GString **export, const char *parent, GIConstantInfo *info)
{
  char *name = gname_to_scm_constant_name(g_base_info_get_name(info));
  g_string_append_printf(*export, "(define %s\n  (gi-constant-value \"\" \"%s\"))\n\n", name, parent);
  free (name);
}

static void
export_enum_info (GString **export, const char *parent, GIEnumInfo *info)
{
  gint n_values = g_enum_info_get_n_values (info);
  gint i = 0;
  GIValueInfo *vi = NULL;

  while (i < n_values) {
    vi = g_enum_info_get_value (info, i);
    char *c_function_name;
    if (parent)
      c_function_name = g_strdup_printf("%s-%s", parent, g_base_info_get_name(vi));
    else
      c_function_name = g_strdup_printf("%s", g_base_info_get_name(vi));

    char *name = gname_to_scm_constant_name(c_function_name);

    g_string_append_printf(*export, "(define %s\n  (gi-enum-value \"\" \"%s\" \"%s\"))\n\n",
               name, parent, g_base_info_get_name(vi));
    g_base_info_unref (vi);
    vi = NULL;
    free (c_function_name);
    free (name);
    i++;
  }

}

static void
export_flag_info (GString **export, const char *parent, GIEnumInfo *info)
{
  gint n_values = g_enum_info_get_n_values (info);
  gint i = 0;
  GIValueInfo *vi = NULL;

  while (i < n_values) {
    vi = g_enum_info_get_value (info, i);
    char *c_function_name;
    if (parent)
      c_function_name = g_strdup_printf("%s-%s", parent, g_base_info_get_name(vi));
    else
      c_function_name = g_strdup_printf("%s", g_base_info_get_name(vi));

    char *name = gname_to_scm_constant_name(c_function_name);
    g_string_append_printf(*export, "(define %s\n  (gi-flag-value \"\" \"%s\" \"%s\"))\n\n",
               name, parent, g_base_info_get_name(vi));
    g_base_info_unref (vi);
    vi = NULL;
    free (c_function_name);
    free (name);
    i++;
  }
}

static void
export_callable_info (GString **export, const char *parent, GICallableInfo *info)
{
  gint n_args;
  GIArgInfo *arg;

  n_args = g_callable_info_get_n_args (info);

  char *c_function_name;
  if (parent)
    c_function_name = g_strdup_printf("%s-%s", parent, g_base_info_get_name(info));
  else
    c_function_name = g_strdup_printf("%s", g_base_info_get_name(info));

  char *name = gname_to_scm_name(c_function_name);

  g_string_append_printf(*export, "(define (%s", name);
  free (name);

  GITypeInfo *return_type = g_callable_info_get_return_type (info);
  g_assert (return_type);
  if (g_type_info_get_tag (return_type) == GI_TYPE_TAG_BOOLEAN
      && !g_type_info_is_pointer (return_type))
    g_string_append_c (*export, '?');
  g_base_info_unref (return_type);

  for (int i = 0; i < n_args; i ++) {
    arg = g_callable_info_get_arg (info, i);
    GIDirection dir = g_arg_info_get_direction (arg);
    if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT) {
      g_string_append_c (*export, ' ');
      name = gname_to_scm_name (g_base_info_get_name (arg));
      g_string_append (*export, name);
      free (name);
    }
#ifdef FIGURE_OUT_ALL_ARG_TYPES
    struct _arg_info_func_name *aifn = g_new(struct _arg_info_func_name,1);
    aifn->ai = arg;
    aifn->name = g_strdup(c_function_name);
    g_ptr_array_add(gi_arg_infos, aifn);
#else
    g_base_info_unref (arg);
#endif
  }

  g_string_append_c (*export, ')');
  g_string_append_c (*export, '\n');

  g_string_append_printf(*export, "  (gi-function-invoke \"%s\"", c_function_name);

  for (int i = 0; i < n_args; i ++) {
    arg = g_callable_info_get_arg (info, i);
    GIDirection dir = g_arg_info_get_direction (arg);
    if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT) {
      g_string_append_c (*export, ' ');
      name = gname_to_scm_name (g_base_info_get_name (arg));
      g_string_append (*export, name);
      free (name);
    }
    g_base_info_unref (arg);
  }

  g_string_append (*export, "))\n\n");
}

static void
export_method_info (GString **export, const char *parent, GICallableInfo *info, gboolean is_new_method)
{
  gint n_args;
  GIArgInfo *arg;

  n_args = g_callable_info_get_n_args (info);

  char *c_function_name;
  if (parent)
    c_function_name = g_strdup_printf("%s-%s", parent, g_base_info_get_name(info));
  else
    c_function_name = g_strdup_printf("%s", g_base_info_get_name(info));

  char *name = gname_to_scm_name(c_function_name);

  g_string_append_printf(*export, "(define (%s", name);
  free (name);

  GITypeInfo *return_type = g_callable_info_get_return_type (info);
  g_assert (return_type);
  if (g_type_info_get_tag (return_type) == GI_TYPE_TAG_BOOLEAN
      && !g_type_info_is_pointer (return_type))
    g_string_append_c (*export, '?');
  g_base_info_unref (return_type);

  g_string_append_printf(*export, " self");

  for (int i = 0; i < n_args; i ++) {
    arg = g_callable_info_get_arg (info, i);
    GIDirection dir = g_arg_info_get_direction (arg);
    if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT) {
      g_string_append_c (*export, ' ');
      name = gname_to_scm_name (g_base_info_get_name (arg));
      g_string_append (*export, name);
      free (name);
    }
#ifdef FIGURE_OUT_ALL_ARG_TYPES
    struct _arg_info_func_name *aifn = g_new(struct _arg_info_func_name,1);
    aifn->ai = arg;
    aifn->name = g_strdup(c_function_name);
    g_ptr_array_add(gi_arg_infos, aifn);
#else
    g_base_info_unref (arg);
#endif
  }

  g_string_append_c (*export, ')');
  g_string_append_c (*export, '\n');

  g_string_append_printf(*export, "  (gi-method-send self \n");
  g_string_append_printf(*export, "     (gi-method-prepare \"%s\"", g_base_info_get_name(info));

  for (int i = 0; i < n_args; i ++) {
    arg = g_callable_info_get_arg (info, i);
    GIDirection dir = g_arg_info_get_direction (arg);
    if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT) {
      g_string_append_c (*export, ' ');
      name = gname_to_scm_name (g_base_info_get_name (arg));
      g_string_append (*export, name);
      free (name);
    }
    g_base_info_unref (arg);
  }

  g_string_append (*export, ")))\n\n");
}

/* FIXME: this is very sigmal to export signal info */
static void
export_signal_info (GString **export, char *parent, GISignalInfo *info, gboolean is_new_method)
{
  gint n_args;
  GIArgInfo *arg;

  n_args = g_callable_info_get_n_args (info);

  char *c_function_name, *c_method_name;
  if (parent)
    c_function_name = g_strdup_printf("%s-%s-signal", parent, g_base_info_get_name(info));
  else
    c_function_name = g_strdup_printf("%s-signal", g_base_info_get_name(info));

  char *name = gname_to_scm_constant_name(c_function_name);

  g_string_append_printf(*export, "(define (%s", name);
  free (name);

  GITypeInfo *return_type = g_callable_info_get_return_type (info);
  g_assert (return_type);
  if (g_type_info_get_tag (return_type) == GI_TYPE_TAG_BOOLEAN
      && !g_type_info_is_pointer (return_type))
    g_string_append_c (*export, '?');
  g_base_info_unref (return_type);

  g_string_append_printf(*export, " self");

  for (int i = 0; i < n_args; i ++) {
    arg = g_callable_info_get_arg (info, i);
    GIDirection dir = g_arg_info_get_direction (arg);
    if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT) {
      g_string_append_c (*export, ' ');
      name = gname_to_scm_name (g_base_info_get_name (arg));
      g_string_append (*export, name);
      free (name);
    }
    g_base_info_unref (arg);
  }

  g_string_append_c (*export, ')');
  g_string_append_c (*export, '\n');

  g_string_append_printf(*export, "  (gi-method-send self \n");
  g_string_append_printf(*export, "     (gi-method-prepare \"%s\"", g_base_info_get_name(info));

  for (int i = 0; i < n_args; i ++) {
    arg = g_callable_info_get_arg (info, i);
    GIDirection dir = g_arg_info_get_direction (arg);
    if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT) {
      g_string_append_c (*export, ' ');
      name = gname_to_scm_name (g_base_info_get_name (arg));
      g_string_append (*export, name);
      free (name);
    }
    g_base_info_unref (arg);
  }

  g_string_append (*export, ")))\n\n");
}


#if 0
static void
export_method_info (GString **export, char *parent, GICallableInfo *info, gboolean is_new_method)
{
  gint n_args;
  GIArgInfo *arg;

  n_args = g_callable_info_get_n_args (info);

  const char *c_function_name = g_base_info_get_name(info);
  char *name = gname_to_scm_name(g_base_info_get_name(info));

  g_string_append_printf(*export, ";; %s -> (%s", parent,  name);
  for (int i = 0; i < n_args; i ++) {
    arg = g_callable_info_get_arg (info, i);
    GIDirection dir = g_arg_info_get_direction (arg);
    if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT) {
      g_string_append_c (*export, ' ');
      char *arg_name = gname_to_scm_name (g_base_info_get_name (arg));
      g_string_append (*export, arg_name);
      free (arg_name);
    }
    g_base_info_unref (arg);
  }
  g_string_append(*export, ")\n");

  if (is_new_method) {
    g_string_append_printf(*export, "(define (_%s . args)", name);
    g_string_append_printf(*export, "  (apply gi-method-prepare (append (list \"%s\") args)))\n\n", g_base_info_get_name (info));
  }

  free (name);

}
#endif

static SCM
scm_gi_load_repository(SCM s_namespace, SCM s_version)
{
    gchar *namespace_;
    gchar *version;
    GITypelib *tl;
    GError *error = NULL;
    GString *export;

    SCM_ASSERT(scm_is_string(s_namespace), s_namespace, SCM_ARG1, "gi-load-repository");
    SCM_ASSERT(scm_is_string(s_version), s_version, SCM_ARG2, "gi-load-repository");

    namespace_ = scm_to_utf8_string(s_namespace);
    version = scm_to_utf8_string(s_version);

    tl = g_irepository_require(NULL, namespace_, version, 0, &error);
    if (tl == NULL)
    {
        free(version);
        free(namespace_);
        store_gerror_message(error->message);
        g_error_free(error);
        scm_misc_error("%irepository-require", gerror_msg, SCM_EOL);
        return SCM_UNSPECIFIED;
    }

    export = g_string_new(NULL);
    g_string_append_printf(export, ";; Declaration for %s %s\n", namespace_, version);

    g_debug("Parsing irepository %s %s", namespace_, version);
    int n = g_irepository_get_n_infos(NULL, namespace_);
    for (int i = 0; i < n; i++)
    {
        GIBaseInfo *info;
        GIInfoType type;
        gboolean is_new_method;
        info = g_irepository_get_info(NULL, namespace_, i);
        if (g_base_info_is_deprecated(info))
        {
            g_base_info_unref(info);
            continue;
        }
        type = g_base_info_get_type(info);
        switch (type)
        {
        case GI_INFO_TYPE_CALLBACK:
            insert_into_hash_table("callbacks", namespace_, NULL, &gi_callbacks, info);
            break;
        case GI_INFO_TYPE_FUNCTION:
            insert_into_hash_table("functions", namespace_, NULL, &gi_functions, info);
            export_callable_info(&export, NULL, info);
            break;
        case GI_INFO_TYPE_STRUCT:
        {
            GType gtype = g_registered_type_info_get_g_type(info);
            if (gtype == G_TYPE_NONE)
            {
                g_debug("Not registering struct type '%s' because is has no GType",
                        g_base_info_get_name(info));
                g_base_info_unref(info);
                break;
            }
            g_base_info_ref (info);
            g_type_set_qdata(gtype, gtype_base_info_key, info);
            insert_into_hash_table("structs", namespace_, NULL, &gi_structs, info);
            export_type_info(&export, g_base_info_get_name(info), info);
            gint n_methods = g_struct_info_get_n_methods(info);
            for (gint m = 0; m < n_methods; m++)
            {
                GIFunctionInfo *func_info = g_struct_info_get_method(info, m);
                if (g_function_info_get_flags(func_info) & GI_FUNCTION_IS_METHOD)
                {
                    if (!insert_into_method_table(gtype, func_info, &is_new_method))
                        g_base_info_unref(func_info);
                    else
                        export_method_info(&export, g_base_info_get_name(info), func_info, is_new_method);
                }
                else
                {
                    if (!insert_into_hash_table("functions", namespace_, g_base_info_get_name(info), &gi_functions, func_info))
                        g_base_info_unref(func_info);
                    else
                        export_callable_info(&export, g_base_info_get_name(info), func_info);
                }
            }
        }
        break;
        case GI_INFO_TYPE_ENUM:
            insert_into_hash_table("enums", namespace_, NULL, &gi_enums, info);
            export_enum_info(&export, g_base_info_get_name(info), info);
            break;
        case GI_INFO_TYPE_FLAGS:
            insert_into_hash_table("flags", namespace_, NULL, &gi_flags, info);
            export_flag_info(&export, g_base_info_get_name(info), info);
            break;
        case GI_INFO_TYPE_OBJECT:
        {
            GType gtype = g_registered_type_info_get_g_type(info);
            if (gtype == G_TYPE_NONE)
            {
                g_debug("Not registereing object type '%s' because is has no GType",
                        g_base_info_get_name(info));
                g_base_info_unref(info);
                break;
            }
            g_base_info_ref (info);
            g_type_set_qdata(gtype, gtype_base_info_key, info);
            insert_into_hash_table("objects", namespace_, NULL, &gi_objects, info);
            export_type_info(&export, g_base_info_get_name(info), info);
            gint n_methods = g_object_info_get_n_methods(info);
            for (gint m = 0; m < n_methods; m++)
            {
                GIFunctionInfo *func_info = g_object_info_get_method(info, m);
                if (g_function_info_get_flags(func_info) & GI_FUNCTION_IS_METHOD)
                {
                    if (!insert_into_method_table(gtype, func_info, &is_new_method))
                        g_base_info_unref(func_info);
                    else
                        export_method_info(&export, g_base_info_get_name(info), func_info, is_new_method);
                }
                else
                {
                    if (!insert_into_hash_table("functions", namespace_, g_base_info_get_name(info), &gi_functions, func_info))
                        g_base_info_unref(func_info);
                    else
                        export_callable_info(&export, g_base_info_get_name(info), func_info);
                }
            }
#if 0
        gint n_signals = g_object_info_get_n_signals (info);
        for (gint m = 0; m < n_signals; m ++) {
          GISignalInfo *sig_info = g_object_info_get_signal (info, m);
          if (!(g_signal_info_get_flags (sig_info) & G_SIGNAL_DEPRECATED)) {
        if (!insert_into_signal_table (gtype, sig_info, &is_new_method))
          g_base_info_unref (sig_info);
        else
          export_signal_info (&export, g_base_info_get_name (info), sig_info, is_new_method);
          }
        }
#endif
        }
        break;
        case GI_INFO_TYPE_INTERFACE:
            insert_into_hash_table("interfaces", namespace_, NULL, &gi_interfaces, info);
            break;
        case GI_INFO_TYPE_CONSTANT:
            insert_into_hash_table("constants", namespace_, NULL, &gi_constants, info);
            export_constant_info(&export, g_base_info_get_name(info), info);
            break;
        case GI_INFO_TYPE_UNION:
        {
            GType gtype = g_registered_type_info_get_g_type(info);
            if (gtype == G_TYPE_NONE)
            {
                g_debug("Not registering union type '%s' because is has no GType",
                        g_base_info_get_name(info));
                g_base_info_unref(info);
                break;
            }
            insert_into_hash_table("unions", namespace_, NULL, &gi_unions, info);
            export_type_info(&export, g_base_info_get_name(info), info);
            gint n_methods = g_union_info_get_n_methods(info);
            for (gint m = 0; m < n_methods; m++)
            {
                GIFunctionInfo *func_info = g_object_info_get_method(info, m);
                if (g_function_info_get_flags(func_info) & GI_FUNCTION_IS_METHOD)
                {
                    if (!insert_into_method_table(gtype, func_info, &is_new_method))
                        g_base_info_unref(func_info);
                    else
                        export_method_info(&export, g_base_info_get_name(info), func_info, is_new_method);
                }
                else
                {
                    if (!insert_into_hash_table("functions", namespace_, g_base_info_get_name(info), &gi_functions, func_info))
                        g_base_info_unref(func_info);
                    else
                        export_callable_info(&export, g_base_info_get_name(info), func_info);
                }
            }
        }
        break;
        case GI_INFO_TYPE_VALUE:
            g_critical("Unsupported irepository type 'VALUE'");
            break;
        case GI_INFO_TYPE_SIGNAL:
            g_critical("Unsupported irepository type 'SIGNAL'");
            break;
        case GI_INFO_TYPE_VFUNC:
            g_critical("Unsupported irepository type 'VFUNC'");
            break;
        case GI_INFO_TYPE_PROPERTY:
            g_critical("Unsupported irepository type 'PROPERTY'");
            break;
        case GI_INFO_TYPE_FIELD:
            g_critical("Unsupported irepository type 'FIELD'");
            break;
        case GI_INFO_TYPE_ARG:
            g_critical("Unsupported irepository type 'ARG'");
            break;
        case GI_INFO_TYPE_TYPE:
            g_critical("Unsupported irepository type 'TYPE'");
            break;
        case GI_INFO_TYPE_INVALID:
        case GI_INFO_TYPE_INVALID_0:
        default:
            g_critical("Unsupported irepository type %d", type);
            break;
        }
    }
    free(version);
    free(namespace_);

    return scm_take_locale_string(g_string_free(export, FALSE));
}

static SCM
scm_gi_constant_value (SCM s_namespace, SCM s_name)
{
  char *namespace_;
  char *name;
  char *full_name;

  SCM_ASSERT (scm_is_string (s_namespace), s_namespace, SCM_ARG1, "gi-constant-value");
  SCM_ASSERT (scm_is_string (s_name), s_name, SCM_ARG2, "gi-constant-value");
  namespace_ = scm_to_utf8_string (s_namespace);
  name = scm_to_utf8_string (s_name);

#ifdef PREFIX_NAME_IN_HASH
  full_name = g_strdup_printf ("%s-%s", namespace_, name);
#else
  full_name = g_strdup_printf ("%s", name);
#endif
  gpointer val = g_hash_table_lookup (gi_constants, full_name);
  if (!val) {
    free (namespace_);
    free (name);
    g_free (full_name);
    scm_misc_error ("gi-constant-value",
            "unknown constant ~a in ~a",
            scm_list_2 (s_name, s_namespace));
    return SCM_UNSPECIFIED;
  }

  GIConstantInfo *info = val;
  GITypeInfo *typeinfo;
  typeinfo = g_constant_info_get_type (info);
  GITypeTag typetag;
  typetag = g_type_info_get_tag (typeinfo);

  gint siz;
  GIArgument value;
  siz = g_constant_info_get_value(info, &value);
  SCM ret;

  switch (typetag)
    {
    case GI_TYPE_TAG_BOOLEAN:
      ret = scm_from_bool(value.v_boolean);
      break;
    case GI_TYPE_TAG_DOUBLE:
      ret = scm_from_double(value.v_double);
      break;
    case GI_TYPE_TAG_INT8:
      ret = scm_from_int8(value.v_int8);
      break;
    case GI_TYPE_TAG_INT16:
      ret = scm_from_int16(value.v_int16);
      break;
    case GI_TYPE_TAG_INT32:
      ret = scm_from_int32(value.v_int32);
      break;
    case GI_TYPE_TAG_INT64:
      ret = scm_from_int64(value.v_int64);
      break;
    case GI_TYPE_TAG_UINT8:
      ret = scm_from_uint8(value.v_uint8);
      break;
    case GI_TYPE_TAG_UINT16:
      ret = scm_from_uint16(value.v_uint16);
      break;
    case GI_TYPE_TAG_UINT32:
      ret = scm_from_uint32(value.v_uint32);
      break;
    case GI_TYPE_TAG_UINT64:
      ret = scm_from_uint64(value.v_uint64);
      break;
    case GI_TYPE_TAG_UTF8:
      ret = scm_from_utf8_string(value.v_string);
      break;
    default:
      g_critical ("Constant %s in %s has unsupported type %d",
          name, namespace_, typetag);
      ret = SCM_BOOL_F;
    }
  g_constant_info_free_value (info, &value);
  free (namespace_);
  free (name);
  g_free (full_name);

  return ret;
}

static SCM
scm_gi_flag_or_enum_value (SCM s_namespace, SCM s_category, SCM s_name, gboolean is_enum)
{
  char *name;
  char *full_category;

  SCM_ASSERT (scm_is_string (s_namespace), s_namespace, SCM_ARG1,
          is_enum ? "gi-enum-value" : "gi-flag-value");
  SCM_ASSERT (scm_is_string (s_category), s_category, SCM_ARG2,
          is_enum ? "gi-enum-value" : "gi-flag-value");
  SCM_ASSERT (scm_is_string (s_name), s_name, SCM_ARG3,
          is_enum ? "gi-enum-value" : "gi-flag-value");

  name = scm_to_utf8_string (s_name);

#ifdef PREFIX_NAME_IN_HASH
  {
    char *namespace_ = NULL;
    char *category = NULL;
    namespace_ = scm_to_utf8_string (s_namespace);
    category = scm_to_utf8_string (s_category);
    full_category = g_strdup_printf ("%s-%s", namespace_, category);
    free (category);
    free (namespace_);
  }
#else
  full_category = scm_to_utf8_string (s_category);
#endif

  gpointer val;
  if (is_enum)
    val = g_hash_table_lookup (gi_enums, full_category);
  else
    val = g_hash_table_lookup (gi_flags, full_category);

  g_free (full_category);
  full_category = NULL;

  if (!val) {
    free (name);
    name = NULL;

    scm_misc_error (is_enum ? "gi-enum-value" : "gi-flag-value",
            is_enum ? "unknown enum type '~a' in ~a" : "unknown flag type '~a' in ~a",
            scm_list_2 (s_category, s_namespace));
    return SCM_BOOL_F;
  }

  GIEnumInfo *info = val;
  gint n_values = g_enum_info_get_n_values (info);
  gint i = 0;
  GIValueInfo *vi = NULL;

  while (i < n_values) {
    vi = g_enum_info_get_value (info, i);
    g_assert (vi != NULL);

    g_debug ("flag name search: %s == %s ?",
         name,
         g_base_info_get_name (vi));
    if (strcmp (g_base_info_get_name (vi), name) == 0) {
      break;
    }
    g_base_info_unref (vi);
    vi = NULL;
    i++;
  }

  free (name);
  name = NULL;

  if (i >= n_values) {
    scm_misc_error (is_enum ? "gi-enum-value" : "gi-flag-value",
            is_enum ? "unknown enum '~a' of type '~a' in ~a": "unknown flag '~a' of type '~a' in ~a",
            scm_list_3 (s_name, s_category, s_namespace));
    return SCM_BOOL_F;
  } else {
    SCM ret = scm_from_int64 (g_value_info_get_value (vi));
    g_base_info_unref (vi);
    vi = NULL;
    return ret;
  }

  /* never reached */
  return SCM_BOOL_F;
}

static SCM
scm_gi_flag_value (SCM s_namespace, SCM s_category, SCM s_name)
{
  return scm_gi_flag_or_enum_value (s_namespace, s_category, s_name, FALSE);
}

static SCM
scm_gi_enum_value (SCM s_namespace, SCM s_category, SCM s_name)
{
  return scm_gi_flag_or_enum_value (s_namespace, s_category, s_name, TRUE);
}

static SCM
scm_gi_struct_ref (SCM s_ptr, SCM s_namespace, SCM s_type_name, SCM s_field_name)
{
  char *full_type_name = NULL;

  SCM_ASSERT (SCM_POINTER_P (s_ptr), s_ptr, SCM_ARG1,
          "gi-struct-ref");
  SCM_ASSERT (scm_is_string (s_namespace), s_namespace, SCM_ARG2,
          "gi-struct-ref");
  SCM_ASSERT (scm_is_string (s_type_name), s_type_name, SCM_ARG3,
          "gi-struct-ref");
  SCM_ASSERT (scm_is_string (s_field_name), s_field_name, SCM_ARG4,
          "gi-struct-ref");

#ifdef PREFIX_NAME_IN_HASH
  {
    char *namespace_ = scm_to_utf8_string (s_namespace);
    char *type_name = scm_to_utf8_string (s_type_name);
    full_type_name = g_strdup_printf ("%s-%s", namespace_, type_name);
    free (type_name);
    free (namespace_);
  }
#else
  full_type_name = scm_to_utf8_string (s_type_name);
#endif

  GIStructInfo *si = g_hash_table_lookup (gi_structs, full_type_name);
  g_free (full_type_name);
  full_type_name = NULL;

  if (!si) {
    scm_misc_error ("gi-struct-ref",
            "unknown struct type '~a' in ~a",
            scm_list_2 (s_type_name, s_namespace));
    return SCM_BOOL_F;
  }

  gint n_fields = g_struct_info_get_n_fields (si);
  gint i = 0;
  GIFieldInfo *fi = NULL;
  char *field_name = scm_to_utf8_string (s_field_name);

  while (i < n_fields) {
    fi = g_struct_info_get_field (si, i);
    g_assert (fi != NULL);

    g_debug ("field name search: %s == %s ?",
         field_name,
         g_base_info_get_name (fi));
    if (strcmp (g_base_info_get_name (fi), field_name) == 0) {
      break;
    }
    g_base_info_unref (fi);
    fi = NULL;
    i++;
  }

  free (field_name);
  if (i >= n_fields) {
    scm_misc_error ("gi-struct-ref",
            "unknown field '~a' in struct '~a' in ~a",
            scm_list_3 (s_field_name, s_type_name, s_namespace));
    return SCM_BOOL_F;
  } else {
    gboolean ok;
    GIArgument arg;
    void *ptr = scm_to_pointer (s_ptr);
    ok = g_field_info_get_field (fi, ptr, &arg);
    g_base_info_unref (fi);
    fi = NULL;

    if (!ok) {
      scm_misc_error ("gi-struct-ref",
              "cannot unpack field '~a' in struct '~a'",
              scm_list_2 (s_field_name, s_type_name));
      return SCM_BOOL_F;
    } else {
      GITypeInfo *ti = g_field_info_get_type (fi);
      SCM output = gi_giargument_to_object (&arg, ti, FALSE);
      g_base_info_unref (ti);
      return output;
    }
  }

  /* never get here */
  return SCM_BOOL_F;
}

static SCM
scm_gi_struct_set (SCM s_ptr, SCM s_namespace, SCM s_type_name, SCM s_field_name, SCM s_value)
{
  char *full_type_name = NULL;

  // SCM_VALIDATE_POINTER (1, s_ptr);
  SCM_ASSERT (scm_is_string (s_namespace), s_namespace, SCM_ARG2,
          "gi-struct-set");
  SCM_ASSERT (scm_is_string (s_type_name), s_type_name, SCM_ARG3,
          "gi-struct-set");
  SCM_ASSERT (scm_is_string (s_field_name), s_field_name, SCM_ARG4,
          "gi-struct-set");

#ifdef PREFIX_NAME_IN_HASH
  {
    char *namespace_ = scm_to_utf8_string (s_namespace);
    char *type_name = scm_to_utf8_string (s_type_name);
    full_type_name = g_strdup_printf ("%s-%s", namespace_, type_name);
    free (type_name);
    free (namespace_);
  }
#else
  full_type_name = scm_to_utf8_string (s_type_name);
#endif

  GIStructInfo *si = g_hash_table_lookup (gi_structs, full_type_name);
  g_free (full_type_name);
  full_type_name = NULL;

  if (!si) {
    scm_misc_error ("gi-struct-ref",
            "unknown struct type '~a' in ~a",
            scm_list_2 (s_type_name, s_namespace));
    return SCM_BOOL_F;
  }

  gint n_fields = g_struct_info_get_n_fields (si);
  gint i = 0;
  GIFieldInfo *fi = NULL;
  char *field_name = scm_to_utf8_string (s_field_name);

  while (i < n_fields) {
    fi = g_struct_info_get_field (si, i);
    g_assert (fi != NULL);

    g_debug ("field name search: %s == %s ?",
         field_name,
         g_base_info_get_name (fi));
    if (strcmp (g_base_info_get_name (fi), field_name) == 0) {
      break;
    }
    g_base_info_unref (fi);
    fi = NULL;
    i++;
  }

  free (field_name);
  if (i >= n_fields) {
    scm_misc_error ("gi-struct-set",
            "unknown field '~a' in struct '~a' in ~a",
            scm_list_3 (s_field_name, s_type_name, s_namespace));
    return SCM_BOOL_F;
  } else {
    gboolean ok;
    GIArgument arg;
    GITypeInfo *ti = g_field_info_get_type (fi);
    arg = gi_argument_from_object ("gi-struct-set",
                   s_value,
                   ti,
                   GI_TRANSFER_NOTHING);
    g_base_info_unref (ti);
    ti = NULL;
    void *ptr = scm_to_pointer (s_ptr);
    ok = g_field_info_set_field (fi, ptr, &arg);
    g_base_info_unref (fi);
    fi = NULL;

    if (!ok) {
      scm_misc_error ("gi-struct-set",
              "cannot set field '~a' in struct '~a' to '~a'",
              scm_list_3 (s_field_name, s_type_name, s_value));
      return SCM_BOOL_F;
    } else {
      return SCM_BOOL_T;
    }
  }

  /* never get here */
  return SCM_BOOL_F;
}

static void
function_info_count_args (GIFunctionInfo *info, int *in, int *out)
{
  /* Count the number of required input arguments, and store
   * the arg info in a newly allocate array. */
  int n_args = g_callable_info_get_n_args ((GICallableInfo *) info);
  int n_input_args = 0;
  int n_output_args = 0;

  for (int i = 0; i < n_args; i ++) {
    GIArgInfo *ai = g_callable_info_get_arg ((GICallableInfo *) info, i);
    g_assert (ai != NULL);

    GIDirection dir = g_arg_info_get_direction (ai);
    g_base_info_unref (ai);

    if (dir == GI_DIRECTION_IN)
      n_input_args ++;
    else if (dir == GI_DIRECTION_OUT)
      n_output_args ++;
    else if (dir == GI_DIRECTION_INOUT) {
      n_input_args ++;
      n_output_args ++;
    }
  }
  // if (g_function_info_get_flags (info) & GI_FUNCTION_IS_METHOD)
  //  n_input_args ++;
  *in = n_input_args;
  *out = n_output_args;
}

static gboolean
function_info_typecheck_args(GIFunctionInfo *func_info, SCM s_args, char **errstr)
{
    GIDirection dir;
    GIArgInfo *arg_info;
    gboolean type_ok = FALSE;

    int n_args = g_callable_info_get_n_args((GICallableInfo *)func_info);
    int i_input = 0;

    for (int i = 0; i < n_args; i++)
    {
        arg_info = g_callable_info_get_arg((GICallableInfo *)func_info, i);
        g_assert(arg_info != NULL);

        dir = g_arg_info_get_direction(arg_info);

        if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT)
        {
            SCM arg = scm_list_ref(s_args, scm_from_int(i_input));
            type_ok = gi_giargument_check_scm_type(arg, arg_info, errstr);
            i_input++;
        }
        g_base_info_unref(arg_info);
        if (!type_ok)
        {
            char *tmp = *errstr;
            *errstr = g_strdup_printf("In arg %d %s", i + 1, tmp);
            g_free(tmp);
        }
        break;
    }
    return type_ok;
}

static GIArgument *
function_info_convert_args(GIFunctionInfo *func_info, SCM s_args)
{
    GIDirection dir;
    GIArgInfo *arg_info;
    GIArgument *in_args = g_new0(GIArgument, scm_to_int(scm_length(s_args)));

    int n_args = g_callable_info_get_n_args((GICallableInfo *)func_info);
    int i_input = 0;

    for (int i = 0; i < n_args; i++)
    {
        arg_info = g_callable_info_get_arg((GICallableInfo *)func_info, i);
        g_assert(arg_info != NULL);

        dir = g_arg_info_get_direction(arg_info);

        if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT)
        {
            SCM arg = scm_list_ref(s_args, scm_from_int(i_input));
            in_args[i_input] = gi_argument_from_object("gi-function-invoke",
                                                       arg,
                                                       g_arg_info_get_type(arg_info),
                                                       g_arg_info_get_ownership_transfer(arg_info));
            i_input++;
        }
        g_base_info_unref(arg_info);
    }

    return in_args;
}

static GIArgument *
method_info_convert_args (GIFunctionInfo *func_info, SCM s_object, SCM s_args)
{
  GIDirection dir;
  GIArgInfo *arg_info;
  GIArgument *in_args = g_new0 (GIArgument, scm_to_int (scm_length (s_args)) + 1);

  int n_args = g_callable_info_get_n_args ((GICallableInfo *) func_info);
  int i_input = 0;

  in_args[0].v_pointer = gi_gobject_get_obj (s_object);
  for (int i = 0; i < n_args; i ++) {
    arg_info = g_callable_info_get_arg ((GICallableInfo *) func_info, i);
    g_assert (arg_info != NULL);

    dir = g_arg_info_get_direction (arg_info);

    if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT) {
      SCM arg = scm_list_ref (s_args, scm_from_int (i_input));
      in_args[i_input + 1] = gi_argument_from_object ("gi-function-invoke",
                          arg,
                          g_arg_info_get_type (arg_info),
                          g_arg_info_get_ownership_transfer (arg_info));
      i_input ++;
    }
    g_base_info_unref (arg_info);
  }

  return in_args;
}

static void
function_info_release_args (GIFunctionInfo *func_info, GIArgument *args)
{
  GIDirection dir;
  GIArgInfo *arg_info;
  GITypeInfo *type_info;

  int n_args = g_callable_info_get_n_args ((GICallableInfo *) func_info);
  int i_input = 0;

  for (int i = 0; i < n_args; i ++) {
    arg_info = g_callable_info_get_arg ((GICallableInfo *) func_info, i);
    type_info = g_arg_info_get_type (arg_info);

    dir = g_arg_info_get_direction (arg_info);

    if (dir == GI_DIRECTION_IN || dir == GI_DIRECTION_INOUT) {
      gi_giargument_release (&(args[i]),
                 type_info,
                 g_arg_info_get_ownership_transfer (arg_info),
                 dir);
      i_input ++;
    }
    g_base_info_unref (arg_info);
    g_base_info_unref (type_info);
  }

}


/* The inner part of a method call.  It just looks up a method
 * from the list and appends the method and its arguments together.
 * A full method call is
 * scm_gi_method_send (object, scm_gi_method_prepare ("methodname", list_of_args))
 */
static SCM
scm_gi_method_prepare (SCM s_method_name, SCM s_list_of_args)
{
  SCM_ASSERT(scm_is_string (s_method_name), s_method_name, SCM_ARG1, "gi-method-prepare");
  SCM_ASSERT(scm_is_true (scm_list_p (s_list_of_args)), s_list_of_args, SCM_ARG2, "gi-mthod-prepare");
  /* Look-up the method by name. */
  char *name = scm_to_utf8_string (s_method_name);
  /* Make a list of the method and the args, for dispatch. */
  gpointer ptr = g_hash_table_lookup (gi_methods, name);
  free (name);
  if (ptr == NULL) {
    scm_misc_error ("gi-method-prepare",
            "Unknown method ~a",
            scm_list_1 (s_method_name));
  }
  return scm_append (scm_list_2 (scm_list_2 (scm_from_pointer (ptr, NULL), s_method_name), s_list_of_args));
}

/* Given a wrapped GObject, struct, or union, call the attached method
 * with the given args, if the method is applicable to the object. */
static SCM
scm_gi_method_send(SCM s_object, SCM s_method_args_list)
{
    /* Find out of this type of argument or of any of this argument's
     parent types map to this method. */
    GType type;
    GHashTable *method_hash = scm_to_pointer(scm_car(s_method_args_list));
    GIFunctionInfo *info;
    SCM s_name = scm_cadr(s_method_args_list);
    char *method_name = scm_to_utf8_string(scm_cadr(s_method_args_list));

    type = gi_gobject_get_ob_type(s_object);
    while (!(info = g_hash_table_lookup(method_hash, GINT_TO_POINTER(type))))
    {
        const char *tname = g_type_name(type);
        g_debug("Did not find a method %s of type %s for object %p",
                method_name,
                tname,
                gi_gobject_get_obj(s_object));

        if (!(type = g_type_parent(type)))
        {
            free(method_name);
            scm_misc_error("gi-method-send",
                           "Cannot find a method '~a' for ~s",
                           scm_list_2(scm_cadr(s_method_args_list),
                                      s_object));
        }
    }

    g_debug("Invoking method %s of type %s for object %p of type %s",
            method_name,
            g_type_name(type),
            gi_gobject_get_obj(s_object),
            g_type_name(gi_gobject_get_ob_type(s_object)));

    SCM s_args = scm_cddr(s_method_args_list);

    int n_input_args_received = scm_to_int(scm_length(s_args));
    g_debug("\t%d input arguments received", n_input_args_received);

    int n_input_args, n_output_args;
    function_info_count_args(info, &n_input_args, &n_output_args);
    g_debug("\t%d input arguments expected", n_input_args);
    g_debug("\t%d output arguments expected", n_output_args);

    if (n_input_args_received != n_input_args)
    {
        scm_misc_error("function-invoke",
                       "wrong number of input arguments for method '~a', expected ~a, received ~a",
                       scm_list_3(s_name,
                                  scm_from_int(n_input_args),
                                  scm_from_int(n_input_args_received)));
        return SCM_BOOL_F;
    }

    char *errstr = NULL;
    if (!function_info_typecheck_args(info, s_args, &errstr))
    {
        g_assert(errstr != NULL);

        SCM s_errstr = scm_from_utf8_string(errstr);
        g_free(errstr);
        scm_misc_error("gi-method-send",
                       "wrong type argument for method '~a' - ~s",
                       scm_list_2(s_name, s_errstr));
        return SCM_BOOL_F;
    }

    GIArgument *in_args = method_info_convert_args(info, s_object, s_args);

    /* Allocate a GIArgument list of the output and return values. */
    GIArgument *out_args = NULL;
    if (n_output_args > 0)
        out_args = g_new0(GIArgument, n_output_args);
    GIArgument return_arg;

    /* Make the call. */
    GError *err = NULL;
    gboolean ret = g_function_info_invoke(info, in_args, n_input_args + 1,
                                          out_args, n_output_args,
                                          &return_arg, &err);
    if (ret)
        g_debug("Invoked method %s", method_name);
    else
        g_debug("Failed to invoked method %s", method_name);

    /* Free any allocated input */
    function_info_release_args(info, in_args);
    g_free(in_args);
    in_args = NULL;

    /* If there is a GError, write an error, free, and exit. */
    if (!ret)
    {
        char str[256];
        memset(str, 0, 256);
        strncpy(str, err->message, 255);
        g_error_free(err);

        scm_misc_error("gi-method-send",
                       "error invoking method '~a': ~a",
                       scm_list_2(s_name, scm_from_utf8_string(str)));
        return SCM_BOOL_F;
    }

    /* We've actually made a call.  Hooray! Convert the output
   * arguments and return values into Scheme objects.  Free the
   * C objects if necessary.  Return the output either as
   * a plain list or as a values list. */
    GITypeInfo *return_typeinfo = g_callable_info_get_return_type(info);
    SCM s_return = gi_giargument_to_object(&return_arg,
                                           return_typeinfo,
                                           g_callable_info_get_caller_owns(info));
    g_base_info_unref(return_typeinfo);

    SCM output;
    if (scm_is_eq(s_return, SCM_UNSPECIFIED))
        output = SCM_EOL;
    else
        output = scm_list_1(s_return);

    int n_args = g_callable_info_get_n_args((GICallableInfo *)info);
    for (int i = 0; i < n_args; i++)
    {
        GIArgInfo *arg_info = g_callable_info_get_arg((GICallableInfo *)info, i);
        g_assert(arg_info != NULL);

        GIDirection dir = g_arg_info_get_direction(arg_info);

        if (dir == GI_DIRECTION_OUT || dir == GI_DIRECTION_INOUT)
        {
            GITypeInfo *arg_typeinfo = g_arg_info_get_type(arg_info);
            SCM entry = gi_giargument_to_object(&out_args[i],
                                                arg_typeinfo,
                                                g_arg_info_get_ownership_transfer(arg_info));
            output = scm_append(scm_list_2(output, scm_list_1(entry)));
            g_base_info_unref(arg_typeinfo);
        }
        g_base_info_unref(arg_info);
    }

    g_debug("Completed invocation of method %s", method_name);
    free(method_name);
    if (n_output_args == 0)
        return s_return;
    else
        return output;
}

static SCM
scm_gi_function_invoke(SCM s_name, SCM s_args)
{
    GError *err = NULL;

    SCM_ASSERT(scm_is_string(s_name), s_name, SCM_ARG1,
               "gi-function-invoke");

    GIFunctionInfo *info = NULL;

    {
        char *name = scm_to_utf8_string(s_name);
        g_debug("in gi-function-invoke for %s", name);
        info = g_hash_table_lookup(gi_functions, name);
        free(name);
        name = NULL;

        name = scm_to_utf8_string(scm_simple_format(SCM_BOOL_F, scm_from_utf8_string("args ~S"), scm_list_1(s_args)));
        g_debug(" %s", name);
        free(name);
    }

    if (!info)
    {
        scm_misc_error("gi-function-invoke",
                       "unknown procedure '~a'",
                       scm_list_1(s_name));
        return SCM_BOOL_F;
    }

    int n_input_args_received;
    if (SCM_UNBNDP(s_args))
        n_input_args_received = 0;
    else
        n_input_args_received = scm_to_int(scm_length(s_args));
    g_debug("\t%d input arguments received", n_input_args_received);

    /* Count the number of required input arguments, and store
   * the arg info in a newly allocate array. */
    int n_input_args, n_output_args;
    function_info_count_args(info, &n_input_args, &n_output_args);
    g_debug("\t%d input arguments expected", n_input_args);
    g_debug("\t%d output arguments expected", n_output_args);

    if (n_input_args_received != n_input_args)
    {
        scm_misc_error("function-invoke",
                       "wrong number of input arguments for funtion '~a', expected ~S, received ~s",
                       scm_list_3(s_name,
                                  scm_from_int(n_input_args),
                                  scm_from_int(n_input_args_received)));
        return SCM_BOOL_F;
    }

    char *errstr = NULL;
    if (!function_info_typecheck_args(info, s_args, &errstr))
    {
        g_assert(errstr != NULL);

        SCM s_errstr = scm_from_utf8_string(errstr);
        g_free(errstr);
        scm_misc_error("gi-function-invoke",
                       "wrong type argument for function '~a' - ~s",
                       scm_list_2(s_name, s_errstr));
        return SCM_BOOL_F;
    }

    GIArgument *in_args = function_info_convert_args(info, s_args);

    /* Allocate a GIArgument list of the output and return values. */
    GIArgument *out_args = NULL;
    if (n_output_args > 0)
        out_args = g_new0(GIArgument, n_output_args);
    GIArgument return_arg;

    /* Make the call. */
    gboolean ret = g_function_info_invoke(info, in_args, n_input_args,
                                          out_args, n_output_args,
                                          &return_arg, &err);

    /* Free any allocated input */
    function_info_release_args(info, in_args);
    g_free(in_args);
    in_args = NULL;

    /* If there is a GError, write an error, free, and exit. */
    if (!ret)
    {
        char str[256];
        memset(str, 0, 256);
        strncpy(str, err->message, 255);
        g_error_free(err);

        scm_misc_error("gi-function-invoke",
                       "error invoking function '~a': ~a",
                       scm_list_2(s_name, scm_from_utf8_string(str)));
        return SCM_BOOL_F;
    }

    /* We've actually made a call.  Hooray! Convert the output
   * arguments and return values into Scheme objects.  Free the
   * C objects if necessary.  Return the output either as
   * a plain list or as a values list. */
    GITypeInfo *return_typeinfo = g_callable_info_get_return_type(info);
    SCM s_return = gi_giargument_to_object(&return_arg,
                                           return_typeinfo,
                                           g_callable_info_get_caller_owns(info));
    g_base_info_unref(return_typeinfo);
    SCM output;
    if (scm_is_eq(s_return, SCM_UNSPECIFIED))
        output = SCM_EOL;
    else
        output = scm_list_1(s_return);

    int n_args = g_callable_info_get_n_args((GICallableInfo *)info);
    for (int i = 0; i < n_args; i++)
    {
        GIArgInfo *arg_info = g_callable_info_get_arg((GICallableInfo *)info, i);
        g_assert(arg_info != NULL);

        GIDirection dir = g_arg_info_get_direction(arg_info);

        if (dir == GI_DIRECTION_OUT || dir == GI_DIRECTION_INOUT)
        {
            GITypeInfo *arg_typeinfo = g_arg_info_get_type(arg_info);
            SCM entry = gi_giargument_to_object(&out_args[i],
                                                arg_typeinfo,
                                                g_arg_info_get_ownership_transfer(arg_info));
            output = scm_append(scm_list_2(output, scm_list_1(entry)));
            g_base_info_unref(arg_typeinfo);
        }
        g_base_info_unref(arg_info);
    }

    if (n_output_args == 0)
        return s_return;
    else
        return output;
}

static void
unload_repository(const char *category, GHashTable **p_hash_table)
{
    if (*p_hash_table)
    {
        g_debug("destroying %s hash table", category);
        g_hash_table_destroy(*p_hash_table);
        *p_hash_table = NULL;
    }
}

static SCM
scm_gi_unload_repositories(void)
{
    unload_repository("constants", &gi_constants);
    unload_repository("enums", &gi_enums);
    unload_repository("flags", &gi_flags);
    unload_repository("functions", &gi_functions);
    unload_repository("methods", &gi_methods);
    unload_repository("callbacks", &gi_callbacks);
    unload_repository("structs", &gi_structs);
    unload_repository("unions", &gi_unions);
    unload_repository("objects", &gi_objects);
    unload_repository("interfaces", &gi_interfaces);
    return SCM_UNSPECIFIED;
}

GType
gir_lookup_type (const char *namespace, const char *name)
{
    gpointer ptr = NULL;
  if (gi_objects)
    ptr = g_hash_table_lookup (gi_objects, name);
  if (!ptr && gi_structs)
    ptr = g_hash_table_lookup (gi_structs, name);
  if (!ptr && gi_unions)
    ptr = g_hash_table_lookup (gi_unions, name);
    if (!ptr)
        return G_TYPE_NONE;
    return g_registered_type_info_get_g_type (ptr);
}

static SCM
scm_gi_lookup_type (SCM s_type_name)
{
  SCM_ASSERT (scm_is_string (s_type_name), s_type_name, SCM_ARG1, "gi-lookup-type");
  char *name = scm_to_utf8_string (s_type_name);
  gpointer ptr = NULL;

  if (gi_objects)
    ptr = g_hash_table_lookup (gi_objects, name);
  if (!ptr && gi_structs)
    ptr = g_hash_table_lookup (gi_structs, name);
  if (!ptr && gi_unions)
    ptr = g_hash_table_lookup (gi_unions, name);
  free (name);
  if (!ptr)
    scm_misc_error ("gi-lookup-type",
            "Cannot find an object, struct, or union type named '~a'",
            scm_list_1 (s_type_name));
  return gi_gtype_c2g (g_registered_type_info_get_g_type (ptr));
}

/* re pygi_type_import_by_gi_info */
SCM gi_type_import_by_gi_info(GIBaseInfo *info)
{
    const gchar *namespace_ = g_base_info_get_namespace(info);
    const gchar *name = g_base_info_get_name(info);
    GIInfoType info_type = g_base_info_get_type (info);
    g_debug("gi_type_import_by_gi_info: namespace '%s' name '%s'",
            namespace_, name);

    switch (info_type)
    {
    case GI_INFO_TYPE_STRUCT:
    {
        GType g_type;
        SCM s_type;
        if (g_hash_table_contains(gi_structs, name))
        {
            g_debug("type name '%s' is found in structs", name);
            /* Have we made a Guile type for this struct? */
            g_type = g_registered_type_info_get_g_type((GIRegisteredTypeInfo *)info);
        }
    }
    break;
    default:
        g_critical("unimplemented");
    }
    return SCM_UNSPECIFIED;
}

#ifdef FIGURE_OUT_ALL_ARG_TYPES
static SCM
scm_dump_all_arg_types(void)
{
    guint len = gi_arg_infos->len;
    if (len == 0)
        return SCM_UNSPECIFIED;

    FILE *fp = fopen("arg_infos.txt","wt");

    for (guint i = 0; i < len; i ++)
    {
        struct _arg_info_func_name *aifn = gi_arg_infos->pdata[i];
        GIArgInfo *ai = aifn->ai;
        GIDirection dir = g_arg_info_get_direction (ai);
        GITypeInfo *ti = g_arg_info_get_type (ai);
        gboolean skip = g_arg_info_is_skip (ai);
        if (skip)
            continue;

        fprintf(fp, "%-11s", g_type_tag_to_string (g_type_info_get_tag (ti)));
        if (g_type_info_is_pointer (ti))
            fprintf(fp, "* ");
        else
            fprintf(fp, "  ");

        if (dir == GI_DIRECTION_IN)
            fprintf(fp, "IN    ");
        else if (dir == GI_DIRECTION_INOUT)
            fprintf(fp, "INOUT ");
        else if (dir == GI_DIRECTION_OUT)
            fprintf(fp, "OUT   ");


        if (g_type_info_get_tag (ti) == GI_TYPE_TAG_ARRAY)
        {
            fprintf(fp, "LEN %3d SIZE %3d ", g_type_info_get_array_length(ti), g_type_info_get_array_fixed_size (ti));
            if (g_type_info_is_zero_terminated (ti))
                fprintf(fp, "ZERO_TERM ");
            else
                fprintf(fp, "          ");
            GIArrayType arrt =  g_type_info_get_array_type (ti);
            if (arrt == GI_ARRAY_TYPE_C)
                fprintf(fp, "C      ");
            else if (arrt == GI_ARRAY_TYPE_BYTE_ARRAY)
                fprintf(fp, "BYTE   ");
            else if (arrt == GI_ARRAY_TYPE_ARRAY)
                fprintf(fp, "GArray ");
            else if (arrt == GI_ARRAY_TYPE_PTR_ARRAY)
                fprintf(fp, "PTR    ");

            GITypeInfo *pti = g_type_info_get_param_type(ti, 0);
            fprintf(fp, "%-11s", g_type_tag_to_string (g_type_info_get_tag (pti)));
            if (g_type_info_is_pointer (pti))
                fprintf(fp, "* ");
            else
                fprintf(fp, "  ");
        GIBaseInfo *pbi = g_type_info_get_interface(pti);
        if (pbi)
        {
            GIInfoType pit = g_base_info_get_type(pbi);
            if (pit == GI_INFO_TYPE_INVALID)
                fprintf(fp, "INVALID   ");
            else if (pit == GI_INFO_TYPE_FUNCTION)
                fprintf(fp, "FUNCTION  ");
            else if (pit == GI_INFO_TYPE_CALLBACK)
                fprintf(fp, "CALLBACK  ");
            else if (pit == GI_INFO_TYPE_STRUCT)
                fprintf(fp, "STRUCT    ");
            else if (pit == GI_INFO_TYPE_STRUCT)
                fprintf(fp, "BOXED     ");
            else if (pit == GI_INFO_TYPE_ENUM)
                fprintf(fp, "ENUM      ");
            else if (pit == GI_INFO_TYPE_FLAGS)
                fprintf(fp, "FLAGS     ");
            else if (pit == GI_INFO_TYPE_OBJECT)
                fprintf(fp, "OBJECT    ");
            else if (pit == GI_INFO_TYPE_INTERFACE)
                fprintf(fp, "INTERFACE ");
            else if (pit == GI_INFO_TYPE_CONSTANT)
                fprintf(fp, "CONSTANT  ");
            else if (pit == GI_INFO_TYPE_UNION)
                fprintf(fp, "UNION     ");
            else if (pit == GI_INFO_TYPE_VALUE)
                fprintf(fp, "VALUE     ");
            else if (pit == GI_INFO_TYPE_SIGNAL)
                fprintf(fp, "SIGNAL    ");
            else if (pit == GI_INFO_TYPE_VFUNC)
                fprintf(fp, "VFUNC     ");
            else if (pit == GI_INFO_TYPE_PROPERTY)
                fprintf(fp, "PROPERTY  ");
            else if (pit == GI_INFO_TYPE_FIELD)
                fprintf(fp, "FIELD     ");
            else if (pit == GI_INFO_TYPE_ARG)
                fprintf(fp, "ARG       ");
            else if (pit == GI_INFO_TYPE_TYPE)
                fprintf(fp, "TYPE      ");
            fprintf(fp, "%-11s ", g_base_info_get_name (pbi));

        }
        }

        GIBaseInfo *bi = g_type_info_get_interface(ti);
        if (bi)
        {
            GIInfoType it = g_base_info_get_type(bi);
            if (it == GI_INFO_TYPE_INVALID)
                fprintf(fp, "INVALID   ");
            else if (it == GI_INFO_TYPE_FUNCTION)
                fprintf(fp, "FUNCTION  ");
            else if (it == GI_INFO_TYPE_CALLBACK)
                fprintf(fp, "CALLBACK  ");
            else if (it == GI_INFO_TYPE_STRUCT)
                fprintf(fp, "STRUCT    ");
            else if (it == GI_INFO_TYPE_STRUCT)
                fprintf(fp, "BOXED     ");
            else if (it == GI_INFO_TYPE_ENUM)
                fprintf(fp, "ENUM      ");
            else if (it == GI_INFO_TYPE_FLAGS)
                fprintf(fp, "FLAGS     ");
            else if (it == GI_INFO_TYPE_OBJECT)
                fprintf(fp, "OBJECT    ");
            else if (it == GI_INFO_TYPE_INTERFACE)
                fprintf(fp, "INTERFACE ");
            else if (it == GI_INFO_TYPE_CONSTANT)
                fprintf(fp, "CONSTANT  ");
            else if (it == GI_INFO_TYPE_UNION)
                fprintf(fp, "UNION     ");
            else if (it == GI_INFO_TYPE_VALUE)
                fprintf(fp, "VALUE     ");
            else if (it == GI_INFO_TYPE_SIGNAL)
                fprintf(fp, "SIGNAL    ");
            else if (it == GI_INFO_TYPE_VFUNC)
                fprintf(fp, "VFUNC     ");
            else if (it == GI_INFO_TYPE_PROPERTY)
                fprintf(fp, "PROPERTY  ");
            else if (it == GI_INFO_TYPE_FIELD)
                fprintf(fp, "FIELD     ");
            else if (it == GI_INFO_TYPE_ARG)
                fprintf(fp, "ARG       ");
            else if (it == GI_INFO_TYPE_TYPE)
                fprintf(fp, "TYPE      ");
            fprintf(fp, "%-11s ", g_base_info_get_name (bi));

        }


        gboolean null = g_arg_info_may_be_null (ai);
        if (null)
            fprintf(fp, "NULL_OK ");
        else
            fprintf(fp, "        ");
        gboolean caller_allocate = g_arg_info_is_caller_allocates (ai);
        if (caller_allocate)
            fprintf(fp, "ALLOC ");
        else
            fprintf(fp, "      ");
        gboolean optional = g_arg_info_is_optional (ai);
        if (optional)
            fprintf(fp, "OPT ");
        else
            fprintf(fp, "    ");
        GITransfer transfer = g_arg_info_get_ownership_transfer (ai);
        if (transfer == GI_TRANSFER_NOTHING)
            fprintf(fp, "CONST   ");
        else if (transfer == GI_TRANSFER_CONTAINER)
            fprintf(fp, "SHALLOW ");
        else if (transfer == GI_TRANSFER_EVERYTHING)
            fprintf(fp, "DEEP    ");
        fprintf(fp, "%s %s ", g_base_info_get_name(ai), aifn->name);
        fprintf(fp, "\n");
    }
    fclose(fp);
    return SCM_UNSPECIFIED;
}
#endif

void
gir_init_func2(void)
{
#ifdef FIGURE_OUT_ALL_ARG_TYPES
    gi_arg_infos = g_ptr_array_new();
#endif
  scm_c_define_gsubr ("gi-load-repository", 2, 0, 0,
              scm_gi_load_repository);
  scm_c_define_gsubr ("gi-unload-repositories", 0, 0, 0,
              scm_gi_unload_repositories);
  scm_c_define_gsubr ("gi-constant-value", 2, 0, 0, scm_gi_constant_value);
  scm_c_define_gsubr ("gi-flag-value", 3, 0, 0, scm_gi_flag_value);
  scm_c_define_gsubr ("gi-enum-value", 3, 0, 0, scm_gi_enum_value);
  scm_c_define_gsubr ("gi-struct-ref", 4, 0, 0, scm_gi_struct_ref);
  scm_c_define_gsubr ("gi-struct-set", 5, 0, 0, scm_gi_struct_set);
  scm_c_define_gsubr ("gi-function-invoke", 1, 0, 1, scm_gi_function_invoke);
  scm_c_define_gsubr ("gi-method-prepare", 1, 0, 1, scm_gi_method_prepare);
  scm_c_define_gsubr ("gi-method-send", 2, 0, 0, scm_gi_method_send);
  scm_c_define_gsubr ("gi-lookup-type", 1, 0, 0, scm_gi_lookup_type);
  scm_c_define_gsubr ("gi-dump-arg-types", 0, 0, 0, scm_dump_all_arg_types);
}
