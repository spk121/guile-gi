#ifndef GIG_UTIL_H
#define GIG_UTIL_H

#include <glib.h>
#include <libguile.h>
#include <girepository.h>

// *INDENT-OFF*
G_BEGIN_DECLS
// *INDENT-ON*

G_GNUC_MALLOC gchar *gig_callable_info_make_name(GICallableInfo *info, const gchar *prefix);
const gchar *gig_constant_strip_prefix(const gchar *name, const gchar *strip_prefix);
gchar *gig_gname_to_scm_name(const gchar *gname);
G_GNUC_INTERNAL SCM scm_c_list_ref(SCM list, gsize k);
G_GNUC_INTERNAL gboolean scm_is_list(SCM obj);
G_GNUC_INTERNAL gsize scm_c_length(SCM list);
gpointer scm_dynwind_or_bust(const gchar *subr, gpointer mem);
SCM scm_class_ref(SCM cls, SCM slot);
SCM scm_class_set_x(SCM cls, SCM slot, SCM val);
SCM scm_drop_right_1(SCM lst);
SCM scm_c_reexport(const gchar *name, ...);
void scm_printf(SCM port, const gchar *fmt, ...);
const gchar *g_base_info_get_name_safe(GIBaseInfo *info);
gchar *g_registered_type_info_get_qualified_name(GIRegisteredTypeInfo *info);
G_GNUC_INTERNAL gchar *scm_write_to_utf8_stringn(SCM x, gsize max_len);

#define scm_is_equal(a,b) scm_is_true(scm_equal_p(a,b))

#define SCM_UNBND_TO_BOOL_F(obj) \
    do {                         \
        if (SCM_UNBNDP (obj))    \
            obj = SCM_BOOL_F;    \
    } while (0)                  \

G_END_DECLS
#endif
#define gig_debug_internal(level,domain,...)                  \
    do {                                                      \
        g_log_structured(G_LOG_DOMAIN, level,                 \
                         "CODE_FILE", __FILE__,               \
                         "CODE_LINE", G_STRINGIFY(__LINE__),  \
                         "CODE_FUNC", __func__,               \
                         "GIG_DOMAIN", domain,                \
                         "MESSAGE", __VA_ARGS__);             \
    } while (FALSE)
#define gig_debug_transfer(...) gig_debug_internal(G_LOG_LEVEL_DEBUG, "transfers", __VA_ARGS__)
#define gig_debug_load(...)     gig_debug_internal(G_LOG_LEVEL_DEBUG, "load", __VA_ARGS__)
#define gig_warning_load(...)   gig_debug_internal(G_LOG_LEVEL_WARNING, "load", __VA_ARGS__)
#define gig_critical_load(...)  gig_debug_internal(G_LOG_LEVEL_CRITICAL, "load", __VA_ARGS__)
#if (SCM_MAJOR_VERSION == 2) || (SCM_MAJOR_VERSION == 3 && SCM_MINOR_VERSION == 0 && SCM_MICRO_VERSION < 4)
#define scm_c_bitvector_count(x) scm_to_size_t(scm_bit_count(SCM_BOOL_T, (x)))
#endif
