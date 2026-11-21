// Copyright (C) 2026 Michael L. Gran

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

// Plain structs without a GType that GTK, GSK and GDK need.
//
// guile-gi doesn't link against those libraries, so each struct's
// layout is mirrored here and checked against the typelib before the
// struct is used: if a library changes a layout, the struct is
// reported and left unbound instead of corrupting memory.
//
// Each struct is a foreign object type with three slots: the C
// struct, whether the object owns it, and its GigForeignStruct.
// Plain structs always own a copy of their data, allocated with
// malloc, as are the strings in them.  Opaque structs are borrowed
// pointers, valid only while C lends them, e.g. during a signal.

#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <glib-object.h>
#include "../core.h"
#include "gig_type.h"
#include "gig_type_priv.h"
#include "gig_flag.h"
#include "gig_flag_priv.h"
#include "gig_foreign_struct.h"

typedef enum
{
    FIELD_INT,                  // int32_t
    FIELD_UINT,                 // uint32_t
    FIELD_SIZE,                 // size_t
    FIELD_FLOAT,                // float
    FIELD_DOUBLE,               // double
    FIELD_BOOLEAN,              // gboolean
    FIELD_ENUM,                 // int, of the enum type TYPE_NS.TYPE_NAME
    FIELD_FLAGS,                // unsigned, of the flags type TYPE_NS.TYPE_NAME
    FIELD_STRING,               // char *
    FIELD_STRV,                 // NULL-terminated char **
    FIELD_POINTER,              // void *
    FIELD_BOXED,                // COUNT structs of the boxed type TYPE_NS.TYPE_NAME
    FIELD_DOUBLE_ARRAY          // COUNT doubles
} FieldKind;

typedef struct
{
    const char *name;
    FieldKind kind;
    size_t offset;
    size_t size;                // of the whole field
    size_t count;               // elements, for arrays
    const char *type_ns;
    const char *type_name;
    scm_t_subr ref;
    scm_t_subr set;
    GType gtype;                // of TYPE_NS.TYPE_NAME, looked up when first used
} Field;

struct _GigForeignStruct
{
    const char *ns;
    const char *name;
    const char *c_name;
    size_t size;                // 0 for opaque structs
    Field *fields;
    size_t n_fields;
    scm_t_subr make;            // constructor, or pointer->struct for opaque structs
    SCM type;
    int layout;                 // 0 unchecked, 1 matches the typelib, -1 doesn't
};

enum
{
    SLOT_DATA,
    SLOT_OWNED,
    SLOT_DESC
};

static SCM field_ref(GigForeignStruct *fs, const char *field, SCM obj);
static void field_set(GigForeignStruct *fs, const char *field, SCM obj, SCM val);
static SCM construct(GigForeignStruct *fs, SCM rest);
static SCM from_pointer(GigForeignStruct *fs, SCM ptr);

////////////////////////////////////////////////////////////////
// Mirrors of the C layouts
////////////////////////////////////////////////////////////////

typedef struct
{
    float red, green, blue, alpha;
} MirrorRGBA;                   // GdkRGBA

typedef struct
{
    float x, y;
} MirrorPoint;                  // graphene_point_t

typedef struct
{
    float width, height;
} MirrorSize;                   // graphene_size_t

typedef struct
{
    MirrorPoint origin;
    MirrorSize size;
} MirrorRect;                   // graphene_rect_t

typedef struct
{
    MirrorRect bounds;
    MirrorSize corner[4];
} MirrorRoundedRect;            // GskRoundedRect

typedef struct
{
    float offset;
    MirrorRGBA color;
} MirrorColorStop;              // GskColorStop

typedef struct
{
    MirrorRGBA color;
    float dx, dy, radius;
} MirrorShadow;                 // GskShadow

typedef struct
{
    size_t bytes, chars, lines, line_bytes, line_chars;
} MirrorLocation;               // GskParseLocation, GtkCssLocation

typedef struct
{
    unsigned keycode;
    int group;
    int level;
} MirrorKeymapKey;              // GdkKeymapKey

#define MIRROR_AXIS_LAST 12     // GDK_AXIS_LAST
typedef struct
{
    uint32_t time;
    unsigned flags;
    double axes[MIRROR_AXIS_LAST];
} MirrorTimeCoord;              // GdkTimeCoord

typedef struct
{
    int start, end;
} MirrorPageRange;              // GtkPageRange

typedef struct
{
    void *data;
    int minimum_size;
    int natural_size;
} MirrorRequestedSize;          // GtkRequestedSize

typedef struct
{
    char *display_name;
    char *description;
    char *mime_type;
    char *app_name;
    char *app_exec;
    char **groups;
    gboolean is_private;
} MirrorRecentData;             // GtkRecentData

typedef struct
{
    int type;
    int index;
    int mode;
    char *label;
    char *action_name;
} MirrorPadActionEntry;         // GtkPadActionEntry

////////////////////////////////////////////////////////////////
// The structs
////////////////////////////////////////////////////////////////

// Each struct's fields are listed once, in typelib order, as
// X(id, mirror type, field, kind, count, type namespace, type name).
// From the list come the field table and a getter and setter gsubr for
// each field.

#define DECLARE_ACCESSORS(id, ctype, field, kind, count, tns, tname)    \
    static SCM id##_##field##_ref(SCM obj)                              \
    {                                                                   \
        return field_ref(&id##_struct, #field, obj);                    \
    }                                                                   \
    static SCM id##_##field##_set(SCM obj, SCM val)                     \
    {                                                                   \
        field_set(&id##_struct, #field, obj, val);                      \
        return SCM_UNSPECIFIED;                                         \
    }

#define FIELD_ENTRY(id, ctype, field, kind, count, tns, tname)          \
    { #field, FIELD_##kind, offsetof(ctype, field), sizeof(((ctype *)0)->field), \
      count, tns, tname, (scm_t_subr)id##_##field##_ref,                \
      (scm_t_subr)id##_##field##_set, G_TYPE_INVALID },

#define DEFINE_STRUCT(id, ns, name, c_name, ctype, FIELDS)              \
    static GigForeignStruct id##_struct;                                \
    FIELDS(DECLARE_ACCESSORS)                                           \
    static Field id##_fields[] = { FIELDS(FIELD_ENTRY) };               \
    static SCM id##_make(SCM rest)                                      \
    {                                                                   \
        return construct(&id##_struct, rest);                           \
    }                                                                   \
    static GigForeignStruct id##_struct = {                             \
        ns, name, c_name, sizeof(ctype), id##_fields,                   \
        G_N_ELEMENTS(id##_fields), (scm_t_subr)id##_make, SCM_BOOL_F, 0 \
    };

#define DEFINE_OPAQUE(id, ns, name, c_name)                             \
    static GigForeignStruct id##_struct;                                \
    static SCM id##_from_pointer(SCM ptr)                               \
    {                                                                   \
        return from_pointer(&id##_struct, ptr);                         \
    }                                                                   \
    static GigForeignStruct id##_struct = {                             \
        ns, name, c_name, 0, NULL, 0,                                   \
        (scm_t_subr)id##_from_pointer, SCM_BOOL_F, 0                    \
    };

#define ROUNDED_RECT_FIELDS(X)                                          \
    X(rounded_rect, MirrorRoundedRect, bounds, BOXED, 1, "Graphene", "Rect") \
    X(rounded_rect, MirrorRoundedRect, corner, BOXED, 4, "Graphene", "Size")
DEFINE_STRUCT(rounded_rect, "Gsk", "RoundedRect", "GskRoundedRect",
              MirrorRoundedRect, ROUNDED_RECT_FIELDS)

#define COLOR_STOP_FIELDS(X)                                            \
    X(color_stop, MirrorColorStop, offset, FLOAT, 1, NULL, NULL)        \
    X(color_stop, MirrorColorStop, color, BOXED, 1, "Gdk", "RGBA")
DEFINE_STRUCT(color_stop, "Gsk", "ColorStop", "GskColorStop",
              MirrorColorStop, COLOR_STOP_FIELDS)

#define SHADOW_FIELDS(X)                                                \
    X(shadow, MirrorShadow, color, BOXED, 1, "Gdk", "RGBA")             \
    X(shadow, MirrorShadow, dx, FLOAT, 1, NULL, NULL)                   \
    X(shadow, MirrorShadow, dy, FLOAT, 1, NULL, NULL)                   \
    X(shadow, MirrorShadow, radius, FLOAT, 1, NULL, NULL)
DEFINE_STRUCT(shadow, "Gsk", "Shadow", "GskShadow", MirrorShadow, SHADOW_FIELDS)

#define LOCATION_FIELDS(id, X)                                          \
    X(id, MirrorLocation, bytes, SIZE, 1, NULL, NULL)                   \
    X(id, MirrorLocation, chars, SIZE, 1, NULL, NULL)                   \
    X(id, MirrorLocation, lines, SIZE, 1, NULL, NULL)                   \
    X(id, MirrorLocation, line_bytes, SIZE, 1, NULL, NULL)              \
    X(id, MirrorLocation, line_chars, SIZE, 1, NULL, NULL)
#define PARSE_LOCATION_FIELDS(X) LOCATION_FIELDS(parse_location, X)
#define CSS_LOCATION_FIELDS(X) LOCATION_FIELDS(css_location, X)
DEFINE_STRUCT(parse_location, "Gsk", "ParseLocation", "GskParseLocation",
              MirrorLocation, PARSE_LOCATION_FIELDS)
DEFINE_STRUCT(css_location, "Gtk", "CssLocation", "GtkCssLocation",
              MirrorLocation, CSS_LOCATION_FIELDS)

#define KEYMAP_KEY_FIELDS(X)                                            \
    X(keymap_key, MirrorKeymapKey, keycode, UINT, 1, NULL, NULL)        \
    X(keymap_key, MirrorKeymapKey, group, INT, 1, NULL, NULL)           \
    X(keymap_key, MirrorKeymapKey, level, INT, 1, NULL, NULL)
DEFINE_STRUCT(keymap_key, "Gdk", "KeymapKey", "GdkKeymapKey",
              MirrorKeymapKey, KEYMAP_KEY_FIELDS)

#define TIME_COORD_FIELDS(X)                                            \
    X(time_coord, MirrorTimeCoord, time, UINT, 1, NULL, NULL)           \
    X(time_coord, MirrorTimeCoord, flags, FLAGS, 1, "Gdk", "AxisFlags") \
    X(time_coord, MirrorTimeCoord, axes, DOUBLE_ARRAY, MIRROR_AXIS_LAST, NULL, NULL)
DEFINE_STRUCT(time_coord, "Gdk", "TimeCoord", "GdkTimeCoord",
              MirrorTimeCoord, TIME_COORD_FIELDS)

#define PAGE_RANGE_FIELDS(X)                                            \
    X(page_range, MirrorPageRange, start, INT, 1, NULL, NULL)           \
    X(page_range, MirrorPageRange, end, INT, 1, NULL, NULL)
DEFINE_STRUCT(page_range, "Gtk", "PageRange", "GtkPageRange",
              MirrorPageRange, PAGE_RANGE_FIELDS)

#define REQUESTED_SIZE_FIELDS(X)                                        \
    X(requested_size, MirrorRequestedSize, data, POINTER, 1, NULL, NULL) \
    X(requested_size, MirrorRequestedSize, minimum_size, INT, 1, NULL, NULL) \
    X(requested_size, MirrorRequestedSize, natural_size, INT, 1, NULL, NULL)
DEFINE_STRUCT(requested_size, "Gtk", "RequestedSize", "GtkRequestedSize",
              MirrorRequestedSize, REQUESTED_SIZE_FIELDS)

#define RECENT_DATA_FIELDS(X)                                           \
    X(recent_data, MirrorRecentData, display_name, STRING, 1, NULL, NULL) \
    X(recent_data, MirrorRecentData, description, STRING, 1, NULL, NULL) \
    X(recent_data, MirrorRecentData, mime_type, STRING, 1, NULL, NULL)  \
    X(recent_data, MirrorRecentData, app_name, STRING, 1, NULL, NULL)   \
    X(recent_data, MirrorRecentData, app_exec, STRING, 1, NULL, NULL)   \
    X(recent_data, MirrorRecentData, groups, STRV, 1, NULL, NULL)       \
    X(recent_data, MirrorRecentData, is_private, BOOLEAN, 1, NULL, NULL)
DEFINE_STRUCT(recent_data, "Gtk", "RecentData", "GtkRecentData",
              MirrorRecentData, RECENT_DATA_FIELDS)

#define PAD_ACTION_ENTRY_FIELDS(X)                                      \
    X(pad_action_entry, MirrorPadActionEntry, type, ENUM, 1, "Gtk", "PadActionType") \
    X(pad_action_entry, MirrorPadActionEntry, index, INT, 1, NULL, NULL) \
    X(pad_action_entry, MirrorPadActionEntry, mode, INT, 1, NULL, NULL) \
    X(pad_action_entry, MirrorPadActionEntry, label, STRING, 1, NULL, NULL) \
    X(pad_action_entry, MirrorPadActionEntry, action_name, STRING, 1, NULL, NULL)
DEFINE_STRUCT(pad_action_entry, "Gtk", "PadActionEntry", "GtkPadActionEntry",
              MirrorPadActionEntry, PAD_ACTION_ENTRY_FIELDS)

DEFINE_OPAQUE(toplevel_size, "Gdk", "ToplevelSize", "GdkToplevelSize")
DEFINE_OPAQUE(drag_surface_size, "Gdk", "DragSurfaceSize", "GdkDragSurfaceSize")
DEFINE_OPAQUE(buildable_parse_context, "Gtk", "BuildableParseContext",
              "GtkBuildableParseContext")

static GigForeignStruct *all_structs[] = {
    &rounded_rect_struct,
    &color_stop_struct,
    &shadow_struct,
    &parse_location_struct,
    &css_location_struct,
    &keymap_key_struct,
    &time_coord_struct,
    &page_range_struct,
    &requested_size_struct,
    &recent_data_struct,
    &pad_action_entry_struct,
    &toplevel_size_struct,
    &drag_surface_size_struct,
    &buildable_parse_context_struct,
};

////////////////////////////////////////////////////////////////
// Lookup
////////////////////////////////////////////////////////////////

GType
gig_foreign_struct_get_type(void)
{
    static size_t type = 0;
    if (g_once_init_enter(&type))
        g_once_init_leave(&type, g_pointer_type_register_static("GigForeignStruct"));
    return type;
}

// Checks FS's mirrored layout against the typelib, once.
static bool
layout_matches(GigForeignStruct *fs)
{
    if (fs->layout != 0)
        return fs->layout > 0;

    GIBaseInfo *info = g_irepository_find_by_name(NULL, fs->ns, fs->name);
    bool ok = (info != NULL && GI_IS_STRUCT_INFO(info));
    const char *why = "it is not a struct in the typelib";

    if (ok && fs->size > 0) {
        if (g_struct_info_get_size(info) != fs->size) {
            ok = false;
            why = "its size differs from the typelib's";
        }
        else if ((size_t)g_struct_info_get_n_fields(info) != fs->n_fields) {
            ok = false;
            why = "its fields differ from the typelib's";
        }
        for (size_t i = 0; ok && i < fs->n_fields; i++) {
            GIFieldInfo *fi = g_struct_info_get_field(info, i);
            if (strcmp(g_base_info_get_name(fi), fs->fields[i].name) != 0
                || (size_t)g_field_info_get_offset(fi) != fs->fields[i].offset) {
                ok = false;
                why = "its fields differ from the typelib's";
            }
            g_base_info_unref(fi);
        }
    }
    if (info != NULL)
        g_base_info_unref(info);

    if (!ok)
        gig_warning_load("%s - not binding this struct because %s", fs->c_name, why);
    fs->layout = ok ? 1 : -1;
    return ok;
}

const GigForeignStruct *
gig_foreign_struct_find(GIBaseInfo *info)
{
    if (info == NULL || !GI_IS_STRUCT_INFO(info))
        return NULL;
    const char *ns = g_base_info_get_namespace(info);
    const char *name = g_base_info_get_name(info);
    for (size_t i = 0; i < G_N_ELEMENTS(all_structs); i++) {
        GigForeignStruct *fs = all_structs[i];
        if (strcmp(fs->ns, ns) == 0 && strcmp(fs->name, name) == 0)
            return layout_matches(fs) ? fs : NULL;
    }
    return NULL;
}

const char *
gig_foreign_struct_c_name(const GigForeignStruct *fs)
{
    return fs->c_name;
}

size_t
gig_foreign_struct_size(const GigForeignStruct *fs)
{
    return fs->size;
}

bool
gig_foreign_struct_is_opaque(const GigForeignStruct *fs)
{
    return fs->size == 0;
}

SCM
gig_foreign_struct_scheme_type(const GigForeignStruct *fs)
{
    return fs->type;
}

// The struct's name in its namespace, e.g. "RoundedRect".
const char *
gig_foreign_struct_name(const GigForeignStruct *fs)
{
    return fs->name;
}

size_t
gig_foreign_struct_n_fields(const GigForeignStruct *fs)
{
    return fs->n_fields;
}

const char *
gig_foreign_struct_field_name(const GigForeignStruct *fs, size_t i)
{
    return fs->fields[i].name;
}

// The GType of a field's boxed, enum or flags type.
static GType
field_gtype(GigForeignStruct *fs, Field *f)
{
    if (f->gtype == G_TYPE_INVALID) {
        GIBaseInfo *info = g_irepository_find_by_name(NULL, f->type_ns, f->type_name);
        GType gtype = G_TYPE_NONE;
        if (info != NULL) {
            if (GI_IS_REGISTERED_TYPE_INFO(info))
                gtype = g_registered_type_info_get_g_type(info);
            g_base_info_unref(info);
        }
        f->gtype = gtype;
    }
    if (f->gtype == G_TYPE_NONE && (f->kind == FIELD_BOXED))
        scm_misc_error(fs->c_name, "the type ~A.~A of field ~A is not available",
                       scm_list_3(scm_from_utf8_string(f->type_ns),
                                  scm_from_utf8_string(f->type_name),
                                  scm_from_utf8_string(f->name)));
    return f->gtype;
}

////////////////////////////////////////////////////////////////
// Objects
////////////////////////////////////////////////////////////////

static GigForeignStruct *
desc_of(SCM obj)
{
    if (!SCM_STRUCTP(obj))
        return NULL;
    SCM cls = SCM_CLASS_OF(obj);
    for (size_t i = 0; i < G_N_ELEMENTS(all_structs); i++)
        if (scm_is_eq(cls, all_structs[i]->type))
            return all_structs[i];
    return NULL;
}

static void
free_contents(GigForeignStruct *fs, void *data)
{
    for (size_t i = 0; i < fs->n_fields; i++) {
        Field *f = &fs->fields[i];
        void *p = (char *)data + f->offset;
        if (f->kind == FIELD_STRING)
            free(*(char **)p);
        else if (f->kind == FIELD_STRV) {
            char **strv = *(char ***)p;
            if (strv != NULL)
                for (char **s = strv; *s != NULL; s++)
                    free(*s);
            free(strv);
        }
    }
}

static void
finalize(SCM obj)
{
    void *data = scm_foreign_object_ref(obj, SLOT_DATA);
    GigForeignStruct *fs = scm_foreign_object_ref(obj, SLOT_DESC);
    if (data != NULL && scm_foreign_object_unsigned_ref(obj, SLOT_OWNED)) {
        free_contents(fs, data);
        free(data);
    }
}

static SCM
make_object(GigForeignStruct *fs, void *data, bool owned)
{
    void *slots[3] = { data, (void *)(uintptr_t)owned, fs };
    return scm_make_foreign_object_n(fs->type, 3, slots);
}

static char *
xstrdup_or_null(const char *str)
{
    return str ? xstrdup(str) : NULL;
}

// A new struct that owns a deep copy of SRC, or zeros if SRC is NULL.
static SCM
copy_struct(GigForeignStruct *fs, const void *src)
{
    void *data = xcalloc(1, fs->size);
    if (src != NULL) {
        memcpy(data, src, fs->size);
        for (size_t i = 0; i < fs->n_fields; i++) {
            Field *f = &fs->fields[i];
            void *p = (char *)data + f->offset;
            if (f->kind == FIELD_STRING)
                *(char **)p = xstrdup_or_null(*(char **)p);
            else if (f->kind == FIELD_STRV && *(char ***)p != NULL) {
                char **old = *(char ***)p;
                size_t n = strvlen((const char **)old);
                char **strv = xcalloc(n + 1, sizeof(char *));
                for (size_t k = 0; k < n; k++)
                    strv[k] = xstrdup(old[k]);
                *(char ***)p = strv;
            }
        }
    }
    return make_object(fs, data, true);
}

SCM
gig_foreign_struct_to_scm(const GigForeignStruct *_fs, const void *ptr)
{
    GigForeignStruct *fs = (GigForeignStruct *)_fs;
    if (ptr == NULL)
        return SCM_BOOL_F;
    if (gig_foreign_struct_is_opaque(fs))
        return make_object(fs, (void *)ptr, false);
    return copy_struct(fs, ptr);
}

void *
gig_foreign_struct_peek(const GigForeignStruct *fs, SCM obj, const char *subr, int argpos)
{
    if (gig_foreign_struct_is_opaque(fs) && SCM_POINTER_P(obj))
        return SCM_POINTER_VALUE(obj);
    if (desc_of(obj) != fs)
        scm_wrong_type_arg_msg(subr, argpos, obj, fs->c_name);
    void *data = scm_foreign_object_ref(obj, SLOT_DATA);
    if (data == NULL)
        scm_misc_error(subr, "~S is not initialized", scm_list_1(obj));
    return data;
}

void *
gig_foreign_struct_peek_any(SCM obj)
{
    if (desc_of(obj) == NULL)
        return NULL;
    return scm_foreign_object_ref(obj, SLOT_DATA);
}

////////////////////////////////////////////////////////////////
// Fields
////////////////////////////////////////////////////////////////

static Field *
find_field(GigForeignStruct *fs, const char *name)
{
    for (size_t i = 0; i < fs->n_fields; i++)
        if (strcmp(fs->fields[i].name, name) == 0)
            return &fs->fields[i];
    assert_not_reached();
    return NULL;
}

static SCM
boxed_ref(GigForeignStruct *fs, Field *f, void *p)
{
    GType gtype = field_gtype(fs, f);
    return gig_type_transfer_object(gtype, g_boxed_copy(gtype, p), GI_TRANSFER_EVERYTHING);
}

static void
boxed_set(GigForeignStruct *fs, Field *f, void *p, SCM val, size_t elt_size, int argpos)
{
    GType gtype = field_gtype(fs, f);
    if (!gig_type_check_typed_object(val, gig_type_get_scheme_type(gtype)))
        scm_wrong_type_arg_msg(fs->c_name, argpos, val, g_type_name(gtype));
    void *src = gig_type_peek_object(val);
    if (src == NULL)
        scm_wrong_type_arg_msg(fs->c_name, argpos, val, g_type_name(gtype));
    memcpy(p, src, elt_size);
}

static SCM
field_ref(GigForeignStruct *fs, const char *name, SCM obj)
{
    Field *f = find_field(fs, name);
    void *data = gig_foreign_struct_peek(fs, obj, name, SCM_ARG1);
    void *p = (char *)data + f->offset;

    switch (f->kind) {
    case FIELD_INT:
        return scm_from_int32(*(int32_t *)p);
    case FIELD_UINT:
        return scm_from_uint32(*(uint32_t *)p);
    case FIELD_SIZE:
        return scm_from_size_t(*(size_t *)p);
    case FIELD_FLOAT:
        return scm_from_double(*(float *)p);
    case FIELD_DOUBLE:
        return scm_from_double(*(double *)p);
    case FIELD_BOOLEAN:
        return scm_from_bool(*(gboolean *)p);
    case FIELD_ENUM:
    {
        GType gtype = field_gtype(fs, f);
        if (gtype == G_TYPE_NONE)
            return scm_from_int(*(int *)p);
        return gig_int_to_enum(*(int *)p, gtype);
    }
    case FIELD_FLAGS:
    {
        GType gtype = field_gtype(fs, f);
        if (gtype == G_TYPE_NONE)
            return scm_from_uint(*(unsigned *)p);
        return gig_uint_to_flags(*(unsigned *)p, gtype);
    }
    case FIELD_STRING:
        return *(char **)p ? scm_from_utf8_string(*(char **)p) : SCM_BOOL_F;
    case FIELD_STRV:
    {
        char **strv = *(char ***)p;
        if (strv == NULL)
            return SCM_BOOL_F;
        SCM lst = SCM_EOL;
        for (size_t n = strvlen((const char **)strv); n > 0; n--)
            lst = scm_cons(scm_from_utf8_string(strv[n - 1]), lst);
        return lst;
    }
    case FIELD_POINTER:
        return *(void **)p ? scm_from_pointer(*(void **)p, NULL) : SCM_BOOL_F;
    case FIELD_BOXED:
    {
        if (f->count == 1)
            return boxed_ref(fs, f, p);
        size_t elt_size = f->size / f->count;
        SCM vec = scm_c_make_vector(f->count, SCM_BOOL_F);
        for (size_t i = 0; i < f->count; i++)
            scm_c_vector_set_x(vec, i, boxed_ref(fs, f, (char *)p + i * elt_size));
        return vec;
    }
    case FIELD_DOUBLE_ARRAY:
        return scm_take_f64vector(xmemdup(p, f->size), f->count);
    }
    assert_not_reached();
    return SCM_BOOL_F;
}

// Values in a list or vector, as a vector.
static SCM
sequence_to_vector(SCM val)
{
    if (scm_is_vector(val))
        return val;
    if (scm_is_true(scm_list_p(val)))
        return scm_vector(val);
    return SCM_BOOL_F;
}

static void
field_set(GigForeignStruct *fs, const char *name, SCM obj, SCM val)
{
    Field *f = find_field(fs, name);
    void *data = gig_foreign_struct_peek(fs, obj, name, SCM_ARG1);
    void *p = (char *)data + f->offset;

    switch (f->kind) {
    case FIELD_INT:
        *(int32_t *)p = scm_to_int32(val);
        break;
    case FIELD_UINT:
        *(uint32_t *)p = scm_to_uint32(val);
        break;
    case FIELD_SIZE:
        *(size_t *)p = scm_to_size_t(val);
        break;
    case FIELD_FLOAT:
        *(float *)p = (float)scm_to_double(val);
        break;
    case FIELD_DOUBLE:
        *(double *)p = scm_to_double(val);
        break;
    case FIELD_BOOLEAN:
        *(gboolean *)p = scm_is_true(val);
        break;
    case FIELD_ENUM:
        // A number, an enum, or a symbol naming one of the enum's values.
        if (scm_is_integer(val))
            *(int *)p = scm_to_int(val);
        else if (scm_is_symbol(val) && field_gtype(fs, f) != G_TYPE_NONE)
            *(int *)p = gig_enum_to_int(gig_symbol_to_enum
                                        (gig_type_get_scheme_type(field_gtype(fs, f)), val));
        else
            *(int *)p = gig_enum_to_int(val);
        break;
    case FIELD_FLAGS:
        // A number, a flags value, or a list of symbols naming flags.
        if (scm_is_integer(val))
            *(unsigned *)p = scm_to_uint(val);
        else if (scm_is_true(scm_list_p(val)) && field_gtype(fs, f) != G_TYPE_NONE)
            *(unsigned *)p = gig_flags_to_uint(gig_list_to_flags
                                               (gig_type_get_scheme_type(field_gtype(fs, f)), val));
        else
            *(unsigned *)p = gig_flags_to_uint(val);
        break;
    case FIELD_STRING:
    {
        char *str = scm_is_false(val) ? NULL : scm_to_utf8_string(val);
        free(*(char **)p);
        *(char **)p = str;
        break;
    }
    case FIELD_STRV:
    {
        char **strv = NULL;
        if (scm_is_true(val)) {
            SCM vec = sequence_to_vector(val);
            if (scm_is_false(vec))
                scm_wrong_type_arg_msg(name, SCM_ARG2, val, "list of strings");
            size_t n = scm_c_vector_length(vec);
            for (size_t i = 0; i < n; i++)
                if (!scm_is_string(scm_c_vector_ref(vec, i)))
                    scm_wrong_type_arg_msg(name, SCM_ARG2, val, "list of strings");
            strv = xcalloc(n + 1, sizeof(char *));
            for (size_t i = 0; i < n; i++)
                strv[i] = scm_to_utf8_string(scm_c_vector_ref(vec, i));
        }
        char **old = *(char ***)p;
        if (old != NULL)
            for (char **s = old; *s != NULL; s++)
                free(*s);
        free(old);
        *(char ***)p = strv;
        break;
    }
    case FIELD_POINTER:
        if (scm_is_false(val))
            *(void **)p = NULL;
        else if (SCM_POINTER_P(val))
            *(void **)p = SCM_POINTER_VALUE(val);
        else
            scm_wrong_type_arg_msg(name, SCM_ARG2, val, "pointer or #f");
        break;
    case FIELD_BOXED:
    {
        size_t elt_size = f->size / f->count;
        if (f->count == 1) {
            boxed_set(fs, f, p, val, elt_size, SCM_ARG2);
            break;
        }
        SCM vec = sequence_to_vector(val);
        if (scm_is_false(vec) || scm_c_vector_length(vec) != f->count)
            scm_misc_error(name, "expected ~A values of type ~A, but got ~S",
                           scm_list_3(scm_from_size_t(f->count),
                                      scm_from_utf8_string(g_type_name(field_gtype(fs, f))),
                                      val));
        // Check every element before changing any.
        uint8_t tmp[f->size];
        for (size_t i = 0; i < f->count; i++)
            boxed_set(fs, f, tmp + i * elt_size, scm_c_vector_ref(vec, i), elt_size, SCM_ARG2);
        memcpy(p, tmp, f->size);
        break;
    }
    case FIELD_DOUBLE_ARRAY:
    {
        double tmp[f->count];
        SCM vec = SCM_BOOL_F;
        if (scm_is_true(scm_f64vector_p(val))) {
            // f64vectors are bytevectors.
            if (SCM_BYTEVECTOR_LENGTH(val) == f->size) {
                memcpy(tmp, SCM_BYTEVECTOR_CONTENTS(val), f->size);
                vec = SCM_BOOL_T;
            }
        }
        else {
            vec = sequence_to_vector(val);
            if (scm_is_true(vec) && scm_c_vector_length(vec) == f->count)
                for (size_t i = 0; i < f->count; i++)
                    tmp[i] = scm_to_double(scm_c_vector_ref(vec, i));
            else
                vec = SCM_BOOL_F;
        }
        if (scm_is_false(vec))
            scm_misc_error(name, "expected ~A numbers, but got ~S",
                           scm_list_2(scm_from_size_t(f->count), val));
        memcpy(p, tmp, f->size);
        break;
    }
    }
}

// (make-NAME [FIELD-VALUE ...]): a zero-filled struct with the given
// values for its first fields, in order.
static SCM
construct(GigForeignStruct *fs, SCM rest)
{
    size_t n = scm_c_length(rest);
    if (n > fs->n_fields)
        scm_misc_error(fs->c_name, "~A takes at most ~A field values, but got ~A",
                       scm_list_3(scm_from_utf8_string(fs->c_name),
                                  scm_from_size_t(fs->n_fields), scm_from_size_t(n)));
    SCM obj = copy_struct(fs, NULL);
    for (size_t i = 0; i < n; i++, rest = scm_cdr(rest))
        field_set(fs, fs->fields[i].name, obj, scm_car(rest));
    return obj;
}

// (pointer->NAME POINTER): wraps a pointer to an opaque struct, as a
// signal handler receives it.
static SCM
from_pointer(GigForeignStruct *fs, SCM ptr)
{
    if (!SCM_POINTER_P(ptr))
        scm_wrong_type_arg_msg(fs->c_name, SCM_ARG1, ptr, "pointer");
    if (SCM_POINTER_VALUE(ptr) == NULL)
        return SCM_BOOL_F;
    return make_object(fs, SCM_POINTER_VALUE(ptr), false);
}

////////////////////////////////////////////////////////////////
// Definitions
////////////////////////////////////////////////////////////////

static SCM
define_gsubr(const char *name, int req, int rest, scm_t_subr fn, SCM defs)
{
    scm_c_define_gsubr(name, req, 0, rest, fn);
    return scm_cons(scm_from_utf8_symbol(name), defs);
}

SCM
gig_foreign_struct_define(const GigForeignStruct *_fs)
{
    GigForeignStruct *fs = (GigForeignStruct *)_fs;
    SCM defs = SCM_EOL;
    char *prefix = make_scm_name(fs->name);
    char *name;

    scm_define(scm_class_name(fs->type), fs->type);
    defs = scm_cons(scm_class_name(fs->type), defs);

    if (gig_foreign_struct_is_opaque(fs)) {
        name = concatenate("pointer->", prefix);
        defs = define_gsubr(name, 1, 0, fs->make, defs);
        free(name);
    }
    else {
        name = concatenate("make-", prefix);
        defs = define_gsubr(name, 0, 1, fs->make, defs);
        free(name);

        for (size_t i = 0; i < fs->n_fields; i++) {
            Field *f = &fs->fields[i];
            char *field = make_scm_name(f->name);
            name = concatenate3(prefix, ":", field);
            defs = define_gsubr(name, 1, 0, f->ref, defs);
            free(name);
            name = concatenate4(prefix, ":set-", field, "!");
            defs = define_gsubr(name, 2, 0, f->set, defs);
            free(name);
            free(field);
        }
    }
    gig_debug_load("%s - defined as a foreign struct", fs->c_name);
    free(prefix);
    return scm_reverse_x(defs, SCM_EOL);
}

void
gig_init_foreign_struct(void)
{
    SCM slots = scm_list_3(scm_from_utf8_symbol("data"),
                           scm_from_utf8_symbol("owned"),
                           scm_from_utf8_symbol("desc"));
    // Known to the type system, so that the loader doesn't look for
    // introspection information for it.
    gig_type_register(gig_foreign_struct_get_type(), scm_get_unknown_class());
    for (size_t i = 0; i < G_N_ELEMENTS(all_structs); i++) {
        GigForeignStruct *fs = all_structs[i];
        char *class_name = bracketize(fs->c_name);
        fs->type = scm_permanent_object(scm_make_foreign_object_type
                                        (scm_from_utf8_symbol(class_name), slots, finalize));
        free(class_name);
    }
}
