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

// Field accessors for the structs, unions and objects guile-gi loads.
//
// A getter converts the field like a transfer-none return value:
// strings, boxed values, embedded structs and fixed-size arrays are
// copied, objects get a new reference.  Setters are limited to fields
// the typelib marks writable that hold plain data -- numbers, booleans,
// enums, flags, and embedded structs made only of those -- because for
// pointers the typelib doesn't say who would own the new value.

#include <string.h>
#include <glib-object.h>
#include "../core.h"
#include "../type.h"
#include "gig_argument.h"
#include "gig_data_type.h"
#include "gig_field.h"
#include "gig_util.h"

typedef enum
{
    KIND_POINTER,               // a pointer stored in the field
    KIND_VALUE,                 // a number, boolean, enum or flags
    KIND_INLINE                 // an embedded struct, union or fixed-size array
} FieldKind;

typedef struct
{
    GigTypeMeta meta;
    FieldKind kind;
    size_t offset;
    size_t size;
    GType container;
    char *getter_name;
    char *setter_name;
} GigField;

static SCM accessors;           // "Namespace.Container.field" -> (getter . setter or #f)
static SCM make_getter_proc;
static SCM make_setter_proc;
static SCM field_ref_proc;
static SCM field_set_proc;

static int
n_fields(GIBaseInfo *container)
{
    switch (g_base_info_get_type(container)) {
    case GI_INFO_TYPE_STRUCT:
    case GI_INFO_TYPE_BOXED:
        return g_struct_info_get_n_fields(container);
    case GI_INFO_TYPE_UNION:
        return g_union_info_get_n_fields(container);
    case GI_INFO_TYPE_OBJECT:
        return g_object_info_get_n_fields(container);
    default:
        return 0;
    }
}

static GIFieldInfo *
get_field(GIBaseInfo *container, int i)
{
    switch (g_base_info_get_type(container)) {
    case GI_INFO_TYPE_STRUCT:
    case GI_INFO_TYPE_BOXED:
        return g_struct_info_get_field(container, i);
    case GI_INFO_TYPE_UNION:
        return g_union_info_get_field(container, i);
    case GI_INFO_TYPE_OBJECT:
        return g_object_info_get_field(container, i);
    default:
        return NULL;
    }
}

// Whether a struct or union holds only plain data, so that copying its
// bytes copies it.
static bool
is_plain(GIBaseInfo *info, int depth)
{
    GIInfoType t = g_base_info_get_type(info);
    if (depth > 8 || (t != GI_INFO_TYPE_STRUCT && t != GI_INFO_TYPE_BOXED
                      && t != GI_INFO_TYPE_UNION))
        return false;
    bool plain = true;
    int n = n_fields(info);
    for (int i = 0; plain && i < n; i++) {
        GIFieldInfo *fi = get_field(info, i);
        GITypeInfo *ti = g_field_info_get_type(fi);
        GITypeTag tag = g_type_info_get_tag(ti);
        if (g_type_info_is_pointer(ti))
            plain = false;
        else if (tag == GI_TYPE_TAG_INTERFACE) {
            GIBaseInfo *iface = g_type_info_get_interface(ti);
            GIInfoType it = g_base_info_get_type(iface);
            plain = (it == GI_INFO_TYPE_ENUM || it == GI_INFO_TYPE_FLAGS
                     || is_plain(iface, depth + 1));
            g_base_info_unref(iface);
        }
        else if (tag == GI_TYPE_TAG_ARRAY) {
            GITypeInfo *elt = g_type_info_get_param_type(ti, 0);
            plain = (g_type_info_get_array_fixed_size(ti) >= 0
                     && !g_type_info_is_pointer(elt)
                     && GI_TYPE_TAG_IS_BASIC(g_type_info_get_tag(elt)));
            g_base_info_unref(elt);
        }
        else
            plain = (tag != GI_TYPE_TAG_VOID && GI_TYPE_TAG_IS_BASIC(tag));
        g_base_info_unref(ti);
        g_base_info_unref(fi);
    }
    return plain;
}

static size_t
container_size(GIRegisteredTypeInfo *container)
{
    switch (g_base_info_get_type(container)) {
    case GI_INFO_TYPE_STRUCT:
    case GI_INFO_TYPE_BOXED:
        return g_struct_info_get_size(container);
    case GI_INFO_TYPE_UNION:
        return g_union_info_get_size(container);
    case GI_INFO_TYPE_OBJECT:
    {
        GTypeQuery query;
        g_type_query(g_registered_type_info_get_g_type(container), &query);
        return query.instance_size;
    }
    default:
        return 0;
    }
}

// Fills in F from FI; false if guile-gi can't read the field.
static bool
describe(GIFieldInfo *fi, GigField *f)
{
    if (!(g_field_info_get_flags(fi) & GI_FIELD_IS_READABLE))
        return false;

    memset(f, 0, sizeof(GigField));
    gig_type_meta_init_from_field_info(&f->meta, fi);
    f->offset = g_field_info_get_offset(fi);
    GigTypeMeta *m = &f->meta;
    GType fundamental = G_TYPE_FUNDAMENTAL(m->gtype);

    bool ok = true;
    if (m->is_invalid)
        ok = false;
    else if (m->gtype == G_TYPE_POINTER && m->pointer_type == GIG_DATA_CALLBACK)
        ok = false;             // a slot of a virtual function table
    else if (m->gtype == G_TYPE_PRIV_C_ARRAY && m->has_size)
        ok = false;             // its length is in another field
    else if (m->is_ptr) {
        f->kind = KIND_POINTER;
        f->size = sizeof(void *);
    }
    else if (m->gtype == G_TYPE_PRIV_C_ARRAY) {
        f->kind = KIND_INLINE;
        f->size = m->length * gig_meta_real_item_size(&m->params[0]);
        ok = (m->length != GIG_ARRAY_SIZE_UNKNOWN && f->size > 0);
    }
    else if (m->gtype == G_TYPE_PRIV_FOREIGN_STRUCT) {
        f->kind = KIND_INLINE;
        f->size = gig_foreign_struct_size(m->foreign_struct);
        ok = (f->size > 0);
    }
    else if (fundamental == G_TYPE_BOXED) {
        f->kind = KIND_INLINE;
        f->size = m->item_size;
        ok = (f->size > 0);
    }
    else if (fundamental == G_TYPE_ENUM || fundamental == G_TYPE_FLAGS) {
        f->kind = KIND_VALUE;
        f->size = sizeof(int);
    }
    else if (fundamental == G_TYPE_BOOLEAN || fundamental == G_TYPE_CHAR
             || fundamental == G_TYPE_UCHAR || fundamental == G_TYPE_INT
             || fundamental == G_TYPE_UINT || fundamental == G_TYPE_INT64
             || fundamental == G_TYPE_UINT64 || fundamental == G_TYPE_FLOAT
             || fundamental == G_TYPE_DOUBLE || m->gtype == G_TYPE_GTYPE) {
        f->kind = KIND_VALUE;
        f->size = m->item_size;
        ok = (f->size > 0 && f->size <= sizeof(GIArgument));
    }
    else
        ok = false;             // e.g. an object embedded in an object

    if (!ok)
        gig_data_type_free(m);
    return ok;
}

bool
gig_field_is_readable(GIFieldInfo *fi)
{
    GigField f;
    if (!describe(fi, &f))
        return false;
    gig_data_type_free(&f.meta);
    return true;
}

bool
gig_field_is_writable(GIFieldInfo *fi)
{
    GigField f;
    if (!(g_field_info_get_flags(fi) & GI_FIELD_IS_WRITABLE) || !describe(fi, &f))
        return false;
    bool writable = false;
    if (f.kind == KIND_VALUE)
        writable = (f.meta.gtype != G_TYPE_GTYPE);
    else if (f.kind == KIND_INLINE && G_TYPE_FUNDAMENTAL(f.meta.gtype) == G_TYPE_BOXED) {
        GITypeInfo *ti = g_field_info_get_type(fi);
        GIBaseInfo *iface = g_type_info_get_interface(ti);
        writable = (iface != NULL && is_plain(iface, 0));
        if (iface)
            g_base_info_unref(iface);
        g_base_info_unref(ti);
    }
    gig_data_type_free(&f.meta);
    return writable;
}

// Types known to have bitfields.  Typelibs compiled by current tools
// lose the widths of bitfields, and describe them as whole fields at
// made-up offsets, which would read and write past the real struct.
static const char *bitfield_types[] = {
    "GLib.Date", "GLib.IOChannel", "GLib.HookList", "GLib.ScannerConfig",
    "GObject.Closure", "GObject.ParamSpecString", "Pango.GlyphVisAttr",
};

bool
gig_fields_are_reliable(GIRegisteredTypeInfo *container)
{
    char *name = g_strdup_printf("%s.%s", g_base_info_get_namespace(container),
                                 g_base_info_get_name(container));
    bool known_bitfields = false;
    for (size_t i = 0; i < G_N_ELEMENTS(bitfield_types); i++)
        if (strcmp(name, bitfield_types[i]) == 0)
            known_bitfields = true;
    g_free(name);
    if (known_bitfields)
        return false;

    // Otherwise, a typelib that keeps bitfields' widths reports them,
    // and an object's fields must fit in its instance.
    size_t size = container_size(container);
    int n = n_fields(container);
    bool reliable = true;
    for (int i = 0; reliable && i < n; i++) {
        GIFieldInfo *fi = get_field(container, i);
        GITypeInfo *ti = g_field_info_get_type(fi);
        GigTypeMeta meta = { 0 };
        meta.transfer = GI_TRANSFER_NOTHING;
        gig_type_meta_init_from_field_info(&meta, fi);
        size_t field_size;
        if (meta.is_ptr)
            field_size = sizeof(void *);
        else if (meta.gtype == G_TYPE_PRIV_C_ARRAY && meta.length != GIG_ARRAY_SIZE_UNKNOWN)
            field_size = meta.length * gig_meta_real_item_size(&meta.params[0]);
        else if (meta.gtype == G_TYPE_PRIV_FOREIGN_STRUCT)
            field_size = gig_foreign_struct_size(meta.foreign_struct);
        else
            field_size = meta.item_size;
        if (g_field_info_get_size(fi) != 0
            || g_field_info_get_offset(fi) < 0
            || (size_t)g_field_info_get_offset(fi) + field_size > size)
            reliable = false;
        gig_data_type_free(&meta);
        g_base_info_unref(ti);
        g_base_info_unref(fi);
    }
    return reliable;
}

static char *
peek_field(GigField *f, SCM obj, const char *subr)
{
    void *base = NULL;
    if (gig_type_check_typed_object(obj, gig_type_get_scheme_type(f->container)))
        base = gig_type_peek_object(obj);
    if (base == NULL)
        scm_wrong_type_arg_msg(subr, SCM_ARG1, obj, g_type_name(f->container));
    return (char *)base + f->offset;
}

static SCM
field_ref(SCM data, SCM obj)
{
    GigField *f = scm_to_pointer(data);
    char *p = peek_field(f, obj, f->getter_name);
    GIArgument arg;
    memset(&arg, 0, sizeof(arg));
    switch (f->kind) {
    case KIND_POINTER:
        arg.v_pointer = *(void **)p;
        break;
    case KIND_VALUE:
        // Every member of the union starts at its beginning.
        memcpy(&arg, p, f->size);
        break;
    case KIND_INLINE:
        arg.v_pointer = p;
        break;
    }
    GigTypeMeta meta = f->meta;
    SCM value;
    gig_argument_c_to_scm(f->getter_name, SCM_ARG1, &meta, &arg, &value,
                          GIG_ARRAY_SIZE_UNKNOWN);
    return value;
}

static SCM
field_set(SCM data, SCM obj, SCM value)
{
    GigField *f = scm_to_pointer(data);
    char *p = peek_field(f, obj, f->setter_name);
    if (f->kind == KIND_VALUE) {
        GIArgument arg;
        size_t size;
        GigTypeMeta meta = f->meta;
        memset(&arg, 0, sizeof(arg));
        gig_argument_scm_to_c(f->setter_name, SCM_ARG2, &meta, value, NULL, &arg, &size);
        memcpy(p, &arg, f->size);
    }
    else {
        // A plain embedded struct: copy its bytes.
        if (!gig_type_check_typed_object(value, gig_type_get_scheme_type(f->meta.gtype)))
            scm_wrong_type_arg_msg(f->setter_name, SCM_ARG2, value, g_type_name(f->meta.gtype));
        void *src = gig_type_peek_object(value);
        if (src == NULL)
            scm_wrong_type_arg_msg(f->setter_name, SCM_ARG2, value, g_type_name(f->meta.gtype));
        memcpy(p, src, f->size);
    }
    return SCM_UNSPECIFIED;
}

// (getter . setter-or-#f) for field FI, made once.
static SCM
make_accessors(GIRegisteredTypeInfo *container, GIFieldInfo *fi)
{
    char *key = g_strdup_printf("%s.%s.%s", g_base_info_get_namespace(container),
                                g_base_info_get_name(container), g_base_info_get_name(fi));
    SCM s_key = scm_from_utf8_string(key);
    g_free(key);
    SCM cached = scm_hash_ref(accessors, s_key, SCM_BOOL_F);
    if (scm_is_true(cached))
        return cached;

    GigField *f = xcalloc(1, sizeof(GigField));
    if (!describe(fi, f)) {
        free(f);
        return SCM_BOOL_F;
    }
    f->container = g_registered_type_info_get_g_type(container);
    char *prefix = make_scm_name(g_base_info_get_name(container));
    char *field = gig_field_make_name(fi);
    f->getter_name = concatenate3(prefix, ":", field);
    f->setter_name = concatenate4(prefix, ":set-", field, "!");
    free(prefix);
    free(field);

    SCM data = scm_from_pointer(f, NULL);
    SCM getter = scm_call_3(make_getter_proc, field_ref_proc, data,
                            scm_from_utf8_symbol(f->getter_name));
    SCM setter = SCM_BOOL_F;
    if (gig_field_is_writable(fi)) {
        setter = scm_call_3(make_setter_proc, field_set_proc, data,
                            scm_from_utf8_symbol(f->setter_name));
        getter = scm_make_procedure_with_setter(getter, setter);
    }
    SCM result = scm_cons(getter, setter);
    scm_hash_set_x(accessors, s_key, result);
    return result;
}

char *
gig_field_make_name(GIFieldInfo *fi)
{
    // Without the trailing underscores of names like long_, which
    // avoid C keywords.
    char *gname = xstrdup(g_base_info_get_name(fi));
    for (size_t len = strlen(gname); len > 1 && gname[len - 1] == '_'; len--)
        gname[len - 1] = '\0';
    char *name = make_scm_name(gname);
    free(gname);
    return name;
}

bool
gig_field_is_shadowed(GIRegisteredTypeInfo *container, GIFieldInfo *fi)
{
    int n = 0;
    GIFunctionInfo *(*get)(GIBaseInfo *, int) = NULL;
    switch (g_base_info_get_type(container)) {
    case GI_INFO_TYPE_STRUCT:
    case GI_INFO_TYPE_BOXED:
        n = g_struct_info_get_n_methods(container);
        get = (GIFunctionInfo *(*)(GIBaseInfo *, int))g_struct_info_get_method;
        break;
    case GI_INFO_TYPE_UNION:
        n = g_union_info_get_n_methods(container);
        get = (GIFunctionInfo *(*)(GIBaseInfo *, int))g_union_info_get_method;
        break;
    case GI_INFO_TYPE_OBJECT:
        n = g_object_info_get_n_methods(container);
        get = (GIFunctionInfo *(*)(GIBaseInfo *, int))g_object_info_get_method;
        break;
    default:
        return false;
    }
    char *field = gig_field_make_name(fi);
    bool shadowed = false;
    for (int i = 0; !shadowed && i < n; i++) {
        GIFunctionInfo *method = get(container, i);
        char *name = gig_callable_info_make_name(method, NULL);
        shadowed = (strcmp(name, field) == 0);
        free(name);
        g_base_info_unref(method);
    }
    free(field);
    return shadowed;
}

SCM
gig_fields_define(GIRegisteredTypeInfo *container)
{
    SCM defs = SCM_EOL;
    GType gtype = g_registered_type_info_get_g_type(container);
    if (gtype == G_TYPE_NONE || gtype == G_TYPE_INVALID)
        return defs;
    int n = n_fields(container);
    if (n == 0)
        return defs;
    if (!gig_fields_are_reliable(container)) {
        gig_debug_load("%s - not binding fields because their layout is unknown (bitfields?)",
                       g_base_info_get_name(container));
        return defs;
    }

    for (int i = 0; i < n; i++) {
        GIFieldInfo *fi = get_field(container, i);
        SCM acc = make_accessors(container, fi);
        if (scm_is_false(acc))
            gig_debug_load("%s.%s - not binding this field", g_base_info_get_name(container),
                           g_base_info_get_name(fi));
        else {
            SCM getter = scm_car(acc), setter = scm_cdr(acc);
            SCM name = scm_procedure_name(scm_is_true(scm_procedure_with_setter_p(getter))
                                          ? scm_procedure(getter) : getter);
            // A method of the same name takes precedence.
            if (gig_field_is_shadowed(container, fi))
                gig_debug_load("%s - not binding this field accessor: a method has its name",
                               scm_to_utf8_symbol(name));
            else {
                scm_define(name, getter);
                defs = scm_cons(name, defs);
            }
            if (scm_is_true(setter)) {
                SCM setter_name = scm_procedure_name(setter);
                scm_define(setter_name, setter);
                defs = scm_cons(setter_name, defs);
            }
        }
        g_base_info_unref(fi);
    }
    return scm_reverse_x(defs, SCM_EOL);
}

void
gig_init_field(void)
{
    accessors = scm_permanent_object(scm_c_make_hash_table(256));
    make_getter_proc = scm_permanent_object(scm_c_eval_string
                                            ("(lambda (ref data name)"
                                             "  (let ((getter (lambda (obj) (ref data obj))))"
                                             "    (set-procedure-property! getter 'name name)"
                                             "    getter))"));
    make_setter_proc = scm_permanent_object(scm_c_eval_string
                                            ("(lambda (set data name)"
                                             "  (let ((setter (lambda (obj value) (set data obj value))))"
                                             "    (set-procedure-property! setter 'name name)"
                                             "    setter))"));
    field_ref_proc = scm_permanent_object(scm_c_make_gsubr("%field-ref", 2, 0, 0, field_ref));
    field_set_proc = scm_permanent_object(scm_c_make_gsubr("%field-set!", 3, 0, 0, field_set));
}
