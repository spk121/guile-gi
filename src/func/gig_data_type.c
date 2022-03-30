// Copyright (C) 2019, 2022 Michael L. Gran

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

#include <string.h>
#include <stdio.h>
#include <glib-object.h>
#include "../core.h"
#include "../type.h"
#include "gig_argument.h"
#include "gig_data_type.h"
#include "gig_arg_map_priv.h"
#include "gig_util_priv.h"
#ifndef GIG_PARSER
#include "../gig_glib.h"
#endif

static char arg_type_names[GIG_ARG_TYPE_N_ARGS][64] = {
    [GIG_ARG_TYPE_UNKNOWN] = "unknown",
    [GIG_ARG_TYPE_VOID] = "void",

    [GIG_ARG_TYPE_INT8] = "int8",
    [GIG_ARG_TYPE_UINT8] = "uint8",
    [GIG_ARG_TYPE_INT16] = "int16",
    [GIG_ARG_TYPE_UINT16] = "uint16",
    [GIG_ARG_TYPE_INT32] = "int32",
    [GIG_ARG_TYPE_UINT32] = "uint32",
    [GIG_ARG_TYPE_INT64] = "int64",
    [GIG_ARG_TYPE_UINT64] = "uint64",
    [GIG_ARG_TYPE_UNICHAR] = "unichar",
    [GIG_ARG_TYPE_FLOAT] = "float",
    [GIG_ARG_TYPE_DOUBLE] = "double",
    [GIG_ARG_TYPE_POINTER] = "pointer",

    [GIG_ARG_TYPE_UTF8_STRING] = "utf8-string",
    [GIG_ARG_TYPE_LOCALE_STRING] = "locale-string",
    [GIG_ARG_TYPE_ARRAY] = "array",

    [GIG_ARG_TYPE_GBOOLEAN] = "gboolean",
    [GIG_ARG_TYPE_GTYPE] = "GType",
    [GIG_ARG_TYPE_GERROR] = "GError",
    [GIG_ARG_TYPE_BOXED] = "GBoxed",
    [GIG_ARG_TYPE_INTERFACE] = "GInterface",
    [GIG_ARG_TYPE_OBJECT] = "GObject",
    [GIG_ARG_TYPE_ENUM] = "enum",
    [GIG_ARG_TYPE_FLAGS] = "flags",
    [GIG_ARG_TYPE_VARIANT] = "GVariant",
    [GIG_ARG_TYPE_VALUE] = "GValue",
    [GIG_ARG_TYPE_PARAM] = "GParam",
    [GIG_ARG_TYPE_CALLBACK] = "callback",
    [GIG_ARG_TYPE_OTHER] = "other",

    [GIG_ARG_TYPE_GARRAY] = "GArray",
    [GIG_ARG_TYPE_GPTRARRAY] = "GPtrArray",
    [GIG_ARG_TYPE_GBYTEARRAY] = "GByteArray",
    [GIG_ARG_TYPE_GLIST] = "GList",
    [GIG_ARG_TYPE_GSLIST] = "GSList",
    [GIG_ARG_TYPE_GHASH] = "GHash",
};

const char *
gig_arg_type_name(GigArgType type)
{
    return arg_type_names[type];
}

GigArgType
gig_arg_type_from_name(const char *name)
{
    for (int i = 0; i < GIG_ARG_TYPE_N_ARGS; i++) {
        if (strcmp(name, arg_type_names[i]) == 0)
            return i;
    }
    return GIG_ARG_TYPE_UNKNOWN;
}

SCM
gig_type_meta_to_il(GigTypeMeta *meta)
{
    SCM il = SCM_EOL;
    SCM flags = SCM_EOL;
#define D(k,v) \
    il = scm_cons(scm_cons(scm_from_utf8_symbol(k),(v)),il)
#define F(x) \
    flags = scm_cons(scm_from_utf8_symbol(x), flags);

    D("arg-type", scm_from_utf8_symbol(gig_arg_type_name(meta->arg_type)));

    if (meta->gtype)
        D("gtype-name", scm_from_utf8_string(g_type_name(meta->gtype)));
    if (meta->qname)
        D("qualified-name", scm_from_utf8_string(meta->qname));
    if (meta->is_ptr)
        F("ptr");
    if (meta->is_in)
        F("in");
    if (meta->is_out)
        F("out")
            if (meta->is_skip)
            F("skip");
    if (meta->is_caller_allocates)
        F("preallocated");
    if (meta->is_optional)
        F("optional");
    if (meta->is_nullable)
        F("nullable");
    if (meta->is_invalid)
        F("invalid");
    if (meta->is_zero_terminated)
        F("zero-terminated");
    if (meta->has_length_arg)
        F("sized");
    if (meta->has_fixed_size)
        F("fixed-size");
    if (!scm_is_null(flags))
        D("flags", scm_reverse(flags));
    if (meta->length_arg != 0)
        D("length-arg", scm_from_int(meta->length_arg));
    if (meta->fixed_size != 0)
        D("fixed-size", scm_from_int(meta->fixed_size));
    if (meta->item_size != 0)
        D("item-size", scm_from_size_t(meta->item_size));
    if (meta->transfer == GIG_TRANSFER_NOTHING)
        D("transfer", scm_from_utf8_symbol("nothing"));
    else if (meta->transfer == GIG_TRANSFER_CONTAINER)
        D("transfer", scm_from_utf8_symbol("container"));
    else if (meta->transfer == GIG_TRANSFER_EVERYTHING)
        D("transfer", scm_from_utf8_symbol("everything"));

    if (meta->n_params > 0) {
        SCM param_il = SCM_EOL;
        for (int i = 0; i < meta->n_params; i++) {
            param_il =
                scm_append(scm_list_2
                           (param_il, scm_list_1(gig_type_meta_to_il(&(meta->params[i])))));
        }
        D("params", param_il);
    }
    else if (meta->arg_type == GIG_ARG_TYPE_CALLBACK)
        D("callable-info", gig_amap_to_il(meta->callable_arg_map));
    return scm_reverse(il);
}


#ifndef GIG_PARSER
void
gig_type_meta_from_il(SCM il, GigTypeMeta *meta)
{
    SCM val;
    char *str;
    val = scm_assq_ref(il, scm_from_utf8_symbol("arg-type"));
    str = scm_to_utf8_symbol(val);
    meta->arg_type = gig_arg_type_from_name(str);
    free(str);
    val = scm_assq_ref(il, scm_from_utf8_symbol("gtype-name"));
    if (scm_is_true(val)) {
        str = scm_to_utf8_string(val);
        meta->gtype = G.type_from_name(str);
        free(str);
    }
    val = scm_assq_ref(il, scm_from_utf8_symbol("qualified-name"));
    if (scm_is_true(val)) {
        str = scm_to_utf8_string(val);
        meta->qname = str;
    }

    SCM flags = scm_assq_ref(il, scm_from_utf8_symbol("flags"));
    if (scm_is_true(flags)) {
        if (scm_is_true(scm_memq(scm_from_utf8_symbol("ptr"), flags)))
            meta->is_ptr = 1;
        if (scm_is_true(scm_memq(scm_from_utf8_symbol("in"), flags)))
            meta->is_in = 1;
        if (scm_is_true(scm_memq(scm_from_utf8_symbol("out"), flags)))
            meta->is_out = 1;
        if (scm_is_true(scm_memq(scm_from_utf8_symbol("skip"), flags)))
            meta->is_skip = 1;
        if (scm_is_true(scm_memq(scm_from_utf8_symbol("preallocated"), flags)))
            meta->is_caller_allocates = 1;
        if (scm_is_true(scm_memq(scm_from_utf8_symbol("optional"), flags)))
            meta->is_optional = 1;
        if (scm_is_true(scm_memq(scm_from_utf8_symbol("nullable"), flags)))
            meta->is_nullable = 1;
        if (scm_is_true(scm_memq(scm_from_utf8_symbol("invalid"), flags)))
            meta->is_invalid = 1;
        if (scm_is_true(scm_memq(scm_from_utf8_symbol("zero-terminated"), flags)))
            meta->is_zero_terminated = 1;
        if (scm_is_true(scm_memq(scm_from_utf8_symbol("sized"), flags)))
            meta->has_length_arg = 1;
        if (scm_is_true(scm_memq(scm_from_utf8_symbol("fixed-size"), flags)))
            meta->has_fixed_size = 1;
    }
    val = scm_assq_ref(il, scm_from_utf8_symbol("length-arg"));
    if (scm_is_true(val))
        meta->length_arg = scm_to_int(val);
    val = scm_assq_ref(il, scm_from_utf8_symbol("fixed-size"));
    if (scm_is_true(val))
        meta->fixed_size = scm_to_int(val);
    val = scm_assq_ref(il, scm_from_utf8_symbol("item-size"));
    if (scm_is_true(val))
        meta->item_size = scm_to_size_t(val);
    val = scm_assq_ref(il, scm_from_utf8_symbol("transfer"));
    str = scm_to_utf8_symbol(val);
    if (strcmp(str, "nothing") == 0)
        meta->transfer = GIG_TRANSFER_NOTHING;
    else if (strcmp(str, "container") == 0)
        meta->transfer = GIG_TRANSFER_CONTAINER;
    else if (strcmp(str, "everything") == 0)
        meta->transfer = GIG_TRANSFER_EVERYTHING;
    free(str);

    val = scm_assq_ref(il, scm_from_utf8_symbol("params"));
    if (scm_is_true(val)) {
        meta->n_params = scm_c_length(val);
        meta->params = xcalloc(meta->n_params, sizeof(GigTypeMeta));
        for (int i = 0; i < meta->n_params; i++) {
            SCM entry_il = scm_c_list_ref(val, i);
            gig_type_meta_from_il(entry_il, &(meta->params[i]));
        }
    }

    val = scm_assq_ref(il, scm_from_utf8_symbol("callable-info"));
    if (scm_is_true(val))
        meta->callable_arg_map = gig_amap_new_from_il(val);
}
#endif

void
gig_meta_add_params(GigTypeMeta *meta, int n)
{
    meta->params = xcalloc(n, sizeof(GigTypeMeta));
    meta->n_params = n;
}

size_t
gig_meta_real_item_size(const GigTypeMeta *meta)
{
    if (meta->arg_type == GIG_ARG_TYPE_UTF8_STRING
        || meta->arg_type == GIG_ARG_TYPE_LOCALE_STRING
        || meta->arg_type == GIG_ARG_TYPE_POINTER || meta->is_ptr)
        return sizeof(void *);
    return meta->item_size;
}

void
gig_data_type_free(GigTypeMeta *meta)
{
    for (int i = 0; i < meta->n_params; i++)
        gig_data_type_free(&meta->params[i]);
    if (meta->n_params > 0)
        free(meta->params);

    if (meta->callable_arg_map) {
        gig_amap_free(meta->callable_arg_map);
    }
    free(meta->qname);
    meta->qname = NULL;
}

#define STRLEN 128
char gig_data_type_describe_buf[STRLEN];

#ifndef GIG_PARSER
const char *
gig_type_meta_describe(const GigTypeMeta *meta)
{
    size_t len = 0;
    const char *gtype_name = NULL;
    assert(meta->arg_type < GIG_ARG_TYPE_N_ARGS);
    if (meta->gtype)
        gtype_name = G.type_name(meta->gtype);
    len = snprintf(gig_data_type_describe_buf, STRLEN,
                   "%s%s%s%s%s",
                   (meta->is_ptr ? "pointer to " : ""),
                   (meta->is_caller_allocates ? "caller allocated " : ""),
                   arg_type_names[meta->arg_type],
                   (gtype_name ? " of type " : ""), (gtype_name ? gtype_name : ""));
    if (meta->is_nullable && len < STRLEN)
        snprintf(gig_data_type_describe_buf + len, STRLEN - len, " or NULL");
    return gig_data_type_describe_buf;
}
#endif

#undef STRLEN
