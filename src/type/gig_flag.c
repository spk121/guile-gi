// Copyright (C) 2018, 2019 Michael L. Gran

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

#include <inttypes.h>
#include <libguile.h>
#include "gig_flag.h"
#include "gig_type.h"
#include "gig_type_private.h"

static SCM enum_to_number;
static SCM flags_to_number;
static SCM number_to_enum;
static SCM number_to_flags;
static SCM enum_to_symbol;
static SCM symbol_to_enum;
static SCM flags_to_list;
static SCM list_to_flags;

int
gig_enum_to_int(SCM val)
{
    return scm_to_int(scm_call_1(enum_to_number, val));
}

unsigned
gig_flags_to_uint(SCM val)
{
    return scm_to_uint(scm_call_1(flags_to_number, val));
}

SCM
gig_int_to_enum(int v, GType gtype)
{
    SCM type = gig_type_get_scheme_type(gtype);
    SCM val = scm_from_int(v);
    return scm_call_2(number_to_enum, type, val);
}

SCM
gig_uint_to_flags(unsigned v, GType gtype)
{
    SCM type = gig_type_get_scheme_type(gtype);
    SCM val = scm_from_uint(v);
    return scm_call_2(number_to_flags, type, val);
}

SCM
gig_int_to_enum_with_info(int v, GIEnumInfo *info)
{
    SCM type = gig_type_get_scheme_type_with_info(info);
    SCM val = scm_from_int(v);
    return scm_call_2(number_to_enum, type, val);
}

SCM
gig_uint_to_flags_with_info(unsigned v, GIEnumInfo *info)
{
    SCM type = gig_type_get_scheme_type_with_info(info);
    SCM val = scm_from_uint(v);
    return scm_call_2(number_to_flags, type, val);
}

SCM
gig_symbol_to_enum(SCM type, SCM symbol)
{
    return scm_call_2(symbol_to_enum, type, symbol);
}

SCM
gig_list_to_flags(SCM type, SCM list)
{
    return scm_call_2(list_to_flags, type, list);
}

void
gig_init_flag(void)
{
    enum_to_number = scm_c_public_ref("gi types", "enum->number");
    flags_to_number = scm_c_public_ref("gi types", "flags->number");
    number_to_enum = scm_c_public_ref("gi types", "number->enum");
    enum_to_symbol = scm_c_public_ref("gi types", "enum->symbol");
    symbol_to_enum = scm_c_public_ref("gi types", "symbol->enum");
    number_to_flags = scm_c_public_ref("gi types", "number->flags");
    list_to_flags = scm_c_public_ref("gi types", "list->flags");
    flags_to_list = scm_c_public_ref("gi types", "flags->list");
}

static SCM
define_conversion(const char *fmt, const char *name, SCM proc)
{
    char *_sym = decorate_string(fmt, name);
    SCM sym = scm_from_utf8_symbol(_sym);
    free(_sym);
    scm_define(sym, proc);
    return sym;
}

SCM
gig_define_enum_conversions(GIEnumInfo *info, GType type, SCM defs)
{
    SCM _class;
    scm_dynwind_begin(0);
    char *cls = make_scm_name(g_base_info_get_name(info));
    scm_dynfree(cls);

    if (type != G_TYPE_NONE)
        _class = gig_type_get_scheme_type(type);
    else
        _class = gig_type_get_scheme_type_with_info(info);

#define C(fmt, proc)                                                    \
    do {                                                                \
        defs = scm_cons(define_conversion(fmt, cls,                     \
                                          scm_call_1(proc, _class)),    \
                        defs);                                          \
    } while (0)

    switch (g_base_info_get_type(info)) {
    case GI_INFO_TYPE_ENUM:
        C("%s->number", enum_to_number);
        C("%s->symbol", enum_to_symbol);
        C("number->%s", number_to_enum);
        C("symbol->%s", symbol_to_enum);
        break;
    case GI_INFO_TYPE_FLAGS:
        C("%s->number", flags_to_number);
        C("%s->list", flags_to_list);
        C("number->%s", number_to_flags);
        C("list->%s", list_to_flags);
        break;
    default:
        assert_not_reached();
    }
    scm_dynwind_end();

    return defs;
}

SCM
gig_define_enum(GIEnumInfo *info, SCM defs)
{
    int n_values = g_enum_info_get_n_values(info);
    int i = 0;
    GIValueInfo *vi = NULL;
    GIInfoType t = g_base_info_get_type(info);
    char *_key;
    SCM key;
    SCM _class;

    SCM existing = gig_type_get_scheme_type_with_info(info);
    if (!SCM_UNBNDP(existing))
        return scm_cons(scm_class_name(existing), defs);

    switch (t) {
    case GI_INFO_TYPE_ENUM:
        _class = gig_type_define_with_info(info, scm_list_1(gig_enum_type), SCM_EOL);
        break;
    case GI_INFO_TYPE_FLAGS:
        _class = gig_type_define_with_info(info, scm_list_1(gig_flags_type), SCM_EOL);
        break;
    default:
        assert_not_reached();
    }

    SCM obarray = scm_make_hash_table(scm_from_int(n_values));

    char *_class_name = scm_to_utf8_symbol(scm_class_name(_class));

    while (i < n_values) {
        vi = g_enum_info_get_value(info, i);
        _key = make_scm_name(g_base_info_get_name(vi));
        key = scm_from_utf8_symbol(_key);
        int64_t _val = g_value_info_get_value(vi);
        SCM val;

        switch (t) {
        case GI_INFO_TYPE_ENUM:
            gig_debug_load("%s - add enum %s %d", _class_name, _key, _val);
            val = scm_from_int(_val);
            break;
        case GI_INFO_TYPE_FLAGS:
            gig_debug_load("%s - add flag %s %u", _class_name, _key, _val);
            val = scm_from_uint(_val);
            break;
        default:
            assert_not_reached();
        }

        scm_hashq_set_x(obarray, key, val);

        g_base_info_unref(vi);
        free(_key);
        i++;
    }

    scm_set_class_obarray_slot(_class, obarray);

    scm_define(scm_class_name(_class), _class);
    defs = scm_cons(scm_class_name(_class), defs);
    free(_class_name);

    return defs;
}
