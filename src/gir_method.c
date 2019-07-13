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

#include <glib.h>
#include <libguile.h>
#include "gi_function_info.h"
#include "gi_giargument.h"
#include "gig_object.h"
#include "gi_util.h"
#include "gir_function.h"
#include "gir_method.h"
#include "gir_arg_map.h"
#include "gir_type.h"
#include "gi_util.h"

// This structure is a hash table tree.
// On the first level we have
//  KEY: method name string
//  VALUE: hash tables
// On the second level we have
//  KEY: GType
//  VALUE: GIMethodInfo *
/* GHashTable *gir_method_hash_table = NULL; */

static SCM gir_method_hash_table;

static SCM ensure_generic_proc;
static SCM make_proc;
static SCM srfi1_drop_proc;
static SCM add_method_proc;

static SCM top_type;
static SCM method_type;

static SCM kwd_specializers;
static SCM kwd_formals;
static SCM kwd_procedure;

static SCM
drop1(SCM lst)
{
    return scm_call_2(srfi1_drop_proc, lst, scm_from_int(1));
}

void
gir_method_table_insert(GType type, GIFunctionInfo *info)
{

    g_assert(type != 0);
    g_assert(info != NULL);

    int req, opt;
    SCM formals, specializers;

    SCM self_type = gir_type_get_scheme_type(type);
    g_return_if_fail(scm_is_true(self_type));

    gchar *public_name = gi_function_info_make_name(info, NULL);
    g_return_if_fail(public_name != NULL);

    SCM sym_public_name = scm_from_utf8_symbol(public_name);
    SCM generic = scm_hashq_ref(gir_method_hash_table, sym_public_name, SCM_BOOL_F);
    if (!scm_is_generic(generic)) {
        SCM definition = SCM_BOOL_F;
        if (scm_is_true(scm_defined_p(sym_public_name, scm_c_resolve_module("guile"))))
            definition = scm_c_public_ref("guile", public_name);
        generic = scm_call_2(ensure_generic_proc,
                             definition,
                             sym_public_name);
        scm_hashq_set_x(gir_method_hash_table, sym_public_name, generic);
    }

    gir_gsubr_t *subr = gir_function_create_gsubr(info, public_name, &req, &opt,
                                                  &formals, &specializers);

    g_return_if_fail(subr != NULL);

    SCM proc = scm_c_make_gsubr(public_name, req, opt, 0, subr);

    for (SCM special = specializers; scm_is_pair(special); special = scm_cdr(special))
        if (scm_is_false(scm_car(special)))
            scm_set_car_x(special, top_type);
    scm_set_car_x(formals, scm_from_utf8_symbol("self"));
    scm_set_car_x(specializers, self_type);

    SCM t_formals = formals, t_specializers = specializers;
    do {
        SCM mthd = scm_call_7(make_proc,
                              method_type,
                              kwd_specializers, t_specializers,
                              kwd_formals, t_formals,
                              kwd_procedure, proc);

        scm_call_2(add_method_proc,
                   generic, mthd);

        t_formals = drop1(t_formals);
        t_specializers = drop1(t_specializers);
    } while (--opt > 0);

    scm_c_define(public_name, generic);
    scm_c_export(public_name, NULL);
    g_debug ("defined method %s for %s", public_name, g_type_name (type));
}

void
gir_init_method(void)
{
    gir_method_hash_table = scm_c_make_hash_table(127);

    top_type = scm_c_public_ref("oop goops", "<top>");
    method_type = scm_c_public_ref("oop goops", "<method>");
    ensure_generic_proc = scm_c_public_ref("oop goops", "ensure-generic");
    make_proc = scm_c_public_ref("oop goops", "make");
    add_method_proc = scm_c_public_ref("oop goops", "add-method!");
    srfi1_drop_proc = scm_c_public_ref("srfi srfi-1", "drop");

    kwd_specializers = scm_from_utf8_keyword("specializers");
    kwd_formals = scm_from_utf8_keyword("formals");
    kwd_procedure = scm_from_utf8_keyword("procedure");
}
