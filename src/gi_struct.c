// Copyright (C) 2019 Michael L. Gran

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
#include <libguile.h>
#include <glib.h>
#include <girepository.h>
#include "gi_struct.h"
#include "gir_type.h"

SCM
scm_make_gstruct(SCM s_gtype)
{
    GType type;

    if (scm_is_integer (s_gtype))
        type = scm_to_size_t (s_gtype);
    else
        type = gir_type_get_gtype_from_obj (s_gtype);

    if (scm_is_false (gir_type_get_scheme_type (type)))
        scm_misc_error ("make-struct",
                        "type ~S lacks introspection",
                        scm_list_1 (s_gtype));

    SCM scm_type = gir_type_get_scheme_type(type);
    if (scm_is_false(scm_type))
        scm_misc_error ("make-struct",
                        "unknown type ~S",
                        scm_list_1 (s_gtype));

    GQuark size_quark = g_quark_from_string("size");
    size_t size = GPOINTER_TO_SIZE (g_type_get_qdata(type, size_quark));

    if (size == 0)
        scm_misc_error ("make-struct",
                        "Type ~S has unknown size",
                        scm_list_1 (s_gtype));

    gpointer obj = g_malloc0(size);
    void *params[6] = {GSIZE_TO_POINTER(type),
                       GINT_TO_POINTER(1),
                       obj,
                       NULL,
                       NULL,
                       GINT_TO_POINTER(0)};

    return scm_make_foreign_object_n(scm_type, 6, params);
}

SCM
scm_make_gunion(SCM s_gtype)
{
    GType type;

    if (scm_is_integer (s_gtype))
        type = scm_to_size_t (s_gtype);
    else
        type = gir_type_get_gtype_from_obj (s_gtype);

    if (scm_is_false (gir_type_get_scheme_type (type)))
        scm_misc_error ("make-union",
                        "type ~S lacks introspection",
                        scm_list_1 (s_gtype));

    SCM scm_type = gir_type_get_scheme_type(type);
    if (scm_is_false(scm_type))
        scm_misc_error ("make-union",
                        "unknown type ~S",
                        scm_list_1 (s_gtype));

    GQuark size_quark = g_quark_from_string("size");
    size_t size = GPOINTER_TO_SIZE (g_type_get_qdata(type, size_quark));

    if (size == 0)
        scm_misc_error ("make-union",
                        "Type ~S has unknown size",
                        scm_list_1 (s_gtype));

    gpointer obj = g_malloc0(size);
    void *params[6] = {GSIZE_TO_POINTER(type),
                       GINT_TO_POINTER(1),
                       obj,
                       NULL,
                       NULL,
                       GINT_TO_POINTER(0)};

    return scm_make_foreign_object_n(scm_type, 6, params);
}


void
gi_init_struct(void)
{
    scm_c_define_gsubr("make-gstruct", 1, 0, 0, scm_make_gstruct);
    scm_c_define_gsubr("make-gunion", 1, 0, 0, scm_make_gunion);
    scm_c_export("make-gstruct",
                 "make-gunion",
                 NULL);
}
