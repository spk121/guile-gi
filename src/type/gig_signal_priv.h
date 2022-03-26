// Copyright (C) 2019, 2020, 2022 Michael L. Gran

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

#ifndef GIG_SIGNAL_PRIV_H
#define GIG_SIGNAL_PRIV_H

#include <libguile.h>
#include <glib-object.h>
#include "gig_signal.h"

typedef struct GigSignalSpec_
{
    char *signal_name;
    GSignalFlags signal_flags;
    GSignalAccumulator accumulator;
    void *accu_data;
    GType return_type;
    unsigned n_params;
    GType *param_types;
} GigSignalSpec;

extern SCM gig_signal_type;

GigSignalSpec *gig_signalspec_from_obj(SCM obj);
void gig_free_signalspec(GigSignalSpec *spec);

SCM gig_signal_ref(SCM signal, GigSignalSlot slot);

GClosure *gig_signal_closure_new(SCM instance, GType g_type, const char *signal_name,
                                 SCM callback);

#endif
