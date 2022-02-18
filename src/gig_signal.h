// Copyright (C) 2019, 2020 Michael L. Gran

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
#ifndef GIG_SIGNAL_H
#define GIG_SIGNAL_H
#include <girepository.h>
#include <libguile.h>

typedef struct _GigSignalSpec
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

typedef enum
{
    GIG_SIGNAL_SLOT_NAME,
    GIG_SIGNAL_SLOT_FLAGS,
    GIG_SIGNAL_SLOT_ACCUMULATOR,
    GIG_SIGNAL_SLOT_RETURN_TYPE,
    GIG_SIGNAL_SLOT_PARAM_TYPES,
    GIG_SIGNAL_SLOT_OUTPUT_MASK,
    GIG_SIGNAL_SLOT_COUNT
} GigSignalSlot;

SCM gig_signal_ref(SCM signal, GigSignalSlot slot);
SCM gig_make_signal(size_t n_slots, GigSignalSlot *slots, SCM *slot_values);

GClosure *gig_signal_closure_new(SCM instance, GType g_type, const char *signal_name,
                                 SCM callback);

void gig_init_signal(void);

#endif
