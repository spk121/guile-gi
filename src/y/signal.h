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
#ifndef Y_SIGNAL_H
#define Y_SIGNAL_H
#include <girepository.h>
#include <libguile.h>

typedef struct _Signal_spec
{
    char *signal_name;
    GSignalFlags signal_flags;
    GSignalAccumulator accumulator;
    void *accu_data;
    GType return_type;
    unsigned n_params;
    GType *param_types;
} Signal_spec;

extern SCM signal_type;

Signal_spec *signalspec_from_obj(SCM obj);
void free_signalspec(Signal_spec *spec);

typedef enum
{
    SIGNAL_SLOT_NAME,
    SIGNAL_SLOT_FLAGS,
    SIGNAL_SLOT_ACCUMULATOR,
    SIGNAL_SLOT_RETURN_TYPE,
    SIGNAL_SLOT_PARAM_TYPES,
    SIGNAL_SLOT_OUTPUT_MASK,
    SIGNAL_SLOT_COUNT
} Signal_slot;

SCM signal_ref(SCM signal, Signal_slot slot);
SCM make_signal(size_t n_slots, Signal_slot *slots, SCM *slot_values);

GClosure *signal_closure_new(SCM instance, GType g_type, const char *signal_name,
                                 SCM callback);

void init_signal(void);
#endif

