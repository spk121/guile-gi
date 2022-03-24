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

#ifndef GIG_ARG_MAP_H
#define GIG_ARG_MAP_H

#include <stdbool.h>
#include <stdint.h>
#include <girepository.h>
#include "gig_arg_map.h"
#include "gig_data_type.h"

void gig_amap_dump(const char *name, const GigArgMap *am);

bool gig_amap_output_child_c(GigArgMap *am, int c_output_pos,
                             int *cinvoke_output_array_size_index);
bool gig_amap_input_s_2_input_c(const GigArgMap *amap, int s_input, int *c_input);
bool gig_amap_input_s_2_output_c(const GigArgMap *amap, int s_input, int *c_output);
bool gig_amap_input_s_2_child_input_c(const GigArgMap *amap, int s_input, int *c_input);
bool gig_amap_input_s_2_child_output_c(const GigArgMap *amap, int s_input, int *c_output);
bool gig_amap_get_cinvoke_array_length_indices(const GigArgMap *am, int s_input_pos,
                                               int *c_input_pos, int *c_output_pos);
bool gig_amap_input_i2c(const GigArgMap *amap, int i, int *c);
bool gig_amap_input_i2s(const GigArgMap *amap, int i, int *s);
bool gig_amap_input_c2i(const GigArgMap *amap, int c, int *i);
bool gig_amap_input_s2i(const GigArgMap *amap, int s, int *i);
bool gig_amap_input_c2s(const GigArgMap *amap, int c, int *s);
bool gig_amap_input_s2c(const GigArgMap *amap, int s, int *c);

bool gig_amap_output_i2s(const GigArgMap *amap, int i, int *s);
bool gig_amap_output_c2i(const GigArgMap *amap, int c, int *i);
bool gig_amap_output_s2i(const GigArgMap *amap, int s, int *i);
bool gig_amap_output_c2s(const GigArgMap *amap, int c, int *s);
bool gig_amap_output_s2c(const GigArgMap *amap, int s, int *c);

bool gig_amap_child_i(const GigArgMap *amap, int i, int *ichild);

#endif
