/*
  visibility.h

  Copyright 2022 Michael L. Gran

  This file is part of GNU Guile-GI

  Guile-GI is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  Guile-GI is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU General Public
  License along with Guile-GI.  If not, see
  <https://www.gnu.org/licenses/>.
*/

#ifndef CORE_VISIBILITY_H
#define CORE_VISIBILITY_H

#ifdef GIR_DLL

#ifdef BUILDING_GIG
#define GIG_API __declspec(dllexport)
#else
#define GIG_API __declspec(dllexport)
#endif
#define GIG_LOCAL

#else

#define GIG_API __attribute__ ((visibility("default")))
#define GIG_LOCAL __attribute__ ((visibility("hidden")))

#endif

#endif
