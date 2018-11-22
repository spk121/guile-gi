/*
  visibility.h

  Copyright 2010 Free Software Foundation, Inc.
  Copyright 2018 Michael L. Gran

  This file is part of Guile-GI.

  Guile-GI is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  Guile-GI is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with Guile-Ncurses.  If not, see
  <http://www.gnu.org/licenses/>.
*/

#ifndef VISIBILITY_H
#define VISIBILITY_H

#if defined _WIN32 || defined __CYGWIN__
#define GIR_HELPER_DLL_IMPORT __declspec(dllimport)
#define GIR_HELPER_DLL_EXPORT __declspec(dllexport)
#define GIR_HELPER_DLL_LOCAL
#else
#if __GNUC__ >= 4
#define GIR_HELPER_DLL_IMPORT __attribute__ ((visibility("default")))
#define GIR_HELPER_DLL_EXPORT __attribute__ ((visibility("default")))
#define GIR_HELPER_DLL_LOCAL  __attribute__ ((visibility("hidden")))
#else
#define GIR_HELPER_DLL_IMPORT
#define GIR_HELPER_DLL_EXPORT
#define GIR_HELPER_DLL_LOCAL
#endif
#endif

#ifdef GIR_DLL		 /* defined if GIR is compiled as a DLL */
#ifdef GIR_DLL_EXPORTS	 /* defined if we are building the GIR DLL */
#define GIR_API GIR_HELPER_DLL_EXPORT
#else
#define GIR_API GIR_HELPER_DLL_IMPORT
#endif /* GIR_DLL_EXPORTS */
#define GIR_LOCAL GIR_HELPER_DLL_LOCAL
#else /* GIR_DLL is not defined: this means GIR is a static lib. */
#define GIR_API
#define GIR_LOCAL
#endif

#endif
