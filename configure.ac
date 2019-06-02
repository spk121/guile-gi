dnl Process this file with autoconf to produce a configure script

#    Copyright (C) 2018 Michael L. Gran

#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.

#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <https://www.gnu.org/licenses/>.

################
# Package Info

AC_INIT([Guile GI], [0.0.2], [spk121@yahoo.com], [guile_gi])

AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_SRCDIR([src/gir.c])
AM_INIT_AUTOMAKE

AC_PREREQ([2.63])
AC_CONFIG_HEADERS([src/config.h])
AC_CONFIG_MACRO_DIR([m4])


################
# Libtool versioning

LIBGUILE_GI_INTERFACE="0:0:0"
AC_SUBST([LIBGUILE_GI_INTERFACE])

################
# Check for programs

PKG_PROG_PKG_CONFIG
AC_PROG_CC
AC_CANONICAL_HOST
AC_PROG_INSTALL
AC_PROG_MKDIR_P
AM_PROG_AR

dnl AC_USE_SYSTEM_EXTENSIONS
dnl AC_HEADER_STDC
dnl AC_PROG_SED

# from guile.m4
GUILE_PKG([2.2])
GUILE_PROGS
GUILE_FLAGS
GUILE_SITE_DIR

################
# Guile has three directories that don't follow the GNU Filesystem
# Heirarchy Standards.  If one follows the GNU FHS, files get installed
# in directories in which Guile does not search for them by default.
AC_ARG_WITH([gnu-filesystem-hierarchy],
            [AS_HELP_STRING([--with-gnu-filesystem-hierarchy],
                            [Strictly follow GNU Filesystem Heirarchy when installing, instead of querying Guile to discover the install directories that it expects.])])

case "x$with_gnu_filesystem_hierarchy" in
     xyes)
       # Follow the GNU Filesystem Heirarchy Standard correctly
       # Installed .scm scheme libraries go here
       AC_MSG_CHECKING([Guile site directory])
       guilesitedir="\$(datadir)/guile/site/$guile_effective_version"
       AC_MSG_RESULT([$guilesitedir])
       AC_SUBST(guilesitedir)

       # Compiled libguile-*.so extension libraries go here
       AC_MSG_CHECKING([Guile extension directory])
       guileextensiondir="$libdir/guile/$guile_effective_version"
       AC_MSG_RESULT([$guileextensiondir])
       AC_SUBST(guileextensiondir)

       # Compiled .go scheme libraries go here
       AC_MSG_CHECKING([Guile object directory])
       guileobjectdir="$libdir/guile/$guile_effective_version/site-ccache"
       AC_MSG_RESULT([$guileobjectdir])
       AC_SUBST(guileobjectdir)
       ;;
     *)
       # Get the installation directories from Guile
       # Installed .scm scheme libraries go here
       AC_MSG_CHECKING([Guile site directory])
       guilesitedir=$GUILE_SITE
       AC_MSG_RESULT([$guilesitedir])
       AC_SUBST(guilesitedir)

       # Compiled libguile-*.so extension libraries go here
       AC_MSG_CHECKING([Guile extension directory])
       guileextensiondir=$GUILE_EXTENSION
       AC_MSG_RESULT([$guileextensiondir])
       AC_SUBST(guileextensiondir)

       # Compiled .go scheme libraries go here
       AC_MSG_CHECKING([Guile object directory])
       guileobjectdir=$GUILE_SITE_CCACHE
       AC_MSG_RESULT([$guileobjectdir])
       AC_SUBST(guileobjectdir)
       ;;
esac

################
# Check for Libtool

LT_PREREQ([2.2])
LT_INIT([disable-fast-install disable-static win32-dll])

##################
# Initial guess of platform specific options

case $host_os in
mingw* | cygwin* | os2* | pw32* | cegcc*)
	[building_dll=yes] ;;
*)
	[building_dll=no] ;;
esac
case $host_os in
mingw* )
	[dll_version_info=no] ;;
*)
	[dll_version_info=yes] ;;
esac

AM_CONDITIONAL([BUILDING_DLL], [test "$building_dll" = yes])
AM_CONDITIONAL([DLL_VERSION_INFO], [test "$dll_version_info" = yes])

################
# Check for libraries
PKG_CHECK_MODULES(GLIB, [glib-2.0 >= 2.48.0])
PKG_CHECK_MODULES(GOBJECT, [gobject-2.0])
PKG_CHECK_MODULES(GOBJECT_INTROSPECTION, [gobject-introspection-1.0])
PKG_CHECK_MODULES(FFI, [libffi])

################
# Check for header files

################
# Check for types, structures, compiler characteristics
m4_define([compiler_flags_common],[ dnl
 -D_FORTIFY_SOURCE=2 dnl
 -DSCM_DEBUG_TYPING_STRICTNESS=2 dnl
 -O2 dnl
 -pipe dnl
 -g dnl
 -grecord-gcc-switches dnl
 -Wall dnl
 -Werror=format-security dnl
 -Werror=implicit-function-declaration dnl
 -Werror=shadow dnl
 -fno-omit-frame-pointer dnl
 -fasynchronous-unwind-tables dnl
 -fexceptions dnl
 -fvar-tracking dnl
 -fstack-protector-strong dnl
 -fstack-clash-protection dnl
 -fcf-protection dnl
 -fplugin=annobin dnl
 -Wl,--dynamic-list-data dnl
 -Wl,-z,defs dnl
 -Wl,-z,now dnl
 -Wl,-z,relro dnl
 -Wsuggest-attribute=format dnl
 -Wsuggest-attribute=malloc dnl
 -Wsuggest-attribute=noreturn dnl
 -Wsuggest-attribute=pure dnl
 -fdiagnostics-color=auto dnl
])

AC_ARG_ENABLE([hardening],
            [AS_HELP_STRING([--enable-hardening],
                            [compile with common hardening flags])])
case "x$enable_hardening" in
     xyes)
     CC_CHECK_CFLAGS_APPEND([compiler_flags_common])
     ;;
esac

AC_ARG_ENABLE([reverse-hash],
            [AS_HELP_STRING([--enable-reverse-hash],
                            [have reverse type lookup hash])])
case "x$enable_reverse_hash" in
     xyes)
     AC_DEFINE([ENABLE_GIR_TYPE_SCM_HASH], [1],
               [Define if SCM to GType hash is enabled.])
     ;;
esac


################
# Check for C library functions

AC_CHECK_DECLS([isfinite], [], [], [[#include <math.h>]])

################
# Check for system services

################
# Output

AC_CONFIG_FILES([
 Makefile
 src/Makefile
 doc/Makefile
 docs/Makefile
 m4/Makefile
 build-aux/Makefile
 test/Makefile
 tools/Makefile
 ])
AC_CONFIG_FILES([tools/uninstalled-env], [chmod +x tools/uninstalled-env])
AC_CONFIG_FILES([tools/run-guile], [chmod +x tools/run-guile])
AC_CONFIG_FILES([tools/gdb-guile], [chmod +x tools/gdb-guile])
AC_CONFIG_FILES([tools/gdb-test], [chmod +x tools/gdb-test])
AC_OUTPUT
