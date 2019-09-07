dnl Process this file with autoconf to produce a configure script

#    Copyright (C) 2018, 2019 Michael L. Gran

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

AC_INIT([Guile GI], [0.2.0], [spk121@yahoo.com], [guile_gi])

AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_SRCDIR([src/gig.c])
AM_INIT_AUTOMAKE

AC_PREREQ([2.63])
AC_CONFIG_HEADERS([src/config.h])
AC_CONFIG_MACRO_DIR([m4])


################
# Libtool versioning

LIBGUILE_GI_INTERFACE="3:0:0"
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

AC_PATH_PROG([GLIB_COMPILE_RESOURCES],[glib-compile-resources])

GOBJECT_INTROSPECTION_CHECK([1.46.0])

################
# Guile has three directories that don't follow the GNU File System
# Heirarchy Standards.  If one follows the GNU FHS, files get installed
# in directories in which Guile does not search for them by default.
AC_ARG_WITH([gnu-filesystem-hierarchy],
            [AS_HELP_STRING([--with-gnu-filesystem-hierarchy],
                            [Strictly follow GNU File System Hierarchy when installing, instead of querying Guile to discover the install directories that it expects.])])

AS_CASE(["x$with_gnu_filesystem_hierarchy"], [xyes],
    [
    # Follow the GNU File System Hierarchy Standard correctly
    # Installed .scm scheme libraries go here
    AC_MSG_CHECKING([Guile site directory])
    guilesitedir="\${datadir}/guile/site/$guile_effective_version"
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
    AC_SUBST(guileobjectdir)],
    [
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
    AC_SUBST(guileobjectdir)])

################
# Check for Libtool

LT_PREREQ([2.2])
LT_INIT([disable-fast-install disable-static win32-dll])

##################
# Initial guess of platform specific options

AS_CASE([$host_os],
        [mingw*], [building_dll=yes;dll_version_info=no],
        [cygwin*], [building_dll=yes;dll_version_info=yes],
        [pw32*], [building_dll=yes;dll_version_info=yes],
        [cegcc*], [building_dll=yes;dll_version_info=yes],
        [building_dll=no;dll_version_info=yes])

AM_CONDITIONAL([BUILDING_DLL], [test "$building_dll" = yes])
AM_CONDITIONAL([DLL_VERSION_INFO], [test "$dll_version_info" = yes])

################
# Check for libraries
PKG_CHECK_MODULES(GLIB, [glib-2.0 >= 2.48.0])
PKG_CHECK_MODULES(GOBJECT, [gobject-2.0])
PKG_CHECK_MODULES(GOBJECT_INTROSPECTION, [gobject-introspection-1.0])
PKG_CHECK_MODULES(FFI, [libffi])

PKG_CHECK_VAR(GI_GIR_DIR, [gobject-introspection-1.0], [girdir])
PKG_CHECK_VAR(GI_DATA_DIR, [gobject-introspection-1.0], [gidatadir])

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
AS_IF([test "x$enable_hardening" = xyes],
      [CC_CHECK_CFLAGS_APPEND([compiler_flags_common])])

AC_ARG_ENABLE([coverage],
              [AS_HELP_STRING([--enable-coverage],
                              [compile with gcov support])],
              [AS_CASE(["${enableval}"],
                       [yes], [cov=true],
                       [no], [cov=false],
                       [AC_MSG_ERROR([bad value ${enableval} for --enable-coverage])])],
              [cov=false])
AM_CONDITIONAL([COVERAGE], [test x$cov = xtrue])

AC_ARG_ENABLE([mtrace],
              [AS_HELP_STRING([--enable-mtrace],
                              [compile with gcc's memory tracing support])],
              [AS_CASE(["${enableval}"],
                       [yes], [mtrace=true],
                       [no], [mtrace=false],
                       [AC_MSG_ERROR([bad value ${enableval} for --enable-mtrace])])],
              [mtrace=false])
AM_CONDITIONAL([MTRACE], [test x$mtrace = xtrue])


################
# Check for C library functions

################
# Check for system services

################
# Output

AC_CONFIG_FILES([
 Makefile
 module/gi/config.scm
 ])
AC_CONFIG_FILES([tools/uninstalled-env], [chmod +x tools/uninstalled-env])
AC_CONFIG_FILES([tools/uninstalled-test-env], [chmod +x tools/uninstalled-test-env])
AC_CONFIG_FILES([tools/run-guile], [chmod +x tools/run-guile])
AC_CONFIG_FILES([tools/run-test], [chmod +x tools/run-test])
AC_CONFIG_FILES([tools/gdb-guile], [chmod +x tools/gdb-guile])
AC_CONFIG_FILES([tools/gdb-test], [chmod +x tools/gdb-test])
AC_CONFIG_FILES([tools/guile-gi], [chmod +x tools/guile-gi])
AC_CONFIG_FILES([tools/lcov.sh], [chmod +x tools/lcov.sh])
AC_CONFIG_FILES([tools/valgrind.sh], [chmod +x tools/valgrind.sh])

AC_REQUIRE_AUX_FILE([tap-driver.sh])

AC_OUTPUT
