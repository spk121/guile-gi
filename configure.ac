dnl Process this file with autoconf to produce a configure script
dnl Copyright (C) 2018 Michael L. Gran

################
# Package Info

AC_INIT([Guile GI], [0.0.2], [spk121@yahoo.com], [guile-gi])

AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([foreign subdir-objects silent-rules parallel-tests -Wno-portability -Wall])
AM_MAINTAINER_MODE

AC_PREREQ([2.63])
AC_CONFIG_MACRO_DIR([m4])

AC_CONFIG_SRCDIR([src/gi/gir.c])

################
# Libtool versioning

LIBGUILE_GI_INTERFACE="0:0:0"
AC_SUBST([LIBGUILE_GI_INTERFACE])

################
# Check for programs

PKG_PROG_PKG_CONFIG
AC_PROG_CC
dnl AC_USE_SYSTEM_EXTENSIONS
AC_HEADER_STDC
dnl AC_PROG_SED
AM_PROG_AR

################
# Check for Libtool
LT_PREREQ([2.2])
LT_INIT([dlopen disable-fast-install disable-static win32-dll])

################
# Initial guess of platform specific options

################
# Check for libraries
PKG_CHECK_MODULES(GLIB, [glib-2.0 >= 2.48.0])
PKG_CHECK_MODULES(GOBJECT, [gobject-2.0])
PKG_CHECK_MODULES(GOBJECT_INTROSPECTION, [gobject-introspection-1.0])
PKG_CHECK_MODULES(FFI, [libffi])
PKG_CHECK_MODULES(GUILE, [guile-2.2])

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
                            [Compile with common hardening flags])])
case "x$enable_hardening" in
     xyes)
     CC_CHECK_CFLAGS_APPEND([compiler_flags_common])
     ;;
esac

################
# Check for C library functions

################
# Check for system services

################
# Output

AC_CONFIG_FILES([
 Makefile
 ])
AC_OUTPUT
