dnl Process this file with autoconf to produce a configure script

dnl Copyright (C) Michael L. Gran


################
# Package Info

AC_INIT([Guile GI], [0.0.1], [spk121@yahoo.com], [guile-gi])

AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([foreign parallel-tests -Wno-portability])

AC_PREREQ([2.63])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([src/gir.c])
AC_CONFIG_MACRO_DIR([m4])

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
AM_PROG_CC_C_O
AC_PROG_SED

# from guile.m4
GUILE_PKG([2.2])
GUILE_PROGS
GUILE_FLAGS
GUILE_SITE_DIR

AC_MSG_CHECKING([the Guile effective version])
guile_effective_version = "`$GUILE -c '(display (effective-version))'`"
AC_MSG_RESULT([$guile_effective_version])

################
# Guile has three directories that don't follow the GNU Filesystem
# Hierarchy Standards.  If one follows the GNU FHS, files get installed
# in directories in which Guile does not search for them by default.
AC_ARG_WITH([guile-directories],
            [AS_HELP_STRING([--with-guile-directories],
                            [Query Guile to discover the correct install directories for scripts and extensions, and ignore the GNU Filesystem Hierarchy when installing])])

case "x$with_guile_directories" in
     xyes)
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
     *)
       # Follow the GNU Filesystem Hierarchy Standard correctly
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
esac

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
PKG_CHECK_MODULES(GUILE, [guile-2.2])
PKG_CHECK_MODULES(GOBJECT_INTROSPECTION, [gobject-introspection-1.0])
GOBJECT_INTROSPECTION_CHECK([1.30.0])

################
# Check for header files

################
# Check for types, structures, compiler characteristics
AX_C___ATTRIBUTE__

m4_define([compiler_flags_common],[ dnl
  -pipe dnl
  -Wall dnl
  -Wextra dnl
  -Waggregate-return dnl
  -Wcast-align dnl
  -Wdeclaration-after-statement dnl
  -Werror=missing-include-dirs dnl
  -Werror=pointer-arith dnl
  -Wfloat-equal dnl
  -Wformat=2 dnl
  -Winit-self dnl
  -Wlogical-op dnl
  -Wmissing-declarations dnl
  -Wmissing-include-dirs dnl
  -Wmissing-prototypes dnl
  -Wnested-externs dnl
  -Wno-missing-field-initializers dnl
  -Wno-packed dnl
  -Wno-switch-enum dnl
  -Wno-unused-parameter dnl
  -Wold-style-definition dnl
  -Wshadow dnl
  -Wstrict-prototypes dnl
  -Wsuggest-attribute=format dnl
  -Wsuggest-attribute=malloc dnl
  -Wsuggest-attribute=noreturn dnl
  -Wsuggest-attribute=pure dnl
  -Wundef dnl
  -Wuninitialized dnl
  -Wwrite-strings dnl
  -fdiagnostics-show-option dnl
  -fno-strict-aliasing dnl
  -fstack-protector dnl
  -fstack-protector-strong dnl
  -fno-semantic-interposition dnl
  -fsanitize=address dnl        AddressSanitizer
  -fno-omit-frame-pointer dnl   AddressSanitizer
  -fno-common dnl               AddressSanitizer
  -fsanitize=leak dnl           LeakSanatizer
  -fsanitize=undefined dnl      UndefinedBehaviorSanitizer
  -fsanitize=bounds-strict dnl  UndefinedBEhaviorSanitizer
])

AC_ARG_WITH([so-many-warnings],
            [AS_HELP_STRING([--with-so-many-warnings],
                            [Compile with ALL THE WARNINGS!])])
case "x$with_so_many_warnings" in
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
 src/Makefile
 test/Makefile
 m4/Makefile
 build-aux/Makefile
 doc/Makefile])
AC_REQUIRE_AUX_FILE([tap-driver.sh])
AC_OUTPUT

if test "x$with_guile_directories" = "xyes"; then
   AC_MSG_NOTICE([]
   [The Guile modules will be installed in $guilesitedir.]
   [The Guile extension libraries will be installed in $guileextensiondir.]
   [The compiled modules will be installed in $guileobjectdir.]
   [This is because you ran configure with the `--with-guile-directories' flag]
   [These directories are the ones expected by $GUILE])
else
   # Guile may have a different prefix than this module
   AC_MSG_WARN([]
   [The Guile modules will be installed in $guilesitedir.]
   [The Guile extension libraries will be installed in $guileextensiondir.]
   [The compiled modules will be installed in $guileobjectdir.]
   []
   [Remember that you may have to adjust the `GUILE_LOAD_PATH' environment]
   [variable.])
fi
AC_SUBST([guilesitedir])
