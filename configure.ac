m4_define([version_major],0)
m4_define([version_minor],1)
m4_define([version_micro],0)
m4_define([version_triplet],version_major.version_minor.version_micro)

AC_PREREQ([2.63])
AC_INIT([guile-gir], [version_triplet], [spk121@yahoo.com], [guile-gir])

AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_SRCDIR([src/gir.c])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE([foreign parallel-tests -Wno-portability])

m4_ifdef([AM_SILENT_RULES],[AM_SILENT_RULES([yes])])



################################################################################
# Libtool versioning

m4_define([lt_revision], m4_if(m4_eval(version_minor%2),1,0,version_micro))
m4_define([lt_age], m4_eval(version_minor*100+version_micro-lt_revision))
m4_define([lt_current], m4_eval(version_major+lt_age))
m4_define([lt_triplet],lt_current:lt_revision:lt_age)

LT_VERSION_INFO="lt_triplet()"
AC_SUBST([LT_VERSION_INFO])

################################################################################
# Check for programs

PKG_PROG_PKG_CONFIG
AC_PROG_CC
AC_USE_SYSTEM_EXTENSIONS
AC_HEADER_STDC
AM_PROG_CC_C_O
AC_PROG_SED

# Initialize libtool
LT_PREREQ([2.2])
LT_INIT
LT_LANG([C])

AM_CONDITIONAL([HAVE_GCC],[test "$GCC" = "yes"])

AM_MAINTAINER_MODE([enable])
AC_PROG_CC
LT_INIT
PKG_PROG_PKG_CONFIG

################################################################################
# Enable debugging messages and additional run-time checks.
################################################################################

AC_ARG_ENABLE([debug],
              [AS_HELP_STRING([--enable-debug],
                              [enable extra debugging checks and logging messages])],
              [],[enable_debug=no])

if test "x$enable_debug" = "xyes" ; then
        AC_MSG_RESULT([enabling debug checks and messages])
        AC_DEFINE(GIR_DEBUG,1,[Enable debugging messages.])
fi

################################################################################
# Compilation
################################################################################

m4_define([compiler_flags_common],[ dnl
  -pipe dnl
  -Wall dnl
  -Wcast-align dnl
  -Wendif-labels dnl
  -Werror=declaration-after-statement dnl
  "-Werror=format=2 -Werror=format-nonliteral -Werror=format-security" dnl
  -Werror=implicit-function-declaration dnl
  -Werror=init-self dnl
  -Werror=missing-include-dirs dnl
  -Werror=missing-prototypes dnl
  -Werror=pointer-arith dnl
  -Wextra dnl
  -Wfloat-equal dnl
  -Wlogical-op dnl
  -Wmisleading-indentation dnl
  -Wmissing-declarations dnl
  -Wmissing-include-dirs dnl
  -Wmissing-format-attribute dnl
  -Wmissing-noreturn dnl
  -Wno-missing-field-initializers dnl
  -Wno-switch-enum dnl
  -Wno-unused-parameter dnl
  -Wno-packed dnl
  -Wshadow dnl
  -Wsign-compare dnl
  -Wstrict-aliasing=2 dnl
  -Wundef dnl
  -Wuninitialized dnl
  -Wunsafe-loop-optimizations dnl
  -Wwrite-strings dnl
  -fno-common dnl
  -fdiagnostics-show-option dnl
  -fno-strict-aliasing dnl
  -fstack-protector dnl
  -fstack-protector-strong dnl
  -fno-semantic-interposition dnl
  -Wno-deprecated-declarations dnl
  -Waggregate-return dnl
  -Wimplicit dnl
  -Wnested-externs dnl
  -Wold-style-definition dnl
  -Wstrict-prototypes dnl
])

CC_CHECK_CFLAGS_APPEND([AM_CFLAGS],[CFLAGS],[compiler_flags_common])


################################################################################
# Core
################################################################################

# Check for headers.

AC_CHECK_HEADERS([sys/select.h sys/syslimits.h sys/termios.h sys/wait.h stropts.h termios.h util.h wchar.h pty.h])

PKG_CHECK_MODULES(GLIB, [glib-2.0 >= 2.48.0])
PKG_CHECK_MODULES(GOBJECT, [gobject-2.0])
PKG_CHECK_MODULES(GUILE, [guile-2.2])
PKG_CHECK_MODULES(GOBJECT_INTROSPECTION, [gobject-introspection-1.0])
GOBJECT_INTROSPECTION_CHECK([1.30.0])

################################################################

dnl GTK_DOC_CHECK([1.19], [--flavour no-tmpl])

################################################################

AC_SUBST([GIR_MAJOR_VERSION_NUMBER],[version_major])
AC_SUBST([GIR_MINOR_VERSION_NUMBER],[version_minor])
AC_SUBST([GIR_MICRO_VERSION_NUMBER],[version_micro])
AC_SUBST([AM_CPPFLAGS])
AC_SUBST([AM_CFLAGS])

################################################################

AC_CONFIG_FILES([
 Makefile
 src/Makefile
 test/Makefile
 m4/Makefile
 docs/Makefile])
AC_OUTPUT
