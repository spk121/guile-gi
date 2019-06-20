# Makefile.am - test directory
# Copyright (C) 2018,2019 Michael L. Gran

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

################################################################
# Test driver

.PHONY: test
test: check

AM_TESTS_ENVIRONMENT = \
  GUILE_AUTO_COMPILE=0; \
  export GUILE_AUTO_COMPILE;

AM_CFLAGS = $(GUILE_CFLAGS) $(GOBJECT_INTROSPECTION_CFLAGS)
TEST_EXTENSIONS = .scm

noinst_DATA = lib.scm automake-test-lib.scm

VERSION_INFORMATION_TESTS = \
 MAJOR_VERSION.scm \
 MINOR_VERSION.scm \
 MICRO_VERSION.scm \
 VERSION_MIN_REQUIRED.scm

BASIC_TYPE_TESTS = \
 MAXINT16.scm \
 MAXUINT64.scm \
 GINT32_FORMAT.scm

STANDARD_MACROS_TESTS = \
 DIR_SEPARATOR_S.scm \
 DIR_SEPARATOR.scm \
 SEARCHPATH_SEPARATOR.scm \
 SEARCHPATH_SEPARATOR_S.scm

BYTE_ORDER_MACROS_TESTS = \
 BIG_ENDIAN.scm \
 LITTLE_ENDIAN.scm

NUMERICAL_DEFINITIONS_TESTS = \
 IEEE754_FLOAT_BIAS.scm \
 E.scm \
 PI.scm

ATOMIC_OPERATIONS_TESTS = \
 atomic_int_set.scm

MAIN_EVENT_LOOP_TESTS = \
 main_loop_new.scm \
 main_loop_runs.scm \
 main_loop_quit.scm \
 main_loop_get_context.scm \
 main_loop_timeout.scm \
 main_loop_idle.scm

MEMORY_ALLOCATION_TESTS = \
 malloc.scm \
 try_malloc0.scm

MEMORY_SLICES_TESTS = \
 slice_alloc.scm

IO_CHANNELS_TESTS = \
 io_channel_pipe.scm \
 io_channel_new_file.scm \
 io_channel_unichar.scm

MESSAGE_OUTPUT_TESTS = \
 log_set_handler.scm \
 log_writer_is_journald.scm

STRING_UTILITY_TESTS = \
 strdup.scm \
 strndup.scm \
 strnfill.scm \
 stpcpy.scm \
 strstr_len.scm \
 str_has_prefix.scm \
 str_tokenize_and_fold.scm

BASE64_ENCODING_TESTS = \
 base64_encode.scm

TESTS = \
 $(VERSION_INFORMATION_TESTS) \
 $(BASIC_TYPE_TESTS) \
 $(STANDARD_MACROS_TESTS) \
 $(BYTE_ORDER_MACROS_TESTS) \
 $(NUMERICAL_DEFINITIONS_TESTS) \
 $(ATOMIC_OPERATIONS_TESTS) \
 $(MESSAGE_OUTPUT_TESTS) \
 $(MAIN_EVENT_LOOP_TESTS) \
 $(MEMORY_ALLOCATION_TESTS) \
 $(MEMORY_SLICES_TESTS) \
 $(IO_CHANNELS_TESTS) \
 $(MEMORY_OUTPUT_TESTS) \
 $(STRING_UTILITY_TESTS) \
 $(BASE64_ENCODING_TESTS) \
 memchk_1.scm \
 memchk_2.scm \
 byte_array_new.scm \
 byte_array_new_take.scm \
 bytes_new_null_size.scm \
 bytes_new_null_contents.scm \
 gapplication_1.scm \
 gapplication_2.scm \
 softerror_create.scm \
 strjoinv.scm \
 convert.scm \
 convert_latin1.scm

XFAIL_TESTS = \
 atomic_int_set.scm \
 strjoinv.scm

SCM_LOG_COMPILER = \
  ${top_builddir}/libtool \
  --dlopen=${top_builddir}/src/libguile-gi.la \
  --mode=execute $(GUILE)

AM_SCM_LOG_FLAGS = --no-auto-compile -L $(abs_top_srcdir)/src -L $(abs_top_srcdir) -s

EXTRA_DIST = \
  $(TESTS) \
  lib.scm \
  automake-test-lib.scm \
  example-1.scm \
  browser.scm \
  editor.scm

exampledir = $(datadir)/doc/guile-gi
dist_example_DATA = \
  example-1.scm \
  example-1b.scm \
  example-1c.scm \
  browser.scm \
  editor.scm

CLEANFILES = tmp.txt
