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

noinst_DATA = lib.scm \
  automake-test-lib.scm \
  grilo/base.scm

TYPELIB_TESTS = \
 typelib/document.scm

VERSION_INFORMATION_TESTS = \
 version_info/MAJOR_VERSION.scm \
 version_info/MINOR_VERSION.scm \
 version_info/MICRO_VERSION.scm \
 version_info/VERSION_MIN_REQUIRED.scm

BASIC_TYPE_TESTS = \
 basic_types/MAXINT16.scm \
 basic_types/MAXUINT64.scm \
 basic_types/GINT32_FORMAT.scm

STANDARD_MACROS_TESTS = \
 std_macros/DIR_SEPARATOR_S.scm \
 std_macros/DIR_SEPARATOR.scm \
 std_macros/SEARCHPATH_SEPARATOR.scm \
 std_macros/SEARCHPATH_SEPARATOR_S.scm

BYTE_ORDER_MACROS_TESTS = \
 byte_order/BIG_ENDIAN.scm \
 byte_order/LITTLE_ENDIAN.scm

NUMERICAL_DEFINITIONS_TESTS = \
 basic_types/IEEE754_FLOAT_BIAS.scm \
 basic_types/E.scm \
 basic_types/PI.scm

ATOMIC_OPERATIONS_TESTS = \
 misc/atomic_int_set.scm

MAIN_EVENT_LOOP_TESTS = \
 main_loop/new.scm \
 main_loop/runs.scm \
 main_loop/quit.scm \
 main_loop/get_context.scm \
 main_loop/timeout.scm \
 main_loop/idle.scm

MEMORY_ALLOCATION_TESTS = \
 mem/malloc.scm \
 mem/try_malloc0.scm

MEMORY_SLICES_TESTS = \
 mem/slice_alloc.scm

IO_CHANNELS_TESTS = \
 io/io_channel_pipe.scm \
 io/io_channel_new_file.scm \
 io/io_channel_unichar.scm

MESSAGE_OUTPUT_TESTS = \
 io/log_set_handler.scm \
 io/log_writer_is_journald.scm

STRING_UTILITY_TESTS = \
 string/strdup.scm \
 string/strndup.scm \
 string/strnfill.scm \
 string/stpcpy.scm \
 string/strstr_len.scm \
 string/str_has_prefix.scm \
 string/str_tokenize_and_fold.scm \
 string/strjoinv.scm

CHARACTER_SET_CONVERSION_TESTS = \
 convert/ascii.scm \
 convert/latin1.scm

UNICODE_MANIPULATION_TESTS = \
 unichar/validate_zero.scm \
 unichar/isalpha.scm \
 unichar/compose.scm \
 unichar/combining_class.scm \
 misc/unicode_canonical_ordering.scm

BASE64_ENCODING_TESTS = \
 misc/base64_encode.scm

DATA_CHECKSUM_TESTS = \
 misc/checksum.scm

SECURE_HMAC_DIGESTS_TESTS = \
 hmac/string.scm \
 hmac/data.scm \
 hmac/bytes.scm

DATE_AND_TIME_FUNCTIONS_TESTS = \
 datetime/get_monotonic_time.scm \
 datetime/get_real_time.scm \
 datetime/date_new.scm \
 datetime/date_copy.scm \
 datetime/date_clear.scm

MEMCHK_TESTS = \
 mem/memchk_1.scm \
 mem/memchk_2.scm

BYTE_ARRAY_TESTS = \
 byte_array/byte_array_new.scm \
 byte_array/byte_array_new_take.scm \
 byte_array/bytes_new_null_size.scm \
 byte_array/bytes_new_null_contents.scm

GAPPLICATION_TESTS = \
 gapplication/command-line.scm \
 gapplication/command-line-options.scm \
 gapplication/create1.scm \
 gapplication/create2.scm \
 gapplication/set_resource_path.scm

FUZZY_MATCH_TESTS = \
 misc/fuzzy_match.scm

PARAMSPEC_TESTS = \
 paramspec/getset.scm \
 paramspec/default.scm

GTK_ENTRY_TESTS = \
 gtk_entry/set_alignment.scm \
 gtk_entry/set_alignment_zero.scm

GRILO_TESTS = \
 grilo/audio_simple.scm \
 grilo/audio_multiple_artists.scm \
 grilo/init_commandline.scm

TESTS = \
 $(TYPELIB_TESTS) \
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
 $(CHARACTER_SET_CONVERSION_TESTS) \
 $(BASE64_ENCODING_TESTS) \
 $(UNICODE_MANIPULATION_TESTS) \
 $(DATA_CHECKSUM_TESTS) \
 $(SECURE_HMAC_DIGESTS_TESTS) \
 $(DATE_AND_TIME_FUNCTIONS_TESTS) \
 $(MEMCHK_TESTS) \
 $(BYTE_ARRAY_TESTS) \
 $(GAPPLICATION_TESTS) \
 $(FUZZY_MATCH_TESTS) \
 $(PARAMSPEC_TESTS) \
 $(GTK_ENTRY_TESTS) \
 $(GRILO_TESTS) \
 misc/softerror_create.scm \
 misc/use-typelibs.scm

XFAIL_TESTS = \
 gapplication/command-line.scm \
 grilo/init_commandline.scm \
 misc/atomic_int_set.scm \
 string/strjoinv.scm

SCM_LOG_COMPILER = \
  ${top_builddir}/libtool \
  --dlopen=${top_builddir}/src/libguile-gi.la \
  --mode=execute $(GUILE)

AM_SCM_LOG_FLAGS = --no-auto-compile -L $(abs_top_srcdir)/src -L $(abs_top_srcdir) -s

EXTRA_DIST = \
  $(TESTS) \
  lib.scm \
  automake-test-lib.scm \
  grilo/base.scm

CLEANFILES = tmp.txt
