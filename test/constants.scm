(use-modules (gi) (gi util)
             (srfi srfi-64))

(use-typelibs (("GLib" "2.0")
               #:renamer (protect* '(test-equal test-assert test-skip))))

(test-begin "version")

(test-equal "major version" 2 MAJOR_VERSION)
(test-assert "minor version" (integer? MINOR_VERSION))
(test-assert "micro version" (integer? MICRO_VERSION))
(test-equal "version min required" 2 VERSION_MIN_REQUIRED)

(test-end "version")

(test-begin "basic-types")
(test-equal "maxint16" 32767 MAXINT16)
(test-equal "maxuint64" #xFFFFFFFFFFFFFFFF MAXUINT64)
(test-assert "int32-format" (string? GINT32_FORMAT))

(test-equal "float bias" 127 IEEE754_FLOAT_BIAS)
(test-approximate "E" (exp 1) E (expt 10 -6))
(test-approximate "PI" 3.141592654 PI (expt 10 -6))
(test-end "basic-types" )

(test-begin "std-macros")
(test-assert "dir separator"
  (member DIR_SEPARATOR (map char->integer '(#\/ #\\))))
(test-assert "dir separator string"
  (member DIR_SEPARATOR_S '("/" "\\")))
(test-assert "searchpath separator"
  (member SEARCHPATH_SEPARATOR (map char->integer '(#\: #\;))))
(test-assert "searchpath separator string"
  (member SEARCHPATH_SEPARATOR_S '(":" ";")))

(test-equal "little-endian" 1234 LITTLE_ENDIAN)
(test-equal "big-endian" 4321 BIG_ENDIAN)
(test-equal "pdp-endian" 3412 PDP_ENDIAN)
;; BYTE_ORDER is missing, even though everything related to it is exported
(test-expect-fail "byte order")
(test-assert "byte order"
  (member BYTE_ORDER (list LITTLE_ENDIAN BIG_ENDIAN PDP_ENDIAN)))
(test-end "std-macros")
