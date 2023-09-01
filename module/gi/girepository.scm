;; Copyright (C) 2023 Michael L. Gran

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; A thin wrapper around the C functions provided by libgirepository
(define-module (gi girepository)
  #:export (<gibaseinfo>
            gibaseinfo?
            gibaseinfo=?

            get-dependencies
            get-immediate-dependencies
            get-loaded-namespaces
            get-n-infos
            get-info
            enumerate-versions
            prepend-library-path
            prepend-search-path
            get-search-path
            get-typelib-path
            is-registered?
            require
            require-private
            get-c-prefix
            get-shared-library
            get-version
            find-by-gtype
            find-by-error-domain
            find-by-name
            get-object-gtype-interfaces

            ;; Version info
            get-major-version
            get-minor-version
            get-micro-version
            check-version

            ;; COMMON TYPES
            type-tag-is-basic?
            type-tag-is-numeric?
            type-tag-is-container?
            type-tag-to-string

            ;; GIBaseInfo
            is-base-info?
            base-info-equal?
            base-info-get-type
            base-info-get-namespace
            base-info-get-name
            base-info-get-attribute
            base-info-get-container
            base-info-is-deprecated?
            info-type-to-string
            ;; GICallableInfo
            is-callable-info?
            callable-info-can-throw-gerror?
            callable-info-get-n-args
            callable-info-get-arg
            callable-info-get-caller-owns
            callable-info-get-instance-ownership-transfer
            callable-info-get-return-attribute
            callable-info-get-return-type
            callable-info-is-method?
            callable-info-get-return-attributes
            callable-info-may-return-null?
            callable-info-skip-return?

            ;; GIFunctionInfo
            is-function-info?
            function-info-get-flags
            function-info-get-property
            function-info-get-symbol
            function-info-get-vfunc

            ;; GICallbackInfo
            is-callback-info?

            ;; GISignalInfo
            is-signal-info?
            signal-info-get-flags
            signal-info-get-class-closure
            signal-info-true-stops-emit?

            ;; GIVFuncInfo
            vfunc-info-get-flags
            vfunc-info-get-offset
            vfunc-info-get-signal
            vfunc-info-get-invoker

            ;; GIRegisteredType
            is-registered-type-info
            registered-type-info-get-type-name
            registered-type-info-get-type-init
            registered-type-info-get-g-type

            ;; GIEnumInfo
            is-enum-info?
            enum-info-get-n-values
            enum-info-get-value
            enum-info-get-n-methods
            enum-info-get-method
            enum-info-get-error-domain
            enum-info-get-storage-type

            ;; GIValueInfo
            is-value-info?
            value-info-get-value

            ;; GIStructInfo
            is-struct-info?
            struct-info-find-field
            struct-info-get-alignment
            struct-info-get-size
            struct-info-is-gtype-struct?
            struct-info-is-foreign?
            struct-info-get-n-fields
            struct-info-get-field
            struct-info-get-n-methods
            struct-info-get-method
            struct-info-find-method

            ;; GIUnionInfo
            is-union-info?
            union-info-get-n-fields
            union-info-get-field
            union-info-get-n-methods
            union-info-get-method
            union-info-is-discriminated?
            union-info-get-discriminator-offset
            union-info-get-discriminator-type
            union-info-get-discriminator
            union-info-find-method
            union-info-get-size
            union-info-get-alignment

            ;; GIObjectInfo
            is-object-info?
            object-info-get-abstract?
            object-info-get-fundamental?
            object-info-get-final?
            object-info-get-parent
            object-info-get-type-name
            object-info-get-type-init
            object-info-get-n-constants
            object-info-get-constant
            object-info-get-n-fields
            object-info-get-field
            object-info-get-n-interfaces
            object-info-get-interface
            object-info-get-n-methods
            object-info-get-method
            object-info-find-method
            object-info-find-method-using-interfaces
            object-info-get-n-properties
            object-info-get-property
            object-info-get-n-signals
            object-info-get-signal
            object-info-find-signal
            object-info-get-n-vfuncs
            object-info-get-vfunc
            object-info-find-vfunc
            object-info-find-vfunc-using-interfaces
            object-info-get-class-struct
            object-info-get-ref-function
            object-info-get-unref-function
            object-info-get-set-value-function
            object-info-get-get-value-function

            ;; GIInterfaceInfo
            is-interface-info?
            interface-info-get-n-prerequisites
            interface-info-get-prerequisite
            interface-info-get-n-properties
            interface-info-get-property
            interface-info-get-n-methods
            interface-info-get-method
            interface-info-find-method
            interface-info-get-n-signals
            interface-info-get-signal
            interface-info-find-signal
            interface-info-get-n-vfuncs
            interface-info-get-vfunc
            interface-info-find-vfunc
            interface-info-get-n-constants
            interface-info-get-constant
            interface-info-get-iface-struct

            ;; GIArgInfo
            is-arg-info?
            arg-info-get-closure
            arg-info-get-destroy
            arg-info-get-direction
            arg-info-get-ownership-transfer
            arg-info-get-scope
            arg-info-get-type
            arg-info-may-be-null?
            arg-info-is-caller-allocates?
            arg-info-is-optional?
            arg-info-is-return-value?
            arg-info-is-skip?

            ;; GIConstantInfo
            is-constant-info?
            constant-info-get-type
            constant-info-get-value

            ;; GIFieldInfo
            is-field-info?
            field-info-get-flags
            field-info-get-offset
            field-info-get-size
            field-info-get-type

            ;; GIPropertyInfo
            is-property-info?
            property-info-get-flags
            property-info-get-ownership-transfer
            property-info-get-type
            property-info-get-getter
            property-info-get-setter

            ;; GITypeInfo
            is-type-info?
            type-info-is-pointer?
            type-info-get-tag
            type-info-get-param-type
            type-info-get-interface
            type-info-get-array-length
            type-info-get-array-fixed-size
            type-info-is-zero-terminated?
            type-info-get-array-type
            ))

(eval-when (expand load eval compile)
  (load-extension "libguile-girepository" "gig_init_girepository"))

;; ALL THE PARANOIA

;; There is no official limit for some of these contants.  But we want
;; all the paranoia before we hand things off to C.
(define *max-namespace-name-len* 128)
(define *min-namespace-name-len* 2)
(define *max-version-string-len* 17)
(define *min-version-string-len* 1)
(define *max-name-len* 128)             ; The longest function name
(define *min-name-len* 1)
(define *max-namespace-count* 256)
(define *path-max* 32767)
(define *min-gtype-integer* 1)
(define *max-gtype-integer* #xFFFFffffFFFFffff)     ; Actually pointer
                                        ; sized, so 32-bit
                                        ; on 32-bit
                                        ; systems
(define *min-tag-integer* 0)
(define *max-tag-integer* 31)           ; As of 1.71.1, the actual max is 21 for UNICHAR
(define *min-version-integer* 0)
(define *max-version-integer* 1000)     ; Really probably about 100
(define char-set:alnum-dash-dot
  (string->char-set (string-append "abcdefghijklmnopqrstuvwxyz"
                                   "ABCDEFGHIJKLMNOPQRSTUVWZYZ"
                                   "0123456789"
                                   "-.")))
(define char-set:num-dot
  (string->char-set (string-append "0123456789.")))
(define char-set:alnum-dash-underscore
  (string->char-set (string-append "abcdefghijklmnopqrstuvwxyz"
                                   "ABCDEFGHIJKLMNOPQRSTUVWZYZ"
                                   "0123456789"
                                   "-_")))

(define-syntax assert-namespace-string
  (syntax-rules ()
    ((assert-namespace-string funcname namespace)
     (begin
       (unless (string? namespace)
         (scm-error 'wrong-type-arg funcname "Not a string: ~S"
                    (list namespace) (list namespace)))
       (when (< (string-length namespace) *min-namespace-name-len*)
         (scm-error 'out-of-range funcname "String too short: ~S"
                    (list namespace) (list namespace)))
       (when (> (string-length namespace) *max-namespace-name-len*)
         (scm-error 'out-of-range funcname "String too long: ~S"
                    (list namespace) (list namespace)))
       (unless (string-every char-set:alnum-dash-dot namespace)
         (scm-error 'out-of-range funcname "Invalid characters: ~S"
                    (list namespace) (list namespace)))))))

(define-syntax assert-version-string
  (syntax-rules ()
    ((assert-version-string funcname namespace)
     (begin
       (unless (string? namespace)
         (scm-error 'wrong-type-arg funcname "Not a string: ~S"
                    (list namespace) (list namespace)))
       (when (< (string-length namespace) *min-version-string-len*)
         (scm-error 'out-of-range funcname "String too short: ~S"
                    (list namespace) (list namespace)))
       (when (> (string-length namespace) *max-version-string-len*)
         (scm-error 'out-of-range funcname "String too long: ~S"
                    (list namespace) (list namespace)))
       (unless (string-every char-set:num-dot namespace)
         (scm-error 'out-of-range funcname "Invalid characters: ~S"
                    (list namespace) (list namespace)))))))

(define-syntax assert-loaded-namespace
  (syntax-rules ()
    ((assert-loaded-namespace funcname namespace)
     ;; FIXME: does irepository-get-loaded returned versioned namespacess?
     (unless (member namespace (%irepository-get-loaded-namespaces))
       (scm-error 'program-error funcname "Namespace not loaded: ~S"
                  (list namespace) #f)))))

(define-syntax assert-directory-string
  (syntax-rules ()
    ((assert-directory-string funcname directory)
     (begin
       (unless (string? directory)
         (scm-error 'wrong-type-arg funcname "Not a string: ~S"
                    (list funcname) (list funcname)))
       (when (string-null? directory)
         (scm-error 'out-of-range funcname "String too short: ~S"
                    (list directory) (list directory)))
       (when (> (string-length directory) *path-max*)
         (scm-error 'out-of-range funcname "String too long: ~S"
                    (list directory) (list directory)))
       ;; Note that that 'stat' will throw if directory not found or unreadable.
       (let ((info (stat directory)))
         (unless (eqv? 'directory (stat:type info))
           (scm-error 'misc-error funcname "Not a directory: ~S"
                      (list directory) #f)))))))

(define-syntax assert-name-string
  (syntax-rules ()
    ((assert-name-string funcname name)
     (begin
       (unless (string? name)
         (scm-error 'wrong-type-arg funcname "Not a string: ~S"
                    (list name) (list name)))
       (when (< (string-length name) *min-name-len*)
         (scm-error 'out-of-range funcname "String too short: ~S"
                    (list name) (list name)))
       (when (> (string-length name) *max-name-len*)
         (scm-error 'out-of-range funcname "String too long: ~S"
                    (list name) (list name)))
       (unless (string-every char-set:alnum-dash-underscore name)
         (scm-error 'out-of-range funcname "Invalid characters: ~S"
                    (list name) (list name)))))))

(define-syntax assert-gtype-integer
  (syntax-rules ()
    ((assert-gtype-integer funcname gtype)
     (begin
       (unless (and (exact? gtype) (integer? gtype))
         (scm-error 'wrong-type-arg funcname "Not an exact integer: ~S"
                    (list gtype) (list gtype)))
       (when (< gtype *min-gtype-integer*)
         (scm-error 'out-of-range funcname "Out of range: ~S"
                    (list gtype) (list gtype)))
       (when (>= gtype *max-gtype-integer*)
         (scm-error 'out-of-range funcname "Out of range: ~S"
                    (list gtype) (list gtype)))))))

(define-syntax assert-loaded-gtype
  (syntax-rules ()
    ((assert-loaded-gtype funcname gtype)
     (unless (find-by-gtype gtype)
       (scm-error 'program-error funcname "GType not found in loaded namespaces: ~S"
                  (list gtype) #f)))))

(define-syntax assert-version-integer
  (syntax-rules ()
    ((assert-version-integer funcname version)
     (begin
       (unless (and (exact? version) (integer? version))
         (scm-error 'wrong-type-arg funcname "Not an exact integer: ~S"
                    (list version) (list version)))
       (when (< version *min-version-integer*)
         (scm-error 'out-of-range funcname "Out of range: ~S"
                    (list version) (list version)))
       (when (>= version *max-version-integer*)
         (scm-error 'out-of-range funcname "Out of range: ~S"
                    (list version) (list version)))))))

(define-syntax assert-tag-integer
  (syntax-rules ()
    ((assert-tag-integer funcname name)
     (begin
       (unless (and (exact? tag) (integer? tag))
         (scm-error 'wrong-type-arg funcname "Not an exact integer: ~S"
                    (list tag) (list tag)))
       (when (< tag *min-tag-integer*)
         (scm-error 'out-of-range funcname "Out of range: ~S"
                    (list tag) (list tag)))
       (when (>= index *max-tag-integer*)
         (scm-error 'out-of-range funcname "Out of range: ~S"
                    (list tag) (list tag)))))))

(define-syntax assert-index-integer
  (syntax-rules ()
    ((assert-index-integer funcname n max)
     (begin
       (unless (and (exact? n) (integer? n))
         (scm-error 'wrong-type-arg funcname "Not an exact integer: ~S"
                    (list n) (list n)))
       (when (< n 0)
         (scm-error 'out-of-range funcname "Out of range: ~S"
                    (list n) (list n)))
       (when (>= n max)
         (scm-error 'out-of-range funcname "Out of range: ~S"
                    (list n) (list n)))))))

(define-syntax assert-gibaseinfo
  (syntax-rules ()
    ((assert-gibaseinfo funcname info)
     (unless (gibaseinfo? info)
       (scm-error 'wrong-type-arg funcname "Not a <gibaseinfo>: ~S"
               (list info) (list info))))))

(define (get-dependencies namespace)
  "Given the string NAMESPACE, which is the unversioned namespace name
of an already-loaded namespace, it returns a list of strings of
all (transitive) versioned dependencies for the namespace.  Returned
strings are of the form 'namespace-version'."
  (assert-namespace-string "get-dependencies" namespace)
  (assert-loaded-namespace "get-dependencies" namespace)
  (%irepository-get-dependencies namespace))

(define (get-immediate-dependencies namespace)
  "Given the string NAMESPACE, which is the unversioned namespace name
of an already-loaded namespace, it returns a list of strings of all
immediate versioned dependencies for the namespace.  Returned strings
are of the form 'namespace-version'."
  (assert-namespace-string "get-immediate-dependencies" namespace)
  (assert-loaded-namespace "get-immediate-dependencies" namespace)
  (%irepository-get-immediate-dependencies namespace))

(define (get-loaded-namespaces)
  "Returns a list of the currently loaded namespaces."
  (%irepository-get-loaded-namespaces))

(define (get-n-infos namespace)
  "Returns the number of metadata entries in the given namespace.
The namespace must have already been loaded before calling this
function."
  (assert-namespace-string "get-n-infos" namespace)
  (assert-loaded-namespace "get-n-infos" namespace)
  (%irepository-get-n-infos namespace))

(define (get-info namespace index)
  "This procedure returns a particular metadata entry of a namespace.
The namespace must already have been loaded.  Use 'get-n-infos' to get
total number of metadata entries."
  (assert-namespace-string "get-info" namespace)
  (assert-loaded-namespace "get-info" namespace)
  (assert-index-integer "get-info" index (get-n-infos namespace))
  (%irepository-get-info namespace index))

(define (enumerate-versions namespace)
  "Returns a list of versions (either currently loaded or available) for
namespace."
  (assert-namespace-string "enumerate-versions" namespace)
  (%irepository-enumerate-versions namespace))

(define (prepend-library-path directory)
  "Prepends directory to the search path used to search for shared libraries
referenced by imported namespaces.  This function can be called
multiple times to contribute more directories to the final list of
paths."
  (assert-directory-string "prepend-library-path" directory)
  (%irepository-prepend-library-path directory))

(define (prepend-search-path directory)
  "Prepends directory to the typelib search path.  This function can be
called multiple times to contribute more directories to the final list
of paths."
  (assert-directory-string "prepend-search-path" directory)
  (%irepository-prepend-search-path directory))

(define (get-search-path)
  "Returns a list of directory name strings of the current search path
that girepository will use when loading typelib files.  Note that
modifying the returned list has no effect on the typelib search path."
  (%irepository-get-search-path))

(define (get-typelib-path namespace)
  "The procedure returns the full path to the typelib file that
corresponds to an already-loaded namespace."
  (assert-namespace-string "get-typelib-path" namespace)
  (%irepository-get-typelib-path namespace))

(define* (is-registered? namespace #:optional version)
  "Checks whether a particular namespace (and optionally a specific
version thereof) is currently loaded."
  (assert-namespace-string "is-registered?" namespace)
  (when version
    (assert-version-string "is-registered?" version))
  (%irepository-is-registered namespace version))

(define* (require namespace #:key (version #f)
                  (lazy #f))
  "Loads a namespace.  If the namespace is not already loaded, it
searches for a 'typelib' file in the repository search path.  A
specific version string may be specified.  If a version is not
specified, the latest available version will be used.
Will throw an error if the namespace cannot be loaded."
  (assert-namespace-string "require" namespace)
  (when version
    (assert-version-string "require" version))
  (%irepository-require namespace version
                        (if lazy %LOAD_FLAG_LAZY 0)))

(define* (require-private typelib-dir namespace #:key (version #f)
                          (lazy #f))
  "Loads a namespace.  If the namespace is not already loaded, it
searches for a 'typelib' file in the one specified directory only.  A
specific version string may be specified and is recommended.  If a
version is not specified, the latest available version will be used.

  Integer-valued flags may be provided.  At the moment the is only one
flag currently defined: LOAD_FLAG_LAZY.

  Will throw an error if the namespace cannot be loaded."
  (assert-directory-string "require-private" typelib-dir)
  (assert-namespace-string "require-private" namespace)
  (when version
    (assert-version-string "require-private" version))
  (%irepository-require-private typelib-dir namespace version
                                (if lazy %LOAD_FLAG_LAZY 0)))

(define (get-c-prefix namespace)
  "Returns the 'C prefix' or the C level namespace associated with
the given introspection namespace.  Each C symbol starts with this
string, as well as each type.

The namespace must already have been loaded before calling
this function."
  (assert-namespace-string "get-c-prefix" namespace)
  (assert-loaded-namespace "get-c-prefix" namespace)
  (%irepository-get-c-prefix namespace))

(define (get-shared-library namespace)
  "This function returns a list of strings which are paths to the
shared C libraries associated with the given namespace.  There may be
no shared library path associated, in which case this function will
return an empty list.

Note: The namespace must have already been loaded before calling this
function."
  (assert-namespace-string "get-shared-library" namespace)
  (assert-loaded-namespace "get-shared-library" namespace)
  (let ((lib (%irepository-get-shared-library namespace)))
    (if (not lib)
        '()
        ;; else
        (string-split lib #\,))))

(define (get-version namespace)
  "This function returns, as a string, the loaded version associated
with the given namespace.

Note: The namespace must have already been loaded before calling this
function."
  (assert-namespace-string "get-version" namespace)
  (assert-loaded-namespace "get-version" namespace)
  (%irepository-get-version namespace))

(define (find-by-gtype gtype)
  "Searches all loaded namespaces for a particular GType. Note that in
order to locate the metadata, the namespace corresponding to the type
must first have been loaded. There is currently no mechanism for
determining the namespace which corresponds to an arbitrary GType -
thus, this function will operate most reliably when you know the GType
to originate from be from a loaded namespace.  If found, returns the
#<gibaseinfo> representing metadata about the GType, otherwise #f."
  (assert-gtype-integer "find-by-gtype" gtype)
  (%irepository-find-by-gtype gtype))

(define (find-by-error-domain quark)
  "Given an integer (a quark) that represents a given GError domain,
this procedure returns a #<gibaseinfo> representing metadata about
the enum type corresponding to the error domain.  This function
will fail if the repository corresponding to the error domains has
not already been loaded.

Returns a #<gibaseinfo> on success, or #f otherwise."
  (unless (and (exact? quark) (integer? quark))
    (scm-error 'wrong-type-arg "find-by-error-domain" "Not an exact integer: ~S"
               (list quark) (list quark)))
  (when (< quark 0)
    (scm-error 'out-of-range "find-by-error-domain" "Out of range: ~S"
               (list quark) (list quark)))
  (when (>= quark #xFFFFFFFFFFFFFFFF)
    (scm-error 'out-of-range "find-by-error-domain" "Out of range: ~S"
               (list quark) (list quark)))
  (%irepository-find-by-error-domain quark))

(define (find-by-name namespace name)
  "Searches for a particular entry in a namespace. Before calling this
function for a particular namespace, ensure the namespace has already
been loaded.  Returns a #<gibaseinfo> or #f if not found."
  (assert-namespace-string "find-by-name" namespace)
  (assert-loaded-namespace "find-by-name" namespace)
  (assert-name-string "find-by-name" name)
  (%irepository-find-by-name namespace name))

(define (get-object-gtype-interfaces gtype)
  "Returns a list of #<gibaseinfo> that represent the
interfaces for the given GType.  Note that the
namespace that contains the specified GType must already
be loaded."
  (assert-gtype-integer "get-object-gtype-interfaces" gtype)
  (assert-loaded-gtype "get-object-gtype-interfaces" gtype)
  (%irepository-get-object-gtype-interfaces gtype))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VERSION INFO

(define (get-major-version)
  "Returns the integer value of the major version of GIRepository."
  (%get-major-version))

(define (get-minor-version)
  "Returns the integer value of the minor version of GIRepository."
  (%get-major-version))

(define (get-micro-version)
  "Returns the integer value of the micro version of GIRepository."
  (%get-major-version))

(define (check-version major minor micro)
  "Returns #t if the major / minor / micro triplet is the same
or newer than the current version of GIRepository."
  (assert-version-integer "check-version" major)
  (assert-version-integer "check-version" minor)
  (assert-version-integer "check-version" micro)
  (%check-version major minor micro))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMON TYPES

(define type-tag-enum
  '(void int8 uint8 int16 uint16 int32 uint32 int64 uint64
         float double gtype utf8 filename array interface
         glist gslist ghash error unichar))

(define (type-tag-is-basic? tag)
  "Given a symbol in the type-tag-enum set, it returns #t if the tag is
a basic tag: if it is not a container."
  (unless (member tag type-tag-enum)
    (scm-error 'wrong-type-arg "type-tag-is-basic?" "Not a type-tag-enum symbol: ~S"
               (list tag) (list tag)))
  (%type-tag-is-basic (list-index
                       (lambda (x) (eqv? x tag))
                       type-tag-enum)))

(define (type-tag-is-container? tag)
  "Given a symbol in the type-tag-enum set, it returns #t if the tag is
a container."
  (unless (member tag type-tag-enum)
    (scm-error 'wrong-type-arg "type-tag-is-container?" "Not a type-tag-enum symbol: ~S"
               (list tag) (list tag)))
  (%type-tag-is-container (list-index
                           (lambda (x) (eqv? x tag))
                           type-tag-enum)))

(define (type-tag-is-numeric? tag)
  "Given a symbol in the type-tag-enum set, it returns #t if the tag is
a numeric type."
  (unless (member tag type-tag-enum)
    (scm-error 'wrong-type-arg "type-tag-is-numeric?" "Not a type-tag-enum symbol: ~S"
               (list tag) (list tag)))
  (%type-tag-is-numeric (list-index
                         (lambda (x) (eqv? x tag))
                         type-tag-enum)))

(define (type-tag->string tag)
  "Given a symbol in the type-tag-enum set, it returns a string
representation of the type tag."
  (unless (member tag type-tag-enum)
    (scm-error 'wrong-type-arg "type-tag->string" "Not a type-tag-enum symbol: ~S"
               (list tag) (list tag)))
  (%type-tag-to-string (list-index
                        (lambda (x) (eqv? x tag))
                        type-tag-enum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIBaseInfo

(define info-type-enum
  '(invalid function callback struct boxed
            enum flags object interface constant
            invalid-0 union value signal vfunc
            property field arg type unresolved))

;; gibaseinfo? and gibaseinfo=? are exported primitives

(define (is-base-info? info)
  (gibaseinfo? info))

(define (base-info-get-type info)
  "Returns a symbol from the info-type-enum set that indicates
the type of the given <gibaseinfo>."
  (assert-gibaseinfo "base-info-get-type" info)
  (let ((typ (%base-info-get-type info)))
    (list-ref info-type-enum typ)))

(define (base-info-get-namespace info)
  "Returns the string representation of the namespace associated with
the given <gibaseinfo>."
  (assert-gibaseinfo "base-info-get-namespace" info)
  (%base-info-get-namespace info))

(define (base-info-get-name info)
  "Returns the name string associated with the given <gibaseinfo>."
  (assert-gibaseinfo "base-info-get-name" info)
  (%base-info-get-name info))

(define (base-info-get-attribute info name)
  "Given the string name of an arbitrary attribute associated with
this <gibaseinfo>, it returns the string value of the attribute if it
exists.  Otherwise #f."
  (assert-gibaseinfo "base-info-get-attribute" info)
  (assert-name-string "base-info-get-attribute" name)
  (%base-info-get-attribute info name))

(define (base-info-get-attributes info)
  "Returns an association list of the arbitrary attributes associated
with a given <gibaseinfo>."
  (assert-gibaseinfo "base-info-get-attributes" info)
  (%base-info-get-attributes info))

(define (base-info-get-container info)
  "Returns the parent <gibaseinfo> of a given <gibaseinfo>.  For example,
the parent of a function <gibaseinfo> cound be an object or an interface."
  (assert-gibaseinfo "base-info-get-container" info)
  (%base-info-get-container info))

(define (base-info-is-deprecated? info)
  "Returns #t when the <gibaseinfo> is marked as deprecated."
  (assert-gibaseinfo "base-info-is-deprecated?" info)
  (%base-info-is-deprecated info))

(define (info-type->string tag)
  "Given a symbol from the info-type-enum set, this returns
the string representation of that info type."
  (unless (member tag info-type-enum)
    (scm-error 'wrong-type-arg "info-tag->string" "Not an info-tag-enum symbol: ~S"
               (list tag) (list tag)))
  (%info-type-to-string (list-index
                         (lambda (x) (eqv? x tag))
                         info-type-enum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GICallableInfo

(define transfer-enum
  '(nothing container everything))

(define (is-callable-info? info)
  "Returns #t if the given <gibaseinfo> is a callable or derived from
it, like a function, vfunc, or callback."
  (assert-gibaseinfo "is-callable-info?" info)
  (%is-callable-info info))

(define-syntax assert-gicallableinfo
  (syntax-rules ()
    ((assert-gicallableinfo funcname info)
     (unless (is-callable-info? info)
       (scm-error 'wrong-type-arg funcname "Not a callable <gibaseinfo>: ~S"
                  (list info) (list info))))))

(define (callable-info-can-throw-gerror? info)
  "Returns #t if the given <gibaseinfo> is a callable that can
throw a GError"
  (assert-gicallableinfo "callable-info-can-throw-gerror?" info)
  (%callable-info-can-throw-gerror info))

(define (callable-info-get-n-args info)
  "Returns an integer number of arguments for this callable
<gibaseinfo>."
  (assert-gicallableinfo "callable-info-get-n-args" info)
  (%callable-info-get-n-args info))

(define (callable-info-get-arg info n)
  "Returns the <gibaseinfo> of the nth argument for this callable
<gibaseinfo>."
  (assert-gicallableinfo "callable-info-get-arg" info)
  (assert-index-integer "callable-info-get-arg" n (callable-info-get-n-args info))
  (%callable-info-get-arg info n))

(define (callable-info-get-caller-owns info)
  "Given a callable <gibaseinfo>, this returns a symbol from the
transfer-enum set that indicates whether the caller owns the return
type of this callable."
  (assert-gicallableinfo "callable-info-get-caller-owns" info)
  (let ((ret (%callable-info-get-caller-owns info)))
    (list-ref transfer-enum ret)))

(define (callable-info-get-instance-ownership-transfer info)
  "Given a callable <gibaseinfo>, this returns a symbol from the
transfer-enum set that indicates the ownership transfer of the
instance argument."
  (assert-gicallableinfo "callable-info-get-instance-ownership-transfer" info)
  (let ((ret (%callable-info-get-instance-ownership-transfer info)))
    (list-ref transfer-enum ret)))

(define (callable-info-get-return-attribute info name)
  "Given a callable <gibaseinfo> and a string naming a
return attribute for this callable, this returns the string
value of that return attribute, if it exists.  Otherwise #f."
  (assert-gicallableinfo "callable-info-get-return-attribute" info)
  (assert-name-string "callable-info-get-return-attribute" name)
  (%callable-info-get-return-attribute info name))

(define (callable-info-get-return-type info)
  "Given a callable <gibaseinfo>, this returns the type <gibaseinfo>
describing the return value."
  (assert-gicallableinfo "callable-info-get-return-type" info)
  (%callable-info-get-return-type info))

(define (callable-info-is-method? info)
  "Given a callable <gibaseinfo>, this returns #t if it is a method. In
other words, it returns #t if the arguments to this callable must also
include the 'self' or 'this' argument."
  (assert-gicallableinfo "callable-info-is-method?" info)
  (%callable-info-is-method info))

(define (callable-info-get-return-attributes info)
  "Given a callable <gibaseinfo>, this returns an association list of
return attributes associated with this callable.  The alist can be
empty."
  (assert-gicallableinfo "callable-info-get-return-attributes" info)
  (%callable-info-get-return-attributes info))

(define (callable-info-may-return-null? info)
  "Given a callable <gibaseinfo>, this returns #t if the return value
of the callable can be a null pointer."
  (assert-gicallableinfo "callable-info-may-return-null?" info)
  (%callable-info-may-return-null info))

(define (callable-info-skip-return? info)
  "Given a callable <gibaseinfo>, this returns #t if the return value
of the callable is probably of no interest to a language binding."
  (assert-gicallableinfo "callable-info-skip-return?" info)
  (%callable-info-skip-return info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIFunctionInfo

(define (is-function-info? info)
  "Returns #t if the given <gibaseinfo> is a function, method,
or constructor."
  (assert-gibaseinfo "is-callable-info?" info)
  (%is-function-info info))

(define-syntax assert-gifunctioninfo
  (syntax-rules ()
    ((assert-gifunctioninfo funcname info)
     (unless (is-function-info? info)
       (scm-error 'wrong-type-arg funcname "Not a function <gibaseinfo>: ~S"
                  (list info) (list info))))))

(define (function-info-get-flags info)
  "Returns a list of symbols from the function-info-enum set associated
with the function <gibaseinfo>, e.g. if it is a method, a getter, etc"
  (assert-gifunctioninfo "function-info-get-flags" info)
  (let ((flags (%function-info-get-flags info)))
    (append (if (logtest flags %FUNCTION_IS_METHOD) '(method) '())
            (if (logtest flags %FUNCTION_IS_CONSTRUCTOR) '(constructor) '())
            (if (logtest flags %FUNCTION_IS_GETTER) '(getter) '())
            (if (logtest flags %FUNCTION_IS_SETTER) '(setter) '())
            (if (logtest flags %FUNCTION_WRAPS_VFUNC) '(wraps-vfunc) '())
            (if (logtest flags %FUNCTION_THROWS) '(throws) '()))))

(define (function-info-get-property info)
  "For a given function <gibaseinfo> that is a getter or setter,
this returns the property <gibaseinfo> that this function is
a getter or setter of."
  (assert-gifunctioninfo "function-info-get-property" info)
  (unless (member (function-info-get-flags info) '(getter setter))
    (scm-error 'wrong-type-arg "function-info-get-property" "Not a getter or setter function <gibaseinfo>: ~S"
               (list info) (list info)))
  (%function-info-get-property info))

(define (function-info-get-symbol info)
  "For a given function <gibaseinfo>, this returns the string name of
the exported function, which is a 'symbol' in GObject-Introspection
parlance.  This is not to be confused with a Scheme symbol."
  (assert-gifunctioninfo "function-info-get-symbol" info)
  (%function-info-get-symbol info))

(define (function-info-get-vfunc info)
  "For a given function <gibaseinfo> that is an override of a virtual
function, this returns the <gibaseinfo> of the virtual function that
this function overrides."
  (assert-gifunctioninfo "function-info-get-vfunc" info)
  (unless (member (function-info-get-flags info) '(wraps-vfunc))
    (scm-error 'wrong-type-arg "function-info-get-vfunc" "Not a function <gibaseinfo> that wraps virtual function: ~S"
               (list info) (list info)))
  (%function-info-get-vfunc info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GICallbackInfo

(define (is-callback-info? info)
  "Returns #t if the given <gibaseinfo> is a callback."
  (assert-gibaseinfo "is-callback-info?" info)
  (eq? 'callback (base-info-get-type info)))

(define-syntax assert-gicallbackinfo
  (syntax-rules ()
    ((assert-gicallbackinfo funcname info)
     (unless (is-callback-info? info)
       (scm-error 'wrong-type-arg funcname "Not a callback <gibaseinfo>: ~S"
                  (list info) (list info))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GISignalInfo

(define (is-signal-info? info)
  "Returns #t if the given <gibaseinfo> is a function, method,
or constructor."
  (assert-gibaseinfo "is-callable-info?" info)
  (%is-signal-info info))

(define-syntax assert-gisignalinfo
  (syntax-rules ()
    ((assert-gisignalinfo funcname info)
     (unless (is-signal-info? info)
       (scm-error 'wrong-type-arg funcname "Not a signal <gibaseinfo>: ~S"
                  (list info) (list info))))))

(define (signal-info-get-flags info)
  "Returns a list of symbols from the signal-info-enum set associated
with the signal <gibaseinfo>, e.g. if it is 'run-first, 'run-last, etc"
  (assert-gisignalinfo "signal-info-get-flags" info)
  (let ((flags (%signal-info-get-flags info)))
    (append (if (logtest flags %SIGNAL_RUN_FIRST) '(run-first) '())
            (if (logtest flags %SIGNAL_RUN_LAST) '(run-last) '())
            (if (logtest flags %SIGNAL_RUN_CLEANUP) '(run-cleanup) '())
            (if (logtest flags %SIGNAL_NO_RECURSE) '(no-recurse) '())
            (if (logtest flags %SIGNAL_DETAILED) '(detailed) '())
            (if (logtest flags %SIGNAL_ACTION) '(action) '())
            (if (logtest flags %SIGNAL_NO_HOOKS) '(no-hooks) '())
            (if (logtest flags %SIGNAL_MUST_COLLECT) '(must-collect) '())
            (if (logtest flags %SIGNAL_DEPRECATED) '(deprecated) '())
            (if (and
                 (defined? '%SIGNAL_ACCUMULATOR_FIRST_RUN)
                 (logtest flags %SIGNAL_ACCUMULATOR_FIRST_RUN)) '(accumulator-first-run) '()))))

(define (signal-info-get-class-closure info)
  "Obtain the class closure for this signal if one is set. The class
closure is a virtual function on the type that the signal belongs
to. If the signal lacks a closure #f will be returned."
  (assert-gisignalinfo "signal-info-get-class-closure" info)
  (%signal-info-get-class-closure info))

(define (signal-info-true-stops-emit? info)
  "Obtain if the returning true in the signal handler will stop the
emission of the signal."
  (assert-gisignalinfo "signal-info-true-stops-emit?" info)
  (%signal-info-true-stops-emit info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIVFuncInfo

(define (is-vfunc-info? info)
  "Returns #t if the given <gibaseinfo> is a virtual function."
  (assert-gibaseinfo "is-vfunc-info?" info)
  (%is-vfunc-info info))

(define-syntax assert-givfuncinfo
  (syntax-rules ()
    ((assert-givfuncinfo funcname info)
     (unless (is-vfunc-info? info)
       (scm-error 'wrong-type-arg funcname "Not a vfunc <gibaseinfo>: ~S"
                  (list info) (list info))))))

(define (vfunc-info-get-flags info)
  "Returns a list of symbols associated
with the vfunc <gibaseinfo>, e.g. if it is 'must-chain-up, 'must-override, etc"
  (assert-givfuncinfo "vfunc-info-get-flags" info)
  (let ((flags (%vfunc-info-get-flags info)))
    (append (if (logtest flags %VFUNC_MUST_CHAIN_UP) '(must-chain-up) '())
            (if (logtest flags %VFUNC_MUST_OVERRIDE) '(must-override) '())
            (if (logtest flags %VFUNC_MUST_NOT_OVERRIDE) '(must-not-override) '())
            (if (logtest flags %VFUNC_THROWS) '(throws) '()))))

(define (vfunc-info-get-offset info)
  "Given a vfunc <gibaseinfo>, obtains the offset of the function
pointer in the class struct.  Returns #f if the offset is unknown."
  (assert-givfuncinfo "vfunc-info-get-offset" info)
  (let ((offset (%vfunc-info-get-offset info)))
    (if (equal? offset #xffff)
        #f
        offset)))

(define (vfunc-info-get-signal info)
  "Given a vfunc <gibaseinfo>, obtain the signal <gibaseinfo> for the
virtual function if one is set. The signal comes from the object or
interface to which this virtual function belongs."
  (assert-givfuncinfo "vfunc-info-get-signal" info)
  (%vfunc-info-get-signal info))

(define (vfunc-info-get-invoker info)
  "Given a vfunc <gibaseinfo>, obtain the function <gibaseinfo> for
the associated invoker method (an entry point to the vfunc), if one
exists."
  (assert-givfuncinfo "vfunc-info-get-invoker" info)
  (%vfunc-info-get-invoker info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIRegisteredType

(define (is-registered-type-info? info)
  "Returns #t if the given <gibaseinfo> is a registered type: a
struct with a GType."
  (assert-gibaseinfo "is-registered-type-info?" info)
  (%is-registered-type-info info))

(define-syntax assert-giregisteredtypeinfo
  (syntax-rules ()
    ((assert-giregisteredtypeinfo funcname info)
     (unless (is-registered-type-info? info)
       (scm-error 'wrong-type-arg funcname "Not a registered type <gibaseinfo>: ~S"
                  (list info) (list info))))))

(define (registered-type-info-get-type-name info)
  "Given a registered type <gibaseinfo>, this returns the
string name of that type."
  (assert-giregisteredtypeinfo "registered-type-info-get-name" info)
  (%registered-type-info-get-type-name info))

(define (registered-type-info-get-type-init info)
  "Given a registered type <gibaseinfo>, this returns the string name of
the type init function. The type init function is the function which
will register the GType within the GObject type system."
  (assert-giregisteredtypeinfo "registered-type-info-get-type-init" info)
  (%registered-type-info-get-type-init info))

(define (registered-type-info-get-g-type info)
  "Given a registered type <gibaseinfo>, this returns an integer GType.

IMPORTANT: Do not rely on the actual value of a gtype.  It can change if
the environment at parse type differs from the envirnoment at run time.

The only useful thing to do with this function is to check if a type
info *has* a gtype, or possibly to pass it to
get-object-gtype-interfaces."
  (assert-giregisteredtypeinfo "registered-type-info-get-g-type?" info)
  (%registered-type-info-get-g-type info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIEnumInfo

(define (is-enum-info? info)
  "Returns #t if the given <gibaseinfo> is an enumeration: usually with multiple
associated values."
  (assert-gibaseinfo "is-enum-info?" info)
  (%is-enum-info info))

(define-syntax assert-gienuminfo
  (syntax-rules ()
    ((assert-gienuminfo funcname info)
     (unless (is-enum-info? info)
       (scm-error 'wrong-type-arg funcname "Not an enum <gibaseinfo>: ~S"
                  (list info) (list info))))))

(define (enum-info-get-n-values info)
  "Given an enum <gibaseinfo>, this returns an integer of the number of
values that this enumerated type has."
  (assert-gienuminfo "enum-info-get-n-values" info)
  (%enum-info-get-n-values info))

(define (enum-info-get-value info n)
  "Given an enum <gibaseinfo> and an integer index, this returns
the value <gibaseinfo> for the nth value of the enumeration."
  (assert-gienuminfo "enum-info-get-value" info)
  (assert-index-integer "enum-info-get-value" n (enum-info-get-n-values info))
  (%enum-info-get-value info n))

(define (enum-info-get-n-methods info)
  "Given an enum <gibaseinfo>, this returns an integer of the number of
methods that this enumerated type has."
  (assert-gienuminfo "enum-info-get-n-methods" info)
  (%enum-info-get-n-methods info))

(define (enum-info-get-method info n)
  "Given an enum <gibaseinfo> and an integer index, this returns
the method <gibaseinfo> for the nth method of the enumeration."
  (assert-gienuminfo "enum-info-get-method" info)
  (assert-index-integer "enum-info-get-method" n (enum-info-get-n-methods info))
  (%enum-info-get-method info n))

(define (enum-info-get-error-domain info)
  "Given an enum <gibaseinfo>, this returns
the string representation of the error domain for this enumeration."
  (assert-gienuminfo "enum-info-get-error-domain" info)
  (%enum-info-get-error-domain info))

(define (enum-info-get-storage-type info)
  "Given an enum <gibaseinfo>, this returns a type-tag symbol
of the type used for the enum by the C ABI."
  (assert-gienuminfo "enum-info-get-storage-type" info)
  (let ((tag (%enum-info-get-storage-type info)))
    (%type-tag-is-basic (list-index
                         (lambda (x) (eqv? x tag))
                         type-tag-enum))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIValueInfo

(define (is-value-info? info)
  "Returns #t if the given <gibaseinfo> is a single value of an enumeration."
  (assert-gibaseinfo "is-value-info?" info)
  (%is-value-info info))

(define-syntax assert-givalueinfo
  (syntax-rules ()
    ((assert-givalueinfo funcname info)
     (unless (is-value-info? info)
       (scm-error 'wrong-type-arg funcname "Not a value <gibaseinfo>: ~S"
                  (list info) (list info))))))

(define (value-info-get-value info)
  "Given a value <gibaseinfo>, this returns its integer value."
  (assert-givalueinfo "value-info-get-value" info)
  (%value-info-get-value info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIStructInfo

(define (is-struct-info? info)
  "Returns #t if the given <gibaseinfo> is an structure representing
a C structure."
  (assert-gibaseinfo "is-struct-info?" info)
  (%is-struct-info info))

(define-syntax assert-gistructinfo
  (syntax-rules ()
    ((assert-gistructinfo funcname info)
     (unless (is-struct-info? info)
       (scm-error 'wrong-type-arg funcname "Not a struct <gibaseinfo>: ~S"
                  (list info) (list info))))))

(define (struct-info-find-field info name)
  "Searches for a field with the specified name in a struct <gibaseinfo>.
If found, returns a field <gibaseinfo>.  If not found, returns #f."
  (assert-gistructinfo "struct-info-find-field" info)
  (assert-name-string "struct-info-find-field" name)
  (%struct-info-find-field info name))

(define (struct-info-get-alignment info)
  "Given a struct <gibaseinfo>, returns as an integer the required
memory alignment of the structure."
  (assert-gistructinfo "struct-info-get-alignment" info)
  (%struct-info-get-alignment info))

(define (struct-info-get-size info)
  "Given a struct <gibaseinfo>, returns the number of bytes for the
total size of the structure, if known.  Otherwise returns zero."
  (assert-gistructinfo "struct-info-get-size" info)
  (%struct-info-get-size info))

(define (struct-info-is-gtype-struct? info)
  "For a given struct <gibaseinfo>, returns #t if info has a GType."
  (assert-gistructinfo "struct-info-is-gtype-struct?" info)
  (%struct-info-is-gtype-struct info))

(define (struct-info-is-foreign? info)
  "For a given struct <gibaseinfo>, returns #t if info represents
a struct that cannot be unpacked by a language binding."
  (assert-gistructinfo "struct-info-is-foreign?" info)
  (%struct-info-is-foreign info))

(define (struct-info-get-n-fields info)
  "Given an struct <gibaseinfo>, this returns an integer of the number of
fields that this structerated type has."
  (assert-gistructinfo "struct-info-get-n-fields" info)
  (%struct-info-get-n-fields info))

(define (struct-info-get-field info n)
  "Given an struct <gibaseinfo> and an integer index, this returns
the field <gibaseinfo> for the nth field of the structure."
  (assert-gistructinfo "struct-info-get-field" info)
  (assert-index-integer "struct-info-get-field" n (struct-info-get-n-fields info))
  (%struct-info-get-field info n))

(define (struct-info-get-n-methods info)
  "Given an struct <gibaseinfo>, this returns an integer of the number of
methods that this structerated type has."
  (assert-gistructinfo "struct-info-get-n-methods" info)
  (%struct-info-get-n-methods info))

(define (struct-info-get-method info n)
  "Given an struct <gibaseinfo> and an integer index, this returns
the method <gibaseinfo> for the nth method of the structure."
  (assert-gistructinfo "struct-info-get-method" info)
  (assert-index-integer "struct-info-get-method" n (struct-info-get-n-methods info))
  (%struct-info-get-method info n))

(define (struct-info-find-method info name)
  "Searches for a method with the specified name in a struct <gibaseinfo>.
If found, returns a field function <gibaseinfo>.  If not found, returns #f."
  (assert-gistructinfo "struct-info-find-method" info)
  (assert-name-string "struct-info-find-method" name)
  (%struct-info-find-method info name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIUnionInfo

(define (is-union-info? info)
  "Returns #t if the given <gibaseinfo> represents a C union."
  (assert-gibaseinfo "is-union-info?" info)
  (%is-union-info info))

(define-syntax assert-giunioninfo
  (syntax-rules ()
    ((assert-giunioninfo funcname info)
     (unless (is-union-info? info)
       (scm-error 'wrong-type-arg funcname "Not a union <gibaseinfo>: ~S"
                  (list info) (list info))))))

(define (union-info-get-n-fields info)
  "Given a union <gibaseinfo>, it returns the integer count of
the number of fields in the union."
  (assert-giunioninfo "union-info-get-n-fields" info)
  (%union-info-get-n-fields info))

(define (union-info-get-field info n)
  "Given a union <gibaseinfo>, returns the field <gibaseinfo> of
the nth field in the union."
  (assert-giunioninfo "union-info-get-field"info)
  (assert-index-integer "union-info-get-field" n (union-info-get-n-fields info))
  (%union-info-get-field info n))

(define (union-info-get-n-methods info)
  "Given a union <gibaseinfo>, it returns the integer count of
the number of methods assocaited with this union."
  (assert-giunioninfo "union-info-get-n-methods" info)
  (%union-info-get-n-methods info))

(define (union-info-get-method info n)
  "Given a union <gibaseinfo>, returns the method <gibaseinfo> of
the nth method for this union."
  (assert-giunioninfo "union-info-get-method" info)
  (assert-index-integer "union-info-get-method" n (union-info-get-n-methods info))
  (%union-info-get-method info n))

(define (union-info-is-discriminated? info)
  "Given a union <gibaseinfo>, returns #t if this union contains a
discriminator field."
  (assert-giunioninfo "union-info-is-discriminated?" info)
  (%union-info-is-discriminated info))

(define (union-info-get-discriminator-offset info)
  "Given a union <gibaseinfo> that has a discriminator field,
returns the offset in bytes from the beginning of the union to the
discriminator field."
  (assert-giunioninfo "union-info-get-discriminator-offset" info)
  (unless (union-info-is-discriminated? info)
    (scm-error 'wrong-type-arg "union-info-get-discriminator-offset" "Not a discriminated union <gibaseinfo>: ~S"
               (list info) (list info)))
  (%union-info-get-discriminator-offset info))

(define (union-info-get-discriminator-type info)
  "Given a union <gibaseinfo> that has a discriminator field,
returns a type-info <gibaseinfo> that gives type information
about the discriminator field."
  (assert-giunioninfo "union-info-get-discriminator-type" info)
  (unless (union-info-is-discriminated? info)
    (scm-error 'wrong-type-arg "union-info-discriminator-type" "Not a discriminated union <gibaseinfo>: ~S"
               (list info) (list info)))
  (%union-info-get-discriminator-type info))

(define (union-info-get-discriminator info n)
  "Given a union <gibaseinfo> that has a discriminator field,
obtain discriminator value assigned for n-th union field in the form
of a constant <gibaseinfo>, i.e. n-th union field is the active one if
discriminator contains the constant described by the returned
<gibaseinfo>."
  (assert-giunioninfo "union-info-get-discriminator" info)
  (unless (union-info-is-discriminated? info)
    (scm-error 'wrong-type-arg "union-info-get-discriminator" "Not a discriminated union <gibaseinfo>: ~S"
               (list info) (list info)))
  (assert-index-integer "union-info-get-discriminator" n (union-info-get-n-fields info))
  (%union-info-get-discriminator info n))

(define (union-info-find-method info name)
  "Given a union <gibaseinfo> and the string name of a method,
returns the method <gibaseinfo> if found.  Otherwise #f."
  (assert-giunioninfo "union-info-find-method" info)
  (assert-name-string "union-info-find-method" name)
  (%union-info-find-method info name))

(define (union-info-get-size info)
  "Given a union <gibaseinfo>, return the total size of the
union in bytes."
  (assert-giunioninfo "union-info-get-size" info)
  (%union-info-get-size info))

(define (union-info-get-alignment info)
  "Given a union <gibaseinfo>, return the required memory alignment of the
union in bytes."
  (assert-giunioninfo "union-info-get-alignment" info)
  (%union-info-get-alignment info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIObjectInfo

(define (is-object-info? info)
  "Returns #t if the given <gibaseinfo> represents a GObject
classed type."
  (assert-gibaseinfo "is-object-info?" info)
  (%is-object-info info))

(define-syntax assert-giobjectinfo
  (syntax-rules ()
    ((assert-giobjectinfo funcname info)
     (unless (is-object-info? info)
       (scm-error 'wrong-type-arg funcname "Not a object <gibaseinfo>: ~S"
                  (list info) (list info))))))

(define (object-info-get-abstract? info)
  "Returns #t if the given object <gibaseinfo> is an abstract type,
i.e. cannot be instantiated."
  (assert-giobjectinfo "object-info-get-abstract?" info)
  (%object-info-get-abstract info))

(define (object-info-get-fundamental? info)
  "Returns #t if the given object <gibaseinfo> is a fundamental
type which is not G_TYPE_OBJECT."
  (assert-giobjectinfo "object-info-get-fundamental?" info)
  (%object-info-get-fundamental info))

(define (object-info-get-final? info)
  "Returns #t if the given object <gibaseinfo> is a final
type which cannot be derived."
  (assert-giobjectinfo "object-info-get-final?" info)
  (if (defined? '%object-info-get-final)
      (%object-info-get-final info)
      ;; else
      (error "this version of girepository does not support g_object_info_get_final")))

(define (object-info-get-parent info)
  "Given an object <gibaseinfo>, returns the object <gibaseinfo>
of the parent type."
  (assert-giobjectinfo "object-info-get-parent" info)
  (%object-info-get-parent info))

(define (object-info-get-type-name info)
  "Given an object <gibaseinfo>, returns as a string the name of the
object class or type."
  (assert-giobjectinfo "object-info-get-type-name" info)
  (%object-info-get-type-name info))

(define (object-info-get-type-init info)
  "Given an object <gibaseinfo>, returns, as a string, the name of the
function which, when called, will return the GType function for which
this object type is registered."
  (assert-giobjectinfo "object-info-get-type-init" info)
  (%object-info-get-type-init info))

(define (object-info-get-n-constants info)
  "Given an object <gibaseinfo>, returns, as an integer, the number
of constants that this object type has."
  (assert-giobjectinfo "object-info-get-n-constants" info)
  (%object-info-get-n-constants info))

(define (object-info-get-constant info n)
  "Given an object <gibaseinfo>, returns, as a constant <gibaseinfo>,
the n-th constant associated with this object."
  (assert-giobjectinfo "object-info-get-constant" info)
  (assert-index-integer "object-info-get-constant" n (object-info-get-n-constants info))
  (%object-info-get-constant info n))

(define (object-info-get-n-fields info)
  "Given an object <gibaseinfo>, returns, as an integer, the number
of fields that this object type has."
  (assert-giobjectinfo "object-info-get-n-fields" info)
  (%object-info-get-n-fields info))

(define (object-info-get-field info n)
  "Given an object <gibaseinfo>, returns, as a field-info <gibaseinfo>,
the n-th field associated with this object."
  (assert-giobjectinfo "object-info-get-field" info)
  (assert-index-integer "object-info-get-field" n (object-info-get-n-fields info))
  (%object-info-get-field info n))

(define (object-info-get-n-interfaces info)
  "Given an object <gibaseinfo>, returns, as an integer, the number
of interfaces that this object type has."
  (assert-giobjectinfo "object-info-get-n-interfaces" info)
  (%object-info-get-n-interfaces info))

(define (object-info-get-interface info n)
  "Given an object <gibaseinfo>, returns, as an interface-info <gibaseinfo>,
the n-th interface associated with this object."
  (assert-giobjectinfo "object-info-get-interface" info)
  (assert-index-integer "object-info-get-interface" n (object-info-get-n-interfaces info))
  (%object-info-get-interface info n))

(define (object-info-get-n-methods info)
  "Given an object <gibaseinfo>, returns, as an integer, the number
of methods that this object type has."
  (assert-giobjectinfo "object-info-get-n-methods" info)
  (%object-info-get-n-methods info))

(define (object-info-get-method info n)
  "Given an object <gibaseinfo>, returns, as a function-info <gibaseinfo>,
the n-th method associated with this object."
  (assert-giobjectinfo "object-info-get-method" info)
  (assert-index-integer "object-info-get-method" n (object-info-get-n-methods info))
  (%object-info-get-method info n))

(define (object-info-find-method info name)
  "Given an object-info <gibaseinfo> and the string name of a method,
returns, as a function-info <gibaseinfo> for that method if found.
Otherwise #f."
  (assert-giobjectinfo "object-info-find-method" info)
  (assert-name-string "object-info-find-method" name)
  (%object-info-find-method info name))

(define (object-info-find-method-using-interfaces info name)
  "Given an object-info <gibaseinfo> and the string name of a method,
this searches both the object and any interfaces it implements for a
method by that name.  If found, it returns two values: the
function-info <gibaseinfo> for that method and the object-info
<gibaseinfo> which is either this object or is an interface this
object implements.  Otherwise it returns two values, which are both
#f."
  (assert-giobjectinfo "object-info-find-method-using-interfaces" info)
  (assert-name-string "object-info-find-method-using-interfaces" name)
  (let ((method/interface (%object-info-find-method-using-interfaces info name)))
    (values (car method/interface)
            (cdr method/interface))))

(define (object-info-get-n-properties info)
  "Given an object <gibaseinfo>, returns, as an integer, the number
of properties that this object type has."
  (assert-giobjectinfo "object-info-get-n-properties" info)
  (%object-info-get-n-properties info))

(define (object-info-get-property info n)
  "Given an object <gibaseinfo>, returns, as a property-info <gibaseinfo>,
the n-th property associated with this object."
  (assert-giobjectinfo "object-info-get-property" info)
  (assert-index-integer "object-info-get-property" n (object-info-get-n-properties info))
  (%object-info-get-property info n))

(define (object-info-get-n-signals info)
  "Given an object <gibaseinfo>, returns, as an integer, the number
of signals that this object type has."
  (assert-giobjectinfo "object-info-get-n-signals" info)
  (%object-info-get-n-signals info))

(define (object-info-get-signal info n)
  "Given an object <gibaseinfo>, returns, as a signal-info <gibaseinfo>,
the n-th signal associated with this object."
  (assert-giobjectinfo "object-info-get-signal" info)
  (assert-index-integer "object-info-get-n-signals" n (object-info-get-n-signals info))
  (%object-info-get-signal info n))

(define (object-info-find-signal info name)
  "Given an object-info <gibaseinfo> and the string name of a signal,
returns, a signal-info <gibaseinfo> for that signal if found.
Otherwise #f."
  (assert-giobjectinfo "object-info-find-signal" info)
  (assert-name-string "object-info-find-signal" name)
  (%object-info-find-signal info name))

(define (object-info-get-n-vfuncs info)
  "Given an object <gibaseinfo>, returns, as an integer, the number of
vfuncs that this object type has."
  (assert-giobjectinfo "object-info-get-n-vfuncs" info)
  (%object-info-get-n-vfuncs info))

(define (object-info-get-vfunc info n)
  "Given an object <gibaseinfo>, returns, as a vfunc-info <gibaseinfo>,
the n-th vfunc associated with this object."
  (assert-giobjectinfo "object-info-get-vfunc" info)
  (assert-index-integer "object-info-get-vfunc" n (object-info-get-n-vfuncs info))
  (%object-info-get-vfunc info n))

(define (object-info-find-vfunc info name)
  "Given an object-info <gibaseinfo> and the string name of a vfunc,
returns, a vfunc-info <gibaseinfo> for that vfunc if found.  Otherwise
#f."
  (assert-giobjectinfo "object-info-find-vfunc" info)
  (assert-name-string "object-info-find-vfunc" name)
  (%object-info-find-vfunc info name))

(define (object-info-find-vfunc-using-interfaces info name)
  "Given an object-info <gibaseinfo> and the string name of a vfunc,
this searches both the object and any interfaces it implements for a
vfunc by that name.  If found, it returns two values: the
vfunc-info <gibaseinfo> for that method and the object-info
<gibaseinfo> which is either this object or is an interface this
object implements.  Otherwise it returns two values, which are both
#f."
  (assert-giobjectinfo "object-info-find-vfunc-using-interfaces" info)
  (assert-name-string "object-info-find-vfunc-using-interfaces" name)
  (let ((vfunc/interface (%object-info-find-vfunc-using-interfaces info name)))
    (values (car vfunc/interface)
            (cdr vfunc/interface))))

(define (object-info-get-class-struct info)
  "Given an object <gibaseinfo>, returns, as a struct-info <gibaseinfo>,
the class structure of this object."
  (assert-giobjectinfo "object-info-get-class-struct" info)
  (%object-info-get-class-struct info))

(define (object-info-get-ref-function info)
  "Given an object <gibaseinfo>, returns, as a string, the name of a
function that can be called to increase the reference count of this
object type, if such a function exists.  Otherwise #f."
  (assert-giobjectinfo "object-info-get-ref-function" info)
  (%object-info-get-ref-function info))

(define (object-info-get-unref-function info)
  "Given an object <gibaseinfo>, returns, as a string, the name of a
function that can be called to decrease the reference count of this
object type, if such a function exists.  Otherwise #f."
  (assert-giobjectinfo "object-info-get-unref-function" info)
  (%object-info-get-unref-function info))

(define (object-info-get-set-value-function info)
  "Given an object <gibaseinfo>, returns, as a string, the name of a
function that should be called to set a GValue given an
object instance pointer of this object type.  Otherwise #f."
  (assert-giobjectinfo "object-info-get-set-value-function" info)
  (%object-info-get-set-value-function info))

(define (object-info-get-get-value-function info)
  "Given an object <gibaseinfo>, returns, as a string, the name of a
function that should be called to convert an object instance pointer
of this object type to a GValue.  Otherwise #f."
  (assert-giobjectinfo "object-info-get-get-value-function" info)
  (%object-info-get-set-value-function info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIInterfaceInfo

(define (is-interface-info? info)
  "Returns #t if the given <gibaseinfo> represents a GInterface"
  (assert-gibaseinfo "is-interface-info?" info)
  (%is-interface-info info))

(define-syntax assert-giinterfaceinfo
  (syntax-rules ()
    ((assert-giinterfaceinfo funcname info)
     (unless (is-interface-info? info)
       (scm-error 'wrong-type-arg funcname "Not a interface <gibaseinfo>: ~S"
                  (list info) (list info))))))

(define (interface-info-get-n-prerequisites info)
  "Given an interface-info <gibaseinfo>, returns, as an integer, the
number of prerequisite interfaces that this interface has."
  (assert-giinterfaceinfo "interface-info-get-n-prerequisites" info)
  (%interface-info-get-n-prerequisites info))

(define (interface-info-get-prerequisite info n)
  "Given an interface-info <gibaseinfo>, returns, as an interface-info <gibaseinfo>,
the n-th prerequisite interface associated with this interface."
  (assert-giinterfaceinfo "interface-info-get-prerequisite" info)
  (assert-index-integer "interface-info-get-prerequisite" n (interface-info-get-n-prerequisites info))
  (%interface-info-get-prerequisite info n))

(define (interface-info-get-n-properties info)
  "Given an interface-info <gibaseinfo>, returns, as an integer, the
number of properties that this interface has."
  (assert-giinterfaceinfo "interface-info-get-n-properties" info)
  (%interface-info-get-n-properties info))

(define (interface-info-get-property info n)
  "Given an interface-info <gibaseinfo>, returns, as a property <gibaseinfo>,
the n-th property associated with this interface."
  (assert-giinterfaceinfo "interface-info-get-property" info)
  (assert-index-integer "interface-info-get-property" n (interface-info-get-n-properties info))
  (%interface-info-get-property info n))

(define (interface-info-get-n-methods info)
  "Given an interface-info <gibaseinfo>, returns, as an integer, the
number of methods that this interface has."
  (assert-giinterfaceinfo "interface-info-get-n-methods" info)
  (%interface-info-get-n-methods info))

(define (interface-info-get-method info n)
  "Given an interface-info <gibaseinfo>, returns, as a method <gibaseinfo>,
the n-th method associated with this interface."
  (assert-giinterfaceinfo "interface-info-get-method" info)
  (assert-index-integer "interface-info-get-method" n (interface-info-get-n-methods info))
  (%interface-info-get-method info n))

(define (interface-info-find-method info name)
  "Given an interface-info <gibaseinfo> and the string name
of a method, returns, as a method <gibaseinfo>,
that method if found. Otherwise #f."
  (assert-giinterfaceinfo "interface-info-find-method" info)
  (assert-name-string "interface-info-find-method" name)
  (%interface-info-find-method info name))

(define (interface-info-get-n-signals info)
  "Given an interface-info <gibaseinfo>, returns, as an integer, the
number of signals that this interface has."
  (assert-giinterfaceinfo "interface-info-get-n-signals" info)
  (%interface-info-get-n-signals info))

(define (interface-info-get-signal info n)
  "Given an interface-info <gibaseinfo>, returns, as a signal <gibaseinfo>,
the n-th signal associated with this interface."
  (assert-giinterfaceinfo "interface-info-get-signal" info)
  (assert-index-integer "interface-info-get-signal" n (interface-info-get-n-signals info))
  (%interface-info-get-signal info n))

(define (interface-info-find-signal info name)
  "Given an interface-info <gibaseinfo> and the string name
of a signal, returns, as a signal <gibaseinfo>,
that signal if found. Otherwise #f."
  (assert-giinterfaceinfo "interface-info-find-signal" info)
  (assert-name-string "interface-info-find-signal" name)
  (%interface-info-find-signal info name))

(define (interface-info-get-n-vfuncs info)
  "Given an interface-info <gibaseinfo>, returns, as an integer, the
number of vfuncs that this interface has."
  (assert-giinterfaceinfo "interface-info-get-n-vfuncs" info)
  (%interface-info-get-n-vfuncs info))

(define (interface-info-get-vfunc info n)
  "Given an interface-info <gibaseinfo>, returns, as a vfunc <gibaseinfo>,
the n-th vfunc associated with this interface."
  (assert-giinterfaceinfo "interface-info-get-vfunc" info)
  (assert-index-integer "interface-info-get-vfunc" n (interface-info-get-n-vfuncs info))
  (%interface-info-get-vfunc info n))

(define (interface-info-find-vfunc info name)
  "Given an interface-info <gibaseinfo> and the string name
of a vfunc, returns, as a vfunc <gibaseinfo>,
that vfunc if found. Otherwise #f."
  (assert-giinterfaceinfo "interface-info-find-vfunc" info)
  (assert-name-string "interface-info-find-vfunc" name)
  (%interface-info-find-vfunc info name))

(define (interface-info-get-n-constants info)
  "Given an interface-info <gibaseinfo>, returns, as an integer, the
number of constants that this interface has."
  (assert-giinterfaceinfo "interface-info-get-n-constants" info)
  (%interface-info-get-n-constants info))

(define (interface-info-get-constant info n)
  "Given an interface-info <gibaseinfo>, returns, as a constant <gibaseinfo>,
the n-th constant associated with this interface."
  (assert-giinterfaceinfo "interface-info-get-constant" info)
  (assert-index-integer "interface-info-get-constant" n (interface-info-get-n-constants info))
  (%interface-info-get-constant info n))

(define (interface-info-get-iface-struct info)
  "Given an interface-info <gibaseinfo>, returns, as a struct <gibaseinfo>,
the layout C structure of this interface."
  (assert-giinterfaceinfo "interface-info-get-iface-struct" info)
  (%interface-info-get-iface-struct info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIArgInfo

(define (is-arg-info? info)
  "Returns #t if the given <gibaseinfo> represents an argument of a callable."
  (assert-gibaseinfo "is-arg-info?" info)
  (%is-arg-info info))

(define-syntax assert-giarginfo
  (syntax-rules ()
    ((assert-giarginfo funcname info)
     (unless (is-arg-info? info)
       (scm-error 'wrong-type-arg funcname "Not an arg <gibaseinfo>: ~S"
                  (list info) (list info))))))

(define (arg-info-get-closure info)
  "Given an arg <gibaseinfo>, returns, as an integer,
the index of the user data argument.  This is only valid
for arguments which are callbacks."
  (assert-giarginfo "arg-info-get-closure" info)
  (let* ((typeinfo (false-if-exception (arg-info-get-type info)))
         (baseinfo (false-if-exception (type-info-get-interface typeinfo)))
         (basetype (false-if-exception (base-info-get-type baseinfo))))
    (unless (equal? basetype 'callback)
      (scm-error 'wrong-type-arg "arg-info-get-closure" "Not a closure arg <gibaseinfo>: ~S"
                 (list info) (list info))))
  (%arg-info-get-closure info))

(define (arg-info-get-destroy info)
  "Given an arg <gibaseinfo>, returns, as an integer,
the index of the GDestroyNotify argument.  This is only valid
for arguments which are callbacks."
  (assert-giarginfo "arg-info-get-destroy" info)
  (let* ((typeinfo (false-if-exception (arg-info-get-type info)))
         (baseinfo (false-if-exception (type-info-get-interface typeinfo)))
         (basetype (false-if-exception (base-info-get-type baseinfo))))
    (unless (equal? basetype 'callback)
      (scm-error 'wrong-type-arg "arg-info-get-destroy" "Not a closure arg <gibaseinfo>: ~S"
                 (list info) (list info))))
  (let ((idx (%arg-info-get-destroy info)))
    (if (< idx 0)
        #f
        idx)))

(define (arg-info-get-direction info)
  "Given an arg <gibaseinfo>, returns a symbol
indicating the direction of the argument: 'in, 'out,
or 'inout"
  (assert-giarginfo "arg-info-get-direction" info)
  (let ((dir (%arg-info-get-direction info)))
    (cond
     ((eqv? dir %DIRECTION_IN) 'in)
     ((eqv? dir %DIRECTION_OUT) 'out)
     ((eqv? dir %DIRECTION_INOUT) 'inout)
     (else
      (error "unknown dir")))))

(define (arg-info-get-ownership-transfer info)
  "Given an arg <gibaseinfo>, returns a symbol indicating the
transference of ownership of the argument: 'nothing, 'container, or
'everything."
  (assert-giarginfo "arg-info-get-ownership-transfer" info)
  (let ((dir (%arg-info-get-ownership-transfer info)))
    (cond
     ((eqv? dir %TRANSFER_NOTHING) 'nothing)
     ((eqv? dir %TRANSFER_CONTAINER) 'container)
     ((eqv? dir %TRANSFER_EVERYTHING) 'everything)
     (else
      (error "unknown transfer")))))

(define (arg-info-get-scope info)
  "Given an arg <gibaseinfo> which is a callback argument,
returns a symbol indicating the lifetime of the callback
argument and the resources used to invoke that callback:
'invalid, 'call, 'async, 'notified, 'forever"
  (assert-giarginfo "arg-info-get-scope" info)
  (let* ((typeinfo (false-if-exception (arg-info-get-type info)))
         (baseinfo (false-if-exception (type-info-get-interface typeinfo)))
         (basetype (false-if-exception (base-info-get-type baseinfo))))
    (unless (equal? basetype 'callback)
      (scm-error 'wrong-type-arg "arg-info-get-scope" "Not a closure arg <gibaseinfo>: ~S"
                 (list info) (list info))))
  (let ((dir (%arg-info-get-scope info)))
    (cond
     ((eqv? dir %SCOPE_TYPE_INVALID) 'invalid)
     ((eqv? dir %SCOPE_TYPE_CALL) 'call)
     ((eqv? dir %SCOPE_TYPE_ASYNC) 'async)
     ((eqv? dir %SCOPE_TYPE_NOTIFIED) 'notified)
     ((and
       (defined? '%SCOPE_TYPE_FOREVER)
       (eqv? dir %SCOPE_TYPE_FOREVER) 'forever))
     (else
      (error "unknown scope")))))

(define (arg-info-get-type info)
  "Given an arg <gibaseinfo>, this returns a type <gibaseinfo>,
that describes the type information for the argument."
  (assert-giarginfo "arg-info-get-type" info)
  (%arg-info-get-type info))

(define (arg-info-may-be-null? info)
  "Given an arg <gibaseinfo>, this returns #t if
NULL is a valid value for the argument.  This
implies that the argument is a pointer argument of some type."
  (assert-giarginfo "arg-info-may-be-null?" info)
  (%arg-info-may-be-null info))

(define (arg-info-is-caller-allocates? info)
  "Given an arg <gibaseinfo>, this returns #t if
this argument is a pointer to a struct or object that the
caller must allocate."
  (assert-giarginfo "arg-info-is-caller-allocates?" info)
  (%arg-info-is-caller-allocates info))

(define (arg-info-is-optional? info)
  "Given an arg <gibaseinfo>, this returns #t if this argument is a
pointer to a struct or object that can be ignored.  This means that
for 'out' arguments, a NULL pointer can be passed to ignore the
result."
  (assert-giarginfo "arg-info-is-optional?" info)
  (%arg-info-is-optional info))

(define (arg-info-is-return-value? info)
  "Given an arg <gibaseinfo>, this returns #t if this argument represents
a return value.  It returns #f if it is an argument."
  (assert-giarginfo "arg-info-is-return-value?" info)
  (%arg-info-is-return-value info))

(define (arg-info-is-skip? info)
  "Given an arg <gibaseinfo>, this returns #t if this argument
is not useful for a language binding."
  (assert-giarginfo "arg-info-is-skip?" info)
  (%arg-info-is-skip info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIConstantInfo

(define (is-constant-info? info)
  "Returns #t if the given <gibaseinfo> represents an argument of a callable."
  (assert-gibaseinfo "is-constant-info?" info)
  (%is-constant-info info))

(define-syntax assert-giconstantinfo
  (syntax-rules ()
    ((assert-giconstantinfo funcname info)
     (unless (is-constant-info? info)
       (scm-error 'wrong-type-arg funcname "Not a constant <gibaseinfo>: ~S"
                  (list info) (list info))))))

(define (constant-info-get-type info)
  "Given a constant <gibaseinfo>, this returns a type <gibaseinfo> that
describes the type of the constant."
  (assert-giconstantinfo "constant-info-get-type" info)
  (%constant-info-get-type info))

(define (constant-info-get-value info)
  "Given a constant <gibaseinfo>, this returns the scheme representation,
presuming the constant is a boolean, real number, integer, or
UTF-8 string."
  (assert-giconstantinfo "constant-info-get-type" info)
  (%constant-info-get-value info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIFieldInfo

(define (is-field-info? info)
  "Returns #t if the given <gibaseinfo> represents a field of a struct,
union, or object."
  (assert-gibaseinfo "is-field-info?" info)
  (%is-field-info info))

(define-syntax assert-gifieldinfo
  (syntax-rules ()
    ((assert-gifieldinfo funcname info)
     (unless (is-field-info? info)
       (scm-error 'wrong-type-arg funcname "Not a field <gibaseinfo>: ~S"
                  (list info) (list info))))))

;; field-info-get-field is for runtimes
;; field-info-set-field is for runtime

(define (field-info-get-flags info)
  "Given a field <gibaseinfo> this returns a list of symbols:
such as 'readable or 'writable."
  (assert-gifieldinfo "field-info-get-flags" info)
  (let ((flg (%field-info-get-flags info)))
    (append
     (if (logtest flg %FIELD_IS_READABLE) '(readable) '())
     (if (logtest flg %FIELD_IS_WRITABLE) '(writable) '()))))

(define (field-info-get-offset info)
  "Given a field <gibaseinfo>, obtains the offset in bytes of
the field from the beginning of the struct or union."
  (assert-gifieldinfo "field-info-get-offset" info)
  (%field-info-get-offset info))

(define (field-info-get-size info)
  "Given a field <gibaseinfo>, obtains the size in bits
necessary to hold the field."
  (assert-gifieldinfo "field-info-get-size" info)
  (%field-info-get-offset info))

(define (field-info-get-type info)
  "Given a field <gibaseinfo>, this returns a type <gibaseinfo> that
describes the type of the field."
  (assert-gifieldinfo "field-info-get-type" info)
  (%field-info-get-type info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIPropertyInfo

(define (is-property-info? info)
  "Returns #t if the given <gibaseinfo> represents a property of an object."
  (assert-gibaseinfo "is-property-info?" info)
  (%is-property-info info))

(define-syntax assert-gipropertyinfo
  (syntax-rules ()
    ((assert-gipropertyinfo funcname info)
     (unless (is-property-info? info)
       (scm-error 'wrong-type-arg funcname "Not a property <gibaseinfo>: ~S"
                  (list info) (list info))))))

(define (property-info-get-flags info)
  "Given a property <gibaseinfo> this returns a list of symbols:
such as 'readable or 'writable."
  (assert-gipropertyinfo "property-info-get-flags" info)
  (let ((flg (%property-info-get-flags info)))
    (append
     (if (logtest flg %PARAM_READABLE) '(readable) '())
     (if (logtest flg %PARAM_WRITABLE) '(writable) '())
     (if (= (logand flg %PARAM_READWRITE) %PARAM_READWRITE) '(readwrite) '())
     (if (logtest flg %PARAM_CONSTRUCT) '(construct) '())
     (if (logtest flg %PARAM_CONSTRUCT_ONLY) '(construct-only) '())
     (if (logtest flg %PARAM_LAX_VALIDATION) '(lax-validation) '())
     (if (logtest flg %PARAM_STATIC_NAME) '(static-name) '())
     (if (logtest flg %PARAM_STATIC_NICK) '(static-nick) '())
     (if (logtest flg %PARAM_STATIC_BLURB) '(static-blurb) '())
     (if (logtest flg %PARAM_EXPLICIT_NOTIFY) '(explicit-notify) '())
     (if (logtest flg %PARAM_DEPRECATED) '(deprecated) '()))))

(define (property-info-get-ownership-transfer info)
  "Given a property <gibaseinfo> this returns one of:
'nothing, 'container, or 'everything, describing the exchange
of data from the callee to the caller."
  (assert-gipropertyinfo "property-info-get-ownership-transfer" info)
  (let ((flg (%property-info-get-ownership-transfer info)))
    (cond
     ((eqv? flg %TRANSFER_NOTHING) 'nothing)
     ((eqv? flg %TRANSFER_CONTAINER) 'container)
     ((eqv? flg %TRANSFER_EVERYTHING) 'everything))))

(define (property-info-get-type info)
  "Given a property <gibaseinfo>, this returns a type <gibaseinfo> that
describes the type of the property."
  (assert-gipropertyinfo "property-info-get-type" info)
  (%property-info-get-type info))

(define (property-info-get-getter info)
  "Given a property <gibaseinfo>, obtains the function <gibaseinfo>
that describes the getter for this property."
  (assert-gipropertyinfo "property-info-get-getter" info)
  (%property-info-get-getter info))

(define (property-info-get-setter info)
  "Given a property <gibaseinfo>, obtains the function <gibaseinfo>
that describes the setter for this property."
  (assert-gipropertyinfo "property-info-get-setter" info)
  (%property-info-get-getter info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GITypeInfo

(define (is-type-info? info)
  "Returns #t if the given <gibaseinfo> represents the type of
an argument, return value, field, property, constant,
or union discriminator."
  (assert-gibaseinfo "is-type-info?" info)
  (%is-type-info info))

(define-syntax assert-gitypeinfo
  (syntax-rules ()
    ((assert-gitypeinfo funcname info)
     (unless (is-type-info? info)
       (scm-error 'wrong-type-arg funcname "Not a type <gibaseinfo>: ~S"
                  (list info) (list info))))))

(define (type-info-is-pointer? info)
  "Given a type <gibaseinfo>, returns #t if the type
is passed by reference."
  (assert-gitypeinfo "type-info-is-pointer?" info)
  (%type-info-is-pointer info))

(define (type-info-get-tag info)
  "Given a type <gibaseinfo>, returns a symbol that is the
type tag of the type, such as 'int32, 'double, or 'glist."
  (assert-gitypeinfo "type-info-get-tag" info)
  (let ((tag (%type-info-get-tag info)))
    (list-index
     (lambda (x) (eqv? x tag))
     type-tag-enum)))

(define (type-info-get-param-type info n)
  "Given a type <gibaseinfo>, returns the type <gibaseinfo>
of the n-th parameter."
  (assert-gitypeinfo "type-info-get-param-type" info)
  (unless (and (exact? n) (integer? n))
    (scm-error 'wrong-type-arg "type-info-get-param-type" "Not an exact integer: ~S"
               (list n) (list n)))
  (when (< n 0)
    (scm-error 'out-of-range "type-info-get-param-type" "Out of range: ~S"
               (list n) (list n)))
  ;; FIXME: range check 'n'

  (%type-info-get-param-type info n))

(define (type-info-get-interface info)
  "Given a type <gibaseinfo> which is an interface type,
returns the type <gibaseinfo> of the referenced type."
  (assert-gitypeinfo "type-info-get-interface" info)
  (unless (eqv? (type-info-get-tag info) 'interface)
    (scm-error 'wrong-type-arg "type-info-get-interface" "Not an interface type <gibaseinfo>: ~S"
               (list info) (list info)))
  (%type-info-get-interface info))

(define (type-info-get-array-length info)
  "Given a type <gibaseinfo> which is an array,
returns the argument position of the argument that has the length of
the array."
  (assert-gitypeinfo "type-info-get-array-length" info)
  (unless (eqv? (type-info-get-tag info) 'array)
    (scm-error 'wrong-type-arg "type-info-get-array-length"
               "Not an array type <gibaseinfo>: ~S"
               (list info) (list info)))
  (%type-info-get-array-length info))

(define (type-info-get-array-fixed-size info)
  "Given a type <gibaseinfo> which is an array,
returns the length of the array, if the array has a fixed
size."
  (assert-gitypeinfo "type-info-get-array-fixed-size" info)
  (unless (eqv? (type-info-get-tag info) 'array)
    (scm-error 'wrong-type-arg "type-info-get-array-fixed-size"
               "Not an array type <gibaseinfo>: ~S"
               (list info) (list info)))
  (%type-info-get-array-fixed-size info))

(define (type-info-is-zero-terminated? info)
  "Given a type <gibaseinfo> which is an array,
returns #t if the last element of the array is NULL."
  (assert-gitypeinfo "type-info-is-zero-terminated?" info)
  (unless (eqv? (type-info-get-tag info) 'array)
    (scm-error 'wrong-type-arg "type-info-is-zero-terminated?"
               "Not an array type <gibaseinfo>: ~S"
               (list info) (list info)))
  (%type-info-is-zero-terminated info))

(define (type-info-get-array-type info)
  "Given a type <gibaseinfo> which is an array, returns a symbol that is the
type of array: 'c, 'array, 'ptr-array, or 'byte-array."
  (assert-gitypeinfo "type-info-get-array-type" info)
  (unless (eqv? (type-info-get-tag info) 'array)
    (scm-error 'wrong-type-arg "type-info-get-array-type"
               "Not an array type <gibaseinfo>: ~S"
               (list info) (list info)))
  (let ((tag (%type-info-get-array-type info)))
    (cond
     ((eqv? tag %ARRAY_TYPE_C) 'c)
     ((eqv? tag %ARRAY_TYPE_ARRAY) 'array)
     ((eqv? tag %ARRAY_TYPE_PTR_ARRAY) 'ptr-array)
     ((eqv? tag %ARRAY_TYPE_BYTE_ARRAY) 'byte-array))))
