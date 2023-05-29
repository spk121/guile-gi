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
            irepository-get-dependencies
            irepository-get-immediate-dependencies
            irepository-get-loaded-namespaces
            irepository-get-n-infos
            irepository-get-info
            irepository-enumerate-version
            irepository-prepend-library-path
            irepository-get-search-path
            irepository-get-typelib-path
            irepository-is-registered?
            irepository-require
            irepository-require-private
            irepository-get-c-prefix
            irepository-get-shared-libraries
            irepository-get-version
            irepository-find-by-gtype
            irepository-find-by-error-domain
            irepository-find-by-name
            irepository-get-object-gtype-interfaces

            IREPOSITORY_LOAD_FLAG_LAZY

            type-tag-to-string

            TYPE_TAG_VOID
            TYPE_TAG_BOOLEAN
            TYPE_TAG_INT8
            TYPE_TAG_UINT8
            TYPE_TAG_INT16
            TYPE_TAG_UINT16
            TYPE_TAG_INT32
            TYPE_TAG_UINT32
            TYPE_TAG_INT64
            TYPE_TAG_UINT64
            TYPE_TAG_FLOAT
            TYPE_TAG_DOUBLE
            TYPE_TAG_GTYPE
            TYPE_TAG_UTF8
            TYPE_TAG_FILENAME
            TYPE_TAG_ARRAY
            TYPE_TAG_INTERFACE
            TYPE_TAG_GLIST
            TYPE_TAG_GSLIST
            TYPE_TAG_GHASH
            TYPE_TAG_ERROR
            TYPE_TAG_UNICHAR
            ARRAY_TYPE_C
            ARRAY_TYPE_ARRAY
            ARRAY_TYPE_PTR_ARRAY
            ARRAY_TYPE_BYTE_ARRAY
            TYPE_TAG_N_TYPES

            get-major-version
            get-minor-version
            get-micro-version
            check-version
            MAJOR_VERSION
            MINOR_VERSION
            MICRO_VERSION

            base-info-equal?
            base-info-get-type
            base-info-get-namespace
            base-info-get-name
            base-info-get-attribute
            base-info-get-container
            base-info-is-deprecated?
            info-type-to-string

            INFO_TYPE_INVALID
            INFO_TYPE_FUNCTION
            INFO_TYPE_CALLBACK
            INFO_TYPE_STRUCT
            INFO_TYPE_BOXED
            INFO_TYPE_ENUM
            INFO_TYPE_FLAGS
            INFO_TYPE_OBJECT
            INFO_TYPE_INTERFACE
            INFO_TYPE_INVALID_0
            INFO_TYPE_UNION
            INFO_TYPE_VALUE
            INFO_TYPE_SIGNAL
            INFO_TYPE_VFUNC
            INFO_TYPE_PROPERTY
            INFO_TYPE_FIELD
            INFO_TYPE_ARG
            INFO_TYPE_TYPE
            INFO_TYPE_UNRESOLVED

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

            is-function-info?
            function-info-get-flags
            function-info-get-property
            function-info-get-symbol
            function-info-get-vfunc

            FUNCTION_IS_METHOD
            FUNCTION_IS_CONSTRUCTOR
            FUNCTION_IS_GETTER
            FUNCTION_IS_SETTER
            FUNCTION_WRAPS_VFUNC
            FUNCTION_THROWS

            is-signal-info?
            signal-info-get-flags
            signal-info-get-class-closure
            signal-info-true-stops-emit?

            vfunc-info-get-flags
            vfunc-info-get-offset
            vfunc-info-get-signal
            vfunc-info-get-invoker

            VFUNC_MUST_CHAIN_UP
            VFUNC_MUST_OVERRIDE
            VFUNC_MUST_NOT_OVERRICE
            VFUNC_THROWS

            is-registered-type-info
            registered-type-info-get-type-name
            registered-type-info-get-type-init
            registered-type-info-has-g-type

            is-enum-info?
            is-value-info?
            enum-info-get-n-value
            enum-info-get-value
            enum-info-get-n-methods
            enum-info-get-method
            enum-info-get-error-domain

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

            union-info-get-n-fields
            union-info-get-field
            union-info-get-n-methods
            union-info-get-method
            union-info-is-discriminated?
            union-info-get-discriminator-offset
            union-info-find-method
            union-info-get-size
            union-info-get-alignment

            is-object-info?
            object-info-get-abstract?
            object-info-get-fundamental?
            object-info-get-parent
            object-info-get-type-init
            object-info-get-n-constants
            object-info-get-constant
            object-info-get-n-fields
            object-info-get-field
            object-info-get-n-interfaces
            object-info-get-interface
            object-info-get-n-methods
            object-info-get-method
            object-info-get-n-properties
            object-info-get-property
            object-info-get-n-signals
            object-info-get-signal
            object-info-get-n-vfuncs
            object-info-get-vfunc
            object-info-get-ref-function
            object-info-get-unref-function
            object-info-get-set-value-function
            object-info-get-get-value-function

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
            interface-info-get-n-vfunc
            interface-info-get-vfunc
            interface-info-find-vfunc
            interface-info-get-n-constants
            interface-info-get-constant

            is-arg-info?
            arg-info-get-closure
            arg-info-get-destroy
            arg-info-get-ownership-transfer
            arg-info-get-scope
            arg-info-get-type
            arg-info-may-be-null?
            arg-info-is-caller-allocates?
            arg-info-is-optional?
            arg-info-is-return-value?
            arg-info-is-skip?
            DIRECTION_IN
            DIRECTION_OUT
            DIRECTION_INOUT
            SCOPE_TYPE_INVALID
            SCOPE_TYPE_CALL
            SCOPE_TYPE_ASYNC
            SCOPE_TYPE_NOTIFIED
            TRANSFER_NOTHING
            TRANSFER_CONTAINER
            TRANSFER_EVERYTHING

            is-constant-info?
            constant-info-get-type
            constant-info-get-value

            is-field-info?
            field-info-get-flags
            field-info-get-offset
            field-info-get-size
            field-info-get-type
            FIELD_IS_WRITABLE
            FIELD_IS_READABLE

            is-property-info?
            property-info-get-flags
            property-info-get-ownership-transfer
            property-info-get-type

            is-type-info?
            type-info-is-pointer
            type-info-get-tag
            type-info-get-param-type
            type-info-get-interface
            type-info-get-array-length
            type-info-get-array-fixed-size
            type-info-is-zero-terminated?
            type-info-get-array-type
            ))

(load-extension "libguile-girepository" "gig_init_girepository")

;; In 1.70
(if (defined? 'property-info-get-getter)    (export property-info-get-getter))
(if (defined? 'property-info-get-setter)    (export property-info-get-setter))

;; In 1.72
(if (defined? 'type-tag-is-basic?)    (export type-tag-is-basic?))
(if (defined? 'type-tag-is-container?)    (export type-tag-is-container?))
(if (defined? 'type-tag-is-numeric?)    (export type-tag-is-numeric?))
