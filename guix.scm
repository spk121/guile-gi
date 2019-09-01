;;; Guile-GI --- GObject bindings for Guile.
;;; Copyright (C) 2019 Jan Nieuwenhuizen <janneke@gnu.org>
;;;
;;; This file is part of Guile-GI.
;;;
;;; Guile-GI is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Guile-GI is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guile-GI.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; GNU Guix development package.  To build and play, run:
;;
;;   guix environment --ad-hoc -l guix.scm guile
;;
;; To build and install, run:
;;
;;   guix package -f guix.scm
;;
;; To build it, but not install it, run:
;;
;;   guix build -f guix.scm
;;
;; To use as the basis for a development environment, run:
;;
;;   guix environment -l guix.scm
;;
;;; Code:

(use-modules (guix packages)
             (guix licenses)
             (guix git-download)
             (guix gexp)
             (guix build-system gnu)
             (gnu packages)
             (gnu packages autotools)
             (gnu packages gettext)
             (gnu packages glib)
             (gnu packages gnome)
             (gnu packages guile)
             (gnu packages pkg-config)
             (gnu packages texinfo))

(define %source-dir (dirname (current-filename)))

(package
  (name "guile-gi")
  (version "git")
  (source (local-file %source-dir
                      #:recursive? #t
                      #:select? (git-predicate %source-dir)))
  (build-system gnu-build-system)
  (native-inputs `(("autoconf" ,autoconf)
                   ("automake" ,automake)
                   ("gettext" ,gnu-gettext)
                   ("glib:bin" ,glib "bin")
                   ("libtool" ,libtool)
                   ("pkg-config" ,pkg-config)
                   ("texinfo" ,texinfo)))
  (propagated-inputs `(("glib" ,glib)
                       ("gobject-introspection" ,gobject-introspection)))
  (inputs `(("guile" ,guile-2.2)))
  (arguments
   `(#:configure-flags '("--with-gnu-filesystem-hierarchy")))
  (home-page "https://github.com/spk121/guile-gi")
  (synopsis "GObject bindings for Guile")
  (description
   "Guile-GI is a library for Guile that allows using GObject-based
libraries, such as GTK+3.  Its README comes with the disclaimer: This
is alpha code.")
  (license gpl3+))
