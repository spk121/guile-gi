(use-modules (gi)
             (rnrs bytevectors)
             (system foreign)
             (test automake-test-lib)

             (srfi srfi-64))

(test-begin "everything.scm")

(typelib-require ("Everything" "1.0"))

(test-assert "nullfunc" (begin (nullfunc) #t))

(define-syntax-rule (const-ireturn type f)
  (test-eqv (string-append "const-return-" type)
            0 (f)))

(define-syntax-rule (const-freturn type f)
  (test-eqv (string-append "const-return-" type)
            0.0 (f)))

(define-syntax-rule (const-sreturn type f)
  (test-assert (string-append "const-return-" type)
               (string-null? (f))))

(const-ireturn "gchar" const-return-gchar)
(const-ireturn "gshort" const-return-gshort)
(const-ireturn "gint" const-return-gint)
(const-ireturn "glong" const-return-glong)
(const-ireturn "gint8" const-return-gint8)
(const-ireturn "gint16" const-return-gint16)
(const-ireturn "gint32" const-return-gint32)
(const-ireturn "gint64" const-return-gint64)
(const-ireturn "gintptr" const-return-gintptr)

(const-ireturn "gushort" const-return-gshort)
(const-ireturn "guint" const-return-guint)
(const-ireturn "gulong" const-return-glong)
(const-ireturn "guint8" const-return-guint8)
(const-ireturn "guint16" const-return-guint16)
(const-ireturn "guint32" const-return-guint32)
(const-ireturn "guint64" const-return-guint64)

(const-ireturn "gsize" const-return-gsize)
(const-ireturn "gssize" const-return-gssize)

(const-freturn "gfloat" const-return-gfloat)
(const-freturn "gdouble" const-return-gdouble)

(const-sreturn "utf8" const-return-utf8)
(const-sreturn "filename" const-return-filename)

(test-assert "const-return-gboolean"
             (not (const-return-gboolean?)))

(test-assert "const-return-gpointer"
             (not (const-return-gpointer)))

(test-eq "const-return-gunichar"
         #\null (const-return-gunichar))

(test-eq "const-return-gtype"
         <GObject> (const-return-gtype))

(define-syntax-rule (one-iparam type f)
  (test-assert (string-append "oneparam-" type)
               (begin
                 (f 1)
                 #t)))

(define-syntax-rule (one-fparam type f)
  (test-assert (string-append "oneparam-" type)
               (begin
                 (f 1.0)
                 #t)))

(define-syntax-rule (one-sparam type f)
  (test-assert (string-append "oneparam-" type)
               (begin
                 (f "hello")
                 #t)))

(one-iparam "gchar" oneparam-gchar)
(test-assert "oneparam-gchar-from-char"
             (begin
               (oneparam-gchar #\A)
               #t))

(one-iparam "gshort" oneparam-gshort)
(one-iparam "gint" oneparam-gint)
(one-iparam "glong" oneparam-glong)
(one-iparam "gint8" oneparam-gint8)
(one-iparam "gint16" oneparam-gint16)
(one-iparam "gint32" oneparam-gint32)
(one-iparam "gint64" oneparam-gint64)
(one-iparam "gintptr" oneparam-gintptr)

(one-iparam "gushort" oneparam-gushort)
(one-iparam "guint" oneparam-guint)
(one-iparam "gulong" oneparam-gulong)
(one-iparam "guint8" oneparam-guint8)
(one-iparam "guint16" oneparam-guint16)
(one-iparam "guint32" oneparam-guint32)
(one-iparam "guint64" oneparam-guint64)
(one-iparam "guintptr" oneparam-guintptr)

(one-iparam "gsize" oneparam-gsize)
(one-iparam "gssize" oneparam-gssize)

(one-fparam "gfloat" oneparam-gfloat)
(one-fparam "gdouble" oneparam-gdouble)

(one-sparam "filename" oneparam-filename)
(one-sparam "utf8" oneparam-utf8)

(test-assert "oneparam-gboolean"
             (begin
               (oneparam-gboolean #t)
               #t))

(test-assert "oneparam-gpointer"
           (let ((bv (make-bytevector 1 0)))
             (oneparam-gpointer (bytevector->pointer bv))
             #t))

(test-assert "oneparam-gtype"
             (begin
               (oneparam-gtype G_TYPE_OBJECT)
               #t))

(test-assert "oneparam-gtype-from-class"
             (begin
               (oneparam-gtype <GObject>)
               #t))

(test-assert "oneparam-gunichar"
             (begin
               (oneparam-gunichar #\あ)
               #t))

(define-syntax-rule (oneout-iparam type f)
  (test-eqv (string-append "one-outparam-" type)
            0 (f)))

(define-syntax-rule (oneout-fparam type f)
  (test-eqv (string-append "one-outparam-" type)
            0.0 (f)))

(define-syntax-rule (oneout-sparam type f)
  (test-assert (string-append "one-outparam-" type)
             (string-null? (f))))

(test-expect-fail "one-outparam-gchar-as-char")
(test-assert "one-outparam-gchar-as-char"
             (char=? #\null (one-outparam-gchar)))

(oneout-iparam "gshort" one-outparam-gshort)
(oneout-iparam "gint" one-outparam-gint)
(oneout-iparam "glong" one-outparam-glong)
(oneout-iparam "gint8" one-outparam-gint8)
(oneout-iparam "gint16" one-outparam-gint16)
(oneout-iparam "gint32" one-outparam-gint32)
(oneout-iparam "gint64" one-outparam-gint64)
(oneout-iparam "gintptr" one-outparam-gintptr)

(oneout-iparam "gushort" one-outparam-gushort)
(oneout-iparam "guint" one-outparam-guint)
(oneout-iparam "gulong" one-outparam-gulong)
(oneout-iparam "guint8" one-outparam-guint8)
(oneout-iparam "guint16" one-outparam-guint16)
(oneout-iparam "guint32" one-outparam-guint32)
(oneout-iparam "guint64" one-outparam-guint64)
(oneout-iparam "guintptr" one-outparam-guintptr)

(oneout-iparam "gsize" one-outparam-gsize)
(oneout-iparam "gssize" one-outparam-gssize)

(oneout-fparam "gfloat" one-outparam-gfloat)
(oneout-fparam "gdouble" one-outparam-gdouble)

(oneout-sparam "filename" one-outparam-filename)
(oneout-sparam "utf8" one-outparam-utf8)

(test-assert "one-outparam-gboolean"
             (not (one-outparam-gboolean)))

(test-assert "one-outparam-gpointer"
             (not (one-outparam-gpointer)))

(test-eqv "passthrough-one-gintptr"
          1 (passthrough-one-gintptr 1))

(test-eqv "passthrough-one-guintptr"
          1 (passthrough-one-guintptr 1))

(test-eq "one-outparam-gunichar"
         #\null (one-outparam-gunichar))

(test-assert "passthrough-one-filename"
           (let ((fn "temp.txt"))
             (string=? fn (passthrough-one-filename (string-copy fn)))))

(test-assert "passthrough-one-utf8"
           (let ((str "Việt Nam"))
             (string=? str (passthrough-one-utf8 (string-copy str)))))

(test-assert "passthrough-one-gpointer"
           (let* ((bv (make-bytevector 1))
                  (inptr (bytevector->pointer bv))
                  (outptr (passthrough-one-gpointer inptr)))
             (equal? inptr outptr)))

(test-end "everything.scm")
