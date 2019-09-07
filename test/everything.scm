(use-modules (gi)
             (rnrs bytevectors)
             (system foreign)
             (test automake-test-lib))

(typelib-require ("Everything" "1.0"))

(tap:test! "nullfunc" (nullfunc) #t)

(define-syntax-rule (const-ireturn type f)
  (tap:test! (string-append "const-return-" type)
             (eq? (f) 0)))

(define-syntax-rule (const-freturn type f)
  (tap:test! (string-append "const-return-" type)
             (eqv? (f) 0.0)))

(define-syntax-rule (const-sreturn type f)
  (tap:test! (string-append "const-return-" type)
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

(tap:test! "const-return-gboolean"
           (not (const-return-gboolean?)))

(tap:test! "const-return-gpointer"
           (not (const-return-gpointer)))

(tap:test! "const-return-gunichar"
           (eq? (const-return-gunichar) #\null))

(tap:test! "const-return-gtype"
           (eq? (const-return-gtype) <GObject>))

(define-syntax-rule (one-iparam type f)
  (tap:test! (string-append "oneparam-" type)
             (f 1)
             #t))

(define-syntax-rule (one-fparam type f)
  (tap:test! (string-append "oneparam-" type)
             (f 1.0)
             #t))

(define-syntax-rule (one-sparam type f)
  (tap:test! (string-append "oneparam-" type)
             (f "hello")
             #t))

(one-iparam "gchar" oneparam-gchar)
(tap:test! "oneparam-gchar-from-char"
           (oneparam-gchar #\A)
           #t)

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

(tap:test! "oneparam-gboolean"
           (oneparam-gboolean #t)
           #t)

(tap:test! "oneparam-gpointer"
           (let ((bv (make-bytevector 1 0)))
             (oneparam-gpointer (bytevector->pointer bv))
             #t))

(tap:test! "oneparam-gtype"
           (oneparam-gtype G_TYPE_OBJECT)
           #t)

(tap:test! "oneparam-gtype-from-class"
           (oneparam-gtype <GObject>)
           #t)

(tap:test! "oneparam-gunichar"
           (oneparam-gunichar #\あ)
           #t)

(define-syntax-rule (oneout-iparam type f)
  (tap:test! (string-append "one-outparam-" type)
             (= (f) 0)))

(define-syntax-rule (oneout-fparam type f)
  (tap:test! (string-append "one-outparam-" type)
             (= (f) 0.0)))

(define-syntax-rule (oneout-sparam type f)
  (tap:test! (string-append "one-outparam-" type)
             (string-null? (f))))

(parameterize ((tap:expect-fail #t))
  (tap:test! "one-outparam-gchar-as-char"
             (false-if-exception
              (char=? #\null (one-outparam-gchar)))))

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

(tap:test! "one-outparam-gboolean"
           (not (one-outparam-gboolean)))

(tap:test! "one-outparam-gpointer"
           (not (one-outparam-gpointer)))

(tap:test! "passthrough-one-gintptr"
           (= (passthrough-one-gintptr 1) 1))

(tap:test! "passthrough-one-guintptr"
           (= (passthrough-one-guintptr 1) 1))

(tap:test! "one-outparam-gunichar"
           (eq? (one-outparam-gunichar) #\null))

(tap:test! "passthrough-one-filename"
           (let ((fn "temp.txt"))
             (string=? fn (passthrough-one-filename (string-copy fn)))))

(tap:test! "passthrough-one-utf8"
           (let ((str "Việt Nam"))
             (string=? str (passthrough-one-utf8 (string-copy str)))))

(tap:test! "passthrough-one-gpointer"
           (let* ((bv (make-bytevector 1))
                  (inptr (bytevector->pointer bv))
                  (outptr (passthrough-one-gpointer inptr)))
             (equal? inptr outptr)))

(tap:finish!)
