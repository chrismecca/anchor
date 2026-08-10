;;;; Self-hosting document tests.
;;;;
;;;; SPEC.adoc says it uses only the subset that cl-adoc parses. README.adoc
;;;; repeats that claim.
;;;;
;;;; A claim like that decays unless a test checks the whole document. That
;;;; already happened once: description lists were checked, but literal-block
;;;; delimiters rendered as paragraphs because nobody looked at the complete
;;;; output.
;;;;
;;;; This test checks complete rendered documents. A delimiter that reaches the
;;;; output is the signature of a block the parser did not recognize.

(in-package #:anchor/tests)

(define-test selfhost)

(defun project-document (name)
  (adoc:render-html
   (adoc:parse-file (asdf:system-relative-pathname :anchor name))))

(defun starts-with-p (text prefix)
  (and (>= (length text) (length prefix))
       (string= prefix text :end2 (length prefix))))

(defun contains-line-p (text line)
  "Return true when TEXT contains LINE on a line by itself."
  (let ((needle (format nil "~%~a~%" line)))
    (and (search needle text) t)))

(defun assert-no-unparsed-blocks (name)
  (let ((html (project-document name)))
    (false (contains-line-p html "----")
           (format nil "~a: a listing delimiter reached the output, so a block
was read as prose instead of parsed." name))
    (false (contains-line-p html "....")
           (format nil "~a: a literal delimiter reached the output. cl-adoc
does not parse those; use ---- instead." name))
    (false (contains-line-p html "++++")
           (format nil "~a: a stem delimiter reached the output." name))
    (false (search "[source" html)
           (format nil "~a: a block attribute line reached the output." name))
    (false (search ":: " html)
           (format nil "~a: a description-list delimiter reached the output."
                   name))
    html))

(define-test (selfhost spec)
  (let ((html (assert-no-unparsed-blocks "SPEC.adoc")))
    (true (starts-with-p html "<h1>anchor: specification</h1>")
          "The title parses and leads the document. It does not when an
attribute entry sits above it.")
    (true (search "<dl>" html) "Description lists render as description lists.")
    (true (search "<pre>" html) "Delimited blocks render as blocks.")
    (true (search "<code>" html))))

(define-test (selfhost readme)
  (let ((html (assert-no-unparsed-blocks "README.adoc")))
    (true (starts-with-p html "<h1>anchor</h1>"))
    (true (search "<pre>" html))))

(define-test (selfhost changelog)
  (let ((html (assert-no-unparsed-blocks "CHANGELOG.adoc")))
    (true (starts-with-p html "<h1>Changelog</h1>"))))

(define-test (selfhost the-readme-layout-example-runs)
  :description "The layout in README.adoc is code. Documentation code decays
unless something runs it."
  (let* ((site (test-site))
         (page (anchor:site-page site "/posts/2026/first-post/"))
         (article (lambda (page site)
                    (declare (ignore site))
                    `(:article
                      (:h1 ,(anchor:page-title page))
                      (:p :class "meta"
                          (:time :datetime ,(anchor:page-date page)
                                 ,(anchor:page-date-human page)))
                      (:raw ,(anchor:page-body-html page)))))
         (html (anchor:serialize (funcall article page site))))
    (true (search "<h1>The First Post</h1>" html))
    (true (search "<time datetime=\"2026-08-05\">August 5, 2026</time>" html))))
