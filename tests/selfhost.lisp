;;;; anchor's own documents, rendered.
;;;;
;;;; SPEC.adoc claims this file restricts itself to the subset cl-adoc parses,
;;;; and the README repeats it. A claim like that decays the moment someone
;;;; reaches for a construct nobody checked, which is exactly what happened
;;;; with literal blocks: the description lists were verified and the ....
;;;; delimiters went on rendering as paragraphs for as long as nobody looked.
;;;;
;;;; So the check is here and it looks at the whole document rather than at
;;;; one construct. A delimiter surviving into the output is the signature of
;;;; a block the parser did not recognise, whatever the block happened to be.

(in-package #:anchor/tests)

(define-test selfhost)

(defun project-document (name)
  (adoc:render-html
   (adoc:parse-file (asdf:system-relative-pathname :anchor name))))

(defun starts-with-p (text prefix)
  (and (>= (length text) (length prefix))
       (string= prefix text :end2 (length prefix))))

(defun contains-line-p (text line)
  "Whether TEXT holds LINE on a line of its own."
  (let ((needle (format nil "~%~a~%" line)))
    (and (search needle text) t)))

(defun assert-no-unparsed-blocks (name)
  (let ((html (project-document name)))
    (false (contains-line-p html "----")
           (format nil "~a: a listing delimiter reached the output, so a block
was read as prose rather than parsed." name))
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
          "The title parses and leads the document, which it does not when an
attribute entry sits above it.")
    (true (search "<dl>" html) "Description lists render as description lists.")
    (true (search "<pre>" html) "And the delimited blocks as blocks.")
    (true (search "<code>" html))))

(define-test (selfhost readme)
  (let ((html (assert-no-unparsed-blocks "README.adoc")))
    (true (starts-with-p html "<h1>anchor</h1>"))
    (true (search "<pre>" html))))

(define-test (selfhost changelog)
  (let ((html (assert-no-unparsed-blocks "CHANGELOG.adoc")))
    (true (starts-with-p html "<h1>Changelog</h1>"))))

(define-test (selfhost the-readme-layout-example-runs)
  :description "The layout in README.adoc is code, and code in documentation
rots unless something runs it."
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
