;;;; The configuration cascade.

(in-package #:anchor/tests)

(define-test config)

(defun config-at (site url)
  (anchor:site-config site (anchor:site-page site url)))

(define-test (config cascade)
  (let ((site (test-site)))
    (is string= "A Sample Site" (getf (config-at site "/") :title))
    (is string= "A Person" (getf (config-at site "/") :author))
    (is string= "Someone Else" (getf (config-at site "/posts/") :author)
        "A subdirectory overrides its ancestors.")
    (is string= "A Sample Site" (getf (config-at site "/posts/") :title)
        "And inherits everything it does not override.")
    (is string= "Someone Else"
        (getf (config-at site "/posts/2026/first-post/") :author)
        "The cascade reaches through a directory that declares nothing and
holds no page of its own.")
    (is string= "A Person" (getf (config-at site "/about/") :author)
        "A sibling is unaffected by another branch's override.")))

(define-test (config evaluated)
  (is eq 'anchor:front-page (getf (config-at (test-site) "/archive/") :layout)
      "A quoted symbol in the file arrives as a symbol, which only happens
because the form is evaluated rather than read as data."))

(define-test (config merging)
  (is equal '(:a 1 :b 3 :c 4)
      (anchor::merge-config '(:a 1 :b 2) '(:b 3 :c 4))
      "Child keys win, parent keys survive, and order is stable.")
  (is equal '(:a 1) (anchor::merge-config '(:a 1) nil))
  (is equal '(:a 1) (anchor::merge-config nil '(:a 1))))

(defun with-config-file (text function)
  "Call FUNCTION on a configuration file containing TEXT, then remove it."
  (let ((path (merge-pathnames "broken-anchor.lisp" (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (out path :direction :output :if-exists :supersede)
             (write-string text out))
           (funcall function path))
      (uiop:delete-file-if-exists path))))

(define-test (config failure)
  ;; Both paths end at the same condition: a file that will not read, and one
  ;; that reads and then will not evaluate. A bare literal plist -- the form
  ;; the spec used to show -- fails through the second of these, but writing
  ;; one here would have SBCL compile it and report the undefined function
  ;; before anchor ever sees the error, which is a diagnostic for a user with
  ;; a broken file rather than for a test asserting they are caught.
  (with-config-file "(list :title \"unclosed\""
    (lambda (path)
      (fail (anchor::load-config path) 'anchor:unreadable-configuration
            "A file that will not read is anchor's error.")))
  (with-config-file "(error \"not a configuration\")"
    (lambda (path)
      (fail (anchor::load-config path) 'anchor:unreadable-configuration
            "And so is one that reads and then will not evaluate."))))
