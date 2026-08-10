;;;; Configuration cascade tests.

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
        "A subdirectory inherits values it does not override.")
    (is string= "Someone Else"
        (getf (config-at site "/posts/2026/first-post/") :author)
        "The cascade passes through directories that declare nothing and hold
no page.")
    (is string= "A Person" (getf (config-at site "/about/") :author)
        "A sibling branch is not affected by another branch's override.")))

(define-test (config evaluated)
  (is eq 'anchor:front-page (getf (config-at (test-site) "/archive/") :layout)
      "A quoted symbol arrives as a symbol because the form is evaluated, not
read as data."))

(define-test (config merging)
  (is equal '(:a 1 :b 3 :c 4)
      (anchor::merge-config '(:a 1 :b 2) '(:b 3 :c 4))
      "Child keys win, parent keys survive, and key order is stable.")
  (is equal '(:a 1) (anchor::merge-config '(:a 1) nil))
  (is equal '(:a 1) (anchor::merge-config nil '(:a 1))))

(defun with-config-file (text function)
  "Call FUNCTION with a temporary configuration file containing TEXT.

Remove the file afterward."
  (let ((path (merge-pathnames "broken-anchor.lisp" (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (out path :direction :output :if-exists :supersede)
             (write-string text out))
           (funcall function path))
      (uiop:delete-file-if-exists path))))

(define-test (config failure)
  ;; Both cases should become UNREADABLE-CONFIGURATION: a file that cannot be
  ;; read, and a file that reads but does not evaluate.
  ;;
  ;; Do not test a bare literal plist here. It would make SBCL report the
  ;; undefined function at compile time, before anchor sees the file. That is a
  ;; useful user diagnostic, but not the condition path this test asserts.
  (with-config-file "(list :title \"unclosed\""
    (lambda (path)
      (fail (anchor::load-config path) 'anchor:unreadable-configuration
            "A file that cannot be read is reported as anchor's error.")))
  (with-config-file "(error \"not a configuration\")"
    (lambda (path)
      (fail (anchor::load-config path) 'anchor:unreadable-configuration
            "A file that reads but does not evaluate is reported the same way."))))
