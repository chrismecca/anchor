;;;; Test-name tests.
;;;;
;;;; Parachute identifies a test by its name alone. The parent does not count.
;;;; A second DEFINE-TEST with the same name replaces the first without a
;;;; warning, so the first test never runs.
;;;;
;;;; That happened: (build assets) was replaced by (serve assets), and
;;;; (serialize escaping) by (serialize-xml escaping). The suite stayed green
;;;; while those tests were gone.
;;;;
;;;; This test reads the test files as data and checks that no name is defined
;;;; twice.

(in-package #:anchor/tests)

(define-test names)

(defun defined-test-names ()
  "Return the name of every DEFINE-TEST form in the test files."
  (let ((names '())
        (*package* (find-package '#:anchor/tests)))
    (dolist (file (uiop:directory-files
                   (asdf:system-relative-pathname :anchor "tests/") "*.lisp"))
      (with-open-file (in file)
        (loop for form = (read in nil in)
              until (eq form in)
              when (and (consp form) (eq (first form) 'define-test))
                do (let ((name (second form)))
                     ;; (define-test (parent name) ...) or (define-test name ...)
                     (push (if (consp name) (second name) name) names)))))
    names))

(define-test (names unique)
  :description "A test whose name is reused is silently replaced and never runs."
  (let ((names (defined-test-names)))
    (true (> (length names) 1) "The test files were read.")
    (is equal '()
        (remove-duplicates
         (remove-if-not (lambda (name) (> (count name names) 1)) names))
        "Each test name is defined once.")))
