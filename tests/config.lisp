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

(define-test (config failure)
  (let ((path (merge-pathnames "broken-anchor.lisp" (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (out path :direction :output :if-exists :supersede)
             ;; The form the spec used to show: a literal plist, which is not
             ;; a call to anything.
             (write-string "(:title \"unevaluable\")" out))
           (fail (anchor::load-config path) 'anchor:unreadable-configuration
                 "A configuration file that will not evaluate is anchor's
error, not a stray one from the evaluator."))
      (uiop:delete-file-if-exists path))))
