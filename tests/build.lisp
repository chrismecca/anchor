;;;; Building the sample site end to end.

(in-package #:anchor/tests)

(define-test build)

(defun temporary-output ()
  (uiop:ensure-directory-pathname
   (merge-pathnames (format nil "anchor-test-~36r/" (random (expt 2 32)))
                    (uiop:temporary-directory))))

(defmacro with-built-site ((output) &body body)
  "Build the fixture site into a fresh directory, and remove it afterwards."
  `(let ((,output (temporary-output)))
     (unwind-protect
          (progn (anchor:build (fixture-site) :output ,output)
                 ,@body)
       (uiop:delete-directory-tree ,output :validate t
                                           :if-does-not-exist :ignore))))

(defun read-output (output relative)
  "The contents of RELATIVE under OUTPUT, or NIL when it was not written."
  (let ((path (merge-pathnames relative output)))
    (and (probe-file path) (uiop:read-file-string path))))

(defun count-substring (needle haystack)
  (loop with count = 0
        for start = 0 then (+ found (length needle))
        for found = (search needle haystack :start2 start)
        while found
        do (incf count)
        finally (return count)))

(define-test (build writes-pages)
  (with-built-site (output)
    (true (read-output output "index.html"))
    (true (read-output output "about/index.html"))
    (true (read-output output "posts/index.html"))
    (true (read-output output "posts/2026/first-post/index.html")
          "A page's output path is its URL plus index.html.")))

(define-test (build page-contents)
  (with-built-site (output)
    (let ((html (read-output output "posts/2026/first-post/index.html")))
      (true (search "<!DOCTYPE html>" html))
      (true (search "<title>The First Post</title>" html))
      (true (search "<h1>The First Post</h1>" html))
      (true (search "<time datetime=\"2026-08-05\">August 5, 2026</time>" html))
      (true (search "name=\"viewport\"" html)
            "Every default layout states the viewport.")
      (true (search "content=\"light dark\"" html)
            "And that the page supports both colour schemes.")
      (is = 1 (count-substring "<h1" html)
          "The title appears once, not once from the layout and again from
cl-adoc's rendering of the document."))))

(define-test (build undated-page)
  (with-built-site (output)
    (let ((html (read-output output "about/index.html")))
      (true (search "<h1>About</h1>" html))
      (false (search "<time" html)
             "A page with no date renders without the element, not with an
empty one."))))

(define-test (build relative-root)
  (let ((output (temporary-output)))
    (unwind-protect
         (progn
           (uiop:with-current-directory ((fixture-site))
             (anchor:build "." :output output))
           (true (read-output output "posts/2026/first-post/index.html")
                 "A relative root is resolved against the current directory.
Every other test passes an absolute path, so nothing else covers this."))
      (uiop:delete-directory-tree output :validate t
                                         :if-does-not-exist :ignore))))

(define-test (build assets)
  (with-built-site (output)
    (true (read-output output "style.css")
          "A stylesheet is copied verbatim.")
    (false (read-output output "posts/2026/first-post/notes.adoc")
           "A document is never copied as an asset.")
    (false (read-output output "_media/shared.txt")
           "Reserved directories are not copied.")
    (false (read-output output "_anchor.lisp")
           "Neither is configuration.")))
