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
      (true (search "<title>The First Post — A Sample Site</title>" html)
            "The page's title, qualified by the site's from the cascade.")
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

(define-test (build front-page)
  (with-built-site (output)
    (let ((html (read-output output "index.html")))
      (true (search "<h1>A Sample Site</h1>" html))
      (true (search "<h2><a href=\"/posts/2026/first-post/\">The First Post</a></h2>"
                    html)
            "The newest page leads, linked and with its excerpt.")
      (true (search "A lead paragraph" html))
      (false (search "Body text with <code>code</code>" html)
             "Its body is not repeated on the index, only its excerpt.")
      (true (search "<a href=\"/posts/2026/second-post/\">The Second Post</a>" html)
            "Everything remaining is a row of title and date.")
      (false (search "chronology has something to order" html)
             "A row carries no excerpt."))))

(define-test (build drafts)
  (with-built-site (output)
    (false (read-output output "posts/2026/a-draft/index.html")
           "A draft is not written.")
    (false (search "A Draft" (read-output output "index.html"))
           "Nor listed."))
  (let ((output (temporary-output)))
    (unwind-protect
         (progn
           (anchor:build (fixture-site) :output output :drafts t)
           (true (read-output output "posts/2026/a-draft/index.html")
                 "Asking for drafts writes them.")
           (true (search "A Draft" (read-output output "index.html"))
                 "And the front page leads with it, since it is the newest."))
      (uiop:delete-directory-tree output :validate t
                                         :if-does-not-exist :ignore))))

(define-test (build configured-layout)
  (with-built-site (output)
    (let ((archive (read-output output "archive/index.html"))
          (about (read-output output "about/index.html")))
      (true (search "<a href=\"/posts/2026/first-post/\">" archive)
            "archive/ names :layout in its own _anchor.lisp and gets an index
even though it is not the root page.")
      (false (search "<a href=\"/posts/2026/first-post/\">" about)
             "about/ names none and gets the article layout."))))

(define-test (build footer)
  (with-built-site (output)
    (true (search "<footer>
<p>A Person</p>" (read-output output "about/index.html"))
          "The author comes from the root configuration.")
    (true (search "<footer>
<p>Someone Else</p>"
                  (read-output output "posts/2026/first-post/index.html"))
          "And is overridden for everything under posts/.")))

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
