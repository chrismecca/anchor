;;;; End-to-end build tests.

(in-package #:anchor/tests)

(define-test build)

(defun temporary-output ()
  (uiop:ensure-directory-pathname
   (merge-pathnames (format nil "anchor-test-~36r/" (random (expt 2 32)))
                    (uiop:temporary-directory))))

(defmacro with-built-site ((output) &body body)
  "Build the fixture site into a fresh directory.

Remove the directory afterward."
  `(let ((,output (temporary-output)))
     (unwind-protect
          (progn (anchor:build (fixture-site) :output ,output)
                 ,@body)
       (uiop:delete-directory-tree ,output :validate t
                                           :if-does-not-exist :ignore))))

(defun read-output (output relative)
  "Return the contents of RELATIVE below OUTPUT.

Return NIL when the file was not written."
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
            "The page title is qualified by the site title from the cascade.")
      (true (search "<h1>The First Post</h1>" html))
      (true (search "<time datetime=\"2026-08-05\">August 5, 2026</time>" html))
      (true (search "name=\"viewport\"" html)
            "Every default layout states the viewport.")
      (true (search "content=\"light dark\"" html)
            "Every default layout states that the page supports both color schemes.")
      (is = 1 (count-substring "<h1" html)
          "The title appears once. It is not emitted by both the layout and
cl-adoc's document rendering."))))

(define-test (build undated-page)
  (with-built-site (output)
    (let ((html (read-output output "about/index.html")))
      (true (search "<h1>About</h1>" html))
      (false (search "<time" html)
             "A page with no date omits the time element instead of writing an
empty one."))))

(define-test (build relative-root)
  (let ((output (temporary-output)))
    (unwind-protect
         (progn
           (uiop:with-current-directory ((fixture-site))
             (anchor:build "." :output output))
           (true (read-output output "posts/2026/first-post/index.html")
                 "A relative root is resolved against the current directory.
Other tests pass absolute paths, so they do not cover this."))
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
             "The index shows the excerpt, not the full body.")
      (true (search "<a href=\"/posts/2026/second-post/\">The Second Post</a>" html)
            "Remaining pages are rows with title and date.")
      (false (search "chronology has something to order" html)
             "A row has no excerpt."))))

(define-test (build draft-output)
  (with-built-site (output)
    (false (read-output output "posts/2026/a-draft/index.html")
           "A draft is not written.")
    (false (search "A Draft" (read-output output "index.html"))
           "A draft is not listed."))
  (let ((output (temporary-output)))
    (unwind-protect
         (progn
           (anchor:build (fixture-site) :output output :drafts t)
           (true (read-output output "posts/2026/a-draft/index.html")
                 "Requesting drafts writes them.")
           (true (search "A Draft" (read-output output "index.html"))
                 "The front page leads with the draft because it is newest."))
      (uiop:delete-directory-tree output :validate t
                                         :if-does-not-exist :ignore))))

(define-test (build configured-layout)
  (with-built-site (output)
    (let ((archive (read-output output "archive/index.html"))
          (about (read-output output "about/index.html")))
      (true (search "<a href=\"/posts/2026/first-post/\">" archive)
            "archive/ names :layout in its own _anchor.lisp. It gets an index
even though it is not the root page.")
      (false (search "<a href=\"/posts/2026/first-post/\">" about)
             "about/ names no layout and gets the article layout."))))

(define-test (build footer)
  (with-built-site (output)
    (true (search "<footer>
<p>A Person</p>" (read-output output "about/index.html"))
          "The author comes from the root configuration.")
    (true (search "<footer>
<p>Someone Else</p>"
                  (read-output output "posts/2026/first-post/index.html"))
          "The author is overridden below posts/.")))

(define-test (build policy)
  (let ((output (temporary-output)))
    (unwind-protect
         (progn
           (fail (anchor:build (broken-fixture-site) :output output
                                                     :policy :strict)
                 'anchor:missing-reference
                 ":strict aborts. CI uses this.")
           (handler-bind ((warning #'muffle-warning))
             (anchor:build (broken-fixture-site) :output output
                                                 :policy :permissive))
           (true (read-output output "index.html")
                 ":permissive reports the problem and still writes the site.
--force uses this."))
      (uiop:delete-directory-tree output :validate t
                                         :if-does-not-exist :ignore))))

(define-test (build assets)
  (with-built-site (output)
    (true (read-output output "style.css")
          "A stylesheet is copied without changes.")
    (true (read-output output "posts/2026/first-post/diagram.svg")
          "Page-local media is copied too.")
    (false (read-output output "posts/2026/first-post/notes.adoc")
           "A document is never copied as an asset.")
    (false (read-output output "_anchor.lisp")
           "Configuration is not copied.")))

(define-test (build media-is-copied-when-referenced)
  (with-built-site (output)
    (true (read-output output "_media/portrait.svg")
          "A reserved directory is not copied as a whole. Referenced files
inside _media/ are copied.")
    (false (read-output output "_media/unused.svg")
           "Unreferenced shared media is not copied. A shared directory costs
nothing until a page names a file in it.")
    (false (read-output output "_media/shared.txt"))))
