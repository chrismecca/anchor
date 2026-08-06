;;;; Finding pages in a source tree.

(in-package #:anchor/tests)

(define-test discovery)

(defun urls ()
  (sort (mapcar #'anchor:page-url (anchor:discover-pages (fixture-site)))
        #'string<))

(define-test (discovery urls)
  (is equal '("/" "/about/" "/archive/" "/posts/" "/posts/2026/a-draft/"
              "/posts/2026/first-post/" "/posts/2026/second-post/")
      (urls)
      "Directory structure is URL structure, and the root index is /.
Discovery reports what is on disk, drafts included: leaving them out is the
graph's decision, not the filesystem walk's."))

(define-test (discovery ignores)
  (let ((urls (urls)))
    (false (find "/posts/2026/first-post/notes/" urls :test #'string=)
           "An .adoc file that is not index.adoc is not a page.")
    (false (find-if (lambda (url) (search "_media" url)) urls)
           "A reserved directory is not walked.")))

(define-test (discovery reading)
  (let* ((pages (anchor:discover-pages (fixture-site)))
         (post (find "/posts/2026/first-post/" pages
                     :key #'anchor:page-url :test #'string=))
         (about (find "/about/" pages
                      :key #'anchor:page-url :test #'string=)))
    (is string= "The First Post" (anchor:page-title post))
    (is string= "2026-08-05" (anchor:page-date post))
    (is string= "August 5, 2026" (anchor:page-date-human post))
    (false (anchor:page-date about)
           "A page without a :date: has none, which is not an error here.")
    (false (anchor:page-date-human about))))

(define-test (discovery body-excludes-title)
  (let* ((pages (anchor:discover-pages (fixture-site)))
         (post (find "/posts/2026/first-post/" pages
                     :key #'anchor:page-url :test #'string=))
         (body (anchor:page-body-html post)))
    (false (search "<h1>" body)
           "The layout emits the title itself, so the body must not repeat it.")
    (true (search "<h2" body))
    (true (search "a &lt; b" body)
          "cl-adoc escapes the body; anchor does not touch it again.")))
