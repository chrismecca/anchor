;;;; Site graph tests.

(in-package #:anchor/tests)

(define-test site)

(defun test-site (&rest args)
  (apply #'anchor:make-site (fixture-site) args))

(defun page-at (site url)
  (anchor:site-page site url))

(define-test (site chronology)
  (let ((urls (mapcar #'anchor:page-url (anchor:site-chronology (test-site)))))
    (is equal '("/posts/2026/first-post/"
                "/posts/2026/second-post/"
                "/posts/"
                "/")
        urls
        "Dated pages are ordered most recent first. ISO dates sort as strings.")
    (false (find "/about/" urls :test #'string=)
           "An undated page is not in chronology.")))

(define-test (site sections)
  (let* ((site (test-site))
         (post (page-at site "/posts/2026/first-post/"))
         (posts (page-at site "/posts/"))
         (root (page-at site "/")))
    (is eq posts (anchor:site-parent site post)
        "posts/2026/ has no index.adoc. It is a path component. The nearest
page ancestor is posts/.")
    (is eq root (anchor:site-parent site posts))
    (false (anchor:site-parent site root)
           "The root page has no parent.")
    (is equal (list posts root) (anchor:site-ancestors site post))))

(define-test (site children)
  (let* ((site (test-site))
         (posts (page-at site "/posts/"))
         (root (page-at site "/")))
    (is equal '("/posts/2026/first-post/" "/posts/2026/second-post/")
        (mapcar #'anchor:page-url (anchor:site-children site posts))
        "Children are ordered by date, most recent first.")
    (is equal '("/posts/" "/about/" "/archive/")
        (mapcar #'anchor:page-url (anchor:site-children site root))
        "Dated children come first. Undated children keep discovery order.")))

(define-test (site drafts)
  (let ((without (test-site))
        (with (test-site :drafts t)))
    (false (page-at without "/posts/2026/a-draft/")
           "A page with :draft: is not in the graph.")
    (true (page-at with "/posts/2026/a-draft/")
          "Drafts are included when requested.")
    (is string= "/posts/2026/first-post/"
        (anchor:page-url (first (anchor:site-chronology without)))
        "The draft is newer than every other page. If drafts were only hidden
from output, the draft would lead chronology.")
    (is string= "/posts/2026/a-draft/"
        (anchor:page-url (first (anchor:site-chronology with))))))

(define-test (site excerpts)
  (let* ((site (test-site))
         (second (page-at site "/posts/2026/second-post/"))
         (excerpt (anchor:page-excerpt-html second)))
    (true (search "chronology has something to order" excerpt))
    (true (search "A second preamble paragraph" excerpt)
          "The excerpt is the full preamble, not only its first paragraph.")
    (false (search "past the preamble" excerpt)
           "The excerpt stops at the first heading.")
    (false (search "<h2" excerpt))))
