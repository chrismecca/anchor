;;;; Head tags, and the things a document says about itself.

(in-package #:anchor/tests)

(define-test head)

(defun head-of (url)
  (let* ((site (test-site))
         (page (anchor:site-page site url)))
    (anchor:serialize (anchor:head-tags page site))))

(define-test (head composition)
  (let ((site (test-site)))
    (true (every #'functionp anchor:*head-tags*)
          "The head is a list of functions, not a block of markup, which is
what lets a site drop or replace one member without touching the rest.")
    (let* ((page (anchor:site-page site "/"))
           (anchor:*head-tags* (list #'anchor:head-title-tag)))
      (is string= "<title>A Sample Site</title>
" (anchor:serialize (anchor:head-tags page site))
          "Rebinding the list replaces the head entirely."))))

(define-test (head description)
  (let ((html (head-of "/posts/2026/first-post/")))
    (true (search "name=\"description\"" html))
    (true (search "A lead paragraph" html)
          "With no :description:, the excerpt is what describes the page.")))

(define-test (head declared-description)
  (let ((html (head-of "/posts/2026/second-post/")))
    (true (search "content=\"A post that says what it is about.\"" html)
          "An author who declared a description is believed over the excerpt.")))

(define-test (head canonical-and-cards)
  (let ((html (head-of "/posts/2026/first-post/")))
    (true (search "<link rel=\"canonical\" href=\"https://example.com/posts/2026/first-post/\">"
                  html))
    (true (search "property=\"og:type\" content=\"article\"" html)
          "A page with no children is an article.")
    (true (search "property=\"og:url\" content=\"https://example.com/posts/2026/first-post/\""
                  html))
    (true (search "property=\"og:site_name\" content=\"A Sample Site\"" html))
    (true (search "property=\"article:published_time\" content=\"2026-08-05\"" html))
    (true (search "name=\"twitter:card\" content=\"summary\"" html)))
  (let ((html (head-of "/posts/")))
    (true (search "property=\"og:type\" content=\"website\"" html)
          "A page with children is an index of other pages, not an article.")))

(define-test (head needs-a-base-url)
  (let* ((site (anchor:make-site (fixture-site)))
         (page (anchor:site-page site "/about/")))
    (setf (gethash (namestring (anchor:site-root site))
                   (anchor::site-configs site))
          (list :title "A Sample Site"))
    (let ((html (anchor:serialize (anchor:head-tags page site))))
      (false (search "canonical" html)
             "A canonical link has to be absolute, so without a base there is
none rather than a relative one.")
      (false (search "og:url" html))
      (true (search "twitter:card" html)
            "A summary card needs no absolute URL and survives."))))

(define-test (head stem)
  (let ((site (test-site)))
    (false (anchor:page-uses-stem-p (anchor:site-page site "/about/")))
    (true (anchor:page-uses-stem-p (anchor:site-page site "/posts/2026/second-post/"))
          "The document answers whether it holds mathematics. What a layout
does with the answer is the layout's business; anchor ships no renderer.")))

(define-test (head text-handling)
  (is string= "one two three"
      (anchor::collapse-whitespace (format nil "  one   two~%three  ")))
  (is string= "a b…" (anchor::truncate-text "a b cdef" 4))
  (is string= "short" (anchor::truncate-text "short" 40)))
