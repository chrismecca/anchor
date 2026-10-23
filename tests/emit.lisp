;;;; Emitter tests.

(in-package #:anchor/tests)

(define-test emit)

(defun emitted (site path)
  "Return the emitted content at PATH.

Return NIL when no emitter produced PATH."
  (let ((file (find path (anchor:emit site)
                    :key #'anchor:output-path :test #'string=)))
    (and file (anchor:output-content file))))

(define-test (emit taxonomy)
  (let ((site (test-site)))
    (is equal '("asciidoc" "lisp" "testing") (anchor:site-tags site)
        "Tags are split on commas, trimmed, and returned alphabetically.")
    (is equal '("/posts/2026/first-post/" "/posts/2026/second-post/")
        (mapcar #'anchor:page-url (anchor:site-tagged site "lisp"))
        "Pages under a tag are ordered most recent first.")
    (is equal '("/posts/2026/first-post/")
        (mapcar #'anchor:page-url (anchor:site-tagged site "asciidoc")))))

(define-test (emit tags-page)
  (let ((html (emitted (test-site) "tags/index.html")))
    (true (search "<h2 id=\"lisp\">lisp</h2>" html))
    (true (search "<h2 id=\"asciidoc\">asciidoc</h2>" html))
    (true (search "<a href=\"/posts/2026/second-post/\">The Second Post</a>" html))
    (true (search "<title>Tags — A Sample Site</title>" html)
          "A generated page uses the same head and the root configuration.")
    (true (search "<footer>" html))))

(define-test (emit feed)
  (let ((xml (emitted (test-site) "feed.xml")))
    (true (search "<?xml version=\"1.0\" encoding=\"utf-8\"?>" xml))
    (true (search "xmlns=\"http://www.w3.org/2005/Atom\"" xml))
    (true (search "<id>https://example.com/posts/2026/first-post/</id>" xml)
          "Entry IDs are absolute and come from :base-url.")
    (true (search "<updated>2026-08-05T00:00:00Z</updated>" xml)
          "A page date becomes midnight UTC. That reading does not drift for
readers in other time zones.")
    (true (search "<link rel=\"self\" href=\"https://example.com/feed.xml\"/>" xml)
          "An element with no children closes itself. XML has no void tags.")
    (true (search "<name>A Person</name>" xml))
    (false (search "A Draft" xml)
           "A draft is not in the graph, so it cannot reach the feed.")))

(define-test (emit feed-carries-articles-only)
  (let ((xml (emitted (test-site) "feed.xml")))
    (is = 2 (count-substring "<entry>" xml))
    (true (search "<title>The First Post</title>" xml))
    (false (search "<title>Posts</title>" xml)
           "A page with children is an index of other pages. Sending it in the
feed would duplicate the pages listed on it.")
    (is = 1 (count-substring "<id>https://example.com/</id>" xml)
        "That ID belongs to the feed. Giving the root page the same ID would
claim one resource twice.")))

(define-test (emit feed-escapes-summary)
  (let ((xml (emitted (test-site) "feed.xml")))
    (true (search "&lt;p&gt;" xml)
          "An excerpt is HTML inside an XML text node. Its markup is escaped,
not emitted as feed elements.")))

(define-test (emit sitemap)
  (let ((xml (emitted (test-site) "sitemap.xml")))
    (true (search "<loc>https://example.com/about/</loc>" xml))
    (true (search "<lastmod>2026-03-14</lastmod>" xml))
    (false (search "a-draft" xml))))

(define-test (emit robots)
  (let ((text (emitted (test-site) "robots.txt")))
    (true (search "User-agent: *" text))
    (true (search "Sitemap: https://example.com/sitemap.xml" text))))

;;; The offer in SPEC.adoc
;;;
;;; Per-tag pages are deferred, not missing. SPEC.adoc says a site that wants
;;; them can write an emitter.
;;;
;;; This package does not use #:anchor. Everything below reaches anchor through
;;; exported symbols. The test fails to compile if that offer cannot be used.

(defun per-tag-pages (site)
  "Return one generated page per tag.

This is the emitter SPEC.adoc says a site can write."
  (loop for tag in (anchor:site-tags site)
        for url = (format nil "/tags/~a/" (anchor:tag-id tag))
        collect (anchor:output
                 (format nil "tags/~a/index.html" (anchor:tag-id tag))
                 (anchor:render-document
                  (anchor:page-shell
                   (anchor:generated-page site url tag)
                   site
                   `(:article
                     (:h1 ,tag)
                     (:nav (:ul ,@(mapcar #'anchor:index-row
                                          (anchor:site-tagged site tag))))))))))

(define-test (emit a-site-can-write-an-emitter)
  (let* ((site (test-site))
         (files (per-tag-pages site))
         (lisp (find "tags/lisp/index.html" files
                     :key #'anchor:output-path :test #'string=)))
    (is = 3 (length files) "One page is produced for each tag in use.")
    (true lisp)
    (let ((html (anchor:output-content lisp)))
      (true (search "<h1>lisp</h1>" html))
      (true (search "<a href=\"/posts/2026/second-post/\">The Second Post</a>" html))
      (true (search "<title>lisp — A Sample Site</title>" html)
            "A generated page gets the same head and root configuration as a
page read from disk."))))

(define-test (emit tag-ids-are-one-rule)
  :description "A layout can link to the ID that the tag index uses.

The rule is part of the site interface, not a private detail."
  (is string= "lisp" (anchor:tag-id "lisp"))
  (is string= "two-words" (anchor:tag-id "Two Words"))
  (is string= "c--" (anchor:tag-id "C++"))
  (true (search (format nil "<h2 id=\"~a\">" (anchor:tag-id "asciidoc"))
                (emitted (test-site) "tags/index.html"))
        "The index uses TAG-ID too, so the two values cannot drift."))

(define-test (emit without-base-url)
  :description "A site without :base-url gets only outputs that can be correct."
  (let* ((root (fixture-site))
         (site (anchor:make-site root)))
    ;; Stand in a configuration with no :base-url.
    (setf (gethash (namestring (anchor:site-root site))
                   (anchor::site-configs site))
          (list :title "A Sample Site"))
    (false (emitted site "feed.xml")
           "No base URL means no valid entry IDs. Emit no feed instead of a
broken feed.")
    (false (emitted site "sitemap.xml"))
    (true (emitted site "tags/index.html")
          "The tag index uses site-relative links and is unaffected.")
    (let ((robots (emitted site "robots.txt")))
      (true (search "User-agent: *" robots))
      (false (search "Sitemap:" robots)
             "robots.txt drops the line it cannot complete."))))
