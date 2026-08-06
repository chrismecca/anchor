;;;; Emitters.

(in-package #:anchor/tests)

(define-test emit)

(defun emitted (site path)
  "The content of the output at PATH, or NIL when nothing emitted it."
  (let ((file (find path (anchor:emit site)
                    :key #'anchor:output-path :test #'string=)))
    (and file (anchor:output-content file))))

(define-test (emit taxonomy)
  (let ((site (test-site)))
    (is equal '("asciidoc" "lisp" "testing") (anchor:site-tags site)
        "Every tag in use, alphabetically, split on commas and trimmed.")
    (is equal '("/posts/2026/first-post/" "/posts/2026/second-post/")
        (mapcar #'anchor:page-url (anchor:site-tagged site "lisp"))
        "Pages under a tag come back most recent first.")
    (is equal '("/posts/2026/first-post/")
        (mapcar #'anchor:page-url (anchor:site-tagged site "asciidoc")))))

(define-test (emit tags-page)
  (let ((html (emitted (test-site) "tags/index.html")))
    (true (search "<h2 id=\"lisp\">lisp</h2>" html))
    (true (search "<h2 id=\"asciidoc\">asciidoc</h2>" html))
    (true (search "<a href=\"/posts/2026/second-post/\">The Second Post</a>" html))
    (true (search "<title>Tags — A Sample Site</title>" html)
          "A generated page gets the same head and the root's configuration.")
    (true (search "<footer>" html))))

(define-test (emit feed)
  (let ((xml (emitted (test-site) "feed.xml")))
    (true (search "<?xml version=\"1.0\" encoding=\"utf-8\"?>" xml))
    (true (search "xmlns=\"http://www.w3.org/2005/Atom\"" xml))
    (true (search "<id>https://example.com/posts/2026/first-post/</id>" xml)
          "Entry ids are absolute, built from :base-url.")
    (true (search "<updated>2026-08-05T00:00:00Z</updated>" xml)
          "A date becomes midnight UTC, which is the reading that does not
drift for a reader in another timezone.")
    (true (search "<link rel=\"self\" href=\"https://example.com/feed.xml\"/>" xml)
          "An element with no children closes itself; XML has no void tags.")
    (true (search "<name>A Person</name>" xml))
    (false (search "A Draft" xml)
           "A draft is not in the graph, so it cannot reach the feed.")))

(define-test (emit feed-carries-articles-only)
  (let ((xml (emitted (test-site) "feed.xml")))
    (is = 2 (count-substring "<entry>" xml))
    (true (search "<title>The First Post</title>" xml))
    (false (search "<title>Posts</title>" xml)
           "A page with children is an index of other pages, and delivering
one to a reader duplicates everything listed on it.")
    (is = 1 (count-substring "<id>https://example.com/</id>" xml)
        "That id belongs to the feed. The root page having it too would be
the same resource claimed twice.")))

(define-test (emit feed-escapes-summary)
  (let ((xml (emitted (test-site) "feed.xml")))
    (true (search "&lt;p&gt;" xml)
          "An excerpt is HTML inside an XML text node, so its markup is
escaped rather than reaching the feed as elements.")))

(define-test (emit sitemap)
  (let ((xml (emitted (test-site) "sitemap.xml")))
    (true (search "<loc>https://example.com/about/</loc>" xml))
    (true (search "<lastmod>2026-03-14</lastmod>" xml))
    (false (search "a-draft" xml))))

(define-test (emit robots)
  (let ((text (emitted (test-site) "robots.txt")))
    (true (search "User-agent: *" text))
    (true (search "Sitemap: https://example.com/sitemap.xml" text))))

(define-test (emit without-base-url)
  "A site with no :base-url gets what can still be made correctly."
  (let* ((root (fixture-site))
         (site (anchor:make-site root)))
    ;; Stand in a configuration with no :base-url.
    (setf (gethash (namestring (anchor:site-root site))
                   (anchor::site-configs site))
          (list :title "A Sample Site"))
    (false (emitted site "feed.xml")
           "No base means no valid entry ids, so no feed at all rather than a
broken one.")
    (false (emitted site "sitemap.xml"))
    (true (emitted site "tags/index.html")
          "The tag index links relatively and is unaffected.")
    (let ((robots (emitted site "robots.txt")))
      (true (search "User-agent: *" robots))
      (false (search "Sitemap:" robots)
             "And robots.txt drops the line it cannot fill in."))))
