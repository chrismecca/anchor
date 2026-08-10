;;;; Emitters.
;;;;
;;;; An emitter is a function from a site to a list of output files.
;;;;
;;;; Every derived artifact uses this one interface. The feed, sitemap, crawler
;;;; policy, and tag index are ordinary functions in one list. None is a
;;;; subsystem. A site that wants a fifth artifact writes another function.
;;;;
;;;; The feed and sitemap need absolute URLs, so they need :base-url. A site
;;;; without :base-url gets neither. There is no valid feed with relative entry
;;;; IDs. Emitting a broken feed is worse than emitting none.

(in-package #:anchor)

(defstruct (output (:constructor output (path content)))
  "One file produced by an emitter.

PATH is where it goes. CONTENT is what it contains."
  path
  content)

(defun feed-timestamp (date)
  "Return DATE in the date-time form required by Atom.

A page has a date, not a time. Midnight UTC is the interpretation that does
not drift when the reader is in another time zone."
  (format nil "~aT00:00:00Z" date))

(defun feed-entries (site)
  "Return pages carried by the feed.

Return newest first and no more than :feed-limit.

Sections are excluded. A page with children is an index of other pages. The
front page is one too. Putting those pages in the feed would deliver a list as
well as every item on the list.

It would also give the root page an ID identical to the feed's own ID. That is
one resource claimed twice.

Nothing declares that a page is a section. Having children makes it one, and
the graph already knows that."
  (let* ((limit (or (getf (site-root-config site) :feed-limit) 20))
         (articles (remove-if (lambda (page) (site-children site page))
                              (site-chronology site))))
    (subseq articles 0 (min limit (length articles)))))

(defun atom-feed (site)
  "Return the Atom feed output at /feed.xml."
  (let ((base (base-url site))
        (entries (feed-entries site)))
    (when (and base entries)
      (let* ((config (site-root-config site))
             (title (or (getf config :title) "Untitled"))
             (author (getf config :author))
             (updated (feed-timestamp (page-date (first entries)))))
        (list
         (output "feed.xml"
                 (concatenate
                  'string
                  (xml-declaration)
                  (serialize-xml
                   `(:feed
                     :xmlns "http://www.w3.org/2005/Atom"
                     (:title ,title)
                     (:id ,(concatenate 'string base "/"))
                     (:updated ,updated)
                     (:link :rel "alternate" :href ,(concatenate 'string base "/"))
                     (:link :rel "self" :href ,(concatenate 'string base "/feed.xml"))
                     ,(when author `(:author (:name ,author)))
                     ,@(loop for page in entries
                             for url = (absolute-url site (page-url page))
                             collect `(:entry
                                       (:title ,(page-title page))
                                       (:id ,url)
                                       (:updated ,(feed-timestamp (page-date page)))
                                       (:link :rel "alternate" :href ,url)
                                       (:summary :type "html"
                                                 ,(page-excerpt-html page))))))
                  (string #\Newline))))))))

(defun sitemap (site)
  "Return the sitemap output at /sitemap.xml."
  (let ((base (base-url site)))
    (when base
      (list
       (output "sitemap.xml"
               (concatenate
                'string
                (xml-declaration)
                (serialize-xml
                 `(:urlset
                   :xmlns "http://www.sitemaps.org/schemas/sitemap/0.9"
                   ,@(loop for page in (site-pages site)
                           collect `(:url
                                     (:loc ,(absolute-url site (page-url page)))
                                     ,(when (page-date page)
                                        `(:lastmod ,(page-date page)))))))
                (string #\Newline)))))))

(defun robots-txt (site)
  "Return crawler policy output at /robots.txt.

The policy is advisory and deliberately short. The set of useful user-agent
names changes faster than anchor is released. Enforcement belongs at the
server, which anchor does not configure."
  (let ((base (base-url site)))
    (list (output "robots.txt"
                  (format nil "User-agent: *~%Allow: /~%~@[~%Sitemap: ~a/sitemap.xml~%~]"
                          base)))))

(defun tag-id (tag)
  "Return TAG as an element ID.

Lowercase letters and digits stay as they are. Other characters become
hyphens.

This is exported because a layout that links a tag to its place on /tags/ must
produce the same ID that this page uses. Reimplementing the rule is how the
two values drift apart."
  (string-downcase
   (map 'string
        (lambda (character)
          (if (alphanumericp character) character #\-))
        tag)))

(defun generated-page (site url title)
  "Return a page that anchor writes instead of reads.

The tag index has a title and URL but no source file. Code that renders a page
still needs a PAGE object. The generated page uses the root source path, so
the root configuration applies to it.

This is exported because a custom emitter that writes a page needs it.
SPEC.adoc says a site can write an emitter for pages anchor defers. Without
this function, that offer cannot be used outside the package."
  (make-page :source (merge-pathnames "index.adoc" (site-root site))
             :url url
             :document (adoc:parse-string (format nil "= ~a~%" title))))

(defun tags-page (site)
  "Return one /tags/ page with all tags and their pages.

Do not generate one page per tag. Output filenames are stable, so each tag
page would create a permanent URL, including for tags that later prove to be
mistakes.

A site that wants per-tag pages writes an emitter. That is what this interface
is for."
  (let ((tags (site-tags site)))
    (when tags
      (let* ((page (generated-page site "/tags/" "Tags"))
             (body `(:article
                     (:h1 "Tags")
                     ,@(loop for tag in tags
                             collect `(:section
                                       (:h2 :id ,(tag-id tag) ,tag)
                                       (:nav
                                        (:ul ,@(mapcar #'index-row
                                                       (site-tagged site tag)))))))))
        (list (output "tags/index.html"
                      (render-document (page-shell page site body))))))))

(defparameter *emitters*
  (list #'atom-feed #'sitemap #'robots-txt #'tags-page)
  "Emitters run by a build.

This is a list so a site can add to it or replace it. This list and
*HEAD-TAGS* are anchor's full answer to feeds, crawlers, and social previews.
There is no SEO feature behind them.")

(defun emit (site)
  "Run every emitter over SITE and return the outputs."
  (loop for emitter in *emitters*
        append (funcall emitter site)))
