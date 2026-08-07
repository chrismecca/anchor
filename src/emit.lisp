;;;; Emitters.
;;;;
;;;; An emitter is a function of a site returning a list of output files.
;;;; Every derived artifact goes through this one interface, which is why none
;;;; of them is a subsystem: the feed, the sitemap, the crawler policy and the
;;;; tag index are four ordinary functions in a list, and a site that wants a
;;;; fifth writes one.
;;;;
;;;; The feed and the sitemap need absolute URLs and so need :base-url. A site
;;;; that has not set one gets neither, because there is no such thing as a
;;;; valid feed with relative entry ids, and emitting a broken one would be
;;;; worse than emitting none.

(in-package #:anchor)

(defstruct (output (:constructor output (path content)))
  "One file an emitter produced: where it goes, and what is in it."
  path
  content)

(defun feed-timestamp (date)
  "DATE as the date-time Atom requires.
A page carries a date and not a time. Midnight UTC is the reading of that
date which does not drift when a reader is in another timezone."
  (format nil "~aT00:00:00Z" date))

(defun feed-entries (site)
  "The pages the feed carries, newest first and no more than :feed-limit.
Sections are left out. A page with children is an index of other pages, and
the front page is one too, so carrying them would deliver a reader the
listing as well as everything on it. It would also give the root page an id
identical to the feed's own, which is the same resource claimed twice.

Nothing declares that a page is a section. Having children is what makes one,
and the graph already knows."
  (let* ((limit (or (getf (site-root-config site) :feed-limit) 20))
         (articles (remove-if (lambda (page) (site-children site page))
                              (site-chronology site))))
    (subseq articles 0 (min limit (length articles)))))

(defun atom-feed (site)
  "The Atom feed, at /feed.xml."
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
  "The sitemap, at /sitemap.xml."
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
  "The crawler policy, at /robots.txt.
Advisory, and deliberately short. Which user agents are worth naming changes
faster than anchor is released, and enforcement belongs at a server anchor
does not configure."
  (let ((base (base-url site)))
    (list (output "robots.txt"
                  (format nil "User-agent: *~%Allow: /~%~@[~%Sitemap: ~a/sitemap.xml~%~]"
                          base)))))

(defun tag-id (tag)
  "TAG as an element id: lowercase, and anything else becomes a hyphen.
Exported because a layout that links a tag to its place on /tags/ has to
produce the same string this put in the id, and reimplementing the rule is
how the two come to disagree."
  (string-downcase
   (map 'string
        (lambda (character)
          (if (alphanumericp character) character #\-))
        tag)))

(defun generated-page (site url title)
  "A page anchor writes rather than reads.
The tag index has a title and a URL and no file behind it, and everything
that renders a page wants a page. Its source is the root's, so the root's
configuration is the one that applies to it.

Exported because an emitter that writes a page needs one, and writing an
emitter is what SPEC.adoc offers a site in place of the pages anchor
defers. Without this that offer cannot be taken up from outside the
package."
  (make-page :source (merge-pathnames "index.adoc" (site-root site))
             :url url
             :document (adoc:parse-string (format nil "= ~a~%" title))))

(defun tags-page (site)
  "One /tags/ page listing every tag with its pages grouped beneath it.
Not one page per tag. Output filenames are stable, so each of those would be
a URL owed permanently, including for the tags that turn out to be mistakes.
A site that wants them writes an emitter, which is what this interface is."
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
  "The emitters a build runs.
A list, so that a site can add to it or replace it wholesale. This and the
head tags are the whole of anchor's answer to feeds, crawlers and social
previews. There is no SEO feature behind them.")

(defun emit (site)
  "Run every emitter over SITE and return the outputs they produced."
  (loop for emitter in *emitters*
        append (funcall emitter site)))
