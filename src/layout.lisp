;;;; The default layout.
;;;;
;;;; A layout is a function from a page and a site to an HTML tree.
;;;;
;;;; There is no template language. Common Lisp is the template language. A
;;;; user who wants a different layout writes a different function.
;;;;
;;;; The default layout emits semantic, class-light HTML. This is a rule, not a
;;;; preference. A stylesheet alone must be able to restyle the site
;;;; completely.
;;;;
;;;; If a default layout needs a wrapper element and a class on every node, the
;;;; layout is wrong.
;;;;
;;;; Some layouts render one page and ignore the site. Layouts that list other
;;;; pages need the site argument.

(in-package #:anchor)

(defun head-title (page site)
  "Return PAGE's title text for the HTML title element.

Qualify PAGE's title with the site title when the two differ. The root page is
usually named after the site. Repeating that name twice in a browser tab is
not useful."
  (let ((page-title (page-title page))
        (site-title (getf (site-config site page) :title)))
    (cond ((null site-title) page-title)
          ((null page-title) site-title)
          ((string= page-title site-title) page-title)
          (t (format nil "~a — ~a" page-title site-title)))))

;;; Head tags
;;;
;;; *HEAD-TAGS* is a list of functions. Each function takes a page and a site.
;;; Each function returns a form, a list of forms, or NIL.
;;;
;;; This has the same shape as the emitter list and exists for the same reason:
;;; a site can add to the list, filter it, or replace it. No member has to know
;;; about any other member.
;;;
;;; The head-tag list and emitter list are anchor's full answer to search
;;; engines and social previews. There is no SEO feature behind them.

(defun head-charset (page site)
  (declare (ignore page site))
  '(:meta :charset "utf-8"))

(defun head-viewport (page site)
  (declare (ignore page site))
  '(:meta :name "viewport" :content "width=device-width, initial-scale=1"))

(defun head-color-scheme (page site)
  "Return the color-scheme meta tag.

Light and dark mode belong to the stylesheet. anchor says the page supports
both. It ships no toggle and no JavaScript."
  (declare (ignore page site))
  '(:meta :name "color-scheme" :content "light dark"))

(defun head-title-tag (page site)
  `(:title ,(head-title page site)))

(defun head-description (page site)
  (declare (ignore site))
  (let ((description (page-description page)))
    (when description
      `(:meta :name "description" :content ,description))))

(defun head-canonical (page site)
  (let ((url (absolute-url site (page-url page))))
    (when url `(:link :rel "canonical" :href ,url))))

(defun head-open-graph (page site)
  "Return Open Graph tags for PAGE.

Open Graph controls how links to this page appear elsewhere.

Return NIL when SITE has no :base-url. Useful Open Graph values are absolute
URLs. A card built on relative URLs is worse than no card."
  (let ((url (absolute-url site (page-url page))))
    (when url
      (let ((description (page-description page))
            (site-title (getf (site-config site page) :title)))
        (remove nil
                (list `(:meta :property "og:type"
                              :content ,(if (site-children site page)
                                            "website"
                                            "article"))
                      `(:meta :property "og:title" :content ,(page-title page))
                      `(:meta :property "og:url" :content ,url)
                      (when site-title
                        `(:meta :property "og:site_name" :content ,site-title))
                      (when description
                        `(:meta :property "og:description" :content ,description))
                      (when (page-date page)
                        `(:meta :property "article:published_time"
                                :content ,(page-date page)))))))))

(defun head-twitter (page site)
  "Return Twitter card tags for PAGE.

anchor knows of no image to promote the card to a large summary card."
  (declare (ignore site))
  (let ((description (page-description page)))
    (remove nil
            (list '(:meta :name "twitter:card" :content "summary")
                  `(:meta :name "twitter:title" :content ,(page-title page))
                  (when description
                    `(:meta :name "twitter:description" :content ,description))))))

(defun head-stylesheet (page site)
  (declare (ignore page site))
  '(:link :rel "stylesheet" :href "/style.css"))

(defparameter *head-tags*
  (list #'head-charset
        #'head-viewport
        #'head-color-scheme
        #'head-title-tag
        #'head-description
        #'head-canonical
        #'head-open-graph
        #'head-twitter
        #'head-stylesheet)
  "Functions that build a page head, in order.

Replace the list to replace the head. Remove a member to drop what it
contributes.")

(defun head-tags (page site)
  "Return PAGE's head contents as element forms."
  (loop for tag in *head-tags*
        for form = (funcall tag page site)
        when form collect form))

(defun article (page site)
  "Return PAGE as an article: title, date, body."
  (declare (ignore site))
  `(:article
    (:h1 ,(page-title page))
    ;; No byline. On a single-author site it is noise. The date is what a
    ;; reader needs to decide whether a technical post is still true.
    ,(when (page-date page)
       `(:p :class "meta"
            (:time :datetime ,(page-date page) ,(page-date-human page))))
    (:raw ,(page-body-html page))))

(defun index-row (page)
  "Return one index row: linked title and optional date.

A dated list of titles is the densest useful index. Nothing else belongs in a
row."
  `(:li (:a :href ,(page-url page) ,(page-title page))
        ;; Put the separator in markup, not CSS. A stylesheet must be able to
        ;; restyle the page completely. It must not be required for the page to
        ;; read correctly.
        ,@(when (page-date page)
            `(" " (:time :datetime ,(page-date page) ,(page-date-human page))))))

(defun front-page (page site)
  "Return the site index.

It shows the newest page with its excerpt, then the rest as a list.

Full articles are not repeated here. Page weight is one reason. Duplicate
heading IDs are another. cl-adoc gives headings deterministic IDs, so two
whole articles on one page can produce duplicate element IDs. Excerpts avoid
that."
  (let* ((listed (remove page (site-chronology site)))
         (newest (first listed))
         (remaining (rest listed)))
    `(:article
      (:h1 ,(page-title page))
      (:raw ,(page-body-html page))
      ,(when newest
         `(:section
           (:h2 (:a :href ,(page-url newest) ,(page-title newest)))
           ,(when (page-date newest)
              `(:p :class "meta"
                   (:time :datetime ,(page-date newest)
                          ,(page-date-human newest))))
           (:raw ,(page-excerpt-html newest))))
      ,(when remaining
         `(:nav (:ul ,@(mapcar #'index-row remaining)))))))

(defun site-footer (page site)
  "Return the site footer, or NIL when it would be empty.

The footer is where the author belongs. On a single-author site, a byline on
every article is noise. The date is more useful there."
  (let ((author (getf (site-config site page) :author)))
    (when author
      `(:footer (:p ,author)))))

(defun page-shell (page site body)
  "Return the complete HTML tree around BODY."
  `(:html :lang "en"
          (:head ,@(head-tags page site))
          (:body ,body ,(site-footer page site))))

(defun render-document (tree)
  "Return TREE as a complete HTML document, including doctype."
  (with-output-to-string (out)
    (write-line "<!DOCTYPE html>" out)
    (serialize tree out)))
