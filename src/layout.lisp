;;;; The default layout.
;;;;
;;;; A layout is a function of a page and a site returning an HTML tree. There
;;;; is no template language; Common Lisp is the template language, and a user
;;;; who wants a different layout writes a different function.
;;;;
;;;; What ships here emits semantic, class-light HTML. That is an obligation
;;;; rather than a preference: a stylesheet alone has to be able to restyle
;;;; the site completely, so if a default layout needs a wrapper element and a
;;;; class on every node, the layout is wrong.
;;;;
;;;; A layout that renders one page in isolation ignores the site. The ones
;;;; that list other pages are why the argument is there.

(in-package #:anchor)

(defun head-title (page site)
  "The text of PAGE's title element.
The page's own title, qualified by the site's when the two differ. The root
page is usually named after the site, and repeating that name twice in a
browser tab helps nobody."
  (let ((page-title (page-title page))
        (site-title (getf (site-config site page) :title)))
    (cond ((null site-title) page-title)
          ((null page-title) site-title)
          ((string= page-title site-title) page-title)
          (t (format nil "~a — ~a" page-title site-title)))))

;;; Head tags
;;;
;;; A list of functions of a page and a site, each returning a form, a list of
;;; forms, or NIL. The same shape as the emitter list and for the same reason:
;;; a list is something a site can add to, filter, or replace outright, and
;;; none of the members has to know about any other.
;;;
;;; This list and the emitter list are the whole of anchor's answer to search
;;; engines and social previews. There is no SEO feature behind them.

(defun head-charset (page site)
  (declare (ignore page site))
  '(:meta :charset "utf-8"))

(defun head-viewport (page site)
  (declare (ignore page site))
  '(:meta :name "viewport" :content "width=device-width, initial-scale=1"))

(defun head-color-scheme (page site)
  "Light and dark are the stylesheet's business.
anchor says the page supports both and ships no toggle and no JavaScript."
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
  "Open Graph, which is what a link to this page looks like elsewhere.
Absent entirely without a :base-url, since every useful member of it is an
absolute URL and a card built on relative ones is worse than no card."
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
  "A summary card. anchor knows of no image to promote it to a larger one."
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
  "The functions that build a page's head, in order.
Replace the list to replace the head, or remove a member to drop what it
contributes.")

(defun head-tags (page site)
  "The contents of PAGE's head, as a list of element forms."
  (loop for tag in *head-tags*
        for form = (funcall tag page site)
        when form collect form))

(defun article (page site)
  "PAGE as an article: title, date, body, in that order."
  (declare (ignore site))
  `(:article
    (:h1 ,(page-title page))
    ;; No byline. On a single-author site it is noise, and the date is what a
    ;; reader needs to judge whether a technical post is still true.
    ,(when (page-date page)
       `(:p :class "meta"
            (:time :datetime ,(page-date page) ,(page-date-human page))))
    (:raw ,(page-body-html page))))

(defun index-row (page)
  "One row of an index: a linked title and a date, and nothing else.
A dated list of titles is the densest useful form an index can take, so
nothing else goes in one."
  `(:li (:a :href ,(page-url page) ,(page-title page))
        ;; The separator is in the markup rather than left to the stylesheet.
        ;; A stylesheet has to be able to restyle the page completely; it does
        ;; not have to be present for the page to read.
        ,@(when (page-date page)
            `(" " (:time :datetime ,(page-date page) ,(page-date-human page))))))

(defun front-page (page site)
  "The site index: the newest page with its excerpt, then the rest as a list.
Full articles are not repeated here. Beyond page weight, cl-adoc gives
headings deterministic ids, so two whole articles in one document would
produce duplicate element ids. An excerpt cannot."
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
  "The site footer, or NIL when there is nothing to put in one.
This is where the author goes. A byline on every article is noise on a
single-author site, and the date is what a reader needs there instead."
  (let ((author (getf (site-config site page) :author)))
    (when author
      `(:footer (:p ,author)))))

(defun page-shell (page site body)
  "The whole document around BODY."
  `(:html :lang "en"
          (:head ,@(head-tags page site))
          (:body ,body ,(site-footer page site))))

(defun render-document (tree)
  "TREE as a complete HTML document, doctype and all."
  (with-output-to-string (out)
    (write-line "<!DOCTYPE html>" out)
    (serialize tree out)))
