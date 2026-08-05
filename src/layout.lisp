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

(defun head-tags (page site)
  "The contents of PAGE's head, as a list of element forms.
A list rather than a block of markup so that a site can replace or extend it
by building a different list."
  (declare (ignore site))
  (list '(:meta :charset "utf-8")
        '(:meta :name "viewport" :content "width=device-width, initial-scale=1")
        ;; Light and dark are the stylesheet's business. anchor states that
        ;; the page supports both and ships no toggle and no JavaScript.
        '(:meta :name "color-scheme" :content "light dark")
        `(:title ,(page-title page))
        '(:link :rel "stylesheet" :href "/style.css")))

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

(defun page-shell (page site body)
  "The whole document around BODY."
  `(:html :lang "en"
          (:head ,@(head-tags page site))
          (:body ,body)))
