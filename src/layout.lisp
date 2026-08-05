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
;;;; The SITE argument is accepted and unused. The site graph is not built
;;;; yet, and the layouts that need it are the ones that list other pages.

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

(defun page-shell (page site body)
  "The whole document around BODY."
  `(:html :lang "en"
          (:head ,@(head-tags page site))
          (:body ,body)))
