;;;; package.lisp

(defpackage #:anchor
  (:use #:cl)
  (:export
   ;; Building
   #:build
   #:discover-pages

   ;; Pages
   #:page
   #:page-p
   #:page-source
   #:page-url
   #:page-document
   #:page-title
   #:page-date
   #:page-date-human
   #:page-draft-p
   #:page-tags
   #:page-body-html
   #:page-excerpt-html

   ;; The site graph
   #:site
   #:site-p
   #:make-site
   #:site-root
   #:site-pages
   #:site-chronology
   #:site-page
   #:site-parent
   #:site-children
   #:site-ancestors
   #:site-config
   #:site-root-config
   #:site-tags
   #:site-tagged
   #:site-references

   ;; References
   #:reference
   #:reference-p
   #:reference-page
   #:reference-node
   #:reference-target
   #:reference-kind
   #:reference-path
   #:reference-url
   #:missing-references
   #:referenced-assets

   ;; Emitters
   #:output
   #:output-p
   #:output-path
   #:output-content
   #:*emitters*
   #:emit
   #:atom-feed
   #:sitemap
   #:robots-txt
   #:tags-page
   #:base-url
   #:absolute-url

   ;; Layouts
   #:serialize
   #:serialize-xml
   #:render-document
   #:article
   #:front-page
   #:index-row
   #:page-shell
   #:head-tags
   #:head-title
   #:site-footer

   ;; Conditions
   #:anchor-error
   #:unreadable-document
   #:unreadable-document-path
   #:unreadable-configuration
   #:unreadable-configuration-path
   #:missing-reference
   #:missing-reference-url
   #:missing-reference-target
   #:missing-reference-reference))
