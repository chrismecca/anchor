;;;; Building a site.
;;;;
;;;; Source tree in, output tree out. Assets are copied into the output rather
;;;; than served from the source, so that the output directory is
;;;; self-contained and "orphan" has an unambiguous meaning later.
;;;;
;;;; This is the walking skeleton: every page is rendered with the one layout
;;;; there is. The site graph, the configuration cascade and the emitters are
;;;; not here yet, which is why BUILD passes NIL where a site will go.

(in-package #:anchor)

(defun page-output-path (page output)
  "Where PAGE is written under OUTPUT.
A URL of \"/posts/2026/first/\" becomes \"posts/2026/first/index.html\", and
the root page becomes \"index.html\"."
  (merge-pathnames (concatenate 'string (subseq (page-url page) 1) "index.html")
                   output))

(defun page-layout (page site)
  "The layout function for PAGE.
The root page gets the front page and every other page gets the article.
This is provisional: layouts are chosen by the :layout key once the
configuration cascade exists, and this rule goes away when it does."
  (declare (ignore site))
  (if (string= (page-url page) "/") #'front-page #'article))

(defun render-page (page site)
  "PAGE as a complete HTML document."
  (with-output-to-string (out)
    (write-line "<!DOCTYPE html>" out)
    (serialize (page-shell page site (funcall (page-layout page site) page site))
               out)))

(defun write-page (page output &optional site)
  "Write PAGE into OUTPUT and return the path written."
  (let ((path (page-output-path page output)))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output
                                 :if-exists :supersede
                                 :external-format :utf-8)
      (write-string (render-page page site) stream))
    path))

(defun asset-files (root)
  "Every file under ROOT that is copied to the output verbatim.
Documents are not assets: an .adoc file is either a page, which is rendered,
or an include, which belongs to the document that includes it. Reserved names
are skipped along with the directories holding them."
  (loop for directory in (content-directories root)
        append (loop for file in (uiop:directory-files directory)
                     for name = (file-namestring file)
                     unless (or (reserved-name-p name)
                                (string-equal "adoc" (pathname-type file)))
                       collect file)))

(defun copy-assets (root output)
  "Copy every asset under ROOT into OUTPUT and return the paths written."
  (loop for file in (asset-files root)
        for target = (merge-pathnames (uiop:enough-pathname file root) output)
        do (ensure-directories-exist target)
           (uiop:copy-file file target)
        collect target))

(defun build (root &key output drafts)
  "Build the site rooted at ROOT. Returns the site that was built.
OUTPUT defaults to _site/ under ROOT, which discovery skips because it is a
reserved name, so a build never reads its own output. DRAFTS includes pages
carrying :draft:, which is what serve does and what --drafts asks for."
  (let* ((site (make-site root :drafts drafts))
         (root (site-root site))
         ;; The output directory is not resolved with the root's TRUENAME,
         ;; since it usually does not exist until the first page is written.
         (output (uiop:ensure-absolute-pathname
                  (uiop:ensure-directory-pathname
                   (or output (merge-pathnames "_site/" root)))
                  (uiop:getcwd))))
    (dolist (page (site-pages site))
      (write-page page output site))
    (copy-assets root output)
    site))
