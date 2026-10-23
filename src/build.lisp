;;;; Build a site.
;;;;
;;;; The source tree goes in. The output tree comes out.
;;;;
;;;; Assets are copied into the output tree instead of served from the source
;;;; tree. The output directory is therefore self-contained, and "orphan" has
;;;; one clear meaning later.
;;;;
;;;; The build creates the site graph, reports build problems according to
;;;; *POLICY*, renders pages, runs emitters, and copies assets.

(in-package #:anchor)

(defun page-output-path (page output)
  "Return where PAGE is written below OUTPUT.

A URL of \"/posts/2026/first/\" becomes
\"posts/2026/first/index.html\". The root page becomes \"index.html\"."
  (merge-pathnames (concatenate 'string (subseq (page-url page) 1) "index.html")
                   output))

(defun page-layout (page site)
  "Return the layout function for PAGE.

Use the cascaded :layout value when one is configured. The value can be a
function or a symbol that names one.

Without configuration, the root page uses FRONT-PAGE and every other page uses
ARTICLE. A site with no _anchor.lisp still gets an index page."
  (let ((configured (getf (site-config site page) :layout)))
    (cond ((functionp configured) configured)
          (configured (fdefinition configured))
          ((string= (page-url page) "/") #'front-page)
          (t #'article))))

(defun render-page (page site)
  "Return PAGE as a complete HTML document."
  (render-document
   (page-shell page site (funcall (page-layout page site) page site))))

(defun write-page (page output &optional site)
  "Write PAGE below OUTPUT and return the path written."
  (let ((path (page-output-path page output)))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output
                                 :if-exists :supersede
                                 :external-format :utf-8)
      (write-string (render-page page site) stream))
    path))

(defparameter *policy* :interactive
  "How a build handles a problem.

:strict:: Abort on any condition. CI uses this.
:permissive:: Report the condition and continue. --force uses this.
:interactive:: Signal the condition with a restart available. A REPL build
  stops at the offending page with live state, and a person decides. In a
  script with no debugger, this aborts like :strict.

:interactive is the default because it is one reason anchor is written in
Lisp.")

(defun report-problem (condition)
  "Handle CONDITION according to *POLICY*."
  (ecase *policy*
    (:strict (error condition))
    (:permissive (warn "~a" condition))
    (:interactive
     (with-simple-restart (continue "Leave it as written and go on.")
       (error condition)))))

(defun report-missing-references (site)
  "Signal MISSING-REFERENCE for each unresolved target in SITE.

Reference resolution only classifies. This is where a broken reference becomes
a build problem. *POLICY* decides how serious that problem is."
  (dolist (reference (missing-references site))
    (report-problem
     (make-condition 'missing-reference
                     :reference reference
                     :url (page-url (reference-page reference))
                     :target (reference-target reference)))))

(defun report-missing-dates (site)
  "Signal MISSING-DATE for each page in SITE that should have a date."
  (dolist (page (undated-in-dated-section site))
    (report-problem (make-condition 'missing-date :url (page-url page)))))

(defun write-output (file output)
  "Write emitter output FILE below OUTPUT and return the path written."
  (let ((path (merge-pathnames (output-path file) output)))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output
                                 :if-exists :supersede
                                 :external-format :utf-8)
      (write-string (output-content file) stream))
    path))

(defun asset-files (root)
  "Return files below ROOT that are copied to output without changes.

Documents are not assets. An .adoc file is either a page, which is rendered,
or an include, which belongs to the document that includes it.

Reserved and hidden names are skipped, along with the directories that have
them."
  (loop for directory in (content-directories root)
        append (loop for file in (uiop:directory-files directory)
                     for name = (file-namestring file)
                     unless (or (reserved-name-p name)
                                (hidden-name-p name)
                                (string-equal "adoc" (pathname-type file)))
                       collect file)))

(defun site-asset-files (site)
  "Return files SITE writes to output without changes.

Ordinary assets are copied whether or not a document points at them. An
unreferenced ordinary asset is something check reports, not something build
silently drops.

Reserved directories are different. _media/ holds shared assets for a subtree.
Only referenced files in reserved directories are copied. Adding a file there
costs nothing until a page names it."
  (let ((root (site-root site)))
    (append (asset-files root)
            (remove-if-not (lambda (path)
                             (reserved-path-p path root))
                           (referenced-assets site)))))

(defun reserved-path-p (path root)
  "Return true when PATH is below a reserved directory of ROOT."
  (let ((relative (uiop:enough-pathname path root)))
    (some #'reserved-name-p (rest (pathname-directory relative)))))

(defun copy-assets (site output)
  "Copy SITE's assets into OUTPUT and return the paths written."
  (let ((root (site-root site)))
    (loop for file in (site-asset-files site)
          for target = (merge-pathnames (uiop:enough-pathname file root) output)
          do (ensure-directories-exist target)
             (uiop:copy-file file target)
          collect target)))

(defun build (root &key output drafts (policy *policy*))
  "Build the site rooted at ROOT.

Return the site and the output directory.

OUTPUT defaults to _site/ below ROOT. Discovery skips it because it is a
reserved name, so a build never reads its own output.

DRAFTS includes pages with :draft:. serve uses this, and --drafts asks for it.

POLICY is bound for the whole build. One setting governs every problem the
build reports."
  (let* ((*policy* policy)
         (site (make-site root :drafts drafts))
         (root (site-root site))
         ;; Do not resolve the output directory with ROOT's TRUENAME. It often
         ;; does not exist until the first page is written.
         (output (uiop:ensure-absolute-pathname
                  (uiop:ensure-directory-pathname
                   (or output (merge-pathnames "_site/" root)))
                  (uiop:getcwd))))
    (report-missing-references site)
    (report-missing-dates site)
    (dolist (page (site-pages site))
      (write-page page output site))
    (dolist (file (emit site))
      (write-output file output))
    (copy-assets site output)
    (values site output)))
