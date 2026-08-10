;;;; Integrity checks.
;;;;
;;;; Most checks read the reference relation. The rest are a walk plus a hash
;;;; table.
;;;;
;;;; That is why the relation exists. Once anchor knows what points at what,
;;;; "is anything broken?" and "is anything unused?" become lookups instead of
;;;; searches.
;;;;
;;;; check does not delete, move, or rewrite. It reports and returns.

(in-package #:anchor)

(defparameter *fnv-offset* 14695981039346656037)
(defparameter *fnv-prime* 1099511628211)

(defun file-bytes (path)
  "Return the contents of PATH as a byte vector."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length stream)
                             :element-type '(unsigned-byte 8))))
      (read-sequence bytes stream)
      bytes)))

(defun file-size (path)
  "Return the size of PATH in bytes."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (file-length stream)))

(defun content-hash (path)
  "Return a content hash for PATH.

The hash is for identity only. FNV-1a is not cryptographic and does not need
to be.

The hash groups files that might be identical. Files in a group are then
compared byte for byte. A collision costs a comparison, not a wrong answer.

Hashes are never used for output names. Output filenames are stable. Permanent
URLs are worth more than cache busting."
  (let ((hash *fnv-offset*))
    (loop for byte across (file-bytes path)
          do (setf hash (ldb (byte 64 0) (* (logxor hash byte) *fnv-prime*))))
    hash))

(defun partition-identical (paths)
  "Group PATHS by byte-for-byte equality."
  (let ((groups '()))
    (dolist (path paths)
      (let* ((bytes (file-bytes path))
             (cell (assoc bytes groups :test #'equalp)))
        (if cell
            (push path (cdr cell))
            (push (cons bytes (list path)) groups))))
    (mapcar (lambda (cell) (nreverse (cdr cell))) (nreverse groups))))

(defun duplicate-groups (paths)
  "Return groups of PATHS with identical content under different names."
  (let ((by-hash (make-hash-table :test #'eql))
        (duplicates '()))
    (dolist (path paths)
      (push path (gethash (content-hash path) by-hash)))
    (maphash (lambda (hash candidates)
               (declare (ignore hash))
               (when (rest candidates)
                 (dolist (group (partition-identical (nreverse candidates)))
                   (when (rest group)
                     (push group duplicates)))))
             by-hash)
    duplicates))

(defun all-directories (root)
  "Return ROOT and every directory below it, except output directories."
  (cons root
        (loop for sub in (uiop:subdirectories root)
              unless (string= "_site" (directory-name sub))
                append (all-directories sub))))

(defun checkable-files (root)
  "Return files below ROOT that could be assets.

Reserved directories are walked here even though discovery skips them. An
unused file in _media/ is exactly the kind of thing worth reporting.

Documents and configuration are not assets and are not counted."
  (loop for directory in (all-directories root)
        append (loop for file in (uiop:directory-files directory)
                     unless (or (string-equal "adoc" (pathname-type file))
                                (string= *config-file* (file-namestring file)))
                       collect file)))

(defun orphan-files (site)
  "Return files that exist but no document references.

This means referenced by no document. A stylesheet named by a layout instead
of by a page is an expected orphan.

That is why orphans are reported and never deleted. This list is advice, not
authority over what is unused."
  (let ((referenced (mapcar #'truename (referenced-assets site))))
    (remove-if (lambda (path) (member (truename path) referenced :test #'equal))
               (checkable-files (site-root site)))))

(defstruct (page-weight (:constructor make-page-weight (page html assets)))
  "The bytes one page costs a reader.

HTML is the page itself. ASSETS is referenced media."
  page
  html
  assets)

(defun page-weight-total (weight)
  (+ (page-weight-html weight) (page-weight-assets weight)))

(defun page-weights (site)
  "Return page weights for SITE, heaviest first."
  (sort
   (loop for page in (site-pages site)
         for assets = (remove-duplicates
                       (loop for reference in (site-references site)
                             when (and (eq page (reference-page reference))
                                       (eq :asset (reference-kind reference))
                                       (reference-path reference))
                               collect (reference-path reference))
                       :test #'equal)
         collect (make-page-weight page
                                   (length (render-page page site))
                                   (reduce #'+ (mapcar #'file-size assets)
                                           :initial-value 0)))
   #'> :key #'page-weight-total))

(defstruct (report (:constructor %make-report))
  "The result of a check run."
  site
  missing
  undated
  orphans
  duplicates
  weights)

(defun check (root &key drafts)
  "Build the graph for ROOT and report on integrity.

Nothing is written. check reads the same graph a build would use, but does
not render or copy output."
  (let ((site (make-site root :drafts drafts)))
    (%make-report :site site
                  :missing (missing-references site)
                  :undated (undated-in-dated-section site)
                  :orphans (orphan-files site)
                  :duplicates (duplicate-groups (checkable-files (site-root site)))
                  :weights (page-weights site))))

(defun report-passed-p (report)
  "Return true when REPORT found no site-breaking problems.

Broken references and forgotten dates make a site wrong. Orphans and
duplicates may be intentional, so they are reported but do not fail the
report."
  (and (null (report-missing report))
       (null (report-undated report))))

(defun relative-name (site path)
  (namestring (uiop:enough-pathname path (site-root site))))

(defun print-report (report &optional (stream *standard-output*))
  "Write REPORT to STREAM."
  (let ((site (report-site report)))
    (format stream "~&missing~%")
    (if (report-missing report)
        (dolist (reference (report-missing report))
          (format stream "  ~a references ~s~%"
                  (page-url (reference-page reference))
                  (reference-target reference)))
        (format stream "  none~%"))

    (format stream "~&~%undated~%")
    (if (report-undated report)
        (dolist (page (report-undated report))
          (format stream "  ~a, where every other page in the section is dated~%"
                  (page-url page)))
        (format stream "  none~%"))

    (format stream "~&~%orphans~%")
    (if (report-orphans report)
        (dolist (path (report-orphans report))
          (format stream "  ~a~%" (relative-name site path)))
        (format stream "  none~%"))

    (format stream "~&~%duplicates~%")
    (if (report-duplicates report)
        (dolist (group (report-duplicates report))
          (format stream "  ~{~a~^ = ~}~%"
                  (mapcar (lambda (path) (relative-name site path)) group)))
        (format stream "  none~%"))

    (format stream "~&~%weight~%")
    (dolist (weight (report-weights report))
      (format stream "  ~8d  ~a~@[  (~d in media)~]~%"
              (page-weight-total weight)
              (page-url (page-weight-page weight))
              (when (plusp (page-weight-assets weight))
                (page-weight-assets weight))))

    ;; This accounts for the one place anchor resolves a name by searching.
    (format stream "~&~%resolutions~%")
    (dolist (reference (site-references site))
      (when (reference-url reference)
        (format stream "  ~a: ~s -> ~a~%"
                (page-url (reference-page reference))
                (reference-target reference)
                (reference-url reference))))
    report))
