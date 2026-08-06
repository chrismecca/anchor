;;;; The configuration cascade.
;;;;
;;;; An _anchor.lisp in any directory configures that directory and everything
;;;; beneath it, merged parent to child, so a subdirectory overrides its
;;;; ancestors and no central registry expresses that it does.
;;;;
;;;; The file is evaluated rather than read as data, so the forms before the
;;;; last one can do something -- defining the layout function that the last
;;;; one's :layout key goes on to name. That is why this is a Lisp file and
;;;; not an ini file, and it means anchor runs code out of the source tree.
;;;; The source tree is assumed to be the user's own. anchor is not a sandbox
;;;; and does not try to be one.
;;;;
;;;; This is also the cascade that _media/ resolution uses. There is one such
;;;; rule in anchor and both things are meant to walk by it.

(in-package #:anchor)

(defparameter *config-file* "_anchor.lisp"
  "The name of a configuration file.")

(defun load-config (path)
  "Evaluate every form in PATH and return the value of the last one.
*PACKAGE* is bound around the evaluation, so an IN-PACKAGE inside a
configuration file affects that file and stops at its end."
  (handler-case
      (with-open-file (stream path)
        (let ((*package* *package*)
              (eof '#:eof)
              (value nil))
          (loop for form = (read stream nil eof)
                until (eq form eof)
                do (setf value (eval form)))
          value))
    (error (cause)
      (error 'unreadable-configuration :path path :cause cause))))

(defun directory-config (directory)
  "The configuration declared in DIRECTORY itself, ignoring its ancestors."
  (let ((path (merge-pathnames *config-file* directory)))
    (when (probe-file path)
      (load-config path))))

(defun config-keys (config)
  "The keys of CONFIG, in the order they appear."
  (loop for (key nil) on config by #'cddr collect key))

(defun config-has-key-p (config key)
  "Whether CONFIG names KEY at all.
GETF cannot answer this, since a key present with a NIL value and a key that
is absent both read back as NIL, and a configuration is allowed to set a key
to NIL in order to switch off what an ancestor turned on."
  (not (null (get-properties config (list key)))))

(defun merge-config (parent child)
  "PARENT and CHILD merged, with CHILD winning where both name a key.
The parent's key order is kept and keys the child introduces are appended in
its own order, so merging the same two configurations always produces the
same list. SETF GETF would be shorter and would put new keys at the front,
which makes the result depend on which file happened to declare what."
  (let ((keys '()))
    (dolist (key (config-keys parent)) (pushnew key keys))
    (dolist (key (config-keys child)) (pushnew key keys))
    (loop for key in (nreverse keys)
          append (list key (if (config-has-key-p child key)
                               (getf child key)
                               (getf parent key))))))

(defun directory-configs (root)
  "A table of directory namestring to merged configuration.
Built by descending from ROOT and carrying what has accumulated so far, which
is the cascade itself: a directory's configuration is its ancestors' with its
own laid over the top."
  (let ((table (make-hash-table :test #'equal)))
    (labels ((walk (directory inherited)
               (let ((config (merge-config inherited
                                           (directory-config directory))))
                 (setf (gethash (namestring directory) table) config)
                 (dolist (sub (uiop:subdirectories directory))
                   (unless (reserved-name-p (directory-name sub))
                     (walk sub config))))))
      (walk root nil))
    table))
