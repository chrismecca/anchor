;;;; The configuration cascade.
;;;;
;;;; An _anchor.lisp in any directory configures that directory and every
;;;; directory below it. Configuration is merged parent to child, so a
;;;; subdirectory can override its ancestors without a central registry.
;;;;
;;;; The file is evaluated instead of read as data. Earlier forms can therefore
;;;; do work, such as defining the layout function named by the final plist's
;;;; :layout key.
;;;;
;;;; That is why this is a Lisp file, not an ini file. It also means anchor
;;;; runs code from the source tree. The source tree is assumed to be the
;;;; user's own. anchor is not a sandbox and does not try to be one.
;;;;
;;;; _media/ resolution uses the same cascade. anchor has one cascade rule, and
;;;; both configuration and shared media use it.

(in-package #:anchor)

(defparameter *config-file* "_anchor.lisp"
  "Name of a configuration file.")

(defun load-config (path)
  "Evaluate every form in PATH and return the value of the last form.

*PACKAGE* is bound during evaluation. An IN-PACKAGE form in a configuration
file affects that file and stops at the file boundary."
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
  "Return the configuration declared in DIRECTORY.

Ignore ancestor configuration."
  (let ((path (merge-pathnames *config-file* directory)))
    (when (probe-file path)
      (load-config path))))

(defun config-keys (config)
  "Return the keys of CONFIG in source order."
  (loop for (key nil) on config by #'cddr collect key))

(defun config-has-key-p (config key)
  "Return true when CONFIG contains KEY.

GETF cannot answer this. A present key with a NIL value and an absent key both
read back as NIL. Configuration can set a key to NIL to disable a value
enabled by an ancestor."
  (not (null (get-properties config (list key)))))

(defun merge-config (parent child)
  "Merge PARENT and CHILD, with CHILD taking precedence.

Keep the parent's key order. Append keys introduced by CHILD in CHILD's order.
This makes the merge stable.

SETF GETF would be shorter, but would put new keys at the front. Then the
result would depend on which file happened to declare a key."
  (let ((keys '()))
    (dolist (key (config-keys parent)) (pushnew key keys))
    (dolist (key (config-keys child)) (pushnew key keys))
    (loop for key in (nreverse keys)
          append (list key (if (config-has-key-p child key)
                               (getf child key)
                               (getf parent key))))))

(defun directory-configs (root)
  "Return a table from directory namestring to merged configuration.

The table is built by descending from ROOT and carrying the accumulated
configuration. That is the cascade: each directory gets its ancestors'
configuration with its own configuration laid over it."
  (let ((table (make-hash-table :test #'equal)))
    (labels ((walk (directory inherited)
               (let ((config (merge-config inherited
                                           (directory-config directory))))
                 (setf (gethash (namestring directory) table) config)
                 (dolist (sub (uiop:subdirectories directory))
                   (let ((name (directory-name sub)))
                     (unless (or (reserved-name-p name) (hidden-name-p name))
                       (walk sub config)))))))
      (walk root nil))
    table))
