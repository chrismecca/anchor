;;;; The command line.
;;;;
;;;; The three commands the spec names, and nothing else.
;;;;
;;;; The option parser is twenty lines and has no dependency behind it. anchor
;;;; takes a directory, three flags and a path, and a library for that would
;;;; be larger than the thing it parses.
;;;;
;;;; RUN returns an exit code and never quits, so the whole command line can
;;;; be exercised from the REPL and from tests. MAIN is the only part that
;;;; ends a process.

(in-package #:anchor)

(defparameter *version*
  (or (ignore-errors
       (asdf:component-version (asdf:find-system "anchor" nil)))
      "unknown")
  "Read from the system definition, so there is one place it is written.")

(defparameter *usage*
  "anchor -- a static site generator for AsciiDoc

usage:
  anchor build [DIR] [--output DIR] [--drafts] [--force]
  anchor serve [DIR] [--output DIR] [--port N]
  anchor check [DIR] [--drafts]

  DIR is the source directory and defaults to the current one.

options:
  --output DIR  Write the site to DIR instead of to DIR/_site/.
  --port N      Serve on N instead of 4000.
  --drafts      Include pages carrying :draft:.
  --force       Report problems and carry on rather than stopping.

serve includes drafts and rebuilds when the source changes.
")

(defun parse-options (arguments)
  "Split ARGUMENTS into positional arguments and an option plist."
  (let ((positional '())
        (options '()))
    (loop while arguments
          for argument = (pop arguments)
          do (cond
               ((string= argument "--drafts") (setf (getf options :drafts) t))
               ((string= argument "--force") (setf (getf options :force) t))
               ((string= argument "--output")
                (let ((value (pop arguments)))
                  (unless value
                    (error 'usage-error :message "--output needs a directory"))
                  (setf (getf options :output) value)))
               ((string= argument "--port")
                (let ((value (pop arguments)))
                  (unless value
                    (error 'usage-error :message "--port needs a number"))
                  (setf (getf options :port)
                        (or (parse-integer value :junk-allowed t)
                            (error 'usage-error
                                   :message (format nil "--port wants a number, not ~a"
                                                    value))))))
               ((and (> (length argument) 2) (string= "--" argument :end2 2))
                (error 'usage-error
                       :message (format nil "unknown option ~a" argument)))
               (t (push argument positional))))
    (values (nreverse positional) options)))

(defun option-policy (options)
  "The build policy OPTIONS ask for.
:strict by default rather than the *POLICY* default of :interactive. There is
no debugger behind a command line, so stopping at the offending page would
only mean stopping."
  (if (getf options :force) :permissive :strict))

(defun command-build (root options)
  (multiple-value-bind (site output)
      (build root :output (getf options :output)
                  :drafts (getf options :drafts)
                  :policy (option-policy options))
    (format t "~&built ~d page~:p into ~a~%" (length (site-pages site)) output)
    0))

(defun command-serve (root options)
  (let ((server (serve root :port (or (getf options :port) 4000)
                            :output (getf options :output))))
    (format t "~&serving ~a on http://127.0.0.1:~d/~%"
            (server-output server) (server-port server))
    (wait server)
    0))

(defun command-check (root options)
  (let ((report (check root :drafts (getf options :drafts))))
    (print-report report)
    (if (report-passed-p report) 0 1)))

(defun run (arguments)
  "Run ARGUMENTS and return an exit code."
  (handler-case
      (let ((command (first arguments)))
        (cond
          ((or (null command)
               (string= command "-h")
               (string= command "--help"))
           (write-string *usage*)
           0)
          ((string= command "--version")
           (format t "anchor ~a~%" *version*)
           0)
          (t
           (multiple-value-bind (positional options)
               (parse-options (rest arguments))
             (let ((root (or (first positional) ".")))
               (cond
                 ((string= command "build") (command-build root options))
                 ((string= command "serve") (command-serve root options))
                 ((string= command "check") (command-check root options))
                 (t (error 'usage-error
                           :message (format nil "unknown command ~a"
                                            command)))))))))
    (usage-error (problem)
      (format *error-output* "anchor: ~a~%~%~a" problem *usage*)
      2)
    (anchor-error (problem)
      (format *error-output* "anchor: ~a~%" problem)
      1)
    (file-error (problem)
      (format *error-output* "anchor: ~a~%" problem)
      1)))

(defun main ()
  "The entry point of the built executable."
  (uiop:quit (run (uiop:command-line-arguments))))
