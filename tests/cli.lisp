;;;; Command-line tests.

(in-package #:anchor/tests)

(define-test cli)

(defun run-cli (&rest arguments)
  "Run ARGUMENTS through the command line.

Return three values: exit code, standard output, and error output.

RUN returns a code instead of quitting. That is what makes this testable."
  (let* ((errors (make-string-output-stream))
         (code nil)
         (out (with-output-to-string (stream)
                (let ((*standard-output* stream)
                      (*error-output* errors))
                  (setf code (anchor:run arguments))))))
    (values code out (get-output-stream-string errors))))

(define-test (cli help)
  (multiple-value-bind (code stdout) (run-cli)
    (is = 0 code "No arguments prints usage and succeeds.")
    (true (search "anchor build" stdout)))
  (multiple-value-bind (code stdout) (run-cli "--help")
    (is = 0 code)
    (true (search "--drafts" stdout)))
  (multiple-value-bind (code stdout) (run-cli "--version")
    (is = 0 code)
    (true (search "anchor" stdout))))

(define-test (cli unknown)
  (multiple-value-bind (code stdout stderr) (run-cli "frobnicate")
    (declare (ignore stdout))
    (is = 2 code "An invalid command line exits 2. A failed run exits 1.")
    (true (search "unknown command frobnicate" stderr))
    (true (search "usage:" stderr) "The error output also shows usage."))
  (multiple-value-bind (code stdout stderr) (run-cli "build" "--nonsense")
    (declare (ignore stdout))
    (is = 2 code)
    (true (search "unknown option --nonsense" stderr)))
  (multiple-value-bind (code stdout stderr) (run-cli "build" "--output")
    (declare (ignore stdout))
    (is = 2 code)
    (true (search "--output needs a directory" stderr)
          "An option with no value is caught instead of treated as absent.")))

(define-test (cli check-exit-codes)
  (multiple-value-bind (code stdout)
      (run-cli "check" (namestring (fixture-site)))
    (is = 0 code "A sound site exits 0.")
    (true (search "resolutions" stdout) "The report goes to stdout."))
  (multiple-value-bind (code stdout)
      (run-cli "check" (namestring (broken-fixture-site)))
    (is = 1 code "A broken reference exits non-zero. CI reads this.")
    (true (search "nonexistent.png" stdout))))

(define-test (cli build)
  (let ((target (temporary-output)))
    (unwind-protect
         (multiple-value-bind (code stdout)
             (run-cli "build" (namestring (fixture-site))
                      "--output" (namestring target))
           (is = 0 code)
           (true (search "built 6 pages into" stdout))
           (true (read-output target "index.html")))
      (uiop:delete-directory-tree target :validate t
                                         :if-does-not-exist :ignore))))

(define-test (cli build-failure)
  (let ((target (temporary-output)))
    (unwind-protect
         (progn
           (multiple-value-bind (code stdout stderr)
               (run-cli "build" (namestring (broken-fixture-site))
                        "--output" (namestring target))
             (declare (ignore stdout))
             (is = 1 code "A build stops on a broken reference. The command
line uses :strict because there is no debugger to stop in.")
             (true (search "nonexistent.png" stderr)))
           (multiple-value-bind (code) (run-cli "build"
                                                (namestring (broken-fixture-site))
                                                "--output" (namestring target)
                                                "--force")
             (is = 0 code "--force reports problems and continues.")
             (true (read-output target "index.html"))))
      (uiop:delete-directory-tree target :validate t
                                         :if-does-not-exist :ignore))))

(define-test (cli missing-directory)
  (multiple-value-bind (code stdout stderr) (run-cli "check" "/no/such/place/")
    (declare (ignore stdout))
    (is = 1 code "A missing directory is an ordinary failure.")
    (true (plusp (length stderr))
          "The command reports the problem instead of showing a backtrace.")))
