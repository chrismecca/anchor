;;;; The command line.

(in-package #:anchor/tests)

(define-test cli)

(defun run-cli (&rest arguments)
  "Run ARGUMENTS through the command line.
Returns the exit code, what went to standard output, and what went to error
output. RUN returns a code rather than quitting, which is what makes this
possible at all."
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
    (is = 2 code "An unusable command line exits 2, apart from a failed run.")
    (true (search "unknown command frobnicate" stderr))
    (true (search "usage:" stderr) "And says what the usable ones are."))
  (multiple-value-bind (code stdout stderr) (run-cli "build" "--nonsense")
    (declare (ignore stdout))
    (is = 2 code)
    (true (search "unknown option --nonsense" stderr)))
  (multiple-value-bind (code stdout stderr) (run-cli "build" "--output")
    (declare (ignore stdout))
    (is = 2 code)
    (true (search "--output needs a directory" stderr)
          "An option that swallowed its value is caught rather than treated
as though it were absent.")))

(define-test (cli check-exit-codes)
  (multiple-value-bind (code stdout)
      (run-cli "check" (namestring (fixture-site)))
    (is = 0 code "A sound site exits 0.")
    (true (search "resolutions" stdout) "And the report goes to stdout."))
  (multiple-value-bind (code stdout)
      (run-cli "check" (namestring (broken-fixture-site)))
    (is = 1 code "A broken reference exits non-zero, which is what CI reads.")
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
             (is = 1 code "A build stops on a broken reference, because the
command line runs :strict where there is no debugger to stop at.")
             (true (search "nonexistent.png" stderr)))
           (multiple-value-bind (code) (run-cli "build"
                                                (namestring (broken-fixture-site))
                                                "--output" (namestring target)
                                                "--force")
             (is = 0 code "--force reports and carries on.")
             (true (read-output target "index.html"))))
      (uiop:delete-directory-tree target :validate t
                                         :if-does-not-exist :ignore))))

(define-test (cli missing-directory)
  (multiple-value-bind (code stdout stderr) (run-cli "check" "/no/such/place/")
    (declare (ignore stdout))
    (is = 1 code "A directory that is not there is an ordinary failure.")
    (true (plusp (length stderr))
          "And it says so rather than showing a backtrace.")))
