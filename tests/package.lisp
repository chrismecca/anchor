;;;; package.lisp

(defpackage #:anchor/tests
  (:use #:cl #:parachute)
  (:export #:run))

(in-package #:anchor/tests)

(defun fixture-site ()
  "The sample site the discovery and build tests run against."
  (asdf:system-relative-pathname :anchor "tests/fixtures/site/"))
