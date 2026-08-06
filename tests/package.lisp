;;;; package.lisp

(defpackage #:anchor/tests
  (:use #:cl #:parachute)
  (:export #:run))

(in-package #:anchor/tests)

(defun fixture-site ()
  "The sample site the discovery and build tests run against."
  (asdf:system-relative-pathname :anchor "tests/fixtures/site/"))

(defun broken-fixture-site ()
  "A site with one reference that resolves to nothing.
Separate from the sample site, since a broken reference there would fail
every test that builds it."
  (asdf:system-relative-pathname :anchor "tests/fixtures/broken/"))

(defun undated-fixture-site ()
  "A site where one page in a dated section has no date."
  (asdf:system-relative-pathname :anchor "tests/fixtures/undated/"))
