#|
  This file is a part of bt-semaphore project.
  Copyright (c) 2013 Ralph Möritz (ralph.moeritz@outlook.com)
|#

(in-package :bt-semaphore-test)

;;;; Define test suites

(defsuite make-semaphore-suite ())
(defsuite signal-semaphore-suite ())
(defsuite wait-on-semaphore-suite ())
(defsuite try-semaphore-suite ())

;;;; Define tests

;; make-semaphore

(defun assert-semaphore-count-eql (count &optional init-semaphore-count)
  (let ((sem (if init-semaphore-count
                 (make-semaphore :count count)
                 (make-semaphore))))
    (assert-eql count (semaphore-count sem))))

(defun assert-semaphore-name-equal (name &optional init-semaphore-name)
  (let ((sem (if init-semaphore-name
                 (make-semaphore :name name)
                 (make-semaphore))))
    (assert-equal name (semaphore-name sem))))

(deftest make-semaphore-sans-count (make-semaphore-suite)
  (assert-semaphore-count-eql 0))

(deftest make-semaphore-with-count-1 (make-semaphore-suite)
  (assert-semaphore-count-eql 1 t))

(deftest make-unnamed-semaphore (make-semaphore-suite)
  (assert-semaphore-name-equal nil))

(deftest make-named-semaphore (make-semaphore-suite)
  (assert-semaphore-name-equal "sem" t))

;;;; Run tests

(run-suite 'make-semaphore-suite :use-debugger t)
