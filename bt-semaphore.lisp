;;;; bt-semaphore.lisp

(defpackage #:bt-semaphore
  (:use #:cl #:bordeaux-threads)
  (:export #:make-semaphore #:signal-semaphore #:wait-on-semaphore #:semaphore-count))

(in-package #:bt-semaphore)

;;;;;;;;;;;;;;;;;;;;;
;; semaphore class ;;
;;;;;;;;;;;;;;;;;;;;;

(defclass semaphore ()
  ((lock    :initform (bordeaux-threads:make-lock))
   (condvar :initform (bordeaux-threads:make-condition-variable))
   (count   :initarg  :count)
   (name    :initarg  :name
            :accessor :semaphore-name)))

;;;;;;;;;;;;;;;;;;;;;;;
;; generic functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric signal-semaphore (instance &optional n)
  (:documentation "Increment the count of the semaphore instance by n. If there
  are threads waiting on this semaphore, then at most n (but at least one) of
  them are woken up."))

(defgeneric wait-on-semaphore (instance)
  (:documentation "Decrement the count of the semaphore instance if the count
  would not be negative, else blocks until the semaphore can be
  decremented. Returns t on success.."))

(defgeneric semaphore-count (instance)
  (:documentation "Returns the current count of the semaphore instance."))

;;;;;;;;;;;;;
;; methods ;;
;;;;;;;;;;;;;

(defmethod signal-semaphore ((instance semaphore) &optional (n 1))
  (with-slots ((lock lock)
               (condvar condvar)
               (count count)) instance
      (bordeaux-threads:with-lock-held (lock)
        (setf count (+ count n))
        (bordeaux-threads:condition-notify condvar))))

(defmethod wait-on-semaphore ((instance semaphore))
  (with-slots ((lock lock)
               (condvar condvar)
               (count count)) instance
    (bordeaux-threads:with-lock-held (lock)
      (loop
         until (> count 0)
         do (bordeaux-threads:condition-wait condvar lock))
      (decf count)))
  t)

(defmethod semaphore-count ((instance semaphore))
  (with-slots ((lock lock)
               (count count)) instance
    (bordeaux-threads:with-lock-held (lock)
      count)))

;;;;;;;;;;;;;;;;;;;;;;
;; helper functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun make-semaphore (&key name (count 0))
  "Create a semaphore with the supplied name and count."
  (make-instance 'semaphore
                 :name name
                 :count count))
