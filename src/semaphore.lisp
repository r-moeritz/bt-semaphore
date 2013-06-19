;;;; semaphore.lisp

(in-package #:bt-semaphore)

;;;;;;;;;;;;;;;;;;;;;
;; semaphore class ;;
;;;;;;;;;;;;;;;;;;;;;

(defclass semaphore ()
  ((lock    :initform (bt:make-lock))
   (condvar :initform (bt:make-condition-variable))
   (count   :initarg  :count)
   (name    :initarg  :name
            :accessor semaphore-name)
   (waiting :initform 0)))

;;;;;;;;;;;;;;;;;;;;;;;
;; generic functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric signal-semaphore (instance &optional n)
  (:documentation "Increments the count of the semaphore instance by n. If
  there are threads waiting on this semaphore, then n of them are woken up."))

(defgeneric wait-on-semaphore (instance)
  (:documentation "Decrements the count of the semaphore instance if the count
  would not be negative, else blocks until the semaphore can be
  decremented. Returns t on success."))

(defgeneric semaphore-count (instance)
  (:documentation "Returns the current count of the semaphore instance."))

(defgeneric try-semaphore (instance &optional n)
  (:documentation "Try to decrement the count of semaphore by n. Returns nil if
  the count were to become negative, otherwise returns t."))

;;;;;;;;;;;;;
;; methods ;;
;;;;;;;;;;;;;

(defmethod signal-semaphore ((instance semaphore) &optional (n 1))
  (with-slots ((lock lock)
               (condvar condvar)
               (count count)
               (waiting waiting)) instance
      (bt:with-lock-held (lock)
        (setf count (+ count n))
        (dotimes (_ waiting)
          (bt:condition-notify condvar)))))

(defmethod wait-on-semaphore ((instance semaphore))
  (with-slots ((lock lock)
               (condvar condvar)
               (count count)
               (waiting waiting)) instance
    (bt:with-lock-held (lock)
      (incf waiting)
      (loop
         until (> count 0)
         do (bt:condition-wait condvar lock))
      (decf waiting)
      (decf count)))
  t)

(defmethod semaphore-count ((instance semaphore))
  (with-slots ((lock lock)
               (count count)) instance
    (bt:with-lock-held (lock)
      count)))

(defmethod try-semaphore ((instance semaphore) &optional (n 1))
  (with-slots ((lock lock)
               (count count)) instance
    (bt:with-lock-held (lock)
      (if (< (- count n) 0)
          nil
          (progn 
            (setf count (- count n))
            t)))))

;;;;;;;;;;;;;;;;;;;;;;
;; helper functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun make-semaphore (&key name (count 0))
  "Create a semaphore with the supplied name and count."
  (make-instance 'semaphore
                 :name name
                 :count count))
