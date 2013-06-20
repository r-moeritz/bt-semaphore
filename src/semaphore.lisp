#|
  This file is a part of bt-semaphore project.
  Copyright (c) 2013 Ralph Möritz (ralph.moeritz@outlook.com)
|#

(in-package :bt-semaphore)

(defclass semaphore ()
  ((lock    :initform (bt:make-lock))
   (condvar :initform (bt:make-condition-variable))
   (count   :initarg  :count)
   (name    :initarg  :name
            :accessor semaphore-name)
   (waiting :initform 0)))

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

(defun make-semaphore (&key name (count 0))
  "Create a semaphore with the supplied name and count."
  (make-instance 'semaphore
                 :name name
                 :count count))
