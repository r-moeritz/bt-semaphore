;;;; package.lisp

(defpackage #:bt-semaphore
  (:nicknames #:bt-sem)
  (:use #:cl #:bordeaux-threads)
  (:export #:make-semaphore #:signal-semaphore #:wait-on-semaphore 
           #:semaphore-count #:semaphore-name #:try-semaphore))
