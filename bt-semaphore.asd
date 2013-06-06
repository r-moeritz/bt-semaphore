;;;; bt-semaphore.asd

(asdf:defsystem #:bt-semaphore
  :description "A simple semaphore class for bordeaux-threads inspired by SBCL's semaphore."
  :author "Ralph Möritz"
  :license "MIT"
  :depends-on (#:bordeaux-threads)
  :components ((:file "bt-semaphore")))
