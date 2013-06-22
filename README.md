# bt-semaphore

A simple semaphore class for bordeaux-threads inspired by SBCL's semaphore.

## Installation

```
cd ~/quicklisp/local-projects
git clone https://github.com/ralph-moeritz/bt-semaphore
```

## Usage

There are seven functions of interest at the moment:

 - `make-semaphore` creates a semaphore instance
 - `wait-on-semaphore` blocks until the semaphore can be decremented (ie. its
   count > 0) or the timeout has expired
 - `signal-semaphore` increments the semaphore & wakes n waiting threads
 - `try-semaphore` decrements the semaphore without blocking
 - `semaphore-count` returns the current count of the semaphore
 - `semaphore-waiters` returns the number of threads waiting on semaphore
 - `semaphore-name` is an accessor for the semaphore's name slot

To illustrate, here's a tiny example:

```common-lisp
(ql:quickload 'bt-semaphore)

(defun semaphore-demo ()
  (defparameter sem (bt-sem:make-semaphore))
  (defparameter lock (bt:make-lock))
  (defparameter num 0)
  (format t "num is ~d~%~%" num)
  
  (format t "spawn 10 threads with 5s timeout~%")
  (loop
    repeat 10
    do (bt:make-thread
         (lambda ()
           (if (bt-sem:wait-on-semaphore sem :timeout 5)
             (bt:with-lock-held (lock)
               (incf num))))))
  (sleep 0.5)
  (format t "there are ~d waiting threads~%~%" (bt-sem:semaphore-waiters sem))
  
  (format t "signal 5 threads~%")
  (bt-sem:signal-semaphore sem 5)
  (sleep 0.5)
  (format t "num is ~d~%" num)
  (format t "there are ~d waiting threads~%~%" (bt-sem:semaphore-waiters sem))

  (format t "5s sleep~%")
  (sleep 5)
  (format t "num is ~d~%" num) 
  (format t "there are ~d waiting threads~%~%" (bt-sem:semaphore-waiters sem))

  (format t "stubbornly try to signal 5 threads~%")
  (bt-sem:signal-semaphore sem 5)
  (format t "num is ~d~%" num) 
  (format t "there are ~d waiting threads~%" (bt-sem:semaphore-waiters sem)))
```

## Status

The basics are done. It's not yet a replacement for `SB-THREAD:SEMAPHORE`, but
we're getting there.

## Author

* Ralph Möritz (ralph.moeritz@outlook.com)

## License

Copyright (c) Ralph Möritz 2013.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

**THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.**
