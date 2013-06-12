# bt-semaphore

A simple semaphore class for bordeaux-threads inspired by SBCL's semaphore.

## Usage

### Installation

```
cd ~/quicklisp/local-projects
git clone https://github.com/ralph-moeritz/bt-semaphore
```

### Usage

There are only six functions of interest at the moment:

 - `make-semaphore` creates a semaphore instance
 - `semaphore-count` returns the current count of the semaphore
 - `wait-on-semaphore` blocks until the semaphore can be decremented (ie. its
   count > 0)
 - `signal-semaphore` increments the semaphore & wakes any threads blocked by a
   call to `wait-on-semaphore`
 - `semaphore-name` is an accessor for the semaphore's name slot
 - `try-semaphore` decrements the semaphore without blocking

To illustrate, here's a tiny example:

```common-lisp
(ql:quickload 'bt-semaphore)

(defvar sem (bt-semaphore:make-semaphore))
(defvar lock (bt-semaphore:make-lock))
(defvar num 0)

;; Create 10 threads, each waiting on the semaphore.
(dotimes (_ 10)
  (bordeaux-threads:make-thread
   (lambda ()
     (bt-semaphore:wait-on-semaphore sem)
     (bordeaux-threads:with-lock-held (lock)
       (incf num)))))

;; Wake 5 of them.
(bt-semaphore:signal-semaphore sem 5)

(princ num) ;; prints 5
```

## Status

The basics are done. It's not yet a replacement for `SB-THREAD:SEMAPHORE`, but
we're getting there.

## License

Copyright (c) Ralph Möritz 2013.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

**THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.**
