;;;; callback tests with side effects

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(in-package :cl-user)

;;; callbacks only on a few platforms
;;; (actually, all platforms claim to support them now,
;;; and :alien-callbacks is almost everywhere defined.
;;; However mips doesn't seem to correctly implement them,
;;; making the feature indicator somewhat useless)
#+(or (not alien-callbacks) mips) (invoke-restart 'run-tests::skip-file)

;;; simple callback for a function

(defun thunk ()
  (write-string "hi"))

(defvar *thunk*
  (sb-alien::alien-callback (function c-string) #'thunk))

(with-test (:name (:callback :c-string)
            ;; The whole file is broken, report one test
            ;; and skip the rest.
            :broken-on :interpreter)
  (assert (equal (with-output-to-string (*standard-output*)
                   (alien-funcall *thunk*))
                 "hi")))

;;; Simple test parameter passing

(defun a-num (x)
  (format t "x=~A" x))

(defvar *a-num*
  (sb-alien::alien-callback (function c-string int) #'a-num))

(with-test (:name (:callback :a-num)
            ;; The whole file is broken, report one test
            ;; and skip the rest.
            :broken-on :interpreter)
  (assert (equal (with-output-to-string (*standard-output*)
                   (alien-funcall *a-num* 42))
                 "x=42")))

;; WITH-ALIEN is broken when interpreted, e.g.
;; (with-alien ((x int 10)) x), see lp#992362, lp#1731556
(when (eq sb-ext:*evaluator-mode* :interpret)
  (invoke-restart 'run-tests::skip-file))

;;; simple callback for a symbol

(defun add-two-ints (arg1 arg2)
  (+ arg1 arg2))

(defvar *add-two-ints*
  (sb-alien::alien-callback (function int int int) 'add-two-ints))

(assert (= (alien-funcall *add-two-ints* 555 444444) 444999))

;;; actually using a callback with foreign code

#+win32 (sb-alien:load-shared-object "ntdll.dll")

(define-alien-routine qsort void
  (base (* t))
  (nmemb int)
  (size int)
  (compar (function int (* double) (* double))))

(define-alien-callable double*-cmp int ((arg1 (* double)) (arg2 (* double)))
  (let ((a1 (deref arg1))
        (a2 (deref arg2)))
    (cond ((= a1 a2) 0)
          ((< a1 a2) -1)
          (t 1))))

(let* ((vector (coerce '(0.1d0 0.5d0 0.2d0 1.2d0 1.5d0 2.5d0 0.0d0 0.1d0 0.2d0 0.3d0)
                       '(vector double-float)))
       (sorted (sort (copy-seq vector) #'<)))
  (gc :full t)
  (sb-sys:with-pinned-objects (vector)
    (qsort (sb-sys:vector-sap vector)
           (length vector)
           (alien-size double :bytes)
           (alien-callable-function 'double*-cmp)))
  (assert (equalp vector sorted)))

;;; returning floats

(define-alien-callable redefined-fun int ()
  0)

(eval
 '(define-alien-callable redefined-fun int ()
   42))

(assert (= 42 (alien-funcall (alien-callable-function 'redefined-fun))))

(define-alien-callable return-single float ((x float))
  x)

(define-alien-callable return-double double ((x double))
  x)

(defconstant spi (coerce pi 'single-float))

(assert (= spi (alien-funcall (alien-callable-function 'return-single) spi)))
(assert (= pi (alien-funcall (alien-callable-function 'return-double) pi)))


;;; redefining and invalidating alien callables

(define-alien-callable foo int ()
  13)

(defvar *old-foo* (alien-callable-function 'foo))

(multiple-value-bind (p valid) (sb-alien::alien-callback-p *old-foo*)
  (assert p)
  (assert valid))

(assert (= 13 (alien-funcall *old-foo*)))

(define-alien-callable foo int ()
  26)

(multiple-value-bind (p valid) (sb-alien::alien-callback-p *old-foo*)
  (assert p)
  (assert (not valid)))

(multiple-value-bind (res err)
    (ignore-errors (alien-funcall *old-foo*))
  (assert (and (not res) (typep err 'error))))

(assert (= 26 (alien-funcall (alien-callable-function 'foo))))

;;; callbacks with void return values

(with-test (:name :void-return)
  (sb-alien::alien-lambda void ()
    (values)))

;;; tests for integer-width problems in callback result handling

(defvar *add-two-ints*
  (sb-alien::alien-callback (function int int int) #'+))
(defvar *add-two-shorts*
  (sb-alien::alien-callback (function short short short) #'+))

;;; The original test cases here were what are now (:int-result
;;; :sign-extension) and (:int-result :underflow-detection), the latter
;;; of which would fail on 64-bit platforms.  Upon further investigation,
;;; it turned out that the same tests with a SHORT return type instead of
;;; an INT return type would also fail on 32-bit platforms.

(with-test (:name (:short-result :sign-extension))
  (assert (= (alien-funcall *add-two-shorts* #x-8000 1) -32767)))

(with-test (:name (:short-result :underflow-detection))
  (assert-error (alien-funcall *add-two-shorts* #x-8000 -1)))

(with-test (:name (:int-result :sign-extension))
  (assert (= (alien-funcall *add-two-ints* #x-80000000 1) -2147483647)))

(with-test (:name (:int-result :underflow-detection))
  (assert-error (alien-funcall *add-two-ints* #x-80000000 -1)))

;;; tests for handling 64-bit arguments - this was causing problems on
;;; ppc - CLH, 2005-12-01

(defvar *add-two-long-longs*
  (sb-alien::alien-callback
   (function (integer 64) (integer 64) (integer 64)) 'add-two-ints))
(with-test (:name :long-long-callback-arg)
  (assert (= (alien-funcall *add-two-long-longs*
                            (ash 1 60)
                            (- (ash 1 59)))
             (ash 1 59))))

(defvar *add-two-unsigned-long-longs*
  (sb-alien::alien-callback
   (function (unsigned 64) (unsigned 64) (unsigned 64))
   'add-two-ints))
(with-test (:name :unsigned-long-long-callback-arg)
  (assert (= (alien-funcall *add-two-unsigned-long-longs*
                            (ash 1 62)
                            (ash 1 62))
             (ash 1 63))))

;;; test for callbacks of various arities
;;; CLH 2005-12-21

(defmacro alien-apply-form (f args)
  `(let ((a ,args))
     `(alien-funcall ,,f ,@a)))

(defmacro alien-apply (f &rest args)
  `(eval (alien-apply-form ,f ,@args)))

(defun iota (x) (if (equalp x 1) (list x) (cons x (iota (1- x)))))

(defparameter *type-abbreviations*
  '((sb-alien:char . "c")
    (sb-alien:unsigned-char . "uc")
    (sb-alien:short . "h")
    (sb-alien:unsigned-short . "uh")
    (sb-alien:int . "i")
    (sb-alien:unsigned-int . "ui")
    ((sb-alien:integer 64) . "l")
    ((sb-alien:unsigned 64) . "ul")
    (sb-alien:long-long . "ll")
    (sb-alien:unsigned-long-long . "ull")
    (sb-alien:float . "f")
    (sb-alien:double . "d")))

(defun parse-callback-arg-spec (spec)
  (let ((l (coerce spec 'list)))
    (loop for g in l by #'cddr
       collect (car (rassoc (string-downcase g) *type-abbreviations* :test #'equal)))))

(defmacro define-callback-adder (&rest types)
  (let ((fname (format nil "*add-~{~A~^-~}*"
                       (mapcar
                        #'(lambda (x)
                            (cdr (assoc x *type-abbreviations*)))
                        (mapcar
                         #'(lambda (y) (find-symbol (string-upcase y) 'sb-alien))
                         (cdr types))))))
    `(progn
      (defparameter ,(intern
                      (string-upcase fname))
        (sb-alien::alien-callback (function ,@types) '+)))))

(with-test (:name :define-2-int-callback)
  (define-callback-adder int int int))
(with-test (:name :call-2-int-callback)
  (assert (= (alien-apply *add-i-i* (iota 2)) 3)))

(with-test (:name :define-3-int-callback)
  (define-callback-adder int int int int))
(with-test (:name :call-3-int-callback)
  (assert (= (alien-apply *add-i-i-i* (iota 3)) 6)))

(with-test (:name :define-4-int-callback)
  (define-callback-adder int int int int int))
(with-test (:name :call-4-int-callback)
  (assert (= (alien-apply *add-i-i-i-i* (iota 4)) 10)))

(with-test (:name :define-5-int-callback)
  (define-callback-adder int int int int int int))
(with-test (:name :call-5-int-callback)
  (assert (= (alien-apply *add-i-i-i-i-i* (iota 5)) 15)))

(with-test (:name :define-6-int-callback)
  (define-callback-adder int int int int int int int))
(with-test (:name :call-6-int-callback)
  (assert (= (alien-apply *add-i-i-i-i-i-i* (iota 6)) 21)))

(with-test (:name :define-7-int-callback)
  (define-callback-adder int int int int int int int int))
(with-test (:name :call-7-int-callback)
  (assert (= (alien-apply *add-i-i-i-i-i-i-i* (iota 7)) 28)))

(with-test (:name :define-8-int-callback)
  (define-callback-adder int int int int int int int int int))
(with-test (:name :call-8-int-callback)
  (assert (= (alien-apply *add-i-i-i-i-i-i-i-i* (iota 8)) 36)))

(with-test (:name :define-9-int-callback)
  (define-callback-adder int int int int int int int int int int))
(with-test (:name :call-9-int-callback)
  (assert (= (alien-apply *add-i-i-i-i-i-i-i-i-i* (iota 9)) 45)))

(with-test (:name :define-10-int-callback)
  (define-callback-adder int int int int int int int int int int int))
(with-test (:name :call-10-int-callback)
  (assert (= (alien-apply *add-i-i-i-i-i-i-i-i-i-i* (iota 10)) 55)))

(with-test (:name :define-11-int-callback)
  (define-callback-adder int int int int int int int int int int int int))
(with-test (:name :call-11-int-callback)
  (assert (= (alien-apply *add-i-i-i-i-i-i-i-i-i-i-i* (iota 11)) 66)))

(with-test (:name :define-12-int-callback)
  (define-callback-adder int int int int int int int int int int int int int))
(with-test (:name :call-12-int-callback)
  (assert (= (alien-apply *add-i-i-i-i-i-i-i-i-i-i-i-i* (iota 12)) 78)))

(with-test (:name :define-2-float-callback)
  (define-callback-adder float float float))
(with-test (:name :call-2-float-callback)
  (assert (= (alien-apply *add-f-f* (iota 2.0s0)) 3.0s0)))

(with-test (:name :define-3-float-callback)
  (define-callback-adder float float float float))
(with-test (:name :call-3-float-callback)
  (assert (= (alien-apply *add-f-f-f* (iota 3.0s0)) 6.0s0)))

(with-test (:name :define-4-float-callback)
  (define-callback-adder float float float float float))
(with-test (:name :call-4-float-callback)
  (assert (= (alien-apply *add-f-f-f-f* (iota 4.0s0)) 10.0s0)))

(with-test (:name :define-5-float-callback)
  (define-callback-adder float float float float float float))
(with-test (:name :call-5-float-callback)
  (assert (= (alien-apply *add-f-f-f-f-f* (iota 5.0s0)) 15.0s0)))

(with-test (:name :define-6-float-callback)
  (define-callback-adder float float float float float float float))
(with-test (:name :call-6-float-callback)
  (assert (= (alien-apply *add-f-f-f-f-f-f* (iota 6.0s0)) 21.0s0)))

(with-test (:name :define-7-float-callback)
  (define-callback-adder float float float float float float float float))
(with-test (:name :call-7-float-callback)
  (assert (= (alien-apply *add-f-f-f-f-f-f-f* (iota 7.0s0)) 28.0s0)))

(with-test (:name :define-8-float-callback)
  (define-callback-adder float float float float float float float float float))
(with-test (:name :call-8-float-callback)
  (assert (= (alien-apply *add-f-f-f-f-f-f-f-f* (iota 8.0s0)) 36.0s0)))

(with-test (:name :define-9-float-callback)
  (define-callback-adder float float float float float float float float float float))
(with-test (:name :call-9-float-callback)
  (assert (= (alien-apply *add-f-f-f-f-f-f-f-f-f* (iota 9.0s0)) 45.0s0)))

(with-test (:name :define-10-float-callback)
  (define-callback-adder float float float float float float float float float float float))
(with-test (:name :call-10-float-callback)
  (assert (= (alien-apply *add-f-f-f-f-f-f-f-f-f-f* (iota 10.0s0)) 55.0s0)))

(with-test (:name :define-11-float-callback)
  (define-callback-adder float float float float float float float float float float float float))
(with-test (:name :call-11-float-callback)
  (assert (= (alien-apply *add-f-f-f-f-f-f-f-f-f-f-f* (iota 11.0s0)) 66.0s0)))

(with-test (:name :define-12-float-callback)
  (define-callback-adder float float float float float float float float float float float float float))
(with-test (:name :call-12-float-callback)
  (assert (= (alien-apply *add-f-f-f-f-f-f-f-f-f-f-f-f* (iota 12.0s0)) 78.0s0)))

(with-test (:name :define-2-double-callback)
  (define-callback-adder double double double))
(with-test (:name :call-2-double-callback)
  (assert (= (alien-apply *add-d-d* (iota 2.0d0)) 3.0d0)))

(with-test (:name :define-3-double-callback)
  (define-callback-adder double double double double))
(with-test (:name :call-3-double-callback)
  (assert (= (alien-apply *add-d-d-d* (iota 3.0d0)) 6.0d0)))

(with-test (:name :define-4-double-callback)
  (define-callback-adder double double double double double))
(with-test (:name :call-4-double-callback)
  (assert (= (alien-apply *add-d-d-d-d* (iota 4.0d0)) 10.0d0)))

(with-test (:name :define-5-double-callback)
  (define-callback-adder double double double double double double))
(with-test (:name :call-5-double-callback)
  (assert (= (alien-apply *add-d-d-d-d-d* (iota 5.0d0)) 15.0d0)))

(with-test (:name :define-6-double-callback)
  (define-callback-adder double double double double double double double))
(with-test (:name :call-6-double-callback)
  (assert (= (alien-apply *add-d-d-d-d-d-d* (iota 6.0d0)) 21.0d0)))

(with-test (:name :define-7-double-callback)
  (define-callback-adder double double double double double double double double))
(with-test (:name :call-7-double-callback)
  (assert (= (alien-apply *add-d-d-d-d-d-d-d* (iota 7.0d0)) 28.0d0)))

(with-test (:name :define-8-double-callback)
  (define-callback-adder double double double double double double double double double))
(with-test (:name :call-8-double-callback)
  (assert (= (alien-apply *add-d-d-d-d-d-d-d-d* (iota 8.0d0)) 36.0d0)))

(with-test (:name :define-9-double-callback)
  (define-callback-adder double double double double double double double double double double))
(with-test (:name :call-9-double-callback)
  (assert (= (alien-apply *add-d-d-d-d-d-d-d-d-d* (iota 9.0d0)) 45.0d0)))

(with-test (:name :define-10-double-callback)
  (define-callback-adder double double double double double double double double double double double))
(with-test (:name :call-10-double-callback)
  (assert (= (alien-apply *add-d-d-d-d-d-d-d-d-d-d* (iota 10.0d0)) 55.0d0)))

(with-test (:name :define-11-double-callback)
  (define-callback-adder double double double double double double double double double double double double))
(with-test (:name :call-11-double-callback)
  (assert (= (alien-apply *add-d-d-d-d-d-d-d-d-d-d-d* (iota 11.0d0)) 66.0d0)))

(with-test (:name :define-12-double-callback)
  (define-callback-adder double double double double double double double double double double double double double))
(with-test (:name :call-12-double-callback)
  (assert (= (alien-apply *add-d-d-d-d-d-d-d-d-d-d-d-d* (iota 12.0d0)) 78.0d0)))

(with-test (:name :define-int-float-callback)
  (define-callback-adder float int float))
(with-test (:name :call-int-float-callback)
  (assert (= (alien-funcall *add-i-f* 1 2.0s0) 3.0s0)))

(with-test (:name :define-float-int-callback)
  (define-callback-adder float float int))
(with-test (:name :call-float-int-callback)
  (assert (= (alien-funcall *add-f-i* 2.0s0 1) 3.0s0)))

(with-test (:name :define-int-double-callback)
  (define-callback-adder double int double))
(with-test (:name :call-int-double-callback)
  (assert (= (alien-funcall *add-i-d* 1 2.0d0) 3.0d0)))

(with-test (:name :define-double-int-callback)
  (define-callback-adder double double int))
(with-test (:name :call-double-int-callback)
  (assert (= (alien-funcall *add-d-i* 2.0d0 1) 3.0d0)))

(with-test (:name :define-double-float-callback)
  (define-callback-adder double double float))
(with-test (:name :call-double-float-callback)
  (assert (= (alien-funcall *add-d-f* 2.0d0 1.0s0) 3.0d0)))

(with-test (:name :define-float-double-callback)
  (define-callback-adder double float double))
(with-test (:name :call-float-double-callback)
  (assert (= (alien-funcall *add-f-d* 1.0s0 2.0d0) 3.0d0)))

(with-test (:name :define-double-float-int-callback)
  (define-callback-adder double double float int))
(with-test (:name :call-double-float-int-callback)
  (assert (= (alien-funcall *add-d-f-i* 2.0d0 1.0s0 1) 4.0d0)))

(with-test (:name :define-float-float-double-callback)
  (define-callback-adder double float float double))
(with-test (:name :call-float-float-double-callback)
  (assert (= (alien-funcall *add-f-f-d* 2.0s0 1.0s0 3.0d0) 6.0d0)))

(with-test (:name :define-int-float-double-callback)
  (define-callback-adder double int float double))
(with-test (:name :call-int-float-double-callback)
  (assert (= (alien-funcall *add-i-f-d* 1 1.0s0 2.0d0) 4.0d0)))

(with-test (:name :define-int-ulonglong-callback)
  (define-callback-adder unsigned-long-long int unsigned-long-long))
(with-test (:name :call-int-ulonglong-callback)
  (assert (= (alien-funcall *add-i-ull* 1 #x200000003) #x200000004)))

(with-test (:name :define-int-int-int-int-int-ulonglong-callback)
  (define-callback-adder unsigned-long-long int int int int int unsigned-long-long))
(with-test (:name :call-int-int-int-int-int-ulonglong-callback)
  (assert (= (alien-funcall *add-i-i-i-i-i-ull* 0 0 0 0 1 #x200000003) #x200000004)))


;;; passing and returning structs of various size and alignment
(defmacro make-point-test (type fields-with-lisp-types &optional args-before args-after)
  "Create test for point-type structs"
  (let*
      ((pkw (intern (symbol-name type) "KEYWORD"))
       (slot-values
         (loop for (field type) in fields-with-lisp-types
               collect `(,field ,(coerce (random 1000) type))))
       (slot-inits
         (loop for (field value) in slot-values
               collect `(setf (slot p ',field) ,value)))
       (before
         (loop for type in args-before collect `(coerce (random 1000) type)))
       (after
         (loop for type in args-after collect `(coerce (random 1000) type)))
       (fns
         (loop for (field _) in fields-with-lisp-types
               collect (intern
                        (format nil "~A-~A" (symbol-name type) (symbol-name field)))))
       (test-body
         (loop for (field value) in slot-values
               for fn in fns
               collect
               `(assert (= ,value
                           (alien-funcall
                            (alien-callable-function ',fn) ,@before p ,@after))))))
    `(with-test (:name ,(list :callback pkw :before args-before :after args-after)
                 :broken-on :interpreter)
       (with-alien ((p ,type))
         ,@slot-inits
         ,@test-body))))
;; Point2 Long
(define-alien-type point2l (struct point2l
                                  (x (integer 64))
                                  (y (integer 64))))
(define-alien-callable point2l-x (integer 64) ((p point2l))
  (slot p 'x))
(define-alien-callable point2l-y (integer 64) ((p point2l))
  (slot p 'y))
;;(define-alien-callable point2l-7x
;;    (integer 64) ((a (integer 64)) (b (integer 64)) (c (integer 64))
;;                                   (d (integer 64)) (e (integer 64)) (f (integer 64))
;;                                   (p point2l))
;;  (declare (ignore a b c d e f))
;;  (slot p 'x))
;;(define-alien-callable point2l-7y
;;    (integer 64) ((a (integer 64)) (b (integer 64)) (c (integer 64))
;;                                   (d (integer 64)) (e (integer 64)) (f (integer 64))
;;                                   (p point2l))
;;  (declare (ignore a b c d e f))
;;  (slot p 'y))
;;(define-alien-callable point2l-7-1-x
;;    (integer 64) ((a (integer 64)) (b (integer 64)) (c (integer 64))
;;                                   (d (integer 64)) (e (integer 64)) (f (integer 64))
;;                                   (p point2l) (h (integer 64)))
;;  (declare (ignore a b c d e f h))
;;  (slot p 'x))
;;(define-alien-callable point2l-7-1-y
;;    (integer 64) ((a (integer 64)) (b (integer 64)) (c (integer 64))
;;                                   (d (integer 64)) (e (integer 64)) (f (integer 64))
;;                                   (p point2l) (h (integer 64)))
;;  (declare (ignore a b c d e f h))
;;  (slot p 'y))
;;(define-alien-callable point2l-8x
;;    (integer 64) ((a (integer 64)) (b (integer 64)) (c (integer 64)) (d (integer 64))
;;                                   (e (integer 64)) (f (integer 64)) (g (integer 64))
;;                                   (p point2l))
;;  (declare (ignore a b c d e f g))
;;  (slot p 'x))
;;(define-alien-callable point2l-8y
;;    (integer 64) ((a (integer 64)) (b (integer 64)) (c (integer 64)) (d (integer 64))
;;                                   (e (integer 64)) (f (integer 64)) (g (integer 64))
;;                                   (p point2l))
;;  (declare (ignore a b c d e f g))
;;  (slot p 'y))
;;(define-alien-callable point2l-9x
;;    (integer 64) ((a (integer 64)) (b (integer 64)) (c (integer 64)) (d (integer 64))
;;                                   (e (integer 64)) (f (integer 64)) (g (integer 64))
;;                                   (h (integer 64))
;;                                   (p point2l))
;;  (declare (ignore a b c d e f g h))
;;  (slot p 'x))
;;(define-alien-callable point2l-9y
;;    (integer 64) ((a (integer 64)) (b (integer 64)) (c (integer 64)) (d (integer 64))
;;                                   (e (integer 64)) (f (integer 64)) (g (integer 64))
;;                                   (h (integer 64))
;;                                   (p point2l))
;;  (declare (ignore a b c d e f g h))
;;  (slot p 'y))
(format t "~A~%" (macroexpand-1 '(make-point-test point2l ((x integer) (y integer)))))
(make-point-test point2l ((x integer) (y integer)))

(with-test (:name (:callback :point2l)
            :broken-on :interpreter)
  (with-alien ((p point2l))
    (setf (slot p 'x) 8)
    (setf (slot p 'y) 9)
    (assert (= 8 (alien-funcall (alien-callable-function 'point2l-x) p)))
    (assert (= 9 (alien-funcall (alien-callable-function 'point2l-y) p)))
    ;;(assert (= 8 (alien-funcall (alien-callable-function 'point2l-7x) 1 2 3 4 5 6 p)))
    ;;(assert (= 9 (alien-funcall (alien-callable-function 'point2l-7y) 1 2 3 4 5 6 p)))
    ;;(assert (= 8 (alien-funcall (alien-callable-function 'point2l-7-1-x) 1 2 3 4 5 6 p 8)))
    ;;(assert (= 9 (alien-funcall (alien-callable-function 'point2l-7-1-y) 1 2 3 4 5 6 p 8)))
    ;;(assert (= 8 (alien-funcall (alien-callable-function 'point2l-8x) 1 2 3 4 5 6 7 p)))
    ;;(assert (= 9 (alien-funcall (alien-callable-function 'point2l-8y) 1 2 3 4 5 6 7 p)))
    ;;(assert (= 8 (alien-funcall (alien-callable-function 'point2l-9x) 1 2 3 4 5 6 7 8 p)))
    ;;(assert (= 9 (alien-funcall (alien-callable-function 'point2l-9y) 1 2 3 4 5 6 7 8 p)))
    ))

;; ;; Point2 Int
;; (define-alien-type point2i (struct point2i
;;                                   (x (integer 32))
;;                                   (y (integer 32))))
;; (define-alien-callable point2i-x (integer 32) ((p point2i))
;;   (slot p 'x))
;; (define-alien-callable point2i-y (integer 32) ((p point2i))
;;   (slot p 'y))
;; (define-alien-callable point2i-7x
;;     (integer 32) ((a (integer 32)) (b (integer 32)) (c (integer 32))
;;                                    (d (integer 32)) (e (integer 32)) (f (integer 32))
;;                                    (p point2i))
;;   (declare (ignore a b c d e f))
;;   (slot p 'x))
;; (define-alien-callable point2i-7y
;;     (integer 32) ((a (integer 32)) (b (integer 32)) (c (integer 32))
;;                                    (d (integer 32)) (e (integer 32)) (f (integer 32))
;;                                    (p point2i))
;;   (declare (ignore a b c d e f))
;;   (slot p 'y))
;; (with-test (:name (:callback :point2i)
;;             :broken-on :interpreter)
;;   (with-alien ((p point2i))
;;     (setf (slot p 'x) 10)
;;     (setf (slot p 'y) 11)
;;     (assert (= 10 (alien-funcall (alien-callable-function 'point2i-x) p)))
;;     (assert (= 11 (alien-funcall (alien-callable-function 'point2i-y) p)))
;;     (assert (= 10 (alien-funcall (alien-callable-function 'point2i-7x) 1 2 3 4 5 6 p)))
;;     (assert (= 11 (alien-funcall (alien-callable-function 'point2i-7y) 1 2 3 4 5 6 p)))))
;; ;; Point3 Long
;; (define-alien-type point3l (struct point3l
;;                                   (x (integer 64))
;;                                   (y (integer 64))
;;                                   (z (integer 64))))
;; (define-alien-callable point3l-x (integer 64) ((p point3l))
;;   (slot p 'x))
;; (define-alien-callable point3l-y (integer 64) ((p point3l))
;;   (slot p 'y))
;; (define-alien-callable point3l-z (integer 64) ((p point3l))
;;   (slot p 'z))
;; (define-alien-callable point3l-7x
;;     (integer 64) ((a (integer 64)) (b (integer 64)) (c (integer 64))
;;                                    (d (integer 64)) (e (integer 64)) (f (integer 64))
;;                                    (p point3l))
;;   (declare (ignore a b c d e f))
;;   (slot p 'x))
;; (define-alien-callable point3l-7y
;;     (integer 64) ((a (integer 64)) (b (integer 64)) (c (integer 64))
;;                                    (d (integer 64)) (e (integer 64)) (f (integer 64))
;;                                    (p point3l))
;;   (declare (ignore a b c d e f))
;;   (slot p 'y))
;; (define-alien-callable point3l-7z
;;     (integer 64) ((a (integer 64)) (b (integer 64)) (c (integer 64))
;;                                    (d (integer 64)) (e (integer 64)) (f (integer 64))
;;                                    (p point3l))
;;   (declare (ignore a b c d e f))
;;   (slot p 'z))
;; (with-test (:name (:callback :point3l)
;;             :broken-on :interpreter)
;;   (with-alien ((p point3l))
;;     (setf (slot p 'x) 1)
;;     (setf (slot p 'y) 2)
;;     (setf (slot p 'z) 3)
;;     (assert (= 1 (alien-funcall (alien-callable-function 'point3l-x) p)))
;;     (assert (= 2 (alien-funcall (alien-callable-function 'point3l-y) p)))
;;     (assert (= 3 (alien-funcall (alien-callable-function 'point3l-z) p)))
;;     (assert (= 1 (alien-funcall (alien-callable-function 'point3l-7x) 1 2 3 4 5 6 p)))
;;     (assert (= 2 (alien-funcall (alien-callable-function 'point3l-7y) 1 2 3 4 5 6 p)))
;;     (assert (= 3 (alien-funcall (alien-callable-function 'point3l-7z) 1 2 3 4 5 6 p)))))
;; ;; Point3 Double
;; (define-alien-type point3d (struct point3d
;;                                   (x double)
;;                                   (y double)
;;                                   (z double)))
;; (define-alien-callable point3d-x double ((p point3d))
;;   (slot p 'x))
;; (define-alien-callable point3d-y double ((p point3d))
;;   (slot p 'y))
;; (define-alien-callable point3d-z double ((p point3d))
;;   (slot p 'z))
;; (define-alien-callable point3d-7x
;;     double ((a (integer 64)) (b (integer 64)) (c (integer 64))
;;                                    (d (integer 64)) (e (integer 64)) (f (integer 64))
;;                                    (p point3d))
;;   (declare (ignore a b c d e f))
;;   (slot p 'x))
;; (define-alien-callable point3d-7y
;;     double ((a (integer 64)) (b (integer 64)) (c (integer 64))
;;                                    (d (integer 64)) (e (integer 64)) (f (integer 64))
;;                                    (p point3d))
;;   (declare (ignore a b c d e f))
;;   (slot p 'y))
;; (define-alien-callable point3d-7z
;;     double ((a (integer 64)) (b (integer 64)) (c (integer 64))
;;                                    (d (integer 64)) (e (integer 64)) (f (integer 64))
;;                                    (p point3d))
;;   (declare (ignore a b c d e f))
;;   (slot p 'z))
;; (with-test (:name (:callback :point3d)
;;             :broken-on :interpreter)
;;   (with-alien ((p point3d))
;;     (setf (slot p 'x) 42.0d0)
;;     (setf (slot p 'y) 43.0d0)
;;     (setf (slot p 'z) 44.0d0)
;;     (assert (= 42.0d0 (alien-funcall (alien-callable-function 'point3d-x) p)))
;;     (assert (= 43.0d0 (alien-funcall (alien-callable-function 'point3d-y) p)))
;;     (assert (= 44.0d0 (alien-funcall (alien-callable-function 'point3d-z) p)))
;;     (assert (= 42.0d0 (alien-funcall (alien-callable-function 'point3d-7x) 1 2 3 4 5 6 p)))
;;     (assert (= 43.0d0 (alien-funcall (alien-callable-function 'point3d-7y) 1 2 3 4 5 6 p)))
;;     (assert (= 44.0d0 (alien-funcall (alien-callable-function 'point3d-7z) 1 2 3 4 5 6 p)))))
;;
;; ;; Point3 Int
;; (define-alien-type point3i (struct point3i
;;                                   (x (integer 32))
;;                                   (y (integer 32))
;;                                   (z (integer 32))))
;; (define-alien-callable point3i-x (integer 32) ((p point3i))
;;   (slot p 'x))
;; (define-alien-callable point3i-y (integer 32) ((p point3i))
;;   (slot p 'y))
;; (define-alien-callable point3i-z (integer 32) ((p point3i))
;;   (slot p 'z))
;; (define-alien-callable point3i-7x
;;     (integer 32) ((a (integer 32)) (b (integer 32)) (c (integer 32))
;;                                    (d (integer 32)) (e (integer 32)) (f (integer 32))
;;                                    (p point3i))
;;   (declare (ignore a b c d e f))
;;   (slot p 'x))
;; (define-alien-callable point3i-7y
;;     (integer 32) ((a (integer 32)) (b (integer 32)) (c (integer 32))
;;                                    (d (integer 32)) (e (integer 32)) (f (integer 32))
;;                                    (p point3i))
;;   (declare (ignore a b c d e f))
;;   (slot p 'y))
;; (define-alien-callable point3i-7z
;;     (integer 32) ((a (integer 32)) (b (integer 32)) (c (integer 32))
;;                                    (d (integer 32)) (e (integer 32)) (f (integer 32))
;;                                    (p point3i))
;;   (declare (ignore a b c d e f))
;;   (slot p 'z))
;; (with-test (:name (:callback :point3i)
;;             :broken-on :interpreter)
;;   (with-alien ((p point3i))
;;     (setf (slot p 'x) 4)
;;     (setf (slot p 'y) 5)
;;     (setf (slot p 'z) 6)
;;     (assert (= 4 (alien-funcall (alien-callable-function 'point3i-x) p)))
;;     (assert (= 5 (alien-funcall (alien-callable-function 'point3i-y) p)))
;;     (assert (= 6 (alien-funcall (alien-callable-function 'point3i-z) p)))
;;     (assert (= 4 (alien-funcall (alien-callable-function 'point3i-7x) 1 2 3 4 5 6 p)))
;;     (assert (= 5 (alien-funcall (alien-callable-function 'point3i-7y) 1 2 3 4 5 6 p)))
;;     (assert (= 6 (alien-funcall (alien-callable-function 'point3i-7z) 1 2 3 4 5 6 p)))))
;;
;; ;; Point3 Float
;; (define-alien-type point3f (struct point3f
;;                                    (x float) (y float) (z float)))
;; (define-alien-callable point3f-x float ((p point3f))
;;   (slot p 'x))
;; (define-alien-callable point3f-y float ((p point3f))
;;   (slot p 'y))
;; (define-alien-callable point3f-z float ((p point3f))
;;   (slot p 'z))
;; (define-alien-callable point3f-7x
;;     float ((a (integer 64)) (b (integer 64)) (c (integer 64))
;;                                    (d (integer 64)) (e (integer 64)) (f (integer 64))
;;                                    (p point3f))
;;   (declare (ignore a b c d e f))
;;   (slot p 'x))
;; (define-alien-callable point3f-7y
;;     float ((a (integer 64)) (b (integer 64)) (c (integer 64))
;;                                    (d (integer 64)) (e (integer 64)) (f (integer 64))
;;                                    (p point3f))
;;   (declare (ignore a b c d e f))
;;   (slot p 'y))
;; (define-alien-callable point3f-7z
;;     float ((a (integer 64)) (b (integer 64)) (c (integer 64))
;;                                    (d (integer 64)) (e (integer 64)) (f (integer 64))
;;                                    (p point3f))
;;   (declare (ignore a b c d e f))
;;   (slot p 'z))
;; (with-test (:name (:callback :point3f)
;;             :broken-on :interpreter)
;;   (with-alien ((p point3f))
;;     (setf (slot p 'x) 42.0)
;;     (setf (slot p 'y) 43.0)
;;     (setf (slot p 'z) 44.0)
;;     (assert (= 42.0 (alien-funcall (alien-callable-function 'point3f-x) p)))
;;     (assert (= 43.0 (alien-funcall (alien-callable-function 'point3f-y) p)))
;;     (assert (= 44.0 (alien-funcall (alien-callable-function 'point3f-z) p)))
;;     (assert (= 42.0 (alien-funcall (alien-callable-function 'point3f-7x) 1 2 3 4 5 6 p)))
;;     (assert (= 43.0 (alien-funcall (alien-callable-function 'point3f-7y) 1 2 3 4 5 6 p)))
;;     (assert (= 44.0 (alien-funcall (alien-callable-function 'point3f-7z) 1 2 3 4 5 6 p)))))
