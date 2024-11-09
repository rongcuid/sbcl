;;;; This file is for compiler tests of passing structs by value to/from
;;;; foreign functions.

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

;(in-package :cl-user)
;;;; Bug 313202: C struct pass/return by value
;;; Compile and load shared library

(defvar *soname*)
#+win32
(with-scratch-file (dll "dll")
  (sb-ext:run-program "gcc" `("-shared" "-o" ,dll "alien-struct-by-value.c")
                      :search t)
  (load-shared-object dll))
#-win32
(progn
  (unless (probe-file "alien-struct-by-value.so")
    (sb-ext:run-program "/bin/sh" '("run-compiler.sh" "-sbcl-pic" "-sbcl-shared"
                                    "-o" "alien-struct-by-value.so"
                                    "alien-struct-by-value.c")))
  (setq *soname* (truename "alien-struct-by-value.so"))
  (load-shared-object *soname*))
(defconstant +magic-number+ 42)
;;; Compile and load shared library
(unless (probe-file "alien-struct-by-value.so")
  (sb-ext:run-program "/bin/sh" '("run-compiler.sh" "-sbcl-pic" "-sbcl-shared"
                                  "-o" "alien-struct-by-value.so"
                                  "alien-struct-by-value.c")))
(load-shared-object (truename "alien-struct-by-value.so"))
;;; This forces DEFINE-ALIEN-ROUTINE to be evaluated at runtime so errors can be caught
(defmacro defar (&body args)
  `(eval '(define-alien-routine ,@args)))
(defmacro assert-unimplemented ((&whole def dar name ret &optional (arg nil argp)))
  (declare (ignore dar ret))
  ;; all the "caught 1 fatal ERROR" notices are scary to those not expecting to see them
  `(let ((*error-output* (make-broadcast-stream)))
     (assert-error (eval '(progn ,def
                           ,(if argp `(with-alien ((x ,(second arg))) (,name x)) '(name)))))))
;;; Tiny struct, alignment 8
(define-alien-type nil (struct tiny-align-8 (m0 (integer 64))))
(defar tiny-align-8-get-m0 (integer 64) (m (struct tiny-align-8)))
(defar tiny-align-8-get-m0-1 (integer 64) (i0 long-long) (m (struct tiny-align-8)))
(defar tiny-align-8-get-m0-2 (integer 64) (i0 int) (m (struct tiny-align-8)))
(defar tiny-align-8-get-m0-3 (integer 64)
  (i0 long-long) (i1 long-long) (i2 long-long) (i3 long-long)
  (i4 long-long) (i5 long-long) (i6 long-long) (i7 long-long)
  (m (struct tiny-align-8)))
(defar tiny-align-8-get-m0-4 (integer 64)
  (i0 int) (i1 int) (i2 int) (i3 int)
  (i4 int) (i5 int) (i6 int) (i7 int)
  (m (struct tiny-align-8)))
(defar tiny-align-8-get-m0-5 (integer 64)
  (i0 long-long) (i1 long-long) (i2 long-long) (i3 long-long)
  (i4 long-long) (i5 long-long) (i6 long-long) (i7 long-long)
  (i8 long-long)
  (m (struct tiny-align-8)))
(defar tiny-align-8-get-m0-6 (integer 64)
  (i0 long-long) (i1 long-long) (i2 long-long) (i3 long-long)
  (i4 long-long) (i5 long-long) (i6 long-long) (i7 long-long)
  (i8 int)
  (m (struct tiny-align-8)))
(defar tiny-align-8-mutate void (m (struct tiny-align-8)))
(defar tiny-align-8-ret-0 (struct tiny-align-8))
(defar tiny-align-8-ret-1 (struct tiny-align-8) (m long-long))
(with-test (:name :struct-pass-by-value-tiny-align-8-args :fails-on (not :arm64))
  (with-alien ((m (struct tiny-align-8)))
    ;; Initialize struct
    (setf (slot m 'm0) +magic-number+)
    (flet ((test-members ()
             (assert (= +magic-number+ (tiny-align-8-get-m0 m)))
             (assert (= +magic-number+ (tiny-align-8-get-m0-1 0 m)))
             (assert (= +magic-number+ (tiny-align-8-get-m0-2 0 m)))
             (assert (= +magic-number+ (tiny-align-8-get-m0-3 0 1 2 3 4 5 6 7 m)))
             (assert (= +magic-number+ (tiny-align-8-get-m0-4 0 1 2 3 4 5 6 7 m)))
             (assert (= +magic-number+ (tiny-align-8-get-m0-5 0 1 2 3 4 5 6 7 8 m)))
             (assert (= +magic-number+ (tiny-align-8-get-m0-6 0 1 2 3 4 5 6 7 8 m)))))
      ;; Test struct passing
      (test-members)
      ;; Call a function that mutates struct
      (tiny-align-8-mutate m)
      ;; Test struct has not changed
      (test-members))))
(with-test (:name :struct-return-by-value-tiny-align-8-args :fails-on (not :arm64))
  (with-alien ((m (struct tiny-align-8)))
    (setf m (tiny-align-8-ret-0))
    (assert (= 42 (slot m 'm0)))
    (setf m (tiny-align-8-ret-1 +magic-number+))
    (assert (= (1+ +magic-number+) (slot m 'm0)))))
;;; Small struct, alignment 8
(define-alien-type nil (struct small-align-8 (m0 (integer 64)) (m1 (integer 64))))
(defar small-align-8-get-m0 (integer 64) (m (struct small-align-8)))
(defar small-align-8-get-m1 (integer 64) (m (struct small-align-8)))
(defar small-align-8-get-m0-1 (integer 64)
  (i0 long-long) (i1 long-long)
  (m (struct small-align-8)))
(defar small-align-8-get-m0-2 (integer 64)
  (i0 long-long)
  (m (struct small-align-8)))
(defar small-align-8-get-m0-3 (integer 64)
  (i0 long-long) (i1 long-long) (i2 long-long) (i3 long-long)
  (i4 long-long) (i5 long-long) (i6 long-long) (i7 long-long)
  (m (struct small-align-8)))
(defar small-align-8-get-m0-4 (integer 64)
  (i0 long-long) (i1 long-long) (i2 long-long) (i3 long-long)
  (i4 long-long) (i5 long-long) (i6 long-long)
  (m (struct small-align-8)))
(defar small-align-8-mutate void (m (struct small-align-8)))
(defar small-align-8-ret-0 (struct small-align-8))
(defar small-align-8-ret-1 (struct small-align-8) (m long-long))
(with-test (:name :struct-pass-by-value-small-align-8-args :fails-on (not :arm64))
  (with-alien ((m (struct small-align-8)))
    ;; Initialize struct
    (setf (slot m 'm0) +magic-number+) (setf (slot m 'm1) (1+ +magic-number+))
    (flet ((test-members ()
             (assert (= +magic-number+ (small-align-8-get-m0 m)))
             (assert (= (1+ +magic-number+) (small-align-8-get-m1 m)))
             (assert (= +magic-number+ (small-align-8-get-m0-1 0 1 m)))
             (assert (= +magic-number+ (small-align-8-get-m0-2 0 m)))
             (assert (= +magic-number+ (small-align-8-get-m0-3 0 1 2 3 4 5 6 7 m)))
             (assert (= +magic-number+ (small-align-8-get-m0-4 0 1 2 3 4 5 6 m)))))
      ;; Test struct passing
      (test-members)
      ;; Call a function that mutates struct
      (small-align-8-mutate m)
      ;; Test struct has not changed
      (test-members))))
(with-test (:name :struct-return-by-value-small-align-8-args :fails-on (not :arm64))
  (with-alien ((m (struct small-align-8)))
    (setf m (small-align-8-ret-0))
    (assert (= 33 (slot m 'm0)))
    (assert (= 34 (slot m 'm1)))
    (setf m (small-align-8-ret-1 +magic-number+))))
;;; Large struct, alignment 8
(define-alien-type nil
    (struct large-align-8
            (m0 (integer 64)) (m4 (integer 64)) (m8 (integer 64)) (m12 (integer 64))
            (m1 (integer 64)) (m5 (integer 64)) (m9 (integer 64)) (m13 (integer 64))
            (m2 (integer 64)) (m6 (integer 64)) (m10 (integer 64)) (m14 (integer 64))
            (m3 (integer 64)) (m7 (integer 64)) (m11 (integer 64)) (m15 (integer 64))))
(defmacro def-large-align-8-get (i)
  (let ((lisp-name (sb-int:symbolicate "LARGE-ALIGN-8-GET-M" i)))
    `(defar ,lisp-name (integer 64) (m (struct large-align-8)))))
(defmacro defs-large-align-8-get ()
  "Test functions for each member"
  (let ((defs (loop for i upto 15 collect `(def-large-align-8-get ,i))))
    `(progn ,@defs)))
(defs-large-align-8-get)
(defar large-align-8-get-m0-1 (integer 64)
  (i0 long-long)
  (m (struct large-align-8)))
(defar large-align-8-get-m0-2 (integer 64)
  (i0 long-long) (i1 long-long) (i2 long-long) (i3 long-long)
  (i4 long-long) (i5 long-long) (i6 long-long) (i7 long-long)
  (m (struct large-align-8)))
(defar large-align-8-get-m0-3 (integer 64)
  (i0 long-long) (i1 long-long) (i2 long-long) (i3 long-long)
  (i4 long-long) (i5 long-long) (i6 long-long) (i7 long-long)
  (i8 int)
  (m (struct large-align-8)))
(defar large-align-8-mutate void (m (struct large-align-8)))
(defar large-align-8-ret-0 (struct large-align-8))
(defar large-align-8-ret-1 (struct large-align-8) (m long-long))
(with-test (:name :struct-pass-by-value-large-align-8-args :fails-on (not :arm64))
  (with-alien ((m (struct large-align-8)))
    (macrolet
        ((set-members ()
           "Sets member mN's value to N"
           (loop for i upto 15
                 collect (let ((memb (sb-int:symbolicate "M" i)))
                           `(setf (slot m ',memb) ,(+ i +magic-number+)))
                   into setfs
                 finally (return `(progn ,@setfs))))
         (test-members ()
           "Test that each member has correct value"
           (let ((basics (loop for i upto 15
                               collect (let ((f (sb-int:symbolicate "LARGE-ALIGN-8-GET-M" i)))
                                         `(assert (= ,(+ i +magic-number+) (,f m)))))))
             `(progn
                ,@basics
                (assert (= +magic-number+ (large-align-8-get-m0-1 0 m)))
                (assert (= +magic-number+ (large-align-8-get-m0-2
                                           0 1 2 3 4 5 6 7 m)))
                (assert (= +magic-number+ (large-align-8-get-m0-3
                                           0 1 2 3 4 5 6 7 8 m)))))))
      ;; Initialize struct
      (set-members)
      ;; Test that struct is correctly passed
      (test-members)
      ;; Call a C function that mutates struct locally
      (large-align-8-mutate m)
      ;; Test that the original struct is not modified
      (test-members))))
(with-test (:name :struct-return-by-value-large-align-8 :fails-on (not :arm64))
  (with-alien ((m (struct large-align-8)))
    (setf m (large-align-8-ret-0))
    (assert (= 42 (slot m 'm0))) (assert (= 43 (slot m 'm1)))
    (assert (= 44 (slot m 'm2))) (assert (= 45 (slot m 'm3)))
    (assert (= 46 (slot m 'm4))) (assert (= 47 (slot m 'm5)))
    (assert (= 48 (slot m 'm6))) (assert (= 49 (slot m 'm7)))
    (assert (= 50 (slot m 'm8))) (assert (= 51 (slot m 'm9)))
    (assert (= 52 (slot m 'm10))) (assert (= 53 (slot m 'm11)))
    (assert (= 54 (slot m 'm12))) (assert (= 55 (slot m 'm13)))
    (assert (= 56 (slot m 'm14))) (assert (= 57 (slot m 'm15)))
    (setf m (large-align-8-ret-1 43))
    (assert (= 43 (slot m 'm0))) (assert (= 44 (slot m 'm1)))
    (assert (= 45 (slot m 'm2))) (assert (= 46 (slot m 'm3)))
    (assert (= 47 (slot m 'm4))) (assert (= 48 (slot m 'm5)))
    (assert (= 49 (slot m 'm6))) (assert (= 50 (slot m 'm7)))
    (assert (= 51 (slot m 'm8))) (assert (= 52 (slot m 'm9)))
    (assert (= 53 (slot m 'm10))) (assert (= 54 (slot m 'm11)))
    (assert (= 55 (slot m 'm12))) (assert (= 56 (slot m 'm13)))
    (assert (= 57 (slot m 'm14))) (assert (= 58 (slot m 'm15)))))


;;; Clean up
#-win32 (ignore-errors (delete-file *soname*))
