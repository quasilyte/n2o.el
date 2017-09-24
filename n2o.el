;;; n2o.el --- extra Emacs Lisp optimizer -*- lexical-binding: t -*-

;; Author: Iskander Sharipov <quasilyte@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; "n2o.el", or nitrous is standalone, independent Emacs Lisp optimizer
;; that can be installed into stock Emacs to improve
;; the performance of byte compiled code.
;; Once loaded and enabled (non-nil `n2o-enabled') it
;; transparently applies additional optimizations.
;;
;; The simplest way to inspect how it really works is to
;; look at the `disassemble' function output.
;; (let ((n2o-enabled t)) (disassemble '(lambda () (+ 2 x))))
;; vs
;; (let ((n2o-enabled nil)) (disassemble '(lambda () (+ 2 x))))
;; With `n2o':
;;   varref x
;;   add1
;;   add1
;; Without `n2o':
;;   constant 2
;;   varref x
;;   plus
;; Both snippets lead to the same result, but double `add1' does
;; not require to store "2" constant,
;; which reduces the constant vector size.

;;; Todo:

;; - Add tests.
;; - Add benchmarks.

;;; Code:

(defcustom n2o-enabled t
  "Non-nil value enables additional byte compiler optimizations.

As an effect, produced byte code, hopefully, is faster,
at the cost of slightly slower compilation."
  :group 'bytecomp
  :type 'boolean)

(advice-add 'byte-compile-form :around #'n2o-advice-byte-compile-form)

(defun n2o-advice-byte-compile-form (compile &rest args)
  "Advice function that is designed to wrap `byte-compile-form' (COMPILE).
Does simple forwarding if `n2o-enabled' is nil,
otherwise it acts as additional optimizer that sits between
builtin high level source optimizer and
builtin low level byte code optimizer.

ARGS are normaly a list of `form' and `for-effect', but it may
be untrue if other advice functions are set."
  (if (not n2o-enabled)
      (apply compile args)
    (let* ((form (pop args))
           (for-effect (pop args)))
      (setq form (n2o--source-opt form))
      (n2o--emit-form compile form for-effect))))

(defun n2o--source-opt (form)
  "Try to return optimized version of FORM.
Performs tranformations on the source level only."
  (pcase form
    ;; Spare `2' constant without any speed loss.
    (`(+ ,x 2)
     `(1+ (1+ ,x)))
    (`(+ 2 ,x)
     `(1+ (1+ ,x)))
    (`(- ,x 2)
     `(1- (1- ,x)))
    (`(* ,x 2)
     `(+ ,x ,x))
    (`(* 2 ,x)
     `(+ ,x ,x))
    ;; Do not know how to optimize -- return `form' unchanged.
    (_
     form)))

(defun n2o--emit-form (compile form for-effect)
  "With COMPILE as a fallback, emit optimized FORM.
Matches Lisp forms, outputs byte code.
FOR-EFFECT has the same meaning as in `byte-compile-form'."
  (pcase form
    ;; Noreturn function calls should be compiled with `for-effect' bound to nil,
    ;; because discard after them is a waste.
    (`(error . ,_)
     (funcall compile form nil))
    (`(signal . ,_)
     (funcall compile form nil))
    (`(throw . ,_)
     (funcall compile form nil))
    (`(while ,test . ,body)
     (n2o--emit-while test body))
    (_ (funcall compile form for-effect))))

(defun n2o--emit-while (test body)
  "Compiles (while TEST ...BODY) form.
Like `byte-compile-while', but outputs code that
does only 1 `goto' per iteration instead of 2.
Surprisingly, the speedup is not significant, but
can be measured (for large number of iterations)."
  (let ((cond-label (byte-compile-make-tag))
        (body-label (byte-compile-make-tag)))
    ;; Make a just into condition check right away.
    ;; If condition is true, looping commences.
    ;; This kind of `while' compilation is very
    ;; popular among native code producing compilers.
    (byte-compile-out 'byte-goto cond-label)
    (byte-compile-out-tag body-label)
    (byte-compile-body body t)
    (byte-compile-out-tag cond-label)
    (byte-compile-form test)
    (byte-compile-out 'byte-goto-if-not-nil body-label)
    (setq byte-compile--for-effect nil)))

;;; n2o.el ends here
