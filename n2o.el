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

(defvar n2o--type-map nil
  "Maps function name (symbol) to it's return value type.
The possible types are described in `n2o--typeof' docstring.")

(defun n2o--typeof (x)
  "Tries to infer proper type of X.
If impossible, returns nil.

Type names are keywords.
Possible return values (except nil):
    :int <- (integerp X)
    :float <- (floatp X)
    :num <- (numberp X)
    :str <- (stringp X)
    :bool <- (booleanp X)"
  (cond
   ((listp x)
    (gethash (car x) n2o--type-map))
   ((integerp x) :int)
   ((floatp x) :float)
   ((numberp x) :num) ;; Should go after int and float checks
   ((stringp x) :str)
   ((booleanp x) :bool) ;; Should go after list check
   (t nil)))

(defsubst n2o--?int (x) (eq :int (n2o--typeof x)))
(defsubst n2o--?float (x) (eq :float (n2o--typeof x)))
(defsubst n2o--?num (x) (eq :num (n2o--typeof x)))
(defsubst n2o--?str (x) (eq :str (n2o--typeof x)))

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
    ;; More complex rewrites.
    (`(format . ,_)
     (n2o--rewrite-format form))
    (`(eql ,x ,y)
     (n2o--rewrite-eql form x y))
    (`(= ,x ,y)
     (n2o--rewrite-= form x y))
    ;; Do not know how to optimize -- return `form' unchanged.
    (_ form)))

(defun n2o--rewrite-format (form)
  (pcase form
    (`(format "%d" ,x)
     `(number-to-string ,x))
    (`(format "%s" ,x)
     ;; %s format is inefficient.
     (let ((x-type (n2o--typeof x)))
       (if (memq x-type '(:int :float :num))
           `(number-to-string ,x)
         ;; Could return `prin1-to-string', but it
         ;; requires some investigation whenever this is
         ;; valid or not.
         form)))
    (_ form)))

(defun n2o--rewrite-eql (form x y)
  ;; `eql' is slower than `eq' and `equal'.
  ;; `eql' behaves like `equal' when first operand is float,
  ;; otherwise it calls `eq' under the hood.
  (let ((x-type (n2o--typeof x)))
    (if (and x-type
             (not (eq :float x-type)))
        `(eq ,x ,y)
      form)))

(defun n2o--rewrite-= (form x y)
  ;; For integers, `eq' is a valid and faster alternative.
  (if (and (n2o--?int x)
           (n2o--?int y))
      `(eq ,x ,y)
    form))

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

;; Package initialization.
(let ((type-map (make-hash-table :test 'eq)))
  ;; This map is never "filled enough".
  (dolist (info '(;; `:int' functions.
                  (lsh . :int)
                  (char-syntax . :int)
                  (point . :int)
                  ;; `:float' functions.
                  (float . :float)
                  ;; `:num' functions
                  (string-to-number . :num)
                  ;; `:str' functions.
                  (int-to-string . :str)
                  (number-to-string . :str)
                  (concat . :str)
                  ;; `:bool' functions.
                  (zerop . :bool)))
    (let ((sym (car info))
          (type (cdr info)))
      (puthash sym type type-map)))
  (setq n2o--type-map type-map))

(provide 'n2o)

;;; n2o.el ends here
