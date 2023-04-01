;;; cats-data-applicative.el --- Applicative -*- lexical-binding: t -*-

;; Copyright (C) 2023 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Created: 23rd March 2023

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cats-data-maybe)
(require 'cats-data-ziplist)
(require 'cats-data-functor)
(require 'cats-function-helpers)


;;; Applicative class

;; (cats-pure :: (function ((&f &a) &a) (&f &a)))
(cl-defgeneric cats-pure (pure a)
  "Return A in PURE context.

This function is a wrapper to specify the methods on the PURE
argument.  PURE is an instance of the structure into which A is
embedded.

Examples:

  (cats-pure [] 1)             ;; => [1]
  (cats-pure nil 1)            ;; => (1)
  (cats-pure (cats-nothing) 1) ;; => (cats-just 1)"
  (declare (indent 1)))

;; (cats-apply :: (function ((:F (function (&a) &b)) (:F &a)) (:F &b)))
(cl-defgeneric cats-apply (a b)
  "Apply effect A on B.

Applicative functors must obey the following laws:

  (<*> is infix notation for `cats-apply')

  pure id <*> v = v                            -- Identity
  pure f <*> pure x = pure (f x)               -- Homomorphism
  u <*> pure y = pure ($ y) <*> u              -- Interchange
  pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition")

;; (cats-lift-a2 :: (function ((function (&a &b) &c) (:T &a) (:T &b)) (:T &c)))
(cl-defgeneric cats-lift-a2 (fn a b)
  "Lift a binary function FN to actions A and B."
  (cats-apply (cats-fmap (cats-partial fn 2) a) b))


;;; List

(cl-defmethod cats-pure ((_ list) a)
  "Return A in pure context."
  (list a))

(cl-defmethod cats-apply ((fn list) (a list))
  "Apply FN to A list."
  (apply #'append (mapcar (lambda (f) (mapcar f a)) fn)))


;;; Cons

(cl-defmethod cats-pure ((_ cons) a)
  "Return A in pure context."
  (list a))

(cl-defmethod cats-apply ((fn cons) (a cons))
  "The apply implementation for cons with zipping.

For regular lists the nondeterministic list method is called
instead.

FN and A are cons functors."
  (let ((last-cons-fn (last fn))
        (last-cons-a (last a))
        (fn-len (safe-length fn))
        (a-len (safe-length a)))
    (if (or (/= fn-len a-len)
            (and (not (cdr last-cons-fn))
                 (not (cdr last-cons-a))))
        (cl-call-next-method)
      (let* ((start-fn (seq-take fn fn-len))
             (start-a (seq-take a fn-len))
             (re (cl-mapcar #'funcall start-fn start-a)))
        (setf (cdr (last re))
              (funcall (cdr last-cons-fn) (cdr last-cons-a)))
        re))))


;;; Ziplist

(cl-defmethod cats-pure ((_ cats-data-ziplist) a)
  "Return A in pure context."
  (cats-ziplist (list a)))

(cl-defmethod cats-apply ((fn cats-data-ziplist) (a cats-data-ziplist))
  "Apply FN to A list."
  (cats-ziplist (cl-mapcar #'funcall (oref fn list) (oref a list))))


;;; Vector

(cl-defmethod cats-pure ((_ vector) a)
  "Return A in pure context."
  (vector a))

(cl-defmethod cats-apply ((fn vector) (a vector))
  "Apply FN to A vector."
  (apply #'vconcat (mapcar (lambda (f) (mapcar f a)) fn)))


;;; Maybe
(cl-defmethod cats-pure ((_ cats-data-maybe) a)
  "Return A in pure context."
  (cats-just a))

(cl-defmethod cats-apply ((fn cats-data-maybe) (a cats-data-maybe))
  "Apply FN to A vector."
  (if (and (cats-just-p fn)
           (cats-just-p a))
      (cats-just (funcall (cats-just-value fn) (cats-just-value a)))
    (cats-nothing)))

(provide 'cats-data-applicative)
;;; cats-data-applicative.el ends here
