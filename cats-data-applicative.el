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
  (cats-pure (cats-nothing) 1) ;; => (cats-just 1)")

;; (cats-apply :: (function ((:F (function (&a) &b)) (:F &a)) (:F &b)))
(cl-defgeneric cats-apply (a b)
  "Apply effect A on B.

Applicative functors must obey the following laws:

  (<*> is infix notation for `cats-apply')

  pure id <*> v = v                            -- Identity
  pure f <*> pure x = pure (f x)               -- Homomorphism
  u <*> pure y = pure ($ y) <*> u              -- Interchange
  pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition
")

;; (cats-lift-a2 :: (function ((function (&a &b) &c) (:T &a) (:T &b)) (:T &c)))
(cl-defgeneric cats-lift-a2 (fn a b)
  (cats-apply (cats-fmap (cats-partial fn 2) a) b))


;;; List

(cl-defmethod cats-pure ((_ list) a)
  (list a))

(cl-defmethod cats-apply ((fn list) (a list))
  (apply #'append (mapcar (lambda (f) (mapcar f a)) fn)))


;;; Vector

(cl-defmethod cats-pure ((_ vector) a)
  (vector a))

(cl-defmethod cats-apply ((fn vector) (a vector))
  (apply #'vconcat (mapcar (lambda (f) (mapcar f a)) fn)))


(provide 'cats-data-applicative)
;;; cats-data-applicative.el ends here
