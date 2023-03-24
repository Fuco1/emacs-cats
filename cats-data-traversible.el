;;; cats-data-traversible.el --- Traversible -*- lexical-binding: t -*-

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
(require 'cats-data-functor)
(require 'cats-data-foldable)
(require 'cats-data-applicative)


;;; Traversible

;; (cats-traverse :: (function ((function (&a) (:F &b)) (:T &a)) (:F (:T &b))))
(cl-defgeneric cats-traverse (fn traversible pure)
  "Traverse a traversible structure with a function.

FN is a function that takes a value and maps it to an applicative
effect.

TRAVERSIBLE is the traversible structure.

PURE is an instance of an applicative class returned by fn."
  (cats-sequence-a (cats-fmap fn traversible) pure))

;; (cats-sequence-a :: (function ((:T (:F &a))) (:F (:T &a))))
(cl-defgeneric cats-sequence-a (traversible pure)
  "Evaluate each action in TRAVERSIBLE from left to right, and collect the results.

TRAVERSIBLE is the traversible structure.

PURE is an instance of an applicative class returned by fn."
  (cats-traverse #'identity traversible pure))

;; (cats-mapm :: (function ((function (&a) (:M &b)) (:T &a)) (:M (:T &b))))
(cl-defgeneric cats-mapm (fn traversible return)
  "Same as `cats-traverse' but for monadic actions.

The default implementation of this method is `cats-traverse' but
it exists because some monads can have more efficient
implementation due to monads having more structure than
applicatives."
  (cats-traverse fn traversible return))

;; (cats-sequence :: (function ((:T (:M &a))) (:M (:T &a))))
(cl-defgeneric cats-sequence (traversible return)
  "Same as `cats-sequence-a' but for monadic actions.

The default implementation of this method is `cats-sequence-a'
but it exists because some monads can have more efficient
implementation due to monads having more structure than
applicatives."
  (cats-sequence-a traversible return))


;;; List

(cl-defmethod cats-traverse (fn (lst list) pure)
  "Traverse a list with a function."
  (cats-foldr
   (lambda (it acc)
     (cats-lift-a2 #'cons (funcall fn it) acc))
   (cats-pure pure nil)
   lst))


;;; Maybe

(cl-defmethod cats-traverse (fn (traversible cats-data-maybe) pure)
  "Traverse a maybe with a function."
  (if (cats-nothing-p traversible)
      (cats-pure pure (cats-nothing))
    (cats-fmap
     #'cats-just
     (funcall fn (cats-just-value traversible)))))

(provide 'cats-data-traversible)
;;; cats-data-traversible.el ends here
