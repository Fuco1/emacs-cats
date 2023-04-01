;;; cats-data-traversable.el --- Traversable -*- lexical-binding: t -*-

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


;;; Traversable

;; (cats-traverse :: (function ((function (&a) (:F &b)) (:T &a)) (:F (:T &b))))
(cl-defgeneric cats-traverse (fn traversable &optional pure)
  "Traverse a traversable structure with a function.

FN is a function that takes a value and maps it to an applicative
effect.

TRAVERSABLE is the traversable structure.

PURE is an instance of an applicative class returned by fn.  It
is optional because it is not necessary for non-empty
traversibles."
  (cats-sequence-a (cats-fmap fn traversable) pure))

;; (cats-sequence-a :: (function ((:T (:F &a))) (:F (:T &a))))
(cl-defgeneric cats-sequence-a (traversable &optional pure)
  "Evaluate each action in TRAVERSABLE from left to right, and collect the results.

TRAVERSABLE is the traversable structure.

PURE is an instance of an applicative class returned by fn.  It
is optional because it is not necessary for non-empty
traversibles."
  (cats-traverse #'identity traversable pure))

;; (cats-mapm :: (function ((function (&a) (:M &b)) (:T &a)) (:M (:T &b))))
(cl-defgeneric cats-mapm (fn traversable &optional return)
  "Same as `cats-traverse' but for monadic actions.

FN is a function that takes a value and maps it to a monadic
effect.  TRAVERSABLE is the traversable structure.  RETURN is an
instance of the monad class returned by FN.

The default implementation of this method is `cats-traverse' but
it exists because some monads can have more efficient
implementation due to monads having more structure than
applicatives."
  (cats-traverse fn traversable return))

;; (cats-sequence :: (function ((:T (:M &a))) (:M (:T &a))))
(cl-defgeneric cats-sequence (traversable &optional return)
  "Same as `cats-sequence-a' but for monadic actions.

TRAVERSABLE is the traversable structure.  RETURN is an instance
of the monad class returned by FN.

The default implementation of this method is `cats-sequence-a'
but it exists because some monads can have more efficient
implementation due to monads having more structure than
applicatives."
  (cats-sequence-a traversable return))


;;; List

(cl-defmethod cats-traverse (fn (traversable list) &optional pure)
  "Traverse a list with a function.

FN is a function that takes a value and maps it to an applicative
effect.

TRAVERSABLE is the traversable structure.

PURE is an instance of an applicative class returned by fn.  It
is optional because it is not necessary for non-empty
traversibles."
  (cats-foldr
   (lambda (it acc)
     (cats-lift-a2 #'cons (funcall fn it) acc))
   (cats-pure pure nil)
   traversable))


;;; Ziplist

(cl-defmethod cats-traverse (fn (traversable cats-data-ziplist) &optional pure)
  "Traverse a ziplist with a function.

FN is a function that takes a value and maps it to an applicative
effect.

TRAVERSABLE is the traversable structure.

PURE is an instance of an applicative class returned by fn.  It
is optional because it is not necessary for non-empty
traversibles."
  (cats-foldr
   (lambda (it acc)
     (cats-lift-a2
      ;; like cons but wrap in a ziplist box
      (lambda (a b)
        (cats-ziplist
         (cons a (oref b list))))
      (funcall fn it)
      acc))
   (cats-pure pure (cats-ziplist nil))
   traversable))


;;; Maybe

(cl-defmethod cats-traverse (fn (traversable cats-data-maybe) &optional pure)
  "Traverse a maybe with a function.

FN is a function that takes a value and maps it to an applicative
effect.

TRAVERSABLE is the traversable structure.

PURE is an instance of an applicative class returned by fn.  It
is optional because it is not necessary for non-empty
traversibles."
  (if (cats-nothing-p traversable)
      (cats-pure pure (cats-nothing))
    (cats-fmap
     #'cats-just
     (funcall fn (cats-just-value traversable)))))

(provide 'cats-data-traversable)
;;; cats-data-traversable.el ends here
