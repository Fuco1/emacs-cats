;;; cats-data-foldable.el --- Foldable -*- lexical-binding: t -*-

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

(require 'eieio-base)
(eval-and-compile (setq eieio-backward-compatibility nil))

(require 'cats-data-maybe)
(require 'cats-data-ziplist)
(require 'cats-data-monoid)


;;; Foldable generics

;; &m must be a Monoid
;; (cats-fold-map :: (function ((function (&a) &m) (&t &a) &m) &m))
(cl-defgeneric cats-fold-map (fn foldable mempty)
  "Map each element into a monoid and combine the results with mappend.

FN is the mapping function, FOLDABLE is the structure to fold.
MEMPTY is an instance of the target monoid, this is used to
resolve the correct mempty method.

Examples:

  (cats-fold-map #'identity (cats-just 1) 0)         ;; => 1
  (cats-fold-map #'number-to-string (list 1 2 3) \"\") ;; => \"123\"

See also `cats-foldr'."
  (cats-foldr
   (lambda (it acc) (cats-mappend (funcall fn it) acc))
   (cats-mempty mempty)
   foldable))

;; (cats-foldr :: (function ((function (&a &b) &b) &b (&t &a)) &b))
(cl-defgeneric cats-foldr (fn init foldable)
  "Right-associative fold of a structure.

FN is the folding function, INIT is the initial value, and
FOLDABLE is the structure to fold."
  (funcall
   (oref
    (cats-fold-map
     (lambda (a) (cats-endo (apply-partially fn a)))
     foldable
     (cats-mempty (cats-data-endo)))
    app-endo)
   init))

(cl-defgeneric cats-length (foldable)
  "Return the number of elements in the structure.

FOLDABLE is the structure to fold."
  (cats-foldr (lambda (_ acc) (1+ acc)) 0 foldable))

(cl-defgeneric cats-to-list (foldable)
  "Convert the structure to a list.

FOLDABLE is the structure to fold."
  (cats-foldr (lambda (it acc) (cons it acc)) nil foldable))

(cl-defgeneric cats-find (fn foldable)
  "Find the first element of the structure matching the predicate FN.

FOLDABLE is any foldable structure.

If a value is found, it is returned as `cats-just', otherwise
`cats-nothing' is returned."
  (let ((result nil))
    (catch 'cats-done
      (cats-foldr
       (lambda (it _)
         (when (funcall fn it)
           (setq result (cats-just it))
           (throw 'cats-done t)))
       nil
       foldable))
    (or result (cats-nothing))))


;;; List

(cl-defmethod cats-foldr (fn init (foldable list))
  "Right-associative fold of a structure.

FN is the folding function, INIT is the initial value, and
FOLDABLE is the structure to fold."
  (let* ((acc init)
         (vector (vconcat foldable))
         (i (length vector))
         it)
    (while (> i 0)
      (setq i (1- i)
            it (aref vector i))
      (setq acc (funcall fn it acc)))
    acc))

(cl-defmethod cats-length ((foldable list))
  "Return the number of elements in the FOLDABLE structure."
  (length foldable))

(cl-defmethod cats-to-list ((foldable list))
  "Convert the FOLDABLE structure to a list."
  foldable)


;;; Ziplist

(cl-defmethod cats-foldr (fn init (foldable cats-data-ziplist))
  "Right-associative fold of a structure.

FN is the folding function, INIT is the initial value, and
FOLDABLE is the structure to fold."
  (cats-foldr fn init (oref foldable list)))

(cl-defmethod cats-length ((foldable cats-data-ziplist))
  "Return the number of elements in the FOLDABLE structure."
  (length (oref foldable list)))

(cl-defmethod cats-to-list ((foldable cats-data-ziplist))
  "Convert the FOLDABLE structure to a list."
  (oref foldable list))


;;; Vector

(cl-defmethod cats-fold-map (fn (foldable vector) mempty)
  "Map each element into a monoid and combine the results with mappend.

FN is the mapping function, FOLDABLE is the structure to fold.
MEMPTY is an instance of the target monoid, this is used to
resolve the correct mempty method."
  (cats-fold-map fn (append foldable nil) mempty))

(cl-defmethod cats-foldr (fn init (foldable vector))
  "Right-associative fold of a structure.

FN is the folding function, INIT is the initial value, and
FOLDABLE is the structure to fold."
  (cats-foldr fn init (append foldable nil)))

(cl-defmethod cats-length ((foldable vector))
  "Return the number of elements in the FOLDABLE structure."
  (length foldable))

(cl-defmethod cats-to-list ((foldable vector))
  "Convert the FOLDABLE structure to a list."
  (append foldable nil))


;;; Maybe

(cl-defmethod cats-fold-map (fn (foldable cats-data-maybe) mempty)
  "Map each element into a monoid and combine the results with mappend.

FN is the mapping function, FOLDABLE is the structure to fold.
MEMPTY is an instance of the target monoid, this is used to
resolve the correct mempty method."
  (cats-maybe mempty fn foldable))

(cl-defmethod cats-foldr (fn init (foldable cats-data-maybe))
  "Right-associative fold of a structure.

FN is the folding function, INIT is the initial value, and
FOLDABLE is the structure to fold."
  (if (cats-nothing-p foldable)
      init
    (funcall fn (cats-just-value foldable) init)))

(provide 'cats-data-foldable)
;;; cats-data-foldable.el ends here
