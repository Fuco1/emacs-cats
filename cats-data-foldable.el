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

(require 'dash)

(require 'cats-data-maybe)
(require 'cats-data-monoid)


;;; Foldable generics

;; &m must be a Monoid
;; (cats-fold-map :: (function ((function (&a) &m) (&t &a) &m) &m))
(cl-defgeneric cats-fold-map (fn foldable mempty)
  (cats-foldr
   (lambda (it acc) (cats-mappend (funcall fn it) acc))
   foldable
   (cats-mempty mempty)))

;; (cats-foldr :: (function ((function (&a &b) &b) &b (&t &a)) &b))
(cl-defgeneric cats-foldr (fn init foldable)
  (funcall
   (oref
    (cats-fold-map
     (lambda (a) (cats-endo (apply-partially fn a)))
     foldable
     (cats-mempty (cats-data-endo)))
    app-endo)
   init))

(cl-defgeneric cats-length (foldable)
  (cats-foldr (lambda (_ acc) (1+ acc)) 0 foldable))

(cl-defgeneric cats-to-list (foldable)
  (cats-foldr (lambda (it acc) (cons it acc)) nil foldable))

(cl-defgeneric cats-find (fn foldable)
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

(cl-defmethod cats-fold-map (fn (foldable list) mempty)
  (-reduce-r-from
   (lambda (it acc)
     (cats-mappend (funcall fn it) acc))
   (cats-mempty mempty)
   foldable))

(cl-defmethod cats-foldr (fn init (foldable list))
  (-reduce-r-from fn init foldable))

(cl-defmethod cats-length ((foldable list))
  (length foldable))

(cl-defmethod cats-to-list ((foldable list))
  foldable)


;;; Vector

(cl-defmethod cats-fold-map (fn (foldable vector) mempty)
  (-reduce-r-from
   (lambda (it acc)
     (cats-mappend (funcall fn it) acc))
   (cats-mempty mempty)
   (append foldable nil)))

(cl-defmethod cats-foldr (fn init (foldable vector))
  (-reduce-r-from fn init (append foldable nil)))

(cl-defmethod cats-length ((foldable vector))
  (length foldable))

(cl-defmethod cats-to-list ((foldable vector))
  (append foldable nil))


;;; Maybe

(cl-defmethod cats-fold-map (fn (foldable cats-data-maybe) mempty)
  (cats-maybe mempty fn foldable))

(cl-defmethod cats-foldr (fn init (foldable cats-data-maybe))
  (if (cats-nothing-p foldable)
      init
    (funcall fn (cats-just-value foldable) init)))

(provide 'cats-data-foldable)
;;; cats-data-foldable.el ends here
