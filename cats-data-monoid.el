;;; cats-data-monoid.el --- Monoid -*- lexical-binding: t -*-

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

(require 'eieio)
(eval-and-compile (setq eieio-backward-compatibility nil))

(require 'dash)

(defclass cats-data-endo ()
  ;; (app-endo :: (function (&a) &a))
  ((app-endo :initarg :app-endo))
  "The monoid of endomorphisms under composition.")

(defun cats-endo (a)
  (cats-data-endo :app-endo a))


;;; Monoid generics

;; (cats-mempty :: (function (&a) &a))
(cl-defgeneric cats-mempty (class))

;; (cats-mappend :: (function (&a &a) &a))
(cl-defgeneric cats-mappend (a b))

;; (cats-mconcat :: (function ((list &a) &a) &a))
(cl-defgeneric cats-mconcat (lst &optional mempty)
  (-reduce-r-from #'cats-mappend (cats-mempty mempty) lst))


;;; Endo

(cl-defmethod cats-mempty ((_ cats-data-endo))
  (cats-endo #'identity))

(cl-defmethod cats-mappend ((a cats-data-endo) (b cats-data-endo))
  (cats-endo
   (lambda (x)
     (funcall (oref a app-endo)
              (funcall (oref b app-endo) x)))))


;;; List

(cl-defmethod cats-mempty ((_ list))
  nil)

(cl-defmethod cats-mappend ((list1 list) (list2 list))
  (append list1 list2))

(cl-defmethod cats-mconcat ((lists list) &optional _)
  (apply #'append lists))


;;; Vector

(cl-defmethod cats-mempty ((_ vector))
  (vector))

(cl-defmethod cats-mappend ((vec1 vector) (vec2 vector))
  (vconcat vec1 vec2))

(cl-defmethod cats-mconcat ((vecs vector) &optional _)
  (apply #'vconcat vecs))

(provide 'cats-data-monoid)
;;; cats-data-monoid.el ends here
