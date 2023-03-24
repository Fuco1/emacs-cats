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
  "Construct an endomorphism from A."
  (cats-data-endo :app-endo a))


;;; Monoid generics

;; (cats-mempty :: (function (&a) &a))
(cl-defgeneric cats-mempty (inst)
  "Return the identity element of the monoid of INST's class.")

;; (cats-mappend :: (function (&a &a) &a))
(cl-defgeneric cats-mappend (a b)
  "Return the result of appending A and B.")

;; (cats-mconcat :: (function ((list &a) &a) &a))
(cl-defgeneric cats-mconcat (lst &optional mempty)
  "Return the result of concatenating LST.

MEMPTY is an instance fed to `cats-mempty'."
  (if (null lst)
      (cats-mempty mempty)
    (let ((re (cats-mempty mempty)))
      (while lst
        (setq re (cats-mappend re (pop lst))))
      re)))


;;; Endo

(cl-defmethod cats-mempty ((_ cats-data-endo))
  "Return the identity endomorphism."
  (cats-endo #'identity))

(cl-defmethod cats-mappend ((a cats-data-endo) (b cats-data-endo))
  "Return the composition of A and B."
  (cats-endo
   (lambda (x)
     (funcall (oref a app-endo)
              (funcall (oref b app-endo) x)))))


;;; List

(cl-defmethod cats-mempty ((_ list))
  "Return the empty list."
  nil)

(cl-defmethod cats-mappend ((list1 list) (list2 list))
  "Return the concatenation of LIST1 and LIST2."
  (append list1 list2))


;;; Vector

(cl-defmethod cats-mempty ((_ vector))
  "Return the empty vector."
  (vector))

(cl-defmethod cats-mappend ((vec1 vector) (vec2 vector))
  "Return the result of appending VEC1 and VEC2."
  (vconcat vec1 vec2))


;;; Number

(cl-defmethod cats-mempty ((_ number))
  "Return the zero number."
  0)

(cl-defmethod cats-mappend ((a number) (b number))
  "Return the sum of A and B."
  (+ a b))


;;; String

(cl-defmethod cats-mempty ((_ string))
  "Return the empty string."
  "")

(cl-defmethod cats-mappend ((str1 string) (str2 string))
  "Return the concatenation of STR1 and STR2."
  (concat str1 str2))

(provide 'cats-data-monoid)
;;; cats-data-monoid.el ends here
