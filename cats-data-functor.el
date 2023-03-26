;;; cats-data-functor.el --- Functor -*- lexical-binding: t -*-

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


;;; Functor class

;; (cats-fmap :: (function ((function (&a) &b) (:F &a)) (:F &b)))
(cl-defgeneric cats-fmap (fn a)
  "Map FN over A.

This is a more generic version of `mapcar' working on any type,
not necessarily a sequence.

fmap implementations must obey the following laws:

  fmap id = id                      -- Identity
  fmap (f . g)  ==  fmap f . fmap g -- \"Homomorphism\"")


;;; List

(cl-defmethod cats-fmap (fn (a list))
  "Map FN over A."
  (mapcar fn a))


;;; Cons

(cl-defmethod cats-fmap (fn (a cons))
  "Map FN over A."
  (let ((last-cons (last a)))
    (if (not (cdr last-cons))
        (mapcar fn a)
      (let* ((l (safe-length a))
             (start (mapcar fn (seq-take a l))))
        (setf (cdr (last start))
              (funcall fn (cdr last-cons)))
        start))))


;;; Vector
(cl-defmethod cats-fmap (fn (a vector))
  "Map FN over A."
  (vconcat (mapcar fn a)))


;;; Maybe
(cl-defmethod cats-fmap (fn (a cats-data-maybe))
  "Map FN over A."
  (if (cats-nothing-p a)
      (cats-nothing)
    (cats-just (funcall fn (cats-just-value a)))))

(provide 'cats-data-functor)
;;; cats-data-functor.el ends here
