;;; cats-data-monad.el --- Monad -*- lexical-binding: t -*-

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

(eval-when-compile (require 'cats-macros))

(require 'cats-data-applicative)


;;; Monad class

;; (cats-return :: (function ((function (&a) (&m &a)) &a) (&m &a)))
(cl-defgeneric cats-return (return a)
  "Return A in monadic context.

This function is a useless wrapper simply calling RETURN on A

Its purpose is to help reading the code and delimit the monadic
context from the rest."
  (cats-pure return a))

;; (cats-bind :: (function ((:M &a) (function (&a) (:M &b))) (:M &b)))
(cl-defgeneric cats-bind (a b))

;; (cats-seq :: (function ((:M &a) (:M &b)) (:M &b)))
(cl-defgeneric cats-seq (a b)
  (cats-bind a (lambda (_) b)))

(defclass cats-monad () ())


;;; List

(cl-defmethod cats-bind ((m list) fn)
  (apply #'append (mapcar fn m)))

(cl-defmethod cats-mapm (fn (lst list) return)
  (-reduce-r-from
   (lambda (it acc)
     (cats-do
      (:= x (funcall fn it))
      (:= xs acc)
      (cats-return return (cons x xs))))
   (cats-return return nil)
   lst))


;;; Vector

(cl-defmethod cats-bind ((m vector) fn)
  (apply #'vconcat (mapcar fn m)))

(cl-defmethod cats-mapm (fn (lst vector) return)
  (-reduce-r-from
   (lambda (it acc)
     (cats-do
      (:= x (funcall fn it))
      (:= xs acc)
      (cats-return return (vconcat (vector x) xs))))
   (cats-return return nil)
   (append lst nil)))

(provide 'cats-data-monad)
;;; cats-data-monad.el ends here
