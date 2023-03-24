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

(require 'cats-data-functor)
(require 'cats-data-applicative)


;;; Traversible

;; (cats-traverse :: (function ((function (&a) (:F &b)) (:T &a)) (:F (:T &b))))
(cl-defgeneric cats-traverse (fn traversible pure)
  (cats-sequence-a (cats-fmap fn traversible) pure))

;; (cats-sequence-a :: (function ((:T (:F &a))) (:F (:T &a))))
(cl-defgeneric cats-sequence-a (traversible pure)
  (cats-traverse #'identity traversible pure))

;; (cats-mapm :: (function ((function (&a) (:M &b)) (:T &a)) (:M (:T &b))))
(cl-defgeneric cats-mapm (fn traversible return)
  (cats-traverse fn traversible return))

;; (cats-sequence :: (function ((:T (:M &a))) (:M (:T &a))))
(cl-defgeneric cats-sequence (traversible return)
  (cats-sequence-a traversible return))


;;; List

(cl-defmethod cats-traverse (fn (lst list) pure)
  (-reduce-r-from
   (lambda (it acc)
     (cats-lift-a2 #'cons (funcall fn it) acc))
   (cats-pure pure nil)
   lst))

(provide 'cats-data-traversible)
;;; cats-data-traversible.el ends here
