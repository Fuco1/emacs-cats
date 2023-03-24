;;; cats-function-helpers.el --- Function helpers -*- lexical-binding: t -*-

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

(defun cats-partial (fn &optional n)
  "Curry the function FN with N arguments."
  (setq n (or n 1))
  (if (= n 1)
      (lambda (x) (funcall fn x))
    (lambda (x)
      (cats-partial
       (apply-partially fn x)
       (1- n)))))

(provide 'cats-function-helpers)
;;; cats-function-helpers.el ends here
