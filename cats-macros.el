;;; cats-macros.el --- Macros for cats -*- lexical-binding: t -*-

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

(require 'edebug)

(defun cats-is-quoted-fn (x)
  (or (and (listp x)
           (or
            (eq (car x) 'function)
            (eq (car x) 'quote))
           (functionp (cadr x)))
      (functionp x)))

(def-edebug-spec cats-do-item
  (&or (":=" sexp form) ("let" sexp form) form))

(defmacro cats-do (init &rest actions)
  (declare (debug (cats-do-item &rest cats-do-item)))
  (cond
   ((and (listp init)
         (eq (car init) :=))
    (if actions
        `(cats-bind
          ,(nth 2 init)
          (lambda (,(nth 1 init))
            (cats-do ,@actions)))
      (nth 2 init)))
   ((and (listp init)
         (eq (car init) 'let))
    (if actions
        `(let ((,(nth 1 init) ,(nth 2 init)))
           (cats-do ,@actions))
      (error "let statement can not be last")))
   ((not actions)
    init)
   (t (if (cats-is-quoted-fn (car actions))
          `(cats-bind
            ,init
            (cats-do ,@actions))
        `(cats-seq
          ,init
          (cats-do ,@actions))))))

(provide 'cats-macros)
;;; cats-macros.el ends here
