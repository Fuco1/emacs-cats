;;; cats-data-state.el --- State -*- lexical-binding: t -*-

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

(require 'cats-data-monad)
(eval-when-compile (require 'cats-macros))

(defclass cats-data-state ()
  ((run :initarg :run)))

(defun cats-eval-state (m initial-state)
  (car (funcall (oref m run) initial-state)))

(defun cats-exec-state (m initial-state)
  (cdr (funcall (oref m run) initial-state)))

(defun cats-run-state (m initial-state)
  (funcall (oref m run) initial-state))

(defun cats-state-get ()
  (cats-data-state
   :run (lambda (s) (cons s s))))

(defun cats-state-put (value)
  (cats-data-state
   :run (lambda (_) (cons nil value))))

(defun cats-state-modify (fn)
  (cats-data-state
   :run (lambda (state) (cons nil (funcall fn state)))))

(cl-defmethod cats-fmap (fn (m cats-data-state))
  (cats-data-state
   :run (lambda (st)
          (pcase-let ((`(,a . ,state) (funcall (oref m run) st)))
            (cons (funcall fn a) state)))))

(cl-defmethod cats-apply ((fn cats-data-state) (m cats-data-state))
  (cats-data-state
   :run (lambda (st)
          (pcase-let* ((`(_ . ,state) (funcall (oref fn run) st))
                       (`(,b . ,state2) (funcall (oref m run) state)))
            (cons b state2)))))

(cl-defmethod cats-bind ((m cats-data-state) fn)
  (cats-data-state
   :run (lambda (st)
          (pcase-let* ((`(,a . ,state) (funcall (oref m run) st)))
            (funcall (oref (funcall fn a) run) state)))))

(provide 'cats-data-state)
;;; cats-data-state.el ends here
