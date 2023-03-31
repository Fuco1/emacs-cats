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

(cl-defmethod cl-print-object ((this cats-data-state) stream)
  "Print the object THIS to STREAM."
  (princ "#<cats-data-state " stream)
  (cl-print-object (if (slot-boundp this 'run)
                       (oref this run)
                     nil)
                   stream)
  (princ ">" stream))

(defun cats-eval-state (m initial-state)
  "Evaluate the state monad M with INITIAL-STATE and return result."
  (car (funcall (oref m run) initial-state)))

(defun cats-exec-state (m initial-state)
  "Evaluate the state monad M with INITIAL-STATE and return state."
  (cdr (funcall (oref m run) initial-state)))

(defun cats-run-state (m initial-state)
  "Evaluate the state monad M with INITIAL-STATE and return both result and state."
  (funcall (oref m run) initial-state))

(defun cats-state-get ()
  "Get the state."
  (cats-data-state
   :run (lambda (s) (cons s s))))

(defun cats-state-put (value)
  "Put VALUE into the state."
  (cats-data-state
   :run (lambda (_) (cons nil value))))

(defun cats-state-modify (fn)
  "Modify the state with FN."
  (cats-data-state
   :run (lambda (state) (cons nil (funcall fn state)))))

(cl-defmethod cats-fmap (fn (m cats-data-state))
  "Map FN over the result of M."
  (cats-data-state
   :run (lambda (st)
          (pcase-let ((`(,a . ,state) (funcall (oref m run) st)))
            (cons (funcall fn a) state)))))

(cl-defmethod cats-pure ((_ cats-data-state) v)
  "Return a state monad with value V."
  (cats-data-state
   :run (lambda (st) (cons v st))))

(cl-defmethod cats-apply ((fn cats-data-state) (m cats-data-state))
  "Apply FN to M."
  (cats-data-state
   :run (lambda (st)
          (pcase-let* ((`(,f . ,state) (funcall (oref fn run) st))
                       (`(,x . ,state2) (funcall (oref m run) state)))
            (cons (funcall f x) state2)))))

(cl-defmethod cats-bind ((m cats-data-state) fn)
  "Bind M with FN."
  (cats-data-state
   :run (lambda (st)
          (pcase-let* ((`(,a . ,state) (funcall (oref m run) st)))
            (funcall (oref (funcall fn a) run) state)))))

(provide 'cats-data-state)
;;; cats-data-state.el ends here
