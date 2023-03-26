;;; cats-data-maybe.el --- Maybe -*- lexical-binding: t -*-

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

(defclass cats-data-maybe () () :abstract t)
(defclass cats-data-just (cats-data-maybe) ((value :initarg :value :accessor cats-just-value)))
(defclass cats-nothing (cats-data-maybe eieio-singleton) ())

(cl-defmethod cl-print-object ((this cats-data-just) stream)
  (princ "#<cats-data-just " stream)
  (cl-print-object (oref this value) stream)
  (princ ">" stream))

(cl-defmethod cl-print-object ((_this cats-nothing) stream)
  (princ "#<cats-nothing>" stream))

(defun cats-just (x)
  "Wrap X in a `cats-data-just' object."
  (cats-data-just :value x))

(defun cats-maybe (default fn maybe)
  "Apply FN to the value of MAYBE, or return DEFAULT if MAYBE is `cats-nothing'."
  (if (cats-data-just-p maybe)
      (funcall fn (cats-just-value maybe))
    default))

(defalias 'cats-just-p 'cats-data-just-p)

(provide 'cats-data-maybe)
;;; cats-data-maybe.el ends here
