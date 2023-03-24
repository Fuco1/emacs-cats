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

(require 'cats-data-monad)

(defclass cats-data-maybe () () :abstract t)
(defclass cats-data-just (cats-data-maybe) ((value :initarg :value :accessor cats-just-value)))
(defclass cats-nothing (cats-data-maybe eieio-singleton) ())

(defun cats-just (x)
  (cats-data-just :value x))

(defun cats-maybe (default fn maybe)
  (if (cats-data-just-p maybe)
      (funcall fn (cats-just-value maybe))
    default))

(defalias 'cats-just-p 'cats-data-just-p)

(cl-defmethod cats-fmap (fn (a cats-data-maybe))
  (if (cats-nothing-p a)
      (cats-nothing)
    (cats-just (funcall fn (cats-just-value a)))))

(cl-defmethod cats-pure ((_ cats-data-maybe) a)
  (cats-just a))

(cl-defmethod cats-apply ((fn cats-data-maybe) (a cats-data-maybe))
  (if (and (cats-just-p fn)
           (cats-just-p a))
      (cats-just (funcall (cats-just-value fn) (cats-just-value a)))
    (cats-nothing)))

(cl-defmethod cats-traverse (fn (traversible cats-data-maybe) pure)
  (if (cats-nothing-p traversible)
      (cats-pure pure (cats-nothing))
    (cats-fmap
     #'cats-just
     (funcall fn (cats-just-value traversible)))))

(cl-defmethod cats-bind ((m cats-data-maybe) fn)
  (if (cats-nothing-p m)
      (cats-nothing)
    (funcall fn (cats-just-value m))))

(provide 'cats-data-maybe)
;;; cats-data-maybe.el ends here
