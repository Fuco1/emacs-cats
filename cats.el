;;; cats.el --- Monads for Elisp -*- lexical-binding: t -*-

;; Copyright (C) 2023 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 20th March 2023
;; Package-requires: ((dash "2.17.0") (emacs "26.1"))
;; URL: https://github.com/Fuco1/emacs-cats

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

;; Category-theoretical structures for Elisp.

;;; Code:

(require 'eieio-base)
(eval-and-compile (setq eieio-backward-compatibility nil))

(require 'cats-data-monad)
(require 'cats-data-maybe)
(require 'cats-data-traversable)
(require 'cats-data-foldable)

(provide 'cats)
;;; cats.el ends here
