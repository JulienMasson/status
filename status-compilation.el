;;; status-compilation.el

;; Copyright (C) 2017 Julien Masson

;; Author: Julien Masson <massonju.eseo@gmail.com>
;; Keywords: emacs status compilation

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

(defgroup status-compilation nil
  "Status compilation group."
  :group 'status)

(defface status-compilation-face
  '((t (:weight bold :width ultra-expanded
		:inherit variable-pitch :foreground "sienna2")))
  "face for current compilation"
  :group 'status-compilation)

(defun status-compilation ()
  (when compilation-in-progress
    (propertize "Compiling ..." 'face 'status-compilation-face)))


(provide 'status-compilation)
