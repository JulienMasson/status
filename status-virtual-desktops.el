;;; status-virtual-desktops.el

;; Copyright (C) 2014 Julien Masson

;; Author: Julien Masson <massonju.eseo@gmail.com>
;; Keywords: emacs status virtual-desktops

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

(defgroup status-virtual-desktops nil
  "Status virtual-desktops group."
  :group 'status)

(defcustom status-virtual-desktops-fmt "Desktop: %d"
  "Status format to display the current virtual-desktops in the status area")

(defface status-virtual-desktops-face
  '((t (:weight bold :width ultra-expanded
		:inherit variable-pitch :foreground "orange")))
  "face for current virtual-desktops"
  :group 'status-virtual-desktops)

(eval-after-load "virtual-desktops"
  '(progn (defun status-virtual-desktops ()
	    (unless (zerop virtual-desktops-current)
	      (propertize (format status-virtual-desktops-fmt virtual-desktops-current)
			  'face 'status-virtual-desktops-face)))))

(provide 'status-virtual-desktops)
