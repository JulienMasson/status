;;; status-erc.el

;; Copyright (C) 2016 Julien Masson

;; Author: Julien Masson <massonju.eseo@gmail.com>
;; Keywords: emacs status cpu

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

(defgroup status-erc nil
  "Status cpu group."
  :group 'status)

(defface status-erc-face
  '((t (:weight bold :width ultra-expanded
		:inherit variable-pitch :foreground "pink")))
  "face for date and time"
  :group 'status-erc)

(defcustom status-erc-fmt "%s (%d)"
  "Status format to display the current gnus in the status area"
  :group 'status-gnus)

(defun format-erc (alist)
  (let ((count (cadr alist))
	(name (buffer-name (car alist))))
    (format status-erc-fmt name count)))

(eval-after-load 'erc-track
  '(progn
     (defun status-erc ()
       (let ((erc-string (mapcar 'format-erc erc-modified-channels-alist)))
	 (when erc-string
	   (propertize (mapconcat 'identity erc-string " - ") 'face 'status-erc-face))))))


(provide 'status-erc)
