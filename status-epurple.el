;;; status-epurple.el

;; Copyright (C) 2020 Julien Mason

;; Author: Julien Masson <massonju.eseo@gmail.com>
;; Keywords: emacs status for tab bar mode

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

(require 'epurple)

(defgroup status-epurple nil
  "Status epurple group."
  :group 'status)

(defface status-epurple-face-normal
  '((t (:foreground "DimGray" :weight normal)))
  "Face used for epurple conversation already read"
  :group 'status-epurple)

(defun status-epurple-buffers ()
  (let (buffers)
    (dolist (account epurple-accounts)
      (with-struct-slots (face prpl-buffers) epurple-account account
	(dolist (prpl-buffer prpl-buffers)
	  (with-struct-slots (conv-name buffer mute-p unread-p unread-count)
	    epurple-buffer prpl-buffer
	    (when (and (or (not buffer) (buffer-live-p buffer)) (not mute-p))
	      (let* ((name (decode-coding-string conv-name 'utf-8))
		     (str-face (if unread-p face 'status-epurple-face-normal))
		     (str (if (zerop unread-count) name
			    (format "%s (%s)" name unread-count))))
		(add-to-list 'buffers (propertize str 'face str-face))))))))
    buffers))

(defun status-epurple ()
  (when-let ((buffers (status-epurple-buffers)))
    (string-join buffers (propertize " - " 'face 'status-separator-face))))

(provide 'status-epurple)
