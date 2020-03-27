;;; status-echat.el

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

(require 'echat)

(defgroup status-echat nil
  "Status echat group."
  :group 'status)

(defface status-echat-face-normal
  '((t (:foreground "DimGray" :weight normal)))
  "Face used for echat conversation already read"
  :group 'status-echat)

(defun status-echat-string (unread)
  (let* ((name (plist-get unread :name))
	 (echat (plist-get unread :echat))
	 (count (plist-get unread :count)))
    (propertize (if (zerop count)
		    name
		  (format "%s (%d)" name count))
		'face (oref echat face))))

(defun status-echat-unread-buffer-p (name unreads)
  (cl-find-if (lambda (unread)
		(string= (plist-get unread :name) name))
	      unreads))

(defun status-echat-buffers (unreads)
  (let (buffers)
    (dolist (echat echats)
      (let ((face (oref echat face)))
	(pcase-dolist (`(,name . ,buffer) (oref echat buffers))
	  (when (buffer-live-p buffer)
	    (let* ((unread-buffer-p (status-echat-unread-buffer-p name unreads))
		   (buffer (propertize name 'face (if unread-buffer-p
						      face
						    'status-echat-face-normal))))
	      (add-to-list 'buffers buffer t))))))
    buffers))

(defun status-echat ()
  (let* ((data (delq nil (mapcar #'echat-unread-queries echats)))
	 (unreads (mapcar #'status-echat-string (apply #'append data)))
	 (buffers (status-echat-buffers unreads))
	 (strings (delq nil (append unreads buffers))))
    (when strings (string-join strings " - "))))

(provide 'status-echat)
