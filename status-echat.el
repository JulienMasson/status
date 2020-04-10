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

(defun status-echat-unreads ()
  (let (unreads)
    (dolist (echat echats)
      (let ((face (oref echat face)))
	(dolist (echat-buffer (oref echat buffers))
	  (with-slots (name buffer mute-p unread-p unread-count) echat-buffer
	    (when (and (or (not buffer) (buffer-live-p buffer))
		       (not mute-p))
	      (let ((str-face (if unread-p face 'status-echat-face-normal))
		    (str (if (zerop unread-count) name
			   (format "%s (%s)" name unread-count))))
		(add-to-list 'unreads (propertize str 'face str-face))))))))
    unreads))

(defun status-echat ()
  (when-let ((unreads (status-echat-unreads)))
    (string-join unreads (propertize " - " 'face 'status-separator-face))))

(provide 'status-echat)
