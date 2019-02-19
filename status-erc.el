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

(defface status-erc-face-normal
  '((t (:foreground "DimGray" :weight normal)))
  "Face used for erc conversation already read"
  :group 'status-erc)

(defface status-erc-face-unread
  '((t (:foreground "DarkOrange" :weight ultra-bold)))
  "Face used for erc conversation unread"
  :group 'status-erc)

(defcustom status-erc-fmt-normal "%s"
  "Format used for erc conversation already read"
  :group 'status-erc)

(defcustom status-erc-fmt-unread "%s (%d)"
  "Format used for erc conversation unread"
  :group 'status-erc)

(defcustom status-erc-blacklist nil
  "List of ERC buffers name ignored"
  :group 'status-erc)

(defun erc-blacklist-p (buffer)
  (member buffer (mapcar #'get-buffer status-erc-blacklist)))

(defun erc-chat-list ()
  (seq-filter (lambda (buffer)
		(with-current-buffer buffer
		  (and (not (erc-blacklist-p buffer))
		       (erc-server-buffer-live-p)
		       (not (erc-server-buffer-p)))))
	      (erc-buffer-list)))

(defun erc-chat-assoc (list)
  (mapcar (lambda (buffer)
	    (cons (buffer-name buffer)
		  (car (assoc-default
			buffer
			erc-modified-channels-alist))))
	  list))

(defun erc-chat-propertize (assoc)
  (mapcar (lambda (elem)
	    (let* ((name (car elem))
		   (count (cdr elem)))
	      (if count
	      	  (propertize (format status-erc-fmt-unread name count)
			      'face 'status-erc-face-unread)
		(propertize (format status-erc-fmt-normal name)
			    'face 'status-erc-face-normal))))
	  assoc))

(eval-after-load 'erc-track
  '(progn
     (defun status-erc ()
       (let* ((chat-list (erc-chat-list))
	      (chat-assoc (erc-chat-assoc chat-list))
	      (chat (erc-chat-propertize chat-assoc)))
	 (when chat
	   (mapconcat 'identity chat " - "))))))


(provide 'status-erc)
