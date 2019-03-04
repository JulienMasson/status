;;; status-notmuch.el

;; Copyright (C) 2019 Julien Mason

;; Author: Julien Masson <massonju.eseo@gmail.com>
;; Keywords: emacs status notmuch

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

(defgroup status-notmuch nil
  "Status notmuch group."
  :group 'status)

(defcustom status-notmuch-separator " | "
  "NOTMUCH status separator."
  :group 'status-notmuch)

(defcustom status-notmuch-medium-threshold 10
  "Medium threshold."
  :group 'status-notmuch)

(defcustom status-notmuch-high-threshold 30
  "Medium threshold."
  :group 'status-notmuch)

(defface status-notmuch-face-low
  '((t (:foreground "ForestGreen" :weight bold)))
  "face for current notmuch"
  :group 'status-notmuch)

(defface status-notmuch-face-medium
  '((t (:foreground "DarkOrange" :weight bold)))
  "face for current notmuch"
  :group 'status-notmuch)

(defface status-notmuch-face-high
  '((t (:foreground "DarkRed" :weight bold)))
  "face for current notmuch"
  :group 'status-notmuch)

(defface status-notmuch-face-account
  '((t (:weight bold :width ultra-expanded
		:inherit variable-pitch :foreground "orange")))
  "face for account"
  :group 'status-notmuch)

(defun status-notmuch-get-group-face (nb)
  (cond ((< nb status-notmuch-medium-threshold) 'status-notmuch-face-low)
	((< nb status-notmuch-high-threshold) 'status-notmuch-face-medium)
	('status-notmuch-face-high)))

(defun status-notmuch-folders-propertize (account data-list)
  (let ((list-tmp))
    (mapc (lambda (data)
	    (let* ((folder (replace-regexp-in-string
			    "^.*\\." ""
			    (plist-get data :folder)))
		   (unread (plist-get data :unread)))
	      (when (> unread 0)
		(add-to-list 'list-tmp
			     (propertize (format "%s: %s" folder unread)
					 'face (status-notmuch-get-group-face unread))))))
	  data-list)
    (mapconcat 'identity list-tmp " - ")))

(defun status-notmuch-account-propertize (maildir)
  (let ((account (car maildir))
	(data-list (cdr maildir))
	folders-propertize)
    (setq folders-propertize (status-notmuch-folders-propertize account data-list))
    (unless (string= "" folders-propertize)
      (concat (propertize (format "✉ %s  " account)
			  'face 'status-notmuch-face-account)
	      folders-propertize))))

(defun status-notmuch ()
  (when notmuch-maildir-data-cached
    (let ((accounts (delq nil (mapcar #'status-notmuch-account-propertize notmuch-maildir-data-cached))))
      (when accounts
	(mapconcat 'identity accounts status-notmuch-separator)))))


(provide 'status-notmuch)
