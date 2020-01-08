;;; status-jmail.el

;; Copyright (C) 2019 Julien Mason

;; Author: Julien Masson <massonju.eseo@gmail.com>
;; Keywords: emacs status jmail

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

(defgroup status-jmail nil
  "Status jmail group."
  :group 'status)

(defcustom status-jmail-separator " | "
  "JMAIL status separator."
  :group 'status-jmail)

(defcustom status-jmail-medium-threshold 10
  "Medium threshold."
  :group 'status-jmail)

(defcustom status-jmail-high-threshold 30
  "Medium threshold."
  :group 'status-jmail)

(defcustom status-jmail-blacklist nil
  "Don't display infos from blacklist."
  :group 'status-jmail)

(defface status-jmail-face-low
  '((t (:foreground "ForestGreen" :weight bold)))
  "face for current jmail"
  :group 'status-jmail)

(defface status-jmail-face-medium
  '((t (:foreground "DarkOrange" :weight bold)))
  "face for current jmail"
  :group 'status-jmail)

(defface status-jmail-face-high
  '((t (:foreground "DarkRed" :weight bold)))
  "face for current jmail"
  :group 'status-jmail)

(defface status-jmail-face-account
  '((t (:weight bold :width ultra-expanded
		:inherit variable-pitch :foreground "orange")))
  "face for account"
  :group 'status-jmail)

(defun status-jmail-get-group-face (nb)
  (cond ((< nb status-jmail-medium-threshold) 'status-jmail-face-low)
	((< nb status-jmail-high-threshold) 'status-jmail-face-medium)
	('status-jmail-face-high)))

(defun status-jmail-propertize-data (data)
  (let ((sub (car data))
	(count (cdr data)))
    (propertize (format "%s: %s" sub count)
		'face (status-jmail-get-group-face count))))

(defun status-jmail-propertize-account (account)
  (concat (propertize (format "✉ %s  " (car account))
		      'face 'status-jmail-face-account)
	  (mapconcat 'identity (mapcar #'status-jmail-propertize-data
				       (cdr account)) " - ")))

(defun status-jmail-build-accounts (data-cached)
  (let ((accounts))
    (mapc (lambda (elem)
	    (let ((query (car elem))
		  (count (cdr elem))
		  top sub)
	      (when (string-match "maildir:/\\(.*\\)/\\(.*\\)" query)
		(setq top (match-string 1 query))
		(setq sub (match-string 2 query))
		(if (assoc top accounts)
		      (let ((data (assoc-default top accounts)))
		      	(setcdr (assoc top accounts)
		      		(add-to-list 'data `(,sub . ,count) t)))
		    (add-to-list 'accounts (cons top `((,sub . ,count))) t)))))
	  data-cached)
    accounts))

(defun status-jmail ()
  (when-let* ((data-cached (seq-remove (lambda (elem)
					 (member (car elem) status-jmail-blacklist))
				       jmail-unread-data-cached))
	      (accounts (status-jmail-build-accounts data-cached))
	      (accounts-str (mapcar #'status-jmail-propertize-account accounts)))
      (mapconcat 'identity accounts-str status-jmail-separator)))

(provide 'status-jmail)
