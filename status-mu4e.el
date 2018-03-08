;;; status-mu4e.el

;; Copyright (C) 2018 Julien Mason

;; Author: Julien Masson <massonju.eseo@gmail.com>
;; Keywords: emacs status mu4e

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

(defgroup status-mu4e nil
  "Status mu4e group."
  :group 'status)

(defcustom status-mu4e-separator " | "
  "MU4E status separator."
  :group 'status-mu4e)

(defcustom status-mu4e-medium-threshold 10
  "Medium threshold."
  :group 'status-mu4e)

(defcustom status-mu4e-high-threshold 30
  "Medium threshold."
  :group 'status-mu4e)

(defface status-mu4e-face-low
  '((t (:foreground "ForestGreen" :weight bold)))
  "face for current mu4e"
  :group 'status-mu4e)

(defface status-mu4e-face-medium
  '((t (:foreground "DarkOrange" :weight bold)))
  "face for current mu4e"
  :group 'status-mu4e)

(defface status-mu4e-face-high
  '((t (:foreground "DarkRed" :weight bold)))
  "face for current mu4e"
  :group 'status-mu4e)

(defface status-mu4e-face-account
  '((t (:weight bold :width ultra-expanded
		:inherit variable-pitch :foreground "orange")))
  "face for account"
  :group 'status-mu4e)

(defun status-mu4e-get-group-face (nb)
  (cond ((< nb status-mu4e-medium-threshold) 'status-mu4e-face-low)
	((< nb status-mu4e-high-threshold) 'status-mu4e-face-medium)
	('status-mu4e-face-high)))

(defun status-mu4e-build-assoc-list ()
  (let ((list-tmp '()))
    (mapc #'(lambda (it)
              (let ((path (plist-get it :path))
                    (unread (plist-get it :unread))
                    (level (plist-get it :level)))
                (when (and (= level 1)
                           (> unread 0))
                  (save-match-data
                    (when (string-match "/\\(\\w*\\)/\\(.*\\)" path)
                      (let ((account (match-string 1 path))
                            (folder (match-string 2 path)))
                        (if (assoc-default account list-tmp)
                            (setf (alist-get account list-tmp)
                                  (nconc (assoc-default account list-tmp)
                                         (list (propertize
                                                (format "%s: %s" folder unread)
                                                'face (status-mu4e-get-group-face unread)))))
                          (add-to-list 'list-tmp
                                       `(,(match-string 1 path) . (,(propertize
                                                                     (format "%s: %s" folder unread)
                                                                     'face (status-mu4e-get-group-face unread))))))))))))
          mu4e-maildirs-extension-maildirs)
    (delete-dups list-tmp)))

(defun status-mu4e ()
  (let ((mu4e-string (mapcar (lambda (account)
                               (concat (propertize (format "✉ %s  " (car account))
                                                   'face 'status-mu4e-face-account)
                                       (mapconcat 'identity (cdr account) " - ")))
                             (status-mu4e-build-assoc-list))))
    (when mu4e-string
      (mapconcat 'identity mu4e-string status-mu4e-separator))))


(provide 'status-mu4e)
