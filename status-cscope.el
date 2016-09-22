;;; status-cscope.el

;; Copyright (C) 2015 Julien Masson

;; Author: Julien Masson <massonju.eseo@gmail.com>
;; Keywords: emacs status cscope

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

(defgroup status-cscope nil
  "Status cscope group."
  :group 'status)

(defface status-cscope-face
  '((t (:weight bold :width ultra-expanded
		:inherit variable-pitch :foreground "DarkCyan")))
  "face for current cscope"
  :group 'status-cscope)

(defun get-basename-cscope-database (database)
  (file-name-base (directory-file-name (car database))))

(defun status-cscope ()
  (let ((database (delq nil (mapcar 'get-basename-cscope-database jm-cscope-search-list))))
    (when database
      (propertize (mapconcat 'identity database " - ") 'face 'status-cscope-face))))


(provide 'status-cscope)
