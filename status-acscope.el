;;; status-acscope.el

;; Copyright (C) 2019 Julien Masson

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

(require 'acscope-database)

(defgroup status-acscope nil
  "Status acscope group."
  :group 'status)

(defface status-acscope-face
  '((t (:weight bold :width ultra-expanded
		:inherit variable-pitch :foreground "SeaGreen4")))
  "face for current acscope"
  :group 'status-acscope)

(defun get-basename-acscope-database (database)
  (file-name-base (directory-file-name database)))

(defun status-acscope ()
  (when-let ((database (mapcar 'get-basename-acscope-database acscope-database-list)))
    (propertize (mapconcat 'identity database " - ") 'face 'status-acscope-face)))


(provide 'status-acscope)
