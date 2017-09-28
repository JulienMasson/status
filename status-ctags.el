;;; status-ctags.el

;; Copyright (C) 2017 Julien Masson

;; Author: Julien Masson <massonju.eseo@gmail.com>
;; Keywords: emacs status ctags

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

(defgroup status-ctags nil
  "Status ctags group."
  :group 'status)

(defface status-ctags-face
  '((t (:weight bold :width ultra-expanded
		:inherit variable-pitch :foreground "maroon")))
  "face for current ctags"
  :group 'status-ctags)

(defun status-ctags ()
  (let ((tags (delq nil (mapcar (lambda (tag)
                                  (replace-regexp-in-string ".*\/\\([a-z]*\\)\/TAGS$" "\\1" tag))
                                tags-table-list))))
    (when tags
      (propertize (mapconcat 'identity tags " - ") 'face 'status-ctags-face))))


(provide 'status-ctags)
