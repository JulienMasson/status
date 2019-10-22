;;; status-tab-bar.el

;; Copyright (C) 2019 Julien Mason

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

(defgroup status-tab-bar nil
  "Status tab bar group."
  :group 'status)

(defface status-tab-bar-normal
  '((t (:foreground "DimGray" :weight normal)))
  "Face used to display bar tab"
  :group 'status-tab-bar)

(defface status-tab-bar-current
  '((t (:foreground "DarkOrange" :weight ultra-bold)))
  "Face used to display current index of tab bar"
  :group 'status-tab-bar)

(defun status-tab-bar ()
  (let* ((tabs (funcall tab-bar-tabs-function))
	 (index (tab-bar--current-tab-index tabs))
	 (nbr (length tabs)))
    (when (> nbr 1)
      (concat (propertize "Tabs: " 'face 'status-tab-bar-normal)
	      (propertize (number-to-string (+ index 1)) 'face 'status-tab-bar-current)
	      (propertize (format " / %d" nbr) 'face 'status-tab-bar-normal)))))

(provide 'status-tab-bar)
