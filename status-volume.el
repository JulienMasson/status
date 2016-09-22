;;; status-volume.el

;; Copyright (C) 2015 Julien Masson

;; Author: Julien Masson <massonju.eseo@gmail.com>
;; Keywords: emacs status volume

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

(defgroup status-volume nil
  "Status volume group."
  :group 'status)

(defvar volume-value nil)

(defface status-volume-label-face
  '((t (:weight bold :width ultra-expanded
		:inherit variable-pitch :foreground "orange")))
  "face for volume"
  :group 'status-volume)

(defcustom status-volume-fmt "%s%%"
  "Status format to display the current used volumeory in the status area")

(defcustom status-volume-fmt-state "%s"
  "Status format to display the current used volumeory in the status area")

(defface status-volume-face
  '((t (:weight semi-light :width ultra-expanded
		:inherit variable-pitch :foreground "DimGrey")))
  "face for volume status"
  :group 'status-volume)

(defface status-volume-face-state
  '((t (:weight semi-light :width ultra-expanded
		:inherit variable-pitch :foreground "red")))
  "face for volume status"
  :group 'status-volume)

(defun get-volume()
  (setq default-directory "/")
  (setq volume-str (split-string (shell-command-to-string "amixer get Master") "\n" t))
  (setq volume-values (split-string (car (nthcdr 4 volume-str)) " " t))
  (setq volume-state (subseq (car (nthcdr 5 volume-values)) 1 3))
  (setq volume-value (subseq (car (nthcdr 3 volume-values)) 1 3)))


(defun status-volume ()
  (get-volume)
  (concat (propertize "VOL: " 'face 'status-volume-label-face)
	  (if (string= volume-state "on")
	      (propertize (format status-volume-fmt volume-value) 'face 'status-volume-face)
	    (propertize (format status-volume-fmt-state "Mute") 'face 'status-volume-face-state))))

(provide 'status-volume)
