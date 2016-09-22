;;; status-mem.el

;; Copyright (C) 2015 Julien Masson

;; Author: Julien Masson <massonju.eseo@gmail.com>
;; Keywords: emacs status mem

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

(defgroup status-mem nil
  "Status mem group."
  :group 'status)

(defface status-mem-label-face
  '((t (:weight bold :width ultra-expanded
		:inherit variable-pitch :foreground "orange")))
  "face for date and time"
  :group 'status-mem)

(defcustom status-mem-fmt "%d MB"
  "Status format to display the current used memory in the status area")

(defcustom status-mem-threshold 2000
  "Medium threshold."
  :group 'status-mem)

(defface status-mem-face-low
  '((t (:weight semi-light :width ultra-expanded
		:inherit variable-pitch :foreground "DimGrey")))
  "face for mem status"
  :group 'status-mem)

(defface status-mem-face-high
  '((t (:weight semi-light :width ultra-expanded
		:inherit variable-pitch :foreground "red")))
  "face for mem status"
  :group 'status-mem)

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun get-mem-used ()
  (let ((mem-stat (read-lines "/proc/meminfo"))
        mem-total mem-free mem-buffers mem-cached)
    (dolist (data mem-stat)
      (if (string-match "^MemTotal:\\s-*\\([0-9]+\\) kB" data)
          (setq mem-total (string-to-number (match-string 1 data)))
        (if (string-match "^MemFree:\\s-*\\([0-9]+\\) kB" data)
            (setq mem-free (string-to-number (match-string 1 data)))
          (if (string-match "^Buffers:\\s-*\\([0-9]+\\) kB" data)
              (setq mem-buffers (string-to-number (match-string 1 data)))
            (if (string-match "^Cached:\\s-*\\([0-9]+\\) kB" data)
                (setq mem-cached (string-to-number (match-string 1 data))))))))
    ;; results in MB
    (/ (- mem-total (+ mem-free mem-buffers mem-cached)) 1000)))

(defun status-mem-get-group-face (nb)
  (cond ((< nb status-mem-threshold) 'status-mem-face-low)
	('status-mem-face-high)))

(defun status-mem ()
  (let ((mem-used (get-mem-used)))
    (concat (propertize "RAM: " 'face 'status-mem-label-face)
            (propertize (format status-mem-fmt mem-used) 'face (status-mem-get-group-face mem-used)))))

(provide 'status-mem)
