;;; status-cpu.el

;; Copyright (C) 2015 Julien Masson

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

(defgroup status-cpu nil
  "Status cpu group."
  :group 'status)

;; struct of /proc/stat
;;      user    nice   system  idle      iowait irq   softirq  steal  guest  guest_nice
;; cpu  74608   2520   24433   1117073   6176   4054  0        0      0      0
(cl-defstruct cpu-stat
  user nice system idle iowait irq softirq steal guest guest-nice)

(defvar cpu-total-saved nil)
(defvar cpu-active-saved nil)

(defface status-cpu-label-face
  '((t (:weight bold :width ultra-expanded
		:inherit variable-pitch :foreground "orange")))
  "face for date and time"
  :group 'status-cpu)

(defcustom status-cpu-fmt-usage "%d%% "
  "Status format to display the current used cpu in the status area")

(defcustom status-cpu-fmt-temp "%d°C"
  "Status format to display the current used cpu in the status area")

(defcustom status-cpu-threshold-usage 50
  "Medium threshold."
  :group 'status-cpu)

(defcustom status-cpu-threshold-temp 60
  "Medium threshold."
  :group 'status-cpu)

(defface status-cpu-face-low
  '((t (:weight semi-light :width ultra-expanded
		:inherit variable-pitch :foreground "DimGrey")))
  "face for cpu status"
  :group 'status-cpu)

(defface status-cpu-face-high
  '((t (:weight semi-light :width ultra-expanded
		:inherit variable-pitch :foreground "red")))
  "face for cpu status"
  :group 'status-cpu)

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun parse-proc-stat (one-line)
  (when (string-match "^cpu[0-9]" one-line)
    (let ((data (split-string one-line)))
      (make-cpu-stat :user       (string-to-number (nth 1 data))
		     :nice       (string-to-number (nth 2 data))
		     :system     (string-to-number (nth 3 data))
		     :idle       (string-to-number (nth 4 data))
		     :iowait     (string-to-number (nth 5 data))
		     :irq        (string-to-number (nth 6 data))
		     :softirq    (string-to-number (nth 7 data))
		     :steal      (string-to-number (nth 8 data))
		     :guest      (string-to-number (nth 9 data))
		     :guest-nice (string-to-number (nth 10 data))))))

(defun get-cpu-data ()
  (let ((proc-stat (read-lines "/proc/stat")))
    (delq nil (mapcar 'parse-proc-stat proc-stat))))

(defun get-cpu-usage ()
  (let* ((cpu-data (get-cpu-data))
	 (number-cpu (safe-length cpu-data))
	 (cpu-usage 0))
    (unless (and cpu-total-saved cpu-active-saved)
      (setq cpu-total-saved (make-list number-cpu 0))
      (setq cpu-active-saved (make-list number-cpu 0)))
    (loop for index from 0 to (- number-cpu 1)
	  do (let* (
		    ;; get elements
		    (elements (nth index cpu-data))
		    (user (cpu-stat-user elements))
		    (nice (cpu-stat-nice elements))
		    (system (cpu-stat-system elements))
		    (idle (cpu-stat-idle elements))
		    (iowait (cpu-stat-iowait elements))
		    (irq (cpu-stat-irq elements))
		    (softirq (cpu-stat-softirq elements))
		    (steal (cpu-stat-steal elements))
		    (guest (cpu-stat-guest elements))
		    (guest-nice (cpu-stat-guest-nice elements))
		    ;; total = user + nice + system + idle + iowait + ...
		    (total (+ user nice system idle iowait irq softirq steal guest guest-nice))
		    ;; active = total - idle - iowait
		    (active (- total idle iowait))
		    ;; calculate percentage
		    (diff-total (- total (nth index cpu-total-saved)))
		    (diff-active (- active (nth index cpu-active-saved))))
	       ;; saved values for next calculation
	       (setq cpu-usage (+ (/ (* 100 diff-active) diff-total) cpu-usage))
	       (setcar (nthcdr index cpu-total-saved) total)
	       (setcar (nthcdr index cpu-active-saved) active)))
    (/ cpu-usage number-cpu)))

;; TODO - autodectect temp file path
(defun get-cpu-temp ()
  (let (( temp-string (car (read-lines "/sys/devices/platform/coretemp.0/hwmon/hwmon1/temp2_input"))))
    (/ (string-to-number temp-string) 1000)))

(defun status-cpu-get-group-face-usage (nb)
  (cond ((< nb status-cpu-threshold-usage) 'status-cpu-face-low)
	('status-cpu-face-high)))

(defun status-cpu-get-group-face-temp (nb)
  (cond ((< nb status-cpu-threshold-temp) 'status-cpu-face-low)
	('status-cpu-face-high)))

(defun status-cpu ()
  (let ((cpu-usage (get-cpu-usage))
	(cpu-temp (get-cpu-temp)))
    (concat (propertize "CPU: " 'face 'status-cpu-label-face)
	    (propertize (format status-cpu-fmt-usage cpu-usage) 'face (status-cpu-get-group-face-usage cpu-usage))
	    (propertize (format status-cpu-fmt-temp cpu-temp) 'face (status-cpu-get-group-face-temp cpu-temp)))))

(provide 'status-cpu)
