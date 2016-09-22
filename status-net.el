;;; status-net.el

;; Copyright (C) 2015 Julien Masson

;; Author: Julien Masson <massonju.eseo@gmail.com>
;; Keywords: emacs status net

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

(require 'cl-lib)

(defgroup status-net nil
  "Status net group."
  :group 'status)

(cl-defstruct net-data
  interface recv send down up)

(defvar net-data-saved nil)

(defvar net-interfaces '("wlan0" "eth1"))

(defvar time-elapsed 0)

(defface status-net-label-face
  '((t (:weight bold :width ultra-expanded
		:inherit variable-pitch :foreground "orange")))
  "face for net"
  :group 'status-net)

(defcustom status-net-fmt-up "%s ▲ "
  "Status format to display the current used up net in the status area")

(defcustom status-net-fmt-down "▼ %s"
  "Status format to display the current used down net in the status area")

(defface status-net-face
  '((t (:weight semi-light :width ultra-expanded
		:inherit variable-pitch :foreground "DimGrey")))
  "face for net status"
  :group 'status-net)

(defface status-net-face-high
  '((t (:weight semi-light :width ultra-expanded
		:inherit variable-pitch :foreground "green")))
  "face for net status"
  :group 'status-net)

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun create-net-data (interface)
  (make-net-data :interface interface
		 :recv 0
		 :send 0
		 :down 0
		 :up 0))

(defun update-net-data (data interval)
  (let ((pattern (mapconcat 'identity net-interfaces "\\|"))
	net-data-tmp)
    ;; if match network interface name
    (when (string-match pattern data)
      ;; get net data
      (let* ((one-data (split-string data))
	     (interface (nth 0 one-data))
	     (receive (string-to-number (nth 1 one-data)))
	     (transmit (string-to-number (nth 9 one-data))))
	;; search net data in list saved
	(dolist (net-data net-data-saved)
	  (if (string-match (net-data-interface net-data) interface)
	      ;; calculate down up
	      (let ((down (/ (- receive (net-data-recv net-data)) interval))
		    (up (/ (- transmit (net-data-send net-data)) interval)))
		(setq net-data-tmp (make-net-data :interface interface
						  :recv receive
						  :send transmit
						  :down down
						  :up up))))))
      net-data-tmp)))

(defun network-stat ()
  (let* ((net-stat (read-lines "/proc/net/dev"))
	 (time-now (string-to-number (format-time-string "%s" (current-time))))
	 (interval (- time-now time-elapsed)))
    ;; create/reset net data if needed
    (unless net-data-saved
      (setq net-data-saved (mapcar 'create-net-data net-interfaces)))
    ;; avoid zero division
    (unless (zerop interval)
      (setq net-data-saved (delq nil
				 (cl-mapcar 'update-net-data net-stat
					    (make-list (safe-length net-stat) interval))))
      (setq time-elapsed time-now))))

(defun adapt-format-net (net-format data)
  ;; display in Mb
  (if (> (/ data 1000000) 0)
      (propertize
       (format net-format
	       (format "%dMb" (/ data 1000000)))
       'face
       ;; if above 10 Mb face high
       (if (> (/ data 1000000) 10) 'status-net-face-high 'status-net-face))
    ;; display in Kb
    (if (> (/ data 1000) 0)
	(propertize
	 (format net-format
		 (format "%dKb" (/ data 1000)))
	 'face
	 'status-net-face)
      ;; display in b
      (if (>= data 0)
	  (propertize
	   (format net-format
		   (format "%db" data))
	   'face
	   'status-net-face)))))

(defun status-net-format (net-data)
  (concat
   (propertize (format "%s " (upcase (net-data-interface net-data))) 'face 'status-net-label-face)
   (adapt-format-net status-net-fmt-up (net-data-up net-data))
   (adapt-format-net status-net-fmt-down (net-data-down net-data))))

(defun status-net ()
  (network-stat)
  (let ((status-net-string (delq nil (mapcar 'status-net-format net-data-saved))))
    (when status-net-string
      (mapconcat 'identity status-net-string " - "))))


(provide 'status-net)
