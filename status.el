;;; status.el --- Package to display information when unused

;; Copyright (C) 2014 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Keywords: emacs status

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

;; This module exploits the echo area, when unused, to display custom
;; information.  Have a look at the `status-format' variable to modify
;; the content of the displayed information.

;; Use `toggle-status' to enable or disable it.  In your elisp code
;; (your .emacs for instance) you should better use `turn-on-status'
;; or `turn-off-status' to respectively enable or disable the display
;; of information in the echo area.

;; Use `status-add-to-left' and `status-add-to-right' function to
;; respectively add information to the left hand side and to the right
;; hand side of the echo area.

(require 'status-activity)
(require 'status-project-manager)
(require 'status-purple)
(require 'status-gnus)
(require 'status-virtual-desktops)
(require 'status-mem)
(require 'status-cpu)
(require 'status-net)
(require 'status-volume)
(require 'status-battery)
(require 'status-date)
(require 'status-cscope)
(require 'status-ctags)
(require 'status-compilation)
(require 'status-erc)
(require 'status-mu4e)

(defgroup status nil
  "Status management group."
  :group 'convenience)

(defconst status-module-buffer (get-buffer " *Minibuf-0*")
  "Buffer in which write the status information.")

(defcustom status-format '(("") ())
  "Status format list.
Each item of a sub-list can be :
 - a string: will be displayed as-is
 - a symbol to a string: the symbol value will displayed
 - a symbol to a function: the return value will be displayed and
   has to be a string

If there is only one sub-list, all its items are aligned to the
left hand side.  If there are two sub-lists the first sub-list
items are aligned to the left hand side and the items of the last
sub-list are aligned to the right hand side.  If there are more
than two sub-lists, the previous behavior applies and the \"in
the middle\" sub-lists are equally spread in the middle in order
to keep the same space between all the sub-lists display."
  :group 'status)

(defcustom status-separator " "
  "Status separator string to use between two consecutive items."
  :group 'status)

(defcustom status-refresh-timer-delay 1
  "If different from 0, it starts a timer to automatically
refresh the status information."
  :group 'status)

(defvar status-refresh-timer nil
  "Internal storage for the status timer object.")

(defvar status-activated nil
  "t if status is in used, nil otherwise.")

(defun status-build-item (item)
  (ignore-errors
    (cond ((stringp item) item)
	  ((functionp item) (funcall item))
	  ((symbolp item) (symbol-value item))
	  ((not (stringp item)) ""))))

(defun status-build-items (items)
  (mapconcat 'identity (delq nil (mapcar 'status-build-item items))
	     status-separator))

(defun status-window-width ()
  (let ((edges (window-inside-pixel-edges (minibuffer-window))))
    (- (nth 2 edges) (nth 0 edges))))

(defun status-insert-item (item)
  (insert item)
  (point-marker))

(defun status-current-position ()
  (caaddr (posn-at-point (point) (minibuffer-window))))

(defun status-build ()
  (let* ((items (mapcar 'status-build-items status-format))
	 (width (status-window-width)))
    (with-current-buffer status-module-buffer
      (erase-buffer)
      (let* ((markers (mapcar 'status-insert-item items)))
	(when (>= (length items) 2)
	  (let ((space (/ (- (- width 1) (status-current-position))
			  (1- (length items)))))
	    (dolist (marker markers)
	      (goto-char (marker-position marker))
	      (insert (propertize " " 'display `((space :width (,space))))))))))))

(defun status-update ()
  "Update the status information.  This function is called
periodically it `status-refresh-timer-delay' is set."
  (interactive)
  (when (= (minibuffer-depth) 0)
    (with-current-buffer status-module-buffer
      (let ((saved-status (buffer-string)))
	(condition-case nil
	    (status-build)
	  (error (with-current-buffer status-module-buffer
		   (erase-buffer) (insert saved-status))))))))

(defun turn-on-status ()
  "Unconditionally turn on the status information.  To customize
the status displayed information See `status-format' custom
variable and the `status-add-to-left' and `status-add-to-right'
functions."
  (unless status-activated
    (unless (= 0 status-refresh-timer-delay)
      (setq status-refresh-timer
	    (run-at-time 1 status-refresh-timer-delay 'status-update)))
    (message (propertize "Status enabled." 'face 'success))
    (status-update)))

(defun turn-off-status ()
  "Unconditionally turn off the status information.  To customize
the status displayed information See `status-format' custom
variable and the `status-add-to-left' and `status-add-to-right'
functions."
  (when status-activated
    (when status-refresh-timer
      (cancel-timer status-refresh-timer)
      (setq status-refresh-timer nil))
    (with-current-buffer status-module-buffer
      (erase-buffer))
    (message (propertize "Status disabled." 'face 'error))))

(defun toggle-status ()
  "Toggle whether to display custom information in the echo area.
To customize the status displayed information See `status-format'
customize variable and the `status-add-to-left' and
`status-add-to-right' functions."
  (interactive)
  (if status-activated
      (turn-off-status)
    (turn-on-status))
  (setq status-activated (not status-activated)))

(defun status-add-to (l sym append)
  "Do not call this function.  Use `status-add-to-right'
or `status-add-to-left' instead."
  (unless (delq nil (mapcar (lambda (x) (eq element x)) l))
    (if (not append)
	(cons element l)
      (append l (list element)))))

(defun status-add-to-right (element &optional append)
  "Add ELEMENT to the group on the right hand side of the status buffer.
If ELEMENT is added, it is added at the beginning of the list,
unless the optional argument APPEND is non-nil, in which case
ELEMENT is added at the end."
  (setf (cdr status-format)
	(list (status-add-to (cadr status-format) element append))))

(defun status-add-to-left (element &optional append)
  "Add ELEMENT to the group on the left hand side of the status buffer.
If ELEMENT is added, it is added at the beginning of the list,
unless the optional argument APPEND is non-nil, in which case
ELEMENT is added at the end."
  (setf (car status-format)
	(status-add-to (car status-format) element append)))

(provide 'status)
;;; status.el ends here
