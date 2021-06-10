;;; status-org-gcal.el

;; Copyright (C) 2021 Julien Mason

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

(require 'org-gcal)

(defgroup status-org-gcal nil
  "Status org-gcal group."
  :group 'status)

(defface status-org-gcal-face
  '((t (:foreground "tomato1" :weight bold)))
  "Face used when deadline found in org-gcal"
  :group 'status-org-gcal)

(defcustom status-org-gcal-files nil
  "List of org agenda files to check"
  :group 'status-org-gcal)

(defvar status-org-gcal-events-muted nil)
(defvar status-org-gcal-events-muted-today (org-today))

(defun org-gcal-get-timestamp (begin end)
  (save-excursion
    (goto-char begin)
    (when (re-search-forward ":org-gcal:" end t)
      (forward-char)
      (cadr (org-element-timestamp-parser)))))

(defun org-gcal-extract-timestamp-start (timestamp)
  (list (plist-get timestamp :minute-start)
	(plist-get timestamp :hour-start)
	(plist-get timestamp :day-start)
	(plist-get timestamp :month-start)
	(plist-get timestamp :year-start)))

(defun org-gcal-extract-timestamp-end (timestamp)
  (list (plist-get timestamp :minute-end)
	(plist-get timestamp :hour-end)
	(plist-get timestamp :day-end)
	(plist-get timestamp :month-end)
	(plist-get timestamp :year-end)))

(defun org-gcal-all-events ()
  (let (events)
    (dolist (file status-org-gcal-files)
      (with-current-buffer (find-file-noselect file)
	(org-element-map (org-element-parse-buffer) 'headline
	  (lambda (hl)
	    (when-let* ((title (org-element-property :raw-value hl))
			(begin (org-element-property :contents-begin hl))
			(end (org-element-property :contents-end hl))
			(timestamp (org-gcal-get-timestamp begin end)))
	      (unless (cl-find-if (lambda (event)
				    (and (string= (nth 0 event) title)
					 (eq (nth 1 event) timestamp)))
				  events)
		(push (list title
			    (org-gcal-extract-timestamp-start timestamp)
			    (org-gcal-extract-timestamp-end timestamp))
		      events)))))))
    events))

(defun org-gcal-less-now (mins hours day month year)
  (pcase-let ((`(,_ ,mins-now ,hours-now ,day-now ,month-now ,year-now ,_ ,_ ,_)
	       (parse-time-string (current-time-string))))
    (and (= year year-now) (= month month-now) (= day day-now)
	 (if (and mins hours)
	     (or (< hours hours-now) (and (= hours hours-now) (< mins mins-now)))
	   t))))

(defun org-gcal-greater-now (mins hours day month year)
  (pcase-let ((`(,_ ,mins-now ,hours-now ,day-now ,month-now ,year-now ,_ ,_ ,_)
	       (parse-time-string (current-time-string))))
    (and (= year year-now) (= month month-now) (= day day-now)
	 (if (and mins hours)
	     (or (> hours hours-now) (and (= hours hours-now) (> mins mins-now)))
	   t))))

(defun org-gcal-events-now ()
  (let (events-now)
    (pcase-dolist (`(,title ,start ,end) (org-gcal-all-events))
      (when (and (apply #'org-gcal-less-now start) (apply #'org-gcal-greater-now end))
	(push (propertize title 'face 'status-org-gcal-face) events-now)))
    events-now))

(defun status-org-gcal-mute-event ()
  (interactive)
  (let* ((collection (org-gcal-events-now))
	 (event (completing-read "Toggle event: " collection)))
    (if (cl-member event status-org-gcal-events-muted :test #'string=)
	(setq status-org-gcal-events-muted (cl-delete event status-org-gcal-events-muted
						      :test #'string=))
      (push event status-org-gcal-events-muted))))

(defun status-org-gcal-check-muted ()
  (let ((today (org-today)))
    (unless (= status-org-gcal-events-muted-today today)
      (setq status-org-gcal-events-muted nil)
      (setq status-org-gcal-events-muted-today today))))

(defun status-org-gcal ()
  (status-org-gcal-check-muted)
  (when-let* ((events-now (org-gcal-events-now))
	      (events-now (cl-remove-if (lambda (e)
					  (cl-member e status-org-gcal-events-muted
						     :test #'string=))
					events-now)))
    (string-join events-now (propertize " | " 'face 'status-separator-face))))

(provide 'status-org-gcal)
