;;; status-purple.el

;; Copyright (C) 2014 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Keywords: emacs status purple

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

(require 'purple-status)

(defgroup status-purple nil
  "Status purple group."
  :group 'status)

(defcustom conversation-purple-separator ", "
  "PURPLE conversation separator."
  :group 'status-purple)

(defface conversation-purple-face
  '((t (:weight bold :width ultra-expanded
		:inherit variable-pitch :foreground "lightblue")))
  "face for current purple"
  :group 'status-purple)

(defun conversation-purple-string ()
  (let ((missed (remove-if 'zerop purple-chats :key (rcurry 'slot-value 'unread))))
    (mapconcat (lambda (chat) (format "%s(%d)" (oref chat title) (oref chat unread)))
	       missed conversation-purple-separator)))

(eval-after-load "purple-chat"
  '(progn
     (defun status-purple-conversation ()
       (let ((conversation (conversation-purple-string)))
	 (when (not (string-match conversation ""))
	   (propertize conversation 'face 'conversation-purple-face))))
     (defun status-purple-user ()
       (let ((user (purple-status-find 'id (purple-call-method "PurpleSavedstatusGetCurrent"))))
	 (propertize (concat (symbol-name (oref user typ))) 'face (assoc-default (oref user typ) purple-status-order))))))

(provide 'status-purple)
