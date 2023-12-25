;;; org-fc-collaborate.el --- Functions for sharing cards without user data -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Leon Rische

;; Author: Leon Rische <emacs@leonrische.me>
;; Url: https://www.leonrische.me/pages/org_flashcards.html
;; Package-requires: ((emacs "26.3") (org "9.3"))
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;
;;
;;; Code:

(require 'org-fc-core)

(defun org-fc-strip-all-review-data ()
  "Delete review data from every drill card."
  (interactive)
  (when (yes-or-no-p
         "Delete review data from all items in org-fc directories: are you sure?")
    (dolist (card (org-fc-index org-fc-context-all))
      (let* ((path (plist-get card :path))
             (id (plist-get card :id))
             (type (plist-get card :type)))
        (with-current-buffer (find-file path)
          (goto-char (point-min))
          (org-fc-id-goto id path)
          (funcall (org-fc-type-reset-fn type)))))
    (message "Done."))
  )

;;; Footer

(provide 'org-fc-collaborate)

;;; org-fc-collaborate.el ends here
