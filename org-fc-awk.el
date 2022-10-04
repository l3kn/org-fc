;;; org-awk.el --- AWK based flashcard indexing -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Leon Rische

;; Author: Leon Rische <emacs@leonrische.me>
;; Url: https://www.leonrische.me/pages/org_flashcards.html
;; Package-requires: ((emacs "26.3") (org "9.3"))
;; Version: 0.1.0

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
(require 'org-fc-type-cloze)

;;;; Shell wrappers

(defun org-fc-awk--find (paths)
  "Generate shell code to search PATHS for org files.
Matches all .org files ignoring ones with names don't start with
a '.' to exclude temporary / backup files.
With the '-L' option, 'find' follows symlinks."
  (format
   "find -L %s -name \"*.org\" -not -name \".*\" -print0"
   (mapconcat
    (lambda (path) (shell-quote-argument (expand-file-name path)))
    paths " ")))

(defun org-fc-awk--indexer-variables ()
  "Variables to pass to indexer scripts."
  `(("fc_tag" . ,org-fc-flashcard-tag)
    ("suspended_tag" . ,org-fc-suspended-tag)
    ("type_property" . ,org-fc-type-property)
    ("cloze_type_property" . ,org-fc-type-cloze-type-property)
    ("created_property" . ,org-fc-created-property)
    ("blocked_by_property" . ,org-fc-blocked-by-property)
    ("priority_property" . ,org-fc-priority-property)
    ("review_data_drawer" . ,org-fc-review-data-drawer)))

(cl-defun org-fc-awk--command (file &optional &key variables input)
  "Generate the shell command for calling awk.
The script is called on FILE with (key . value) pairs VARIABLES.
If UTILS is set to a non-nil value, the shared util file is
included, too.  If INPUT is set to a string, use that
file (absolute path) as input."
  (concat "gawk "
          (mapconcat
           (lambda (kv) (format "-v %s=%s" (car kv) (cdr kv)))
           variables
           " ")
          " -f " (expand-file-name "awk/utils.awk" org-fc-source-path)
          " -f " (expand-file-name file org-fc-source-path)
          " " input))

(defun org-fc-awk--pipe (&rest commands)
  "Combine COMMANDS with shell pipes."
  (mapconcat 'identity commands " | "))

(defun org-fc-awk--xargs (command)
  "Generate the shell command for calling COMMAND with xargs."
  (concat "xargs -0 " command))

;; Given two tag strings,
;; one inherited and one for the current card,
;; combine them respecting `org-use-tag-inheritance'
;; and `org-tags-exclude-from-inheritance'.
;; Inheritance code is based on `org-get-tags'
(defun org-fc-awk-combine-tags (itags ltags)
  "Simulate org tag inheritance on ITAGS and LTAGS.
ITAGS and LTAGS are strings `\":tag1:tag2:\"'"
  (delete-dups
   (append
    (org-remove-uninherited-tags (split-string itags ":" t))
    (split-string ltags ":" t))))

(defun org-fc-awk-flatten-index (index)
  "Remove the file-level of INDEX."
  (mapcan
   (lambda (file)
     (mapcar
      (lambda (card)
        (plist-put card :path (plist-get file :path))
        (plist-put card :filetitle (plist-get file :title)))
      (plist-get file :cards)))
   index))

(defun org-fc-awk-index (paths &optional filter)
  "Find cards in PATHS matching an optional FILTER predicate.
FILTER can be either nil or a function taking a single card as
  its input."
  (let ((index (org-fc-awk-index-paths paths)))
    (if filter
        (cl-remove-if-not filter index)
      index)))

(defun org-fc-awk-index-paths (paths)
  "Generate a list of all cards and positions in PATHS."
  (let ((output (shell-command-to-string
                 (org-fc-awk--pipe
                  (org-fc-awk--find paths)
                  (org-fc-awk--xargs
                   (org-fc-awk--command
                    "awk/index.awk"
                    :variables (org-fc-awk--indexer-variables)))))))
    (if (string-prefix-p "(" output)
        (org-fc-awk-flatten-index
         (mapcar
          (lambda (file)
            (plist-put file :cards
                       (mapcar
                        (lambda (card)
                          (plist-put card :blocked-by (split-string
                                                       (plist-get card :blocked-by)
                                                       ","))
                          (plist-put
                           card :tags
                           (org-fc-awk-combine-tags
                            (plist-get card :inherited-tags)
                            (plist-get card :local-tags))))
                        (plist-get file :cards))))
          (read output)))
      (error "Org-fc shell error: %s" output))))

(defun org-fc-awk-stats-reviews ()
  "Statistics for all card reviews.
Return nil there is no history file."
  (if (file-exists-p org-fc-review-history-file)
      (let ((output
             (shell-command-to-string
              (org-fc-awk--command
               "awk/stats_reviews.awk"
               :input org-fc-review-history-file
               :variables `(("min_box" . ,org-fc-stats-review-min-box))))))
        (if (string-prefix-p "(" output)
            (read output)
          (error "Org-fc shell error: %s" output)))))

;;; Footer

(provide 'org-fc-awk)

;;; org-fc-awk.el ends here
