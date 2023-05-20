;;; org-awk.el --- AWK based flashcard indexing -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Leon Rische

;; Author: Leon Rische <emacs@leonrische.me>

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

(defcustom org-fc-awk-find-command 'find
  "Command to use for finding .org files with flashcards.
- `find' (default) slower but pre-installed on many systems
- `ripgrep' ripgrep, faster"
  :type '(choice (const find) (const ripgrep))
  :group 'org-fc)

(defcustom org-fc-awk-find-flags '()
  "Additional flags passed to the `org-fc-awk-find-command'."
  :type '(repeat string)
  :group 'org-fc)

(defcustom org-fc-awk-review-history-limit nil
  "Limit on the number of review history entries used for statistics.
When non-nil, only the last N entries of the history are used.
If nil, all entries in the history are used."
  :type '(choice (const nil) integer)
  :group 'org-fc)

;;;; Shell wrappers

(defun org-fc-awk--find (paths)
  "Generate shell code to search PATHS for org files.
Matches all .org files ignoring ones with names don't start with
a '.' to exclude temporary / backup files.
With the '-L' option, 'find' follows symlinks."
  (format
   (case org-fc-awk-find-command
     ('ripgrep
      "rg ^:FC_CREATED: -L -l --null -g \"[^.]*.org\" %s %s")
     (t
      "find -L %s -name \".*\" -prune -o -name \"[^.]*.org\" -type f -exec grep -l --null \"^:FC_CREATED\" {} \\+ %s"))
   (mapconcat #'identity org-fc-awk-find-flags " ")
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

(defun org-fc-awk-index (paths &optional filter)
  "Generate a list of all cards and positions in PATHS."
  (when paths
    (let ((output (shell-command-to-string
                   (org-fc-awk--pipe
                    (org-fc-awk--find paths)
                    (org-fc-awk--xargs
                     (org-fc-awk--command
                      "awk/index.awk"
                      :variables (org-fc-awk--indexer-variables)))))))
      (if (string-prefix-p "(" output)
          (mapcar
           (lambda (file)
             (let ((cards
                    (mapcar
                     (lambda (card)
                       (plist-put
                        card :tags
                        (org-fc-awk-combine-tags
                         (plist-get card :inherited-tags)
                         (plist-get card :local-tags))))
                     (plist-get file :cards))))
               (plist-put file :cards
                          (if filter
                              (cl-remove-if-not filter cards)
                            cards))))
           (read output))
        (error "Org-fc shell error: %s" output)))))

(defun org-fc-awk-stats-reviews ()
  "Statistics for all card reviews.
Return nil there is no history file."
  (if (file-exists-p org-fc-review-history-file)
      (let ((output
             (shell-command-to-string
              (if org-fc-awk-review-history-limit
                  (org-fc-awk--pipe
                   (format "tail -n%s %s" org-fc-awk-review-history-limit org-fc-review-history-file)
                   (org-fc-awk--command
                    "awk/stats_reviews.awk"
                    :variables `(("min_box" . ,org-fc-stats-review-min-box))))
                (org-fc-awk--command
                 "awk/stats_reviews.awk"
                 :input org-fc-review-history-file
                 :variables `(("min_box" . ,org-fc-stats-review-min-box)))))))
        (if (string-prefix-p "(" output)
            (read output)
          (error "Org-fc shell error: %s" output)))))

;;; Footer

(provide 'org-fc-awk)

;;; org-fc-awk.el ends here
