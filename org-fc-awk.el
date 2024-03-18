;;; org-awk.el --- AWK based flashcard indexing -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024  Leon Rische

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
(require 'org-fc-review-data)
(require 'org-fc-type-cloze)

;;; Custom

(defcustom org-fc-awk-mixed-line-endings nil
  "Support both LF and CRLF line endings in AWK.
This is useful when edit and review the same files under Windows
and Linux/UNIX."
  :type 'boolean
  :group 'org-fc)

;;;; Shell wrappers

(defun org-fc-awk--find (paths)
  "Generate shell code to search PATHS for org files.
Matches all .org files ignoring ones with names don't start with
a `.' to exclude temporary / backup files.
With the `-L' option, `find' follows symlinks."
  (format
   "find -L %s -type f -name \"*.org\" -not -name \".*\" -print0"
   (mapconcat
    (lambda (path) (shell-quote-argument (expand-file-name path)))
    paths " ")))

(defun org-fc-awk--indexer-variables ()
  "Variables to pass to indexer scripts."
  `(("fc_tag" . ,org-fc-flashcard-tag)
    ("suspended_tag" . ,org-fc-suspended-tag)
    ("algo_property" . ,org-fc-algo-property)
    ("type_property" . ,org-fc-type-property)
    ("cloze_type_property" . ,org-fc-type-cloze-type-property)
    ("created_property" . ,org-fc-created-property)
    ("review_data_drawer" . ,org-fc-review-data-drawer)
    ,@(when org-fc-awk-mixed-line-endings
	'(("RS" . "\"\\r?\\n\"")))))

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
  (let ((output (shell-command-to-string
                 (org-fc-awk--pipe
                  (org-fc-awk--find paths)
                  (org-fc-awk--xargs
                   (org-fc-awk--command
                    "awk/index.awk"
                    :variables (org-fc-awk--indexer-variables)))))))
    ;; Naming convention:
    ;; p(file|card) for the plist-based output of AWK
    ;; o(file|card) for eieio objects
    (if (string-prefix-p "(" output)
	(let (files)
	  (dolist (pfile (mapcan #'identity (read (concat "(" output ")"))))
	    (let* ((ofile
		    (org-fc-file
		     :path (plist-get pfile :path)
		     :title (plist-get pfile :title)))
		   (ocards
		    (mapcar
		     (lambda (pcard)
		       (org-fc-card-from-plist
			(plist-put
			 pcard :tags
			 (org-fc-awk-combine-tags
			  (plist-get pcard :inherited-tags)
			  (plist-get pcard :local-tags)))
			ofile))
		     (plist-get pfile :cards)))
		   (fcards (if filter (cl-remove-if-not filter ocards) ocards)))
	      (when fcards
		(oset ofile cards fcards)
		(push ofile files))))
	  (reverse files))
      (error "Org-fc shell error: %s" output))))

;;; Footer

(provide 'org-fc-awk)

;;; org-fc-awk.el ends here
