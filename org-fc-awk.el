;;; org-fc-awk.el --- Functions for interacting with awk indexer scripts -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Leon Rische

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

(defvar org-fc-awk--find-name
  "[a-Z0-9_]*.org"
  "-name argument passed to `find' when searching for org files")

(defun org-fc-awk--find (paths)
  "Generate shell code to search PATHS for org files."
  (format
   "find %s -name \"%s\""
   (mapconcat 'identity paths " ")
   org-fc-awk--find-name))

(defun org-fc-awk--indexer-variables ()
  "Variables to pass to indexer scripts"
  `(("fc_tag" . ,org-fc-flashcard-tag)
    ("suspended_tag" . ,org-fc-suspended-tag)
    ("type_property" . ,org-fc-type-property)
    ("created_property" . ,org-fc-created-property)
    ("review_data_drawer" . ,org-fc-review-data-drawer)))

(cl-defun org-fc-awk--command (file &optional &key variables utils input)
  "Generate the shell command for calling awk on FILE with (key
. value) pairs VARIABLES.  If UTILS is set to a non-nil value,
the shared util file is included, too.  If INPUT is set to a
string, use that file (absolute path) as input."
  (concat "awk "
          ;; TODO: quote strings
          (mapconcat
           (lambda (kv) (format "-v %s=%s" (car kv) (cdr kv)))
           variables
           " ")
          " "
          (if utils
              (concat "-f "
                      (expand-file-name "awk/utils.awk" org-fc-source-path) " "))
          (concat "-f " (expand-file-name file org-fc-source-path))
          " " input))

(defun org-fc-awk--pipe (&rest commands)
  "Combine COMMANDS with shell pipes."
  (mapconcat 'identity commands " | "))

(defun org-fc-awk--xargs (command)
  "Generate the shell command for calling COMMAND with xargs."
  (concat "xargs -n 2500 -P 4 " command))

;;; Parsing Results
;;;; Key-Value

(defun org-fc-awk--key-value-parse (input)
  "Parse a string of newline separated key-value entries,
each separated by a tab, into a keyword-number plist."
  (mapcan
   (lambda (kv)
     (let ((kv (split-string kv "\t")))
       (list
        (intern (concat ":" (car kv)))
        (string-to-number (cadr kv)))))
   (split-string input "\n" t)))

;;;; TSV

(defun org-fc-tsv--parse-element (header element)
  "Parse an ELEMENT of a row given a single HEADER element."
  (if (listp header)
      (pcase (cdr header)
        ('string element)
        ('date (parse-iso8601-time-string element))
        ('number (string-to-number element))
        ('symbol (intern element))
        ('keyword (intern (concat ":" element)))
        ('bool (string= element "1")))
    element))

(defun org-fc-tsv--parse-row (headers elements)
  "Convert two lists of HEADERS and ELEMENTS into a plist,
parsing each element with its header specification."
  (if (null headers)
      '()
    (let ((header (first headers)))
      (assert (not (null elements)))
      `(,(if (listp header) (car header) header)
        ,(org-fc-tsv--parse-element header (first elements))
        .
        ,(org-fc-tsv--parse-row (rest headers) (rest elements))))))

(defun org-fc-tsv-parse (headers input)
  "Parse a tsv INPUT into a plist, give a list of HEADERS."
  (mapcar
   (lambda (row) (org-fc-tsv--parse-row headers (split-string row "\t")))
   (split-string input "\n" t)))

;;;; TSV Headers

(defvar org-fc-awk-card-headers
  '(:path :id (:type . symbol) (:suspended . bool) (:created . date))
  "Headers of the card indexer")

(defvar org-fc-awk-position-headers
  '(:path
    :id
    (:type . symbol)
    (:suspended . bool)
    :position
    (:ease . number)
    (:box . box)
    (:interval . interval)
    (:due . date))
  "Headers of the position indexer")

(defvar org-fc-awk-review-stats-headers
  '((:reviews . number) (:again . number) (:hard . number) (:good . number) (:easy . number))
  "Headers of the review stat aggregator")

;;; AWK wrapper functions

(cl-defun org-fc-awk-cards (&optional (paths org-fc-directories))
  "List all cards in PATHS."
  (org-fc-tsv-parse
   org-fc-awk-card-headers
   (shell-command-to-string
    (org-fc-awk--pipe
     (org-fc-awk--find paths)
     (org-fl-awk--xargs
      (org-fc-awk--command
       "awk/index_cards.awk"
       :utils t
       :variables (org-fc-awk--indexer-variables)))))))

(cl-defun org-fc-awk-stats-cards (&optional (paths org-fc-directories))
  "Statistics for all cards in PATHS."
  (org-fc-awk--key-value-parse
   (shell-command-to-string
    (org-fc-awk--pipe
     (org-fc-awk--find paths)
     (org-fc-awk--xargs
      (org-fc-awk--command
       "awk/index_cards.awk"
       :utils t
       :variables (org-fc-awk--indexer-variables)))
     (org-fc-awk--command "awk/stats_cards.awk" :utils t)))))

;; TODO: Optimize card order for review
(defun org-fc-awk-due-positions-for-paths (paths)
  "Generate a list of due positions cards in randomized order."
  (org-fc-tsv-parse
   org-fc-awk-position-headers
   (shell-command-to-string
    (org-fc-awk--pipe
     (org-fc-awk--find paths)
     (org-fc-awk--xargs
      (org-fc-awk--command
       "awk/index_positions.awk"
       :utils t
       :variables (org-fc-awk--indexer-variables)))
     (org-fc-awk--command "awk/filter_due.awk")
     "shuf"))))

(cl-defun org-fc-awk-stats-positions (&optional (paths org-fc-directories))
  "Statistics for all positions in PATHS."
  (org-fc-awk--key-value-parse
   (shell-command-to-string
    (org-fc-awk--pipe
     (org-fc-awk--find paths)
     (org-fc-awk--xargs
      (org-fc-awk--command
       "awk/index_positions.awk"
       :utils t
       :variables (org-fc-awk--indexer-variables)))
     (org-fc-awk--command "awk/stats_positions.awk")))))

(defun org-fc-awk-stats-reviews ()
  "Statistics for all card reviews."
  (let ((res (org-fc-tsv-parse
              org-fc-awk-review-stats-headers
              (shell-command-to-string
               (org-fc-awk--command
                "awk/stats_reviews.awk"
                :utils t
                :input org-fc-review-history-file)))))
    `(:all ,(first res) :month ,(second res) :week ,(third res) :day ,(fourth res))))

;;; Exports

(provide 'org-fc-awk)
