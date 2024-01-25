;;; org-fc-review-data.el --- Parsing and writing of review-data drawers -*- lexical-binding: t; -*-

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
;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'org)

(require 'org-fc-algo-sm2)

(defcustom org-fc-review-data-drawer "REVIEW_DATA"
  "Name of the drawer used to store review data."
  :type 'string
  :group 'org-fc)

(defclass org-fc-review-data ()
  ((headers
    :initarg :headers
    :initform nil
    :type list)
   (rows
    :initarg :rows
    :initform nil
    :type list)))

(cl-defmethod org-fc-review-data-names ((review-data org-fc-review-data))
  "Return a list of position names set in REVIEW-DATA."
  (mapcar #'car (oref review-data rows)))

(defun org-fc-review-data-parse (default-headers)
  "Parse the review-data drawer of the card at point."
  (if-let ((position (org-fc-review-data-position)))
      (org-with-point-at (car position)
	(let* ((table (org-table-to-lisp))
               (headers
		(mapcar #'intern (car table)))
	       (plist-rows
		(mapcar
		 (lambda (row)
		   (cl-loop
		    for header in headers
		    for cell in row
		    nconc (list header cell)))
		 (cddr table))))
          (org-fc-review-data
           :headers headers
           :rows
	   (mapcar
	    (lambda (row) (cons (plist-get row 'position) row))
	    plist-rows))))
    (org-fc-review-data :headers default-headers)))

(cl-defmethod org-fc-review-data-write ((review-data org-fc-review-data))
  (save-excursion
    (let ((headers (oref review-data headers))
          (position (org-fc-review-data-position 'create)))
      (delete-region (car position) (cdr position))
      (goto-char (car position))
      ;; Write header
      (insert
       "| "
       (mapconcat (lambda (header) (format "%s" header)) headers " | ")
       " |\n")

      ;; Write separator row
      (insert
       "|"
       (mapconcat (lambda (_header) "-") headers "+")
       "|\n")

      ;; Write rows for each position
      (dolist (row-assoc (oref review-data rows))
        (let ((row
               ;; Convert back to a list in the same order as the
               ;; headers
               (mapcar
                (lambda (header)
                  (plist-get (cdr row-assoc) header))
                headers)))
          (insert
           "| "
           (mapconcat (lambda (x) (format "%s" x)) row " | ")
           " |\n")))
      (org-table-align))))

(cl-defmethod org-fc-review-data-get-row ((review-data org-fc-review-data) name)
  "Get the row corresponding to NAME from REVIEW-DATA."
  (alist-get
   name
   (oref review-data rows)
   nil nil #'string=))

(cl-defmethod org-fc-review-data-set-row ((review-data org-fc-review-data) name value)
  "Set the row corresponding to NAME from REVIEW-DATA to VALUE."
  (if-let ((cell (assoc name (oref review-data rows) #'string=)))
      (setcdr cell value)
    (error "no entry found for row name %s" name)))

(cl-defmethod org-fc-review-data-update-row ((review-data org-fc-review-data) name update-plist)
  (if-let ((cell (assoc name (oref review-data rows) #'string=)))
      (let* ((old-plist (cdr cell))
	     (new-plist
	      (cl-loop
	       for header in (oref review-data headers)
	       nconc
	       (list
		header
		(or
		 (plist-get update-plist header)
		 (plist-get old-plist header))))))
	(setcdr cell new-plist))
    (error "no entry found for row name %s" name)))

(cl-defmethod org-fc-review-data-ensure-rows ((review-data org-fc-review-data) names)
  "Ensure REVIEW-DATA has entries for all position NAMES.
Rows with a name not contained in NAMES are removed
and missing entries are set to default values."
  (let ((rows (oref review-data rows)))
    (oset
     review-data
     rows
     (mapcar
      (lambda (name)
	(cons
	 name
	 (alist-get
	  name
	  rows
	  (org-fc-review-data-default name)
	  nil
	  #'string=)))
      names))))

;; Based on `org-log-beginning'
(defun org-fc-review-data-position (&optional create)
  "Return (BEGINNING . END) points of the review data drawer.
When optional argument CREATE is non-nil, the function creates a
drawer, if necessary.  Returned position ignores narrowing.

BEGINNING is the start of the first line inside the drawer,
END is the start of the line with :END: on it."
  (org-with-wide-buffer
   (org-end-of-meta-data)
   (let ((regexp (concat "^[ \t]*:" (regexp-quote org-fc-review-data-drawer) ":[ \t]*$"))
         (end (if (org-at-heading-p) (point)
                (save-excursion (outline-next-heading) (point))))
         (case-fold-search t))
     (catch 'exit
       ;; Try to find existing drawer.
       (while (re-search-forward regexp end t)
         (let ((element (org-element-at-point)))
           (when (eq (org-element-type element) 'drawer)
             (throw 'exit
                    (cons (org-element-property :contents-begin element)
                          (org-element-property :contents-end element))))))
       ;; No drawer found.  Create one, if permitted.
       (when create
         (unless (bolp) (insert "\n"))
         (let ((beg (point)))
           (insert ":" org-fc-review-data-drawer ":\n:END:\n")
           (org-indent-region beg (point)))
         (cons
          (line-beginning-position 0)
          (line-beginning-position 0)))))))

(defun org-fc-review-data-default (position)
  "Default review data for position POSITION."
  (let ((algo (org-fc-algo-sm2)))
    (org-fc-algo-initial-review-data algo position)))

(defun org-fc-review-data-update (names)
  "Update the review data drawer so it contains rows for NAMES.
If a doesn't exist already, it is initialized with default
values. Entries in the table not contained in NAMES are
removed."
  (let* ((algo (org-fc-algo-sm2))
	 (review-data (org-fc-review-data-parse (org-fc-algo-headers algo))))
    (org-fc-review-data-ensure-rows review-data names)
    (org-fc-review-data-write review-data)))

;;; Footer

(provide 'org-fc-review-data)

;;; org-fc-review-data.el ends here
