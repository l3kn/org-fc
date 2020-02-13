;;; org-fc-cache.el --- Cache for flashcard headings and positions -*- lexical-binding: t; -*-

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

(require 'org-el-cache)

;;; Configuration

(defcustom org-fc-cache-file
  "~/.emacs.d/.org-fc-cache.el"
  "File to store the flashcard cache in."
  :type 'string
  :group 'org-fc)

;;; Parsing Functions

(defun org-fc-cache--parse-review-data (el)
  "Extract and parse the review data drawer."
  (org-element-map el 'drawer
    (lambda (child)
      (if (string=
           (org-element-property :drawer-name child)
           org-fc-review-data-drawer)
          (org-fc-cache--review-data-table child)))
    :first-match t))

(defun org-fc-cache--review-data-table (el)
  "Extract and parse the review data table."
  (org-element-map el 'table
    (lambda (el)
      (let (rows past-header)
        (org-element-map el 'table-row
          (lambda (row)
            (pcase (org-element-property :type row)
              ((and 'standard (guard past-header))
               (push row rows))
              ('rule (setq past-header t)))))
        (mapcar #'org-fc-cache--parse-row rows)))
    :first-match t))

(defun org-fc-cache--parse-row (el)
  "Parse a single row into a plist."
  (destructuring-bind (position ease box interval due)
      (org-element-map el 'table-cell
        (lambda (child)
          (org-el-cache-interpret-data
           (car (org-element-contents child)))))
    (list
     :position position
     :ease (string-to-number ease)
     :box (string-to-number box)
     :interval (string-to-number interval)
     :due (parse-iso8601-time-string (concat due "Z")))))

;; TODO: Throw better errors if expected properties are nil
(defun org-fc-cache--parse-cards (el)
  (let ((cards nil)
        (type-prop (intern (concat ":" org-fc-type-property)))
        (created-prop (intern (concat ":" org-fc-created-property))))
    (org-element-map el 'headline
        (lambda (child)
          (let* ((tags (org-element-property :tags child))
                 (is-fc (member org-fc-flashcard-tag tags))
                 (is-suspended (member org-fc-suspended-tag tags)))
            (when is-fc
              (push
               (list
                :id (org-element-property :ID child)
                :type (intern (org-element-property type-prop child))
                :created (parse-iso8601-time-string (org-element-property created-prop child))
                :tags tags
                :suspended is-suspended
                :positions (org-fc-cache--parse-review-data child))
               cards)))))
    cards))

;;; Cache

(def-org-el-cache org-fc-cache
  org-fc-directories
  (lambda (filename el)
    (org-fc-cache--parse-cards el)))

(defun org-fc-cache-update ()
  "Update the flashcard cache."
  (interactive)
  (org-el-cache-update org-fc-cache))

(defun org-fc-cache-force-update ()
  "Force-update (recreate) the flashcard cache."
  (interactive)
  (org-el-cache-force-update org-fc-cache))

;;; Stats

(defun org-fc--plist-inc (plist key by)
  "Increment the value of KEY in PLIST by amount BY."
  (plist-put
   plist
   key
   (+ (or (plist-get plist key) 0) by)))

(defun org-fc-cache-stats ()
  (let ((now (current-time))
        (counts '())
        (total 0)
        (positions 0)
        (due 0)
        (suspended 0)
        (created-stats (make-org-fc-timed-stats org-fc-timespans '(total)))
        (box 0.0)
        (ease 0.0)
        (interval 0.0))
    (org-el-cache-each
     org-fc-cache
     (lambda (key cards)
       (dolist (card cards)
         (let* ((created (plist-get card :created))
                (delta (time-to-seconds (time-subtract now created))))
           (unless (plist-get card :suspended)
             (dolist (position (plist-get card :positions))
               (incf positions)
               (incf box (plist-get position :box))
               (incf ease (plist-get position :ease))
               (incf interval (plist-get position :interval))
               (if (time-less-p (plist-get position :due) now)
                   (incf due)))
             (org-fc-timed-stats-inc created-stats created 'total)))
         (incf total 1)
         (if (plist-get card :suspended)
             (incf suspended 1))
         (setq counts
               (org-fc--plist-inc
                counts
                (plist-get card :type)
                1)))))
    (list
     :total total
     :suspended suspended
     :created (org-fc-timed-stats-data created-stats)
     :types counts
     :due due
     :avg `(:ease ,(/ ease positions) :box ,(/ box positions) :interval ,(/ interval positions)))))

;; TODO: Support directories
(defun org-fc-cache-due-positions-for-paths (paths)
  (let ((now (current-time))
        (paths (mapcar #'expand-file-name paths))
        positions)
    (org-el-cache-each
     org-fc-cache
     (lambda (key cards)
       (if (some (lambda (path) (string-prefix-p path key)) paths)
        (dolist (card cards)
          (unless (plist-get card :suspended)
            (let ((id (plist-get card :id))
                  (type (plist-get card :type)))
              (dolist (position (plist-get card :positions))
                (if (time-less-p (plist-get position :due) now)
                    (push
                     (list
                      :path key
                      :id id
                      :type type
                      :position (plist-get position :position))
                     positions)))))))))
    positions))

(provide 'org-fc-cache)
