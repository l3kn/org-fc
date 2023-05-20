;;; org-fc-type-cloze.el --- Cloze deletion card type -*- lexical-binding: t; -*-

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
;;; Code:

(require 'org-fc-core)

(defcustom org-fc-type-cloze-type-property "FC_CLOZE_TYPE"
  "Property used to store the card's subtype for cloze cards."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-type-cloze-context 1
  "Number of surrounding cards to show for 'context' type cards."
  :type 'number
  :group 'org-fc)

(defface org-fc-type-cloze-hole-face
  '((t (:bold t)))
  "Face for org-fc cloze card holes."
  :group 'org-fc)

(defcustom org-fc-type-cloze-hint-prefix
  "..."
  "Prefix for cloze hints."
  :group 'org-fc)

(defvar org-fc-type-cloze-types
  '(deletion enumeration context single)
  "List of valid cloze card subtypes.")

(defvar org-fc-type-cloze--text '()
  "Text overlay.")
(defvar org-fc-type-cloze--hint '()
  "Hint overlay.")

;;; Hole Parsing / Hiding

(defvar org-fc-type-cloze-hole-re
  (rx
   (seq
    "{{"
    (group-n 1 (* (or (seq "$" (+ (not (any "$"))) "$")
                      (not (any "}"))))) "}"
    (? (seq "{" (group-n 2 (* (or (seq "$" (not (any "$")) "$")
                                  (not (any "}"))))) "}"))
    (? "@" (group-n 3 (+ digit)))
    "}"))
  "Regexp for a cloze holes.")

(defun org-fc-type-cloze-max-hole-id ()
  "Get the max-hole property of the heading at point."
  (if-let ((max-id (org-entry-get (point) org-fc-type-cloze-max-hole-property)))
      (string-to-number max-id)
    -1))

(defun org-fc-type-cloze--parse-holes (current-position end)
  "Starting at point, collect all cloze holes before END.
CURRENT-POSITION is the id of the hole being reviewed.  Returns a
pair (holes . current-index) where current-index is the index of
the hole for the current position."
  (let (holes current-index)
    (while (re-search-forward org-fc-type-cloze-hole-re end t)
      (when (match-beginning 3)
        (push (match-data) holes)
        (if (= current-position (string-to-number (match-string 3)))
            (setq current-index (1- (length holes))))))
    (cons (reverse holes) current-index)))

(defun org-fc-type-cloze--hole-visible-p (type i current-index)
  "Determine whether hole I of card TYPE should be visible based.
CURRENT-INDEX is the index of the current position in the list of all holes."
  (cl-case type
    ('enumeration (< i current-index))
    ('deletion t)
    ('single nil)
    ('context (<= (abs (- i current-index)) org-fc-type-cloze-context))
    (t (error "Org-fc: Unknown cloze card type %s" type))))

(defun org-fc-type-cloze--end ()
  "End of contents of heading at point, excluding subheadings."
  (save-excursion
    ;; If there is no next heading, we end up at `(point-max)`
    (outline-next-heading)
    (1- (point))))

(defun org-fc-type-cloze-hide-holes (position)
  "Hide holes of a card of TYPE in relation to POSITION."
  (org-fc-with-point-at-entry
   (let* ((type (intern (org-entry-get (point) org-fc-type-cloze-type-property)))
          (end (1+ (org-fc-type-cloze--end)))
          (holes-index (org-fc-type-cloze--parse-holes position end))
          (holes (car holes-index))
          (current-index (cdr holes-index)))
     (cl-loop
      for i below (length holes)
      for (hole-beg hole-end text-beg text-end hint-beg hint-end) in holes
      do
      (progn
        ;; Fake position if there is no hint
        (unless hint-beg (setq hint-beg text-end))
        (unless hint-end (setq hint-end text-end))
        (cond
         ;; If the hole is the one currently being reviewed, hide all
         ;; the hole markup, hide the answer, format the hint as
         ;; "[...hint]" and set the font for the whole hole.
         ((= i current-index)
          (org-fc-hide-region hole-beg text-beg "")
          (remove-overlays text-beg text-end)
          (setq org-fc-type-cloze--text
                (org-fc-make-overlay text-beg text-end 'invisible t))
          (org-fc-hide-region text-end hint-beg "")
          (setq org-fc-type-cloze--hint
                (org-fc-overlay-surround
                 (org-fc-make-overlay hint-beg hint-end)
                 (concat "[" org-fc-type-cloze-hint-prefix)
                 "]"
                 'org-fc-type-cloze-hole-face))
          (org-fc-hide-region hint-end hole-end "")
          (org-fc-make-overlay
           hole-beg hole-end
           'face 'org-fc-type-cloze-hole-face))
         ;; If the text of another hole should be visible,
         ;; hide the hole markup and the hint
         ((org-fc-type-cloze--hole-visible-p type i current-index)
          (org-fc-hide-region hole-beg text-beg)
          (org-fc-hide-region text-end hole-end))
         ;; If the text of another hole should not be visible,
         ;; hide the whole hole
         (t (org-fc-hide-region hole-beg hole-end "..."))))))))

;;; Setup / Flipping

(defun org-fc-type-cloze-init (type)
  "Initialize the current heading for use as a cloze card of subtype TYPE.
Processes all holes in the card text."
  (interactive (list
                (intern
                 (completing-read "Cloze Type: " org-fc-type-cloze-types))))
  (unless (member type org-fc-type-cloze-types)
    (error "Invalid cloze card type: %s" type))
  (org-fc--init-card "cloze")
  (org-fc-type-cloze-update)
  (org-set-property org-fc-type-cloze-type-property (format "%s" type)))

(defun org-fc-type-cloze-setup (position)
  "Prepare POSITION of a cloze card for review."
  (setq org-fc-type-cloze--text nil)
  (setq org-fc-type-cloze--hint nil)
  (outline-hide-subtree)
  (org-show-entry)
  (org-fc-type-cloze-hide-holes (string-to-number position))
  (goto-char (overlay-start org-fc-type-cloze--text))
  (recenter))

(defun org-fc-type-cloze-flip ()
  "Flip a cloze card."
  (org-show-children)
  (overlay-put org-fc-type-cloze--text 'invisible nil)
  (org-fc-show-latex)
  ;; Remove all overlays in the region of the hint to get rid of
  ;; latex overlays in the hint, then hide the region again.
  (let* ((hint-start (overlay-start org-fc-type-cloze--hint))
         (hint-end (overlay-end org-fc-type-cloze--hint)))
    (remove-overlays hint-start hint-end)
    (org-fc-hide-region hint-start hint-end)))

(defun org-fc-type-cloze-update ()
  "Update the review data & deletions of the current heading."
  (let* ((end (org-fc-type-cloze--end))
         (hole-id (1+ (org-fc-type-cloze-max-hole-id)))
         ids)
    (save-excursion
      (while (re-search-forward org-fc-type-cloze-hole-re end t)
        (let ((id (match-string 3))
              (hole-end (match-end 0)))
          (unless id
            (setq id hole-id)
            (cl-incf hole-id 1)
            (let ((id-str (number-to-string id)))
              (cl-incf end (+ 1 (length id-str)))
              (goto-char hole-end)
              (backward-char)
              (insert "@" id-str)))
          (push (format "%s" id) ids))))
    (org-set-property
     org-fc-type-cloze-max-hole-property
     (format "%s" (1- hole-id)))
    (org-fc-review-data-update (reverse ids))))

(org-fc-register-type
 'cloze
 'org-fc-type-cloze-setup
 'org-fc-type-cloze-flip
 'org-fc-type-cloze-update)

;;; Footer

(provide 'org-fc-type-cloze)

;;; org-fc-type-cloze.el ends here
