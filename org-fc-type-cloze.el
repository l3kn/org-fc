;;; org-fc-type-cloze.el --- Cloze deletion card type -*- lexical-binding: t; -*-

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

(require 'thingatpt)

(require 'org-fc-core)
(require 'org-fc-review-data)

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
	(when (= current-position (string-to-number (match-string 3)))
	  (setq current-index (1- (length holes))))))
    (cons (reverse holes) current-index)))

(defun org-fc-type-cloze--hole-visible-p (type i current-index)
  "Determine whether hole I of card TYPE should be visible based.
CURRENT-INDEX is the index of the current position in the list of all holes."
  (cl-case type
    (enumeration (< i current-index))
    (deletion t)
    (single nil)
    (context (<= (abs (- i current-index)) org-fc-type-cloze-context))
    (t (error "Org-fc: Unknown cloze card type %s" type))))

(defun org-fc-type-cloze--end ()
  "End of contents of heading at point, excluding subheadings."
  (save-excursion
    ;; If there is no next heading, we end up at `(point-max)`
    (outline-next-heading)
    (1- (point))))

(defun org-fc-type-cloze--string-pad-width (string width)
  "Pad STRING to WIDTH."
  (concat string (make-string (- width (string-width string)) ? )))

(defun org-fc-type-cloze--pom-at-table-p (point-or-marker)
  "Non-nil if POINT-OR-MARKER is inside an Org table."
  (save-excursion
    (goto-char point-or-marker)
    (org-at-table-p)))

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
          (when (org-fc-type-cloze--pom-at-table-p hole-beg)
            (overlay-put
             org-fc-type-cloze--text 'org-fc-type-cloze-hole-width
             (string-width (buffer-substring hole-beg hole-end))))
          (org-fc-hide-region text-end hint-beg "")
          (setq org-fc-type-cloze--hint
                (let ((prefix (concat "[" org-fc-type-cloze-hint-prefix))
                      (suffix "]"))
                  (org-fc-overlay-surround
                   (org-fc-make-overlay hint-beg hint-end)
                   prefix
                   (if (org-fc-type-cloze--pom-at-table-p hole-beg)
                       (org-fc-type-cloze--string-pad-width
                        suffix (- (string-width (buffer-substring hole-beg hole-end))
                                  (string-width (buffer-substring hint-beg hint-end))
                                  (string-width prefix)))
                     suffix)
                   'org-fc-type-cloze-hole-face)))
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
         (t (org-fc-hide-region
             hole-beg hole-end
             (let ((ellipsis "..."))
               (if (org-fc-type-cloze--pom-at-table-p hole-beg)
                   (org-fc-type-cloze--string-pad-width
                    ellipsis (string-width (buffer-substring hole-beg hole-end)))
                 ellipsis))))))))))

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
  (org-fold-show-entry)
  (org-fc-type-cloze-hide-holes (string-to-number position)))

(defun org-fc-type-cloze-flip ()
  "Flip a cloze card."
  (org-fold-show-children)
  (let ((overlay org-fc-type-cloze--text))
    (overlay-put overlay 'invisible nil)
    (when-let ((hole-width (overlay-get overlay 'org-fc-type-cloze-hole-width)))
      (overlay-put
       overlay 'display
       (org-fc-type-cloze--string-pad-width
        (buffer-substring (overlay-start overlay) (overlay-end overlay)) hole-width))))
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

(defun org-fc-cloze-dwim (&optional hint)
  "Convert current active region or word under cursor to Cloze
syntax.

This calls `org-fc--region-to-cloze' with the active region as
the argument then, prompt user for hint. The function will
replace the region with the new string in the format of {{REGION}{HINT}@N}

where REGION is the replaced string.
HINT is what the user specifies in the prompt, will naturally be omitted
if the user specifies an empty string for the prompt.
and N will be the prefix argument the user gives in ARG."
  (interactive "sHint (optional): ")
  (cond
   ((region-active-p)
    (org-fc-type-cloze--mark-hole
     (region-beginning)
     ;; If the region includes a trailing newline, insert the cloze
     ;; hole marker before the newline.
     (save-excursion
       (goto-char (region-end))
       (skip-chars-backward "\n" 1)
       (point))
     hint))
   ((bounds-of-thing-at-point 'word)
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (org-fc-type-cloze--mark-hole (car bounds) (cdr bounds) hint)))
   (t (user-error "Can't determine region to mark as a cloze-hole.")))

  (when (and (org-fc-entry-p)
             (string= "cloze" (org-entry-get nil org-fc-type-property)))
    (org-fc-with-point-at-entry (org-fc-update))))

(defun org-fc-type-cloze--mark-hole (begin end &optional hint)
  "Mark region from BEGIN to END as a cloze hole."
  (save-excursion
    (goto-char end)
    (if (and (stringp hint) (not (string-blank-p hint)))
        (insert (format "}{%s}}" hint))
      (insert "}}"))
    (goto-char begin)
    (insert "{{")))

(org-fc-register-type
 'cloze
 'org-fc-type-cloze-setup
 'org-fc-type-cloze-flip
 'org-fc-type-cloze-update)

;;; Footer

(provide 'org-fc-type-cloze)

;;; org-fc-type-cloze.el ends here
