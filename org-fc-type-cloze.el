;;; org-fc-type-cloze.el --- Cloze-Deletion card type -*- lexical-binding: t; -*-

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

;;; Configuration

(defcustom org-fc-type-cloze-max-hole-property "FC_CLOZE_MAX"
  "Name of the headline property to use for storing the max hole
index."
  :type 'string
  :group 'org-fc)
(defcustom org-fc-type-cloze-type-property "FC_CLOZE_TYPE"
  "Name of the headline property to use for storing the cloze
subtype."
  :type 'string
  :group 'org-fc)

;; NOTE: The context type is not implemented yet
(defvar org-fc-type-cloze-types
  '(deletion enumeration context single))

(defvar org-fc-type-cloze--overlays '())

(defcustom org-fc-type-cloze-context 1
  "Number of surrounding cards to show for 'context' type cards"
  :type 'number
  :group 'org-fc)

(defface org-fc-type-cloze-hole-face
  '((t (:bold t)))
  "Face for org-fc cloze card holes."
  :group 'org-fc)

;;; Hole Regex

(defvar org-fc-type-cloze-hole-re
  (rx
   (seq
    "{{"
    (group-n 1 (* (or (seq "$" (+ (not (any "$"))) "$")
                      (not (any "}"))))) "}"
    (?  (seq "{" (group-n 2 (* (or
                                (seq "$" (not (any "$")) "$")
                                (not (any "}"))))) "}"))
    (?  "@" (group-n 3 (+ digit)))
    "}"))
  "Regexp for a cloze hole without an id.")

(defvar org-fc-type-cloze-id-hole-re
  (rx
   (seq
    "{{"
    (group-n 1 (* (or (seq "$" (+ (not (any "$"))) "$")
                      (not (any "}"))))) "}"
    (?  (seq "{" (group-n 2 (* (or
                                (seq "$" (not (any "$")) "$")
                                (not (any "}"))))) "}"))
    (seq "@" (group-n 3 (+ digit)))
    "}"))
  "Regexp for a cloze hole with an id.")

;;; Hole Parsing / Hiding

(defun org-fc-type-cloze-max-hole-id ()
  (let ((max-id (org-entry-get (point) org-fc-type-cloze-max-hole-property)))
    (if max-id
        (string-to-number max-id)
      -1)))

(defun org-fc-type-cloze-hole (deletion)
  "Generate the string used to mark the hole left by DELETION"
  (format "[%s...]" (or (plist-get deletion :hint) "")))

;; NOTE: The way parts of the hole are hidden / revealed is probably
;; unnecessarily complicated. I couldn't get latex / org text emphasis
;; to work otherwise.  If the hole has no hint, we can't use any
;; properties of match 2.
(defun org-fc-type-cloze--overlay-current ()
  "Generate a list of overlays to display the hole currently
  being reviewed."
  (if (match-beginning 2)
      (list
       :before-text
       (org-fc-hide-region hole-beg (match-beginning 1))
       :text
       (org-fc-hide-region (match-beginning 1) (match-end 1))
       :separator
       (org-fc-hide-region (match-end 1) (match-beginning 2)
                           "[..." 'org-fc-type-cloze-hole-face)
       :hint
       (org-fc-overlay-region (match-beginning 2) (match-end 2)
                              'org-fc-type-cloze-hole-face)
       :after-hint
       (org-fc-hide-region (match-end 2) hole-end
                           "]" 'org-fc-type-cloze-hole-face))
    (list
     :before-text
     (org-fc-hide-region hole-beg (match-beginning 1))
     :text
     (org-fc-hide-region (match-beginning 1) (match-end 1))
     :hint
     (org-fc-hide-region (match-end 1) hole-end
                         "[...]" 'org-fc-type-cloze-hole-face))))

(defun org-fc-type-cloze-hide-holes (hole type)
  (save-excursion
    (org-fc-goto-entry-heading)
    (let* ((el (org-element-at-point))
           (end (org-element-property :contents-end el))
           (overlays nil)
           (seen-current nil))
      (while (re-search-forward org-fc-type-cloze-id-hole-re end t)
        (let ((text (match-string 1))
              (hint (match-string 2))
              (id (string-to-number (match-string 3)))
              (hole-beg (match-beginning 0))
              (hole-end (match-end 0)))
          (if (= hole id)
              (progn (setq overlays (org-fc-type-cloze--overlay-current))
                     (setq seen-current t))
            (case type
              ('enumeration
               (if seen-current
                   (org-fc-hide-region hole-beg hole-end "...")
                 (progn
                   (org-fc-hide-region hole-beg (match-beginning 1))
                   (org-fc-hide-region (match-end 1) hole-end))))
              ('deletion
               (progn
                 (org-fc-hide-region hole-beg (match-beginning 1))
                 (org-fc-hide-region (match-end 1) hole-end)))
              ('single
               (org-fc-hide-region hole-beg hole-end "..."))
              (t (error "org-fc: Unknown cloze card type %s" type))))))
      overlays)))

;;; Setup / Flipping

(defun org-fc-type-cloze-flip ()
  (if-let ((overlays org-fc-type-cloze--overlays))
      (progn
        (if (plist-member overlays :separator)
            (org-fc-hide-overlay (plist-get overlays :separator)))
        (if (plist-member overlays :after-hint)
            (org-fc-hide-overlay (plist-get overlays :after-hint)))
        (org-fc-hide-overlay (plist-get overlays :hint))
        (org-fc-show-overlay
         (plist-get overlays :text)
         'org-fc-type-cloze-hole-face)))
  (org-fc-review-rate-hydra/body))

(defun org-fc-type-cloze-setup (position)
  (let ((hole (string-to-number position))
        (cloze-type (intern (org-entry-get (point) org-fc-type-cloze-type-property))))
    (org-show-subtree)
    (setq
     org-fc-type-cloze--overlays
     (org-fc-type-cloze-hide-holes hole cloze-type)))
    (org-fc-review-flip-hydra/body))

(defun org-fc-type-cloze-read-type ()
  (intern
   (completing-read
    "Cloze Type: "
    org-fc-type-cloze-types)))

(defun org-fc-type-cloze-init (type)
  "Initialize the current heading for use as a cloze card of subtype TYPE.
Processes all holes in the card text."
  (interactive (list (org-fc-type-cloze-read-type)))
  (unless (member type org-fc-type-cloze-types)
    (error "Invalid cloze card type: %s" type))
  (org-fc--init-card "cloze")
  (org-fc-type-cloze-update)
  (org-set-property
   org-fc-type-cloze-type-property
   (format "%s" type)))

(defun org-fc-type-cloze-update ()
  "Update the review data & deletions of the current heading."
  (let* ((el (org-element-at-point))
         (end (org-element-property :contents-end el))
         (hole-id (1+ (org-fc-type-cloze-max-hole-id)))
         ids)
    (save-excursion
      (while (re-search-forward org-fc-type-cloze-hole-re end t)
        (let ((id (match-string 3))
              (hole-end (match-end 0)))
          (unless id
            (setq id hole-id)
            (incf hole-id 1)
            (let ((id-str (number-to-string id)))
              (incf end (+ 1 (length id-str)))
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

(provide 'org-fc-type-cloze)
