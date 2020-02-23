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

;;; Commentary:
;;
;; Card type implementing cloze deletions in different variants
;;
;;; Code:

;;;; Customization

(defcustom org-fc-type-cloze-max-hole-property "FC_CLOZE_MAX"
  "Name of the property to use for storing the max hole index."
  :type 'string
  :group 'org-fc)
(defcustom org-fc-type-cloze-type-property "FC_CLOZE_TYPE"
  "Name of the property to use for storing the cloze subtype."
  :type 'string
  :group 'org-fc)

;; NOTE: The context type is not implemented yet
(defvar org-fc-type-cloze-types
  '(deletion enumeration context single)
  "List of valid cloze card subtypes.")

(defvar org-fc-type-cloze--overlays '())

(defcustom org-fc-type-cloze-context 1
  "Number of surrounding cards to show for 'context' type cards."
  :type 'number
  :group 'org-fc)

(defface org-fc-type-cloze-hole-face
  '((t (:bold t)))
  "Face for org-fc cloze card holes."
  :group 'org-fc)

;;;; Hole Regex

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

;;;; Hole Parsing / Hiding

(defun org-fc-type-cloze-max-hole-id ()
  "Get the max-hole property of the heading at point."
  (if-let ((max-id (org-entry-get (point) org-fc-type-cloze-max-hole-property)))
      (string-to-number max-id)
    -1))

;; NOTE: The way parts of the hole are hidden / revealed is probably
;; unnecessarily complicated. I couldn't get latex / org text emphasis
;; to work otherwise.  If the hole has no hint, we can't use any
;; properties of match 2.
(defun org-fc-type-cloze--overlay-current (hole)
  "Generate a list of overlays for the current card.
HOLE is the id of the hole being reviewed."
  (let ((hole-pos (plist-get hole :hole-pos))
        (text-pos (plist-get hole :text-pos))
        (hint-pos (plist-get hole :hint-pos)))
    (if (car hint-pos)
        (list
         :before-text
         (org-fc-hide-region (car hole-pos) (car text-pos))
         :text
         (org-fc-hide-region (car text-pos) (cdr text-pos))
         :separator
         (org-fc-hide-region (cdr text-pos) (car hint-pos)
                             "[..."
                             'org-fc-type-cloze-hole-face)
         :hint
         (org-fc-overlay-region (car hint-pos) (cdr hint-pos)
                                'org-fc-type-cloze-hole-face)
         :after-hint
         (org-fc-hide-region (cdr hint-pos) (cdr hole-pos)
                             "]"
                             'org-fc-type-cloze-hole-face))
      (list
       :before-text
       (org-fc-hide-region (car hole-pos) (car text-pos))
       :text
       (org-fc-hide-region (car text-pos) (cdr text-pos))
       :hint
       (org-fc-hide-region (cdr text-pos) (cdr hole-pos)
                           "[...]"
                           'org-fc-type-cloze-hole-face)))))

(defun org-fc-type-cloze--parse-holes (current-id end)
  "Starting at point, collect all cloze holes before END.
CURRENT-ID is the id of the hole being reviewed.  Returns a
pair (holes . current-position)."
  (let ((holes nil)
        (current-position nil))
    (while (re-search-forward org-fc-type-cloze-id-hole-re end t)
      (let ((text (match-string 1))
            (hint (match-string 2))
            (id (string-to-number (match-string 3))))
        (push `(:text ,text :hint ,hint :id ,id
                      :hole-pos (,(match-beginning 0) . ,(match-end 0))
                      :text-pos (,(match-beginning 1) . ,(match-end 1))
                      :hint-pos (,(match-beginning 2) . ,(match-end 2)))
              holes)
        ;; Track the position of the current hole in the list of holes
        (if (= current-id id) (setq current-position (1- (length holes))))))
    (cons (reverse holes) current-position)))

(defun org-fc-type-cloze--tag-holes (type holes current-position)
  "Given a list of HOLES and the position of the hole currently being reviewed,
add a :show / :hide / :hint tag to the hole, depending on the
current card TYPE."
  (loop for i below (length holes)
        for hole in holes
        collect
        (if (= i current-position)
            (cons hole :hint)
          (case type
            ('enumeration
             (if (< i current-position)
                 (cons hole :show)
               (cons hole :hide)))
            ('deletion (cons hole :show))
            ('single (cons hole :hide))
            ('context
             (if (<= (abs (- i current-position)) org-fc-type-cloze-context)
                 (cons hole :show)
               (cons hole :hide)))
            (t (error "org-fc: Unknown cloze card type %s" type))))))

(defun org-fc-type-cloze-hide-holes (current-id type)
  (save-excursion
    (org-fc-goto-entry-heading)
    (let* ((el (org-element-at-point))
           (overlays nil)
           (end (org-element-property :contents-end el))
           (holes (org-fc-type-cloze--parse-holes current-id end))
           (tagged-holes (org-fc-type-cloze--tag-holes type (car holes) (cdr holes))))
      (loop for (hole . tag) in (reverse tagged-holes)
            do
            (case tag
              (:show
               (org-fc-hide-region
                (car (plist-get hole :hole-pos))
                (car (plist-get hole :text-pos)))
               (org-fc-hide-region
                (cdr (plist-get hole :text-pos))
                (cdr (plist-get hole :hole-pos))))
              (:hide
               (org-fc-hide-region
                (car (plist-get hole :hole-pos))
                (cdr (plist-get hole :hole-pos))
                "..."))
              (:hint
               (setq overlays (org-fc-type-cloze--overlay-current hole)))))
      overlays)))


;;;; Setup / Flipping

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

;;;; Footer

(provide 'org-fc-type-cloze)

;;; org-fc-type-cloze.el ends here
