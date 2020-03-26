;;; org-fc.el --- Spaced Repetition System for Emacs org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Leon Rische

;; Author: Leon Rische <emacs@leonrische.me>
;; Url: https://www.leonrische.me/pages/org_flashcards.html
;; Package-requires: ((emacs "26.3") (tablist "0.15.0"))
;; Version: 0.0.1

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
;; A Spaced repetition system for Emacs org-mode.
;;
;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'org-id)
(require 'org-element)
(require 'outline)
(require 'parse-time)
(require 'subr-x)
(require 'svg)

(require 'hydra)

;;; Configuration

(defgroup org-fc nil
  "Manage and review flashcards with Emacs."
  :group 'external
  :group 'text)

(defcustom org-fc-directories '("~/org")
  "Directories to search for flashcards."
  :type 'string
  :group 'org-fc)

(defvar org-fc-source-path
  (file-name-directory
   (file-truename (or load-file-name (buffer-file-name))))
  "Location of the org-fc sources.
Used to generate absolute paths to the awk scripts.")

(defcustom org-fc-review-history-file (expand-file-name "org-fc-reviews.tsv" user-emacs-directory)
  "File to store review results in."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-unsuspend-overdue-percentage 0.1
  "Time suspended cards can be overdue before resetting them."
  :type 'float
  :group 'org-fc)

;;;; Org Tags / Properties

(defcustom org-fc-type-property "FC_TYPE"
  "Property used to store the cards type."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-created-property "FC_CREATED"
  "Property used to store the cards creation time."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-type-cloze-max-hole-property "FC_CLOZE_MAX"
  "Name of the property to use for storing the max hole index."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-type-cloze-type-property "FC_CLOZE_TYPE"
  "Name of the property to use for storing the cloze subtype."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-suspended-tag "suspended"
  "Tag for marking suspended cards."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-flashcard-tag "fc"
  "Tag for marking headlines as flashcards."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-review-data-drawer "REVIEW_DATA"
  "Name of the drawer used to store review data."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-card-tags (list org-fc-suspended-tag)
  "Card tags that can be added during review."
  :type 'list
  :group 'org-fc)

(defcustom org-fc-stats-review-min-box 0
  "Minimum box for reviews to include in the review stats."
  :type 'integer
  :group 'org-fc)

;;;; Dashboard

(defcustom org-fc-dashboard-bar-chart-width 400
  "Width of the svg generated to display review statistics."
  :type 'integer
  :group 'org-fc)

(defcustom org-fc-dashboard-bar-chart-height 20
  "Height of the svg generated to display review statistics."
  :type 'integer
  :group 'org-fc)

(defcustom org-fc-dashboard-buffer-name "*org-fc Main*"
  "Name of the buffer to use for displaying the dashboard view."
  :type 'string
  :group 'org-fc)

;;;; Spacing Parameters

(defcustom org-fc-sm2-changes
  '((again . -0.3)
    (hard . -0.15)
    (good . 0.0)
    (easy . 0.15))
  "Changes to a cards ease depending on its rating."
  :type 'list
  :group 'org-fc)

(defcustom org-fc-sm2-fixed-intervals
  '(0.0 0.01 1.0 6.0)
  "Hard-coded intervals for the first few card boxes.
Values are in days."
  :type 'list
  :group 'org-fc)

(defcustom org-fc-sm2-ease-min 1.3 "Lower bound for a cards ease."
  :type 'float
  :group 'org-fc)
(defcustom org-fc-sm2-ease-initial 2.5 "Initial ease."
  :type 'float
  :group 'org-fc)
(defcustom org-fc-sm2-ease-max 5.0 "Upper bound for a cards ease."
  :type 'float
  :group 'org-fc)

(defcustom org-fc-sm2-fuzz-min 0.9
  "Lower bound for random interval fuzz factor."
  :type 'float
  :group 'org-fc)
(defcustom org-fc-sm2-fuzz-max 1.1
  "Upper bound for random interval fuzz factor."
  :type 'float
  :group 'org-fc)

;;;; Font Faces

(defface org-fc-type-cloze-hole-face
  '((t (:bold t)))
  "Face for org-fc cloze card holes."
  :group 'org-fc)

;;; Variables

;; TODO: Allow customizing this, currently that's not possible because
;; the indexers / filters expect a ISO8601 format.
(defvar org-fc-timestamp-format "%FT%TZ"
  "Format to use for storing timestamps.
Defaults to ISO8601")

;; TODO: Allow customizing this once different indexers are supported
(defvar org-fc-indexer
  'awk
  "Indexer to use for finding cards / positions.
Only 'awk is supported at the moment.")

(defvar org-fc-demo-mode nil
  "In demo mode, the review properties & history are not updated.")
(make-variable-buffer-local 'org-fc-demo-mode)

(defvar org-fc-reviewing-existing-buffer nil
  "Track if the current buffer was open before the review.")
(make-variable-buffer-local 'org-fc-reviewing-existing-buffer)

(defvar org-fc-timestamp nil
  "Time the last card was flipped.
Used to calculate the time needed for reviewing a card.")

;;; Helper Functions

(defun org-fc-noop ()
  "Noop-function.")

(defun org-fc-timestamp-now ()
  "ISO8601 timestamp of the current time in the UTC timezone."
  (format-time-string org-fc-timestamp-format nil "UTC"))

(defun org-fc-days-overdue (ts)
  "Number of days between now and the ISO8601 timestamp TS."
  (/ (- (time-to-seconds)
        (time-to-seconds (date-to-time ts)))
     (* 24 60 60)))

(defun org-fc-show-latex ()
  "Show / re-display latex fragments."
  (org-clear-latex-preview)
  (org-latex-preview 4))

;; TODO: Rewrite using skip parameter
(defun org-fc-has-back-heading-p ()
  "Check if the entry at point has a 'Back' subheading.
Used to determine if a card uses the compact style."
  (let ((found nil))
    (org-map-entries
     (lambda ()
       (when (string= (cl-fifth (org-heading-components)) "Back")
         (setq found t)))
     t 'tree)
    found))

(defun org-fc-shuffle (list)
  "Randomize the order of elements in LIST.
This mutates / destroys the input list."
  (sort list (lambda (_a _b) (< (cl-random 1.0) 0.5))))

;; File-scoped variant of `org-id-goto'
(defun org-fc-id-goto (id file)
  "Go to the heading with ID in FILE."
  (let ((position (org-id-find-id-in-file id file)))
    (if position
        (goto-char (cdr position))
      (error "ID %s not found in %s" id file))))

(defun org-fc-timestamp-in (interval)
  "Generate an `org-mode' timestamp INTERVAL days from now."
  (let ((seconds (* interval 60 60 24))
        (now (time-to-seconds)))
    (format-time-string
     org-fc-timestamp-format
     (seconds-to-time (+ now seconds))
     "UTC0")))

;;; Checking for / going to flashcard headings

(defun org-fc-entry-p ()
  "Check if the current heading is a flashcard."
  (member org-fc-flashcard-tag (org-get-tags nil 'local)))

(defun org-fc-suspended-entry-p ()
  "Check if the current heading is a suspended flashcard."
  (let ((tags (org-get-tags nil 'local)))
    (and (member org-fc-flashcard-tag tags)
         (member org-fc-suspended-tag tags))))

(defun org-fc-part-of-entry-p ()
  "Check if the current heading belongs to a flashcard."
  (member org-fc-flashcard-tag (org-get-tags nil)))

(defun org-fc-goto-entry-heading ()
  "Move up to the parent heading marked as a flashcard."
  (unless (org-fc-part-of-entry-p)
    (error "Not inside a flashcard entry"))
  (unless (org-at-heading-p)
    (org-back-to-heading))
  (while (not (org-fc-entry-p))
    (unless (org-up-heading-safe)
      (error "Cannot find a parent heading that is marked as a flashcard"))))

;;; Adding / Removing Tags

(defun org-fc--add-tag (tag)
  "Add TAG to the heading at point."
  (org-set-tags
   (cl-remove-duplicates
    (cons tag (org-get-tags nil 'local))
    :test #'string=)))

(defun org-fc--remove-tag (tag)
  "Add TAG to the heading at point."
  (org-set-tags
   (remove tag (org-get-tags nil 'local))))

;;;###autoload
(defun org-fc-tag-card (tag)
  "Add one of the predefined card TAGs to the current card."
  (interactive (list (completing-read "Tag: " org-fc-card-tags)))
  (org-fc--add-tag tag))

;;; Card Initialization

(defun org-fc--init-card (type)
  "Initialize the current card as a flashcard.
Should only be used by the init functions of card TYPEs."
  (if (org-fc-part-of-entry-p)
      (error "Headline is already a flashcard"))
  (org-back-to-heading)
  (org-set-property
   org-fc-created-property
   (org-fc-timestamp-now))
  (org-set-property org-fc-type-property type)
  (org-id-get-create)
  (org-fc--add-tag org-fc-flashcard-tag))

;;; Card Types
;;;; Type Management

(defvar org-fc-types '()
  "Alist for registering card types.
Entries should be lists (name handler-fn update-fn).
Use `org-fc-register-type' for adding card types.")

(defun org-fc-register-type (name setup-fn flip-fn update-fn)
  "Register a new card type.
Argument NAME Name of the new type.
Argument SETUP-FN Function for initializing a new card of this type.
Argument FLIP-FN Function for flipping a card during review.
Argument UPDATE-FN Function to update a card when it's contents have changed."
  (push
   (list name setup-fn flip-fn update-fn)
   org-fc-types))

(defun org-fc-type-setup-fn (type)
  "Get the review function for a card of TYPE."
  (let ((entry (alist-get type org-fc-types nil nil #'string=)))
    (if entry
        (cl-first entry)
      (error "No such flashcard type: %s" type))))

(defun org-fc-type-flip-fn (type)
  "Get the flip function for a card of TYPE."
  (let ((entry (alist-get type org-fc-types nil nil #'string=)))
    (if entry
        (cl-second entry)
      (error "No such flashcard type: %s" type))))

(defun org-fc-type-update-fn (type)
  "Get the update function for a card of TYPE."
  (let ((entry (alist-get type org-fc-types nil nil #'string=)))
    (if entry
        (cl-third entry)
      (error "No such flashcard type: %s" type))))

;;;; Normal

(defun org-fc-type-normal-init ()
  "Mark headline as card of the normal type."
  (interactive)
  (org-fc--init-card "normal")
  (org-fc-review-data-update '("front")))

(defvar org-fc-type-normal--hidden '())

(defun org-fc-type-normal-setup (_position)
  "Prepare a normal card for review."
  (interactive)
  (if (org-fc-has-back-heading-p)
      (progn
        (org-show-subtree)
        (setq org-fc-type-normal--hidden (org-fc-hide-subheading "Back")))
    (org-flag-subtree t))
  (org-fc-review-flip-hydra/body))

(defun org-fc-type-normal-flip ()
  "Flip a normal card."
  (interactive)
  (save-excursion
    (org-show-subtree)
    (dolist (pos org-fc-type-normal--hidden)
      (goto-char pos)
      (org-show-subtree)))
  (org-fc-review-rate-hydra/body))

(org-fc-register-type
 'normal
 'org-fc-type-normal-setup
 'org-fc-type-normal-flip
 'org-fc-noop)
;;;; Double

(defvar org-fc-type-double-hole-re
  (rx "{{" (group (* (not (any "}")))) "}}"))

(defvar org-fc-type-double--overlay '())

(defun org-fc-type-double-init ()
  "Mark headline as card of the double type."
  (interactive)
  (org-fc--init-card "double")
  (org-fc-review-data-update '("front" "back")))

(defun org-fc-type-double-setup (position)
  "Prepare POSITION of a double card for review."
  (pcase position
    ("front" (org-fc-type-normal-setup position))
    ("back"
     (org-show-subtree)
     (if (org-fc-has-back-heading-p)
         (setq org-fc-type-double--overlay (org-fc-hide-content "[...]\n"))
       (setq org-fc-type-double--overlay (org-fc-hide-heading "[...]")))
     (org-fc-review-flip-hydra/body))
    (_ (error "Invalid double position %s" position))))

(defun org-fc-type-double-flip ()
  "Flip a double card."
  (if org-fc-type-double--overlay
      (delete-overlay org-fc-type-double--overlay))
  (org-show-subtree)
  (org-fc-review-rate-hydra/body))

(org-fc-register-type
 'double
 'org-fc-type-double-setup
 'org-fc-type-double-flip
 'org-fc-noop)

;;;; Cloze

;; NOTE: The context type is not implemented yet
(defvar org-fc-type-cloze-types
  '(deletion enumeration context single)
  "List of valid cloze card subtypes.")

(defvar org-fc-type-cloze--overlays '())

(defcustom org-fc-type-cloze-context 1
  "Number of surrounding cards to show for 'context' type cards."
  :type 'number
  :group 'org-fc)

;;;;; Hole Parsing / Hiding

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

(defvar org-fc-type-cloze-position-hole-re
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

(defun org-fc-type-cloze--parse-holes (current-position end)
  "Starting at point, collect all cloze holes before END.
CURRENT-POSITION is the id of the hole being reviewed.  Returns a
pair (holes . current-index) where current-index is the index of
the hole for the current position."
  (let ((holes nil)
        (current-index nil))
    (while (re-search-forward org-fc-type-cloze-position-hole-re end t)
      (let ((text (match-string 1))
            (hint (match-string 2))
            (position (string-to-number (match-string 3))))
        (push (list
               :text text
               :hint hint
               :hole-pos (cons (match-beginning 0) (match-end 0))
               :text-pos (cons (match-beginning 1) (match-end 1))
               :hint-pos (cons (match-beginning 2) (match-end 2)))
              holes)
        ;; Track the position of the current hole in the list of holes
        (if (= current-position position) (setq current-index (1- (length holes))))))
    (cons (reverse holes) current-index)))

(defun org-fc-type-cloze--tag-holes (type holes current-index)
  "Tag HOLES of a card of TYPE in relation to the hole at CURRENT-INDEX."
  (cl-loop for i below (length holes)
           for hole in holes
           collect
           (if (= i current-index)
               (cons hole :hint)
             (cl-case type
               ('enumeration
                (if (< i current-index)
                    (cons hole :show)
                  (cons hole :hide)))
               ('deletion (cons hole :show))
               ('single (cons hole :hide))
               ('context
                (if (<= (abs (- i current-index)) org-fc-type-cloze-context)
                    (cons hole :show)
                  (cons hole :hide)))
               (t (error "Org-fc: Unknown cloze card type %s" type))))))

(defun org-fc-type-cloze-hide-holes (current-position type)
  "Hide holes of a card of TYPE in relation to the CURRENT-POSITION."
  (save-excursion
    (org-fc-goto-entry-heading)
    (let* ((el (org-element-at-point))
           (overlays nil)
           (end (org-element-property :contents-end el))
           (holes (org-fc-type-cloze--parse-holes current-position end))
           (tagged-holes (org-fc-type-cloze--tag-holes type (car holes) (cdr holes))))
      (cl-loop for (hole . tag) in (reverse tagged-holes) do
               (cl-case tag
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

;;;;; Setup / Flipping

(defun org-fc-type-cloze-init (type)
  "Initialize the current heading for use as a cloze card of subtype TYPE.
Processes all holes in the card text."
  (interactive (list
                (intern
                 (completing-read
                  "Cloze Type: "
                  org-fc-type-cloze-types))))
  (unless (member type org-fc-type-cloze-types)
    (error "Invalid cloze card type: %s" type))
  (org-fc--init-card "cloze")
  (org-fc-type-cloze-update)
  (org-set-property
   org-fc-type-cloze-type-property
   (format "%s" type)))

(defun org-fc-type-cloze-setup (position)
  "Prepare POSITION of a cloze card for review."
  (let ((hole (string-to-number position))
        (cloze-type (intern (org-entry-get (point) org-fc-type-cloze-type-property))))
    (org-show-subtree)
    (setq
     org-fc-type-cloze--overlays
     (org-fc-type-cloze-hide-holes hole cloze-type)))
  (org-fc-review-flip-hydra/body))

(defun org-fc-type-cloze-flip ()
  "Flip a cloze card."
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

;;; Working with Overlays / Hiding Text
;;;; Finding Locations in the Buffer

(defun org-fc-overlay--point-at-end-of-previous ()
  "Value of point at the end of the previous line.
Returns nil if there is no previous line."
  (save-excursion
    (beginning-of-line)
    (if (bobp)
        nil
      (progn (backward-char)
             (point)))))

(defun org-fc-overlay--point-after-title ()
  "Value of point at the first line after the title keyword.
Returns nil if there is no title keyword."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (rx bol "#+TITLE:") nil t)
      (point-at-eol))))

;;;; Showing / Hiding Overlays

(defun org-fc-show-all ()
  "Remove all org-fc overlays in the current buffer."
  (interactive)
  (remove-overlays (point-min) (point-max) 'category 'org-fc-hidden)
  (remove-overlays (point-min) (point-max) 'category 'org-fc-visible))

;; Based on `outline-flag-region'
(defun org-fc-hide-region (from to &optional text face)
  "Hide region FROM ... TO, optionally replacing it with TEXT.
FACE can be used to set the text face of the overlay, e.g. to
make it bold."
  ;; (remove-overlays from to 'category 'org-fc-hidden)
  (let ((o (make-overlay from to nil 'front-advance)))
    (overlay-put o 'display-original (overlay-get o 'display))
    (overlay-put o 'category 'org-fc-hidden)
    (overlay-put o 'evaporate t)
    (if (stringp text)
        (progn
          (overlay-put o 'invisible nil)
          (if face (overlay-put o 'face face))
          (overlay-put o 'display text))
      (overlay-put o 'invisible t))
    o))

(defun org-fc-overlay-region (from to &optional face)
  "Wrap region FROM ... TO in an overlay for later hiding.
FACE can be used to set the text face of the overlay."
  ;; (remove-overlays from to 'category 'org-fc-hidden)
  (let ((o (make-overlay from to)))
    (overlay-put o 'evaporate t)
    (if face (overlay-put o 'face face))
    (overlay-put o 'invisible nil)
    (overlay-put o 'category 'org-fc-visible)
    o))

(defun org-fc-hide-overlay (o)
  "Hide the overlay O."
  (overlay-put o 'category 'org-fc-hidden)
  (overlay-put o 'invisible t)
  (overlay-put o 'display ""))

(defun org-fc-show-overlay (o &optional face)
  "Show the overlay O using an optional font FACE."
  (overlay-put o 'category 'org-fc-hidden)
  (overlay-put o 'invisible nil)
  (if face
      (overlay-put o 'face face)))

;;;; Hiding Drawers

(defun org-fc-hide-drawers ()
  "Hide all drawers after point."
  (save-excursion
    (while (re-search-forward org-drawer-regexp nil t)
      (let ((start (1- (match-beginning 0)))
            (end))
        (if (re-search-forward ":END:" nil t)
            (setq end (point))
          (error "No :END: found for drawer"))
        (org-fc-hide-region start end)))))

;;;; Hiding Headings / Section Contents

(defun org-fc-hide-heading (&optional text)
  "Hide the title of the headline at point.
If TEXT is non-nil, the heading is replaced with TEXT."
  ;; Case sensitive search
  (let ((case-fold-search nil))
    (save-excursion
      (beginning-of-line)
      (if (looking-at org-complex-heading-regexp)
          (org-fc-hide-region (match-beginning 4) (match-end 4) (or text "..."))
        (error "Point is not on a heading")))))

(defun org-fc-hide-subheadings-if (test)
  "Hide subheadings matching the predicate TEST.
TEST is a function taking no arguments and will be called for
each of the immediate subheadings of the current headline, with
the point on the relevant subheading.  TEST should return nil if
the subheading is to be revealed, non-nil if it is to be hidden.
Returns a list containing the position of each immediate
subheading of the current topic."
  (let ((entry-level (org-current-level))
        (sections nil))
    (org-show-subtree)
    (save-excursion
      (org-map-entries
       (lambda ()
         (when (and (not (outline-invisible-p))
                    (> (org-current-level) entry-level))
           (when (or (/= (org-current-level) (1+ entry-level))
                     (funcall test))
             (outline-hide-subtree))
           (push (point) sections)))
       t 'tree))
    (reverse sections)))

(defun org-fc-hide-subheading (name)
  "Hide all subheadings matching NAME."
  (org-fc-hide-subheadings-if
   (lambda () (string= (org-get-heading t) name))))

(defun org-fc-hide-all-subheadings-except (heading-list)
  "Hide all subheadings except HEADING-LIST."
  (org-fc-hide-subheadings-if
   (lambda () (not (member (org-get-heading t) heading-list)))))

(defun org-fc-hide-content (&optional text)
  "Hide the main text of a heading *before* the first subheading.
If TEXT is non-nil, the content is replaced with TEXT."
  (let (start end)
    (save-excursion
      (org-back-to-heading)
      (forward-line)
      (setq start (point)))
    (save-excursion
      (outline-next-heading)
      (setq end (point)))
    (org-fc-hide-region start end text)))

;;;; Outline Trees

(defun org-fc-narrow-tree ()
  "Narrow the outline tree.
Only parent headings of the current heading remain visible."
  (interactive)
  (save-excursion
    (org-fc-goto-entry-heading)
    (let* ((end (org-fc-overlay--point-at-end-of-previous))
           (tags (org-get-tags nil 'local))
           (notitle (member "notitle" tags))
           (noheading (member "noheading" tags))
           (el (org-element-at-point))
           (current-end (org-element-property :contents-end el)))
      (if noheading
          (org-fc-hide-heading))
      (while (org-up-heading-safe)
        (let ((start (point-at-eol))
              (end_ (org-fc-overlay--point-at-end-of-previous)))
          (if (< start end)
              (org-fc-hide-region end start))
          (setq end end_)))
      (let ((at (org-fc-overlay--point-after-title))
            (eop (org-fc-overlay--point-at-end-of-previous)))
        ;; Don't hide anything if the heading is at the beginning of the buffer
        (if eop
            (if (and at (not notitle))
                (org-fc-hide-region at eop)
              (org-fc-hide-region (point-min) eop))))
      (org-fc-hide-region current-end (point-max)))))

;;; Updating Cards

(defun org-fc-map-cards (fn)
  "Call FN for each flashcard headline in the current buffer.
FN is called with point at the headline and no arguments."
  (org-map-entries
   (lambda () (if (org-fc-entry-p) (funcall fn)))))

;;;###autoload
(defun org-fc-update ()
  "Re-process the current flashcard."
  (interactive)
  (unless (org-fc-part-of-entry-p)
    (error "Not part of a flashcard entry"))
  (save-excursion
    (org-fc-goto-entry-heading)
    (let ((type (org-entry-get (point) "FC_TYPE")))
      (funcall (org-fc-type-update-fn type)))))

;;;###autoload
(defun org-fc-update-all ()
  "Re-process all flashcards in the current buffer."
  (interactive)
  (org-fc-map-cards 'org-fc-update))

;;; Suspending / Unsuspending Cards

;;;###autoload
(defun org-fc-suspend-card ()
  "Suspend the headline at point if it is a flashcard."
  (interactive)
  (if (org-fc-entry-p)
      (progn
        (org-fc-goto-entry-heading)
        (org-fc--add-tag org-fc-suspended-tag))
    (message "Entry at point is not a flashcard")))

;;;###autoload
(defun org-fc-suspend-buffer ()
  "Suspend all cards in the current buffer."
  (interactive)
  (org-fc-map-cards 'org-fc-suspend-card))

(defun org-fc--unsuspend-card ()
  "Unsuspend the card at point, updating its review data.
If a position is overdue by more than
`org-fc-unsuspend-overdue-percentage' of its interval, reset it
to box 0, if not, keep the current parameters."
  (when (org-fc-suspended-entry-p)
    (org-fc--remove-tag org-fc-suspended-tag)
    ;; Reset all positions overdue more than `org-fc-unsuspend-overdue-percentage'.
    (org-fc-set-review-data
     (mapcar
      (lambda (row)
        (let* ((pos (cl-first row))
               (interval (string-to-number (cl-fourth row)))
               (due (cl-fifth row))
               (days-overdue (org-fc-days-overdue due)))
          (if (< days-overdue (* org-fc-unsuspend-overdue-percentage interval))
              row
            (org-fc-review-data-default pos))))
      (org-fc-get-review-data)))))

;;;###autoload
(defun org-fc-unsuspend-card ()
  "Unsuspend the headline at point.
Checks if the headline is a suspended card first."
  (interactive)
  (if (org-fc-suspended-entry-p)
      (progn (org-fc-goto-entry-heading)
             (org-fc--unsuspend-card))
    (message "Entry at point is not a suspended flashcard")))

;;;###autoload
(defun org-fc-unsuspend-buffer ()
  "Un-suspend all cards in the current buffer."
  (interactive)
  (org-fc-map-cards 'org-fc--unsuspend-card))

;;; AWK Interface

(defvar org-fc-awk--find-name
  "[a-Z0-9_]*.org"
  "-name argument passed to `find' when searching for org files.")

(defun org-fc-awk--find (paths)
  "Generate shell code to search PATHS for org files."
  (format
   "find %s -name \"%s\""
   (mapconcat 'identity paths " ")
   org-fc-awk--find-name))

(defun org-fc-awk--indexer-variables ()
  "Variables to pass to indexer scripts."
  `(("fc_tag" . ,org-fc-flashcard-tag)
    ("suspended_tag" . ,org-fc-suspended-tag)
    ("type_property" . ,org-fc-type-property)
    ("created_property" . ,org-fc-created-property)
    ("review_data_drawer" . ,org-fc-review-data-drawer)))

(cl-defun org-fc-awk--command (file &optional &key variables utils input)
  "Generate the shell command for calling awk.
The script is called on FILE with (key . value) pairs VARIABLES.
If UTILS is set to a non-nil value, the shared util file is
included, too.  If INPUT is set to a string, use that
file (absolute path) as input."
  (concat "gawk "
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

;;;; TSV / Key-Value Parsing

(defun org-fc-awk--key-value-parse (input)
  "Parse a string INPUT of newline separated key-value entries.
Each key-value entry is separated by a tab.  Results are collected
into a keyword-number plist."
  (mapcan
   (lambda (kv)
     (let ((kv (split-string kv "\t")))
       (list
        (intern (concat ":" (car kv)))
        (string-to-number (cadr kv)))))
   (split-string input "\n" t)))

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
  "Convert two lists of HEADERS and ELEMENTS into a plist.
Each element is parsed using its header specification."
  (if (null headers)
      '()
    (let ((header (car headers)))
      (cl-assert (not (null elements)))
      `(,(if (listp header) (car header) header)
        ,(org-fc-tsv--parse-element header (car elements))
        .
        ,(org-fc-tsv--parse-row (cdr headers) (cdr elements))))))

(defun org-fc-tsv-parse (headers input)
  "Parse a tsv INPUT into a plist, give a list of HEADERS."
  (mapcar
   (lambda (row) (org-fc-tsv--parse-row headers (split-string row "\t")))
   (split-string input "\n" t)))

(defvar org-fc-awk-card-headers
  '(:path :id (:type . symbol) (:suspended . bool) (:created . date))
  "Headers of the card indexer.")

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
  "Headers of the position indexer.")

(defvar org-fc-awk-review-stats-headers
  '((:total . number) (:again . number) (:hard . number) (:good . number) (:easy . number))
  "Headers of the review stat aggregator.")

;;;; AWK Wrapper Functions

(cl-defun org-fc-awk-cards (&optional (paths org-fc-directories))
  "List all cards in PATHS."
  (org-fc-tsv-parse
   org-fc-awk-card-headers
   (shell-command-to-string
    (org-fc-awk--pipe
     (org-fc-awk--find paths)
     (org-fc-awk--xargs
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
  "Generate a list of due positions in PATHS."
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
     (org-fc-awk--command "awk/filter_due.awk")))))

(defun org-fc-awk-positions-for-paths (paths)
  "Generate a list of all positions in PATHS."
  (org-fc-tsv-parse
   org-fc-awk-position-headers
   (shell-command-to-string
    (org-fc-awk--pipe
     (org-fc-awk--find paths)
     (org-fc-awk--xargs
      (org-fc-awk--command
       "awk/index_positions.awk"
       :utils t
       :variables (org-fc-awk--indexer-variables)))))))

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
  "Statistics for all card reviews.
Return nil there is no history file."
  (if (file-exists-p org-fc-review-history-file)
      (let ((res (org-fc-tsv-parse
                  org-fc-awk-review-stats-headers
                  (shell-command-to-string
                   (org-fc-awk--command
                    "awk/stats_reviews.awk"
                    :utils t
                    :input org-fc-review-history-file
                    :variables `(("min_box" . ,org-fc-stats-review-min-box)))))))
        `(:all ,(cl-first res) :month ,(cl-second res) :week ,(cl-third res) :day ,(cl-fourth res)))))

;;; Indexing Cards

(defun org-fc-due-positions-for-paths (paths)
  "Find due positions for all cards in files in PATHS."
  (if (eq org-fc-indexer 'awk)
      (org-fc-shuffle (org-fc-awk-due-positions-for-paths paths))
    (error
     'org-fc-indexer-error
     (format "Indexer %s not implemented yet" org-fc-indexer))))

(defun org-fc-due-positions (context)
  "Return a shuffled list [(file id position)] of due cards for CONTEXT.
Valid contexts:
- 'all, all cards in `org-fc-directories'
- 'buffer, all cards in the current buffer
- a list of paths"
  (if (listp context)
      (org-fc-due-positions-for-paths context)
    (cl-case context
      ('all (org-fc-due-positions-for-paths org-fc-directories))
      ('buffer (org-fc-due-positions-for-paths (list (buffer-file-name))))
      (t (error "Unknown review context %s" context)))))

;;; Review & Spacing
;;;; Spacing Algorithm (SM2)

(defun org-fc-sm2-fuzz (interval)
  "Apply fuzz to INTERVAL.
INTERVAL is by a random factor between `org-fc-sm2-fuzz-min' and
`org-fc-sm2-fuzz-max'"
  (*
   interval
   (+ org-fc-sm2-fuzz-min
      (cl-random (- org-fc-sm2-fuzz-max org-fc-sm2-fuzz-min)))))

(defun org-fc-sm2-next-box (box rating)
  "Calculate the next box of a card in BOX, rated as RATING."
  (cond
   ;; If a card is rated easy, skip the learning phase
   ((and (eq box 0) (eq rating 'easy)) 2)
   ;; If the review failed, go back to box 0
   ((eq rating 'again) 0)
   ;; Otherwise, move forward one box
   (t (1+ box))))

(defun org-fc-sm2-next-ease (ease box rating)
  "Calculate the next ease of a card, based on the review RATING.
EASE and BOX are the current parameters of the card."
  (if (< box 2)
      ease
    (min
     (max
      (+ ease (alist-get rating org-fc-sm2-changes))
      org-fc-sm2-ease-min)
     org-fc-sm2-ease-max)))

(defun org-fc-sm2--next-interval (interval next-box next-ease)
  "Calculate the next interval of a card.
INTERVAL is the current interval of the card, NEXT-BOX and
NEXT-EASE are the new parameters of the card."
  (if (< next-box (length org-fc-sm2-fixed-intervals))
      (nth next-box org-fc-sm2-fixed-intervals)
    (org-fc-sm2-fuzz (* next-ease interval))))

(defun org-fc-sm2-next-parameters (ease box interval rating)
  "Calculate the next parameters of a card, based on the review RATING.
EASE, BOX and INTERVAL are the current parameters of the card."
  (let* ((next-ease (org-fc-sm2-next-ease ease box rating))
         (next-box (org-fc-sm2-next-box box rating))
         (next-interval (org-fc-sm2--next-interval interval next-box next-ease)))
    (list next-ease next-box next-interval)))

;;;; Demo Mode

;;;###autoload
(defun org-fc-demo ()
  "Start a review of the demo file."
  (interactive)
  (let ((path (expand-file-name "demo.org" org-fc-source-path)))
    (with-current-buffer (find-file path)
      (setq-local org-fc-demo-mode t)
      (org-fc-review-buffer))))

;;;; Session Management

(defclass org-fc-review-session ()
  ((current-item :initform nil)
   (ratings :initform nil :initarg :ratings)
   (cards :initform nil :initarg :cards)))

(defun org-fc-make-review-session (cards)
  "Create a new review session with CARDS."
  (make-instance
   'org-fc-review-session
   :ratings
   (if-let ((stats (org-fc-awk-stats-reviews)))
       (plist-get stats :day)
     '(:total 0 :again 0 :hard 0 :good 0 :easy 0))
   :cards cards))

(defun org-fc-session-cards-pending-p (session)
  "Check if there are any cards in SESSION."
  (not (null (oref session cards))))

(defun org-fc-session-pop-next-card (session)
  "Remove and return one card from SESSION."
  (let ((card (pop (oref session cards))))
    (setf (oref session current-item) card)
    card))

(defun org-fc-session-add-rating (session rating)
  "Store RATING in the review history of SESSION."
  (with-slots (ratings) session
    (cl-case rating
      ('again (cl-incf (cl-getf ratings :again) 1))
      ('hard (cl-incf (cl-getf ratings :hard) 1))
      ('good (cl-incf (cl-getf ratings :good) 1))
      ('easy (cl-incf (cl-getf ratings :easy) 1)))
    (cl-incf (cl-getf ratings :total 1))))

(defun org-fc-session-stats-string (session)
  "Generate a string with review stats for SESSION."
  (with-slots (ratings) session
    (let ((total (plist-get ratings :total)))
      (if (cl-plusp total)
          (format "%.2f again, %.2f hard, %.2f good, %.2f easy"
                  (/ (* 100.0 (plist-get ratings :again)) total)
                  (/ (* 100.0 (plist-get ratings :hard)) total)
                  (/ (* 100.0 (plist-get ratings :good)) total)
                  (/ (* 100.0 (plist-get ratings :easy)) total))
        "No ratings yet"))))

(defvar org-fc-review--current-session nil
  "Current review session.")
;;;; Writing Review History

(defun org-fc-review-history-add (elements)
  "Add ELEMENTS to the history csv file."
  (append-to-file
   (concat
    (mapconcat #'identity elements "\t")
    "\n")
   nil
   org-fc-review-history-file))

;;;; Reading / Writing Review Data

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

(defun org-fc-get-review-data ()
  "Get a cards review data as a Lisp object."
  (let ((position (org-fc-review-data-position nil)))
    (if position
        (save-excursion
          (goto-char (car position))
          (cddr (org-table-to-lisp))))))

(defun org-fc-set-review-data (data)
  "Set the cards review data to DATA."
  (save-excursion
    (let ((position (org-fc-review-data-position t)))
      (kill-region (car position) (cdr position))
      (goto-char (car position))
      (insert "| position | ease | box | interval | due |\n")
      (insert "|-|-|-|-|-|\n")
      (cl-loop for datum in data do
               (insert
                "| "
                (mapconcat (lambda (x) (format "%s" x)) datum " | ")
                " |\n"))
      (org-table-align))))

(defun org-fc-review-data-default (position)
  "Default review data for position POSITION."
  (list position org-fc-sm2-ease-initial 0 0
        (org-fc-timestamp-now)))

(defun org-fc-review-data-update (positions)
  "Update review data to POSITIONS.
If a doesn't exist already, it is initialized with default
values.  Entries in the table not contained in POSITIONS are
removed."
  (let ((old-data (org-fc-get-review-data)))
    (org-fc-set-review-data
     (mapcar
      (lambda (pos)
        (or
         (assoc pos old-data #'string=)
         (org-fc-review-data-default pos)))
      positions))))

;;;; Main Loop
;;
;; Cards are reviewed by
;; 1. opening the file they are in
;; 2. calling the setup function for the card type
;; 3. opening a hydra for flipping the card
;; 4. calling the flip function for the card type
;; 5. opening a hydra for rating the card
;; 6. updating the review data based on the rating
;;

;;;###autoload
(defun org-fc-review (context)
  "Start a review session for all cards in CONTEXT.
Called interactively, prompt for the context.
Valid contexts:
- 'all, all cards in `org-fc-directories'
- 'buffer, all cards in the current buffer
- a list of paths"
  (interactive (list (intern (completing-read "Context: " '("all" "buffer")))))
  (if org-fc-review--current-session
      (message "Flashcards are already being reviewed")
    (let ((cards (org-fc-due-positions context)))
      (if (null cards)
          (message "No cards due right now")
        (progn
          (setq org-fc-review--current-session
                (org-fc-make-review-session cards))
          (org-fc-review-next-card))))))

;;;###autoload
(defun org-fc-review-buffer ()
  "Review due cards in the current buffer."
  (interactive)
  (org-fc-review 'buffer))

;;;###autoload
(defun org-fc-review-all ()
  "Review all due cards."
  (interactive)
  (org-fc-review 'all))

(defun org-fc-review-next-card ()
  "Review the next card of the current session."
  (if (org-fc-session-cards-pending-p org-fc-review--current-session)
      (condition-case err
          (let* ((card (org-fc-session-pop-next-card org-fc-review--current-session))
                 (path (plist-get card :path))
                 (id (plist-get card :id))
                 (type (plist-get card :type))
                 (position (plist-get card :position))
                 ;; Prevent messages from hiding the multiple-choice card dialog
                 (inhibit-message t))
            (let ((buffer (find-buffer-visiting path)))
              (with-current-buffer (find-file path)
                ;; If buffer was already open, don't kill it after rating the card
                (if buffer
                    (setq-local org-fc-reviewing-existing-buffer t)
                  (setq-local org-fc-reviewing-existing-buffer nil))
                (goto-char (point-min))
                (org-fc-show-all)
                (org-fc-id-goto id path)
                ;; Make sure the headline the card is in is expanded
                (org-reveal)
                (org-fc-narrow-tree)
                (org-fc-hide-drawers)
                (org-fc-show-latex)
                (org-display-inline-images)
                (setq org-fc-timestamp (time-to-seconds (current-time)))
                (funcall (org-fc-type-setup-fn type) position))))
        (error
         (message "Error during review: %s" (error-message-string err))
         (org-fc-review-quit)))
    (message "Review Done")
    (org-fc-review-quit)))

(defhydra org-fc-review-rate-hydra (:foreign-keys warn)
  "
%(length (oref org-fc-review--current-session cards)) cards remaining
%s(org-fc-session-stats-string org-fc-review--current-session)

"
  ("a" (org-fc-review-rate-card 'again) "Rate as again" :exit t)
  ("h" (org-fc-review-rate-card 'hard) "Rate as hard" :exit t)
  ("g" (org-fc-review-rate-card 'good) "Rate as good" :exit t)
  ("e" (org-fc-review-rate-card 'easy) "Rate as easy" :exit t)
  ("q" org-fc-review-quit "Quit" :exit t))

(defhydra org-fc-review-flip-hydra (:foreign-keys warn)
  "
%(length (oref org-fc-review--current-session cards)) cards remaining
%s(org-fc-session-stats-string org-fc-review--current-session)

"
  ("RET" org-fc-review-flip "Flip" :exit t)
  ("t" org-fc-tag-card "Add Tag")
  ;; Neo-Layout ergonomics
  ("n" org-fc-review-flip "Flip" :exit t)
  ("q" org-fc-review-quit "Quit" :exit t))

(defmacro org-fc-review-with-current-item (var &rest body)
  "Evaluate BODY with the current card bound to VAR.
Before evaluating BODY, check if the heading at point has the
same ID as the current card in the session."
  (declare (indent defun))
  `(if org-fc-review--current-session
       (if-let ((,var (oref org-fc-review--current-session current-item)))
           (if (string= (plist-get ,var :id) (org-id-get))
               (progn ,@body)
             (message "Flashcard ID mismatch"))
         (message "No flashcard review is in progress"))))

(defun org-fc-review-flip ()
  "Flip the current flashcard."
  (interactive)
  (condition-case err
      (org-fc-review-with-current-item card
        (let ((type (plist-get card :type)))
          (funcall (org-fc-type-flip-fn type))))
    (error
     (message "Error flipping card: %s" (error-message-string err))
     (org-fc-review-quit))))

;; TODO: Remove -card suffix
(defun org-fc-review-rate-card (rating)
  "Rate the card at point with RATING."
  (interactive)
  (condition-case err
      (org-fc-review-with-current-item card
        (let* ((path (plist-get card :path))
               (id (plist-get card :id))
               (position (plist-get card :position))
               (now (time-to-seconds (current-time)))
               (delta (- now org-fc-timestamp)))
          (org-fc-session-add-rating org-fc-review--current-session rating)
          (org-fc-review-update-data path id position rating delta)
          (org-fc-show-all)
          (save-buffer)
          (unless org-fc-reviewing-existing-buffer
            (kill-buffer))
          (org-fc-review-next-card)))
    (error
     (message "Error rating card: %s" (error-message-string err))
     (org-fc-review-quit))))

(defun org-fc-review-update-data (path id position rating delta)
  "Update the review data of the card.
Also add a new entry in the review history file.  PATH, ID,
POSITION identify the position that was reviewed, RATING is a
review rating and DELTA the time in seconds between showing and
rating the card."
  (save-excursion
    (org-fc-goto-entry-heading)
    (let* ((data (org-fc-get-review-data))
           (current (assoc position data #'string=)))
      (unless current
        (error "No review data found for this position"))
      (unless (and (boundp 'org-fc-demo-mode) org-fc-demo-mode)
        (let ((ease (string-to-number (cl-second current)))
              (box (string-to-number (cl-third current)))
              (interval (string-to-number (cl-fourth current))))
          (org-fc-review-history-add
           (list
            (org-fc-timestamp-now)
            path
            id
            position
            (format "%.2f" ease)
            (format "%d" box)
            (format "%.2f" interval)
            (symbol-name rating)
            (format "%.2f" delta)))
          (cl-destructuring-bind (next-ease next-box next-interval)
              (org-fc-sm2-next-parameters ease box interval rating)
            (setcdr
             current
             (list (format "%.2f" next-ease)
                   (number-to-string next-box)
                   (format "%.2f" next-interval)
                   (org-fc-timestamp-in next-interval)))
            (org-fc-set-review-data data)))))))

;;;###autoload
(defun org-fc-review-quit ()
  "Quit the review, remove all overlays from the buffer."
  (interactive)
  (setq org-fc-review--current-session nil)
  (org-fc-show-all))

;;; Dashboard

(defun org-fc-review-estimate (paths n)
  "Positions due in PATHS in the next N days."
  (let ((now (+ (time-to-seconds (current-time))
                (* 60 60 24 n))))
    (seq-count
     (lambda (pos) (< (time-to-seconds (plist-get pos :due)) now))
     (org-fc-awk-positions-for-paths paths))))

;;;; Bar Chart Generation

(defun org-fc-dashboard-bar-chart (stat)
  "Generate a svg bar-chart for the plist STAT."
  (let* ((width org-fc-dashboard-bar-chart-width)
         (height org-fc-dashboard-bar-chart-height)
         (total (float (plist-get stat :total)))
         (pos 0)
         (values
          `((,(/ (plist-get stat :again) total) . "red")
            (,(/ (plist-get stat :hard) total) . "yellow")
            (,(/ (plist-get stat :good) total) . "green")
            (,(/ (plist-get stat :easy) total) . "darkgreen")))
         (svg (svg-create width height)))
    (dolist (value values)
      (svg-rectangle svg pos 0 (* width (car value)) height :fill (cdr value))
      (setq pos (+ pos (* width (car value)))))
    (svg-image svg)))

(defun org-fc-dashboard-percent-right (stats)
  "Format review percentages in STATS."
  (let ((total (float (plist-get stats :total))))
    (format "  %5.2f | %5.2f | %5.2f | %5.2f"
            (or (* 100 (/ (plist-get stats :again) total)) 0.0)
            (or (* 100 (/ (plist-get stats :hard) total)) 0.0)
            (or (* 100 (/ (plist-get stats :good) total)) 0.0)
            (or (* 100 (/ (plist-get stats :easy) total)) 0.0))))

;;;; Main View

;; Based on `mu4e-main-view-real'
(defun org-fc-dashboard-view ()
  "Show the dashboard view in the current buffer."
  (interactive)
  (let* ((buf (get-buffer-create org-fc-dashboard-buffer-name))
         (inhibit-read-only t)
         (cards-stats (org-fc-awk-stats-cards))
         (positions-stats (org-fc-awk-stats-positions))
         (reviews-stats (org-fc-awk-stats-reviews)))
    (with-current-buffer buf
      (erase-buffer)
      (insert
       (propertize "Flashcards\n\n" 'face 'org-level-1))

      (insert
       (propertize "  Card Statistics\n\n" 'face 'org-level-1))

      (insert (format "    New: %d (day) %d (week) %d (month) \n"
                      (plist-get cards-stats :created-day)
                      (plist-get cards-stats :created-week)
                      (plist-get cards-stats :created-month)))

      (insert "\n")
      (insert (format
               "    %6d Cards, %d suspended\n"
               (plist-get cards-stats :total)
               (plist-get cards-stats :suspended)))
      (dolist (position '((:type-normal . "Normal")
                          (:type-double . "Double")
                          (:type-text-input . "Text Input")
                          (:type-cloze . "Cloze")))
        (insert
         (format "    %6d %s\n"
                 (or (plist-get cards-stats (car position)) 0)
                 (cdr position))))

      (insert "\n")
      (insert
       (propertize "  Position Statistics\n\n" 'face 'org-level-1))

      (insert (format "    %6d Due Now\n\n" (plist-get positions-stats :due)))

      (dolist (position '((:avg-ease . "Avg. Ease")
                          (:avg-box . "Avg. Box")
                          (:avg-interval . "Avg. Interval (days)")))
        (insert
         (format "    %6.2f %s\n"
                 (plist-get positions-stats (car position))
                 (cdr position))))

      (insert "\n")

      (when reviews-stats
        (insert
         (propertize "  Review Statistics\n\n" 'face 'org-level-1))

        (dolist (scope '((:day . "Day")
                         (:week . "Week")
                         (:month . "Month")
                         (:all . "All")))
          (when-let (stat (plist-get reviews-stats (car scope)))
            (when (> (plist-get stat :total) 0)
              (insert (propertize (format "    %s (%d)\n" (cdr scope) (plist-get stat :total)) 'face 'org-level-1))
              (insert "    ")
              (insert-image (org-fc-dashboard-bar-chart stat))
              (insert (org-fc-dashboard-percent-right stat))
              (insert "\n\n"))))

        (insert "\n"))

      (insert
       (propertize "  [r] Review\n" 'face 'org-level-1))
      (insert
       (propertize "  [q] Quit\n" 'face 'org-level-1)))))

(defvar org-fc-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'org-fc-review-all)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "G") 'org-fc-dashboard-view)
    map))

(define-derived-mode org-fc-dashboard-mode special-mode "org-fc main"
  "Major mode providing an overview of the flashcard system"
  (set (make-local-variable 'revert-buffer-function) #'org-fc-dashboard-view)
  (setq-local cursor-type nil))

;;;###autoload
(defun org-fc-dashboard ()
  "Open a buffer showing the dashboard view."
  (interactive)
  (org-fc-dashboard-view)
  (switch-to-buffer org-fc-dashboard-buffer-name)
  (goto-char (point-min))
  (org-fc-dashboard-mode))

;;; Footer

(provide 'org-fc)

;;; org-fc.el ends here
