;;; org-fc.el --- Spaced Repetition System for Emacs org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Leon Rische

;; Author: Leon Rische <emacs@leonrische.me>
;; Url: https://www.leonrische.me/pages/org_flashcards.html
;; Package-requires: ((emacs "26.3") (org "9.3"))
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
(require 'org-indent)
(require 'org-element)
(require 'outline)
(require 'parse-time)
(require 'subr-x)
(require 'svg)

(require 'org-fc-compat)

;;; Customization

(defgroup org-fc nil
  "Manage and review flashcards with Emacs."
  :group 'external
  :group 'text)

(defcustom org-fc-directories '("~/org/")
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

(defcustom org-fc-shuffle-positions t
  "Shuffle positions before review."
  :type 'boolean
  :group 'org-fc)

(defcustom org-fc-append-failed-cards t
  "Add failed cards to the end of the review session."
  :type 'boolean
  :group 'org-fc)

(defcustom org-fc-index-function #'org-fc-awk-index
  "Function used to index cards in a list of paths."
  :type 'function
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

(defcustom org-fc-suspended-tag "suspended"
  "Tag for marking suspended cards."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-flashcard-tag "fc"
  "Tag for marking headlines as flashcards."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-demo-tag "fc-demo"
  "Tag for marking headlines as demo flashcards.
When demo flashcards are reviewed, their review data is not
updated.  This is used for the `org-fc-demo' and for testing card
types."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-review-data-drawer "REVIEW_DATA"
  "Name of the drawer used to store review data."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-drawer-whitelist '()
  "Drawers that are not hidden during review."
  :type 'list
  :group 'org-fc)

(defcustom org-fc-stats-review-min-box 0
  "Minimum box for reviews to include in the review stats."
  :type 'integer
  :group 'org-fc)

;;;; Spacing Parameters

(defcustom org-fc-algorithm 'sm2-v1
  "Algorithm for spacing reviews of cards."
  :type '(choice (const sm2-v1) (const sm2-v2))
  :group 'org-fc)

(defcustom org-fc-bury-siblings nil
  "If non-nil, show at most one position of a card per review.
Does not apply to cloze single and cloze enumeration cards."
  :type 'boolean
  :group 'org-fc)

;;;; Hooks

(defcustom org-fc-before-setup-hook '()
  "Functions run before a card is set up for review."
  :type 'hook
  :group 'org-fc)

(defcustom org-fc-after-setup-hook '()
  "Functions run after a card is set up for review."
  :type 'hook
  :group 'org-fc)

(defcustom org-fc-before-review-hook '()
  "Functions run when a review session is started."
  :type 'hook
  :group 'org-fc)

(defcustom org-fc-after-review-hook '()
  "Functions run when a review session ends / is quit."
  :type 'hook
  :group 'org-fc)

;;;; Diff

(defcustom org-fc-diff-filler ?-
  "Character for filling diffs when the input was too short."
  :type 'character
  :group 'org-fc)

;;;; Font Faces

;; Based on `magit-diff-added'
(defface org-fc-diff-correct
  `((((class color) (background light))
     :background "#ddffdd"
     :foreground "#22aa22")
    (((class color) (background dark))
     :background "#335533"
     :foreground "#ddffdd"))
  "Face for correct parts of a diff."
  :group 'org-fc)

;; Based on `magit-diff-removed'
(defface org-fc-diff-wrong
  `((((class color) (background light))
     :background "#ffdddd"
     :foreground "#aa2222")
    (((class color) (background dark))
     :background "#553333"
     :foreground "#ffdddd"))
  "Face for wrong parts of a diff."
  :group 'org-fc)

;;; Variables

;; Not customizable because the indexers / filters expect ISO8601
(defvar org-fc-timestamp-format "%FT%TZ"
  "Format to use for storing timestamps.
Defaults to ISO8601")

(defvar org-fc-reviewing-existing-buffer nil
  "Track if the current buffer was open before the review.")
(make-variable-buffer-local 'org-fc-reviewing-existing-buffer)

(defvar org-fc-original-header-line-format nil
  "`header-line-format' before it was set by org-fc.")

(defvar org-fc-timestamp nil
  "Time the last card was flipped.
Used to calculate the time needed for reviewing a card.")

;;; Helper Functions

(defun org-fc-member-p (path)
  "Check if PATH is member of one of the `org-fc-directories'."
  (setq path (expand-file-name path))
  (and (string= (file-name-extension path) "org")
       (cl-some
        (lambda (dir) (string-prefix-p (expand-file-name dir) path))
        org-fc-directories)))

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
  "Show latex fragments of heading at point."
  (org-latex-preview 4))

(defun org-fc-back-heading-position ()
  "Return point at the beginning of an entries 'Back' subheading.
Return nil if there is no such heading.
This is expected to be called on an card entry heading."
  (let ((found nil)
        (level (cl-first (org-heading-components))))
    (org-map-entries
     (lambda ()
       (when (let ((comps (org-heading-components)))
               (and
                (string= (cl-fifth comps) "Back")
                (= (cl-first comps) (1+ level))))
         (setq found (point))))
     t 'tree)
    found))

(defun org-fc-has-back-heading-p ()
  "Check if the entry at point has a 'Back' subheading.
Used to determine if a card uses the compact style."
  (not (null (org-fc-back-heading-position))))

(defun org-fc-shuffle (list)
  "Randomize the order of elements in LIST.
This mutates / destroys the input list."
  (sort list (lambda (_a _b) (< (cl-random 1.0) 0.5))))

(defun org-fc-sorted-random (n)
  "Generate a list of N sorted random numbers."
  (sort (cl-loop for i below n collect (cl-random 1.0)) #'>))

(defun org-fc-zip (as bs)
  "Zip two lists AS and BS."
  (cl-loop for a in as for b in bs collect (cons a b)))

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

(defun org-fc-deemphasize (string)
  "Remove org emphasis markers from STRING.
Returns a pair (marker . body)."
  (if (or (string-match org-emph-re string)
          (string-match org-verbatim-re string))
      (cons (match-string 3 string) (match-string 4 string))
    (cons nil string)))

(defun org-fc-emphasize (string)
  "Apply org emphasis faces to STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (org-do-emphasis-faces (point-max))
    (buffer-string)))

(defun org-fc-indent ()
  "Run `org-indent' on the current headline.
Usually org-indent runs with a delay, so when reviewing a card in
a new file, the cards contents jump to the right (are indented)
during the review.  We can get around this by synchronously
indenting the current heading."
  (if org-indent-mode
      (let ((el (org-element-at-point)))
        (org-indent-add-properties
         (org-element-property :begin el)
         (org-element-property :end el)))))

(defmacro org-fc-with-point-at-entry (&rest body)
  "Execute BODY with point at the card heading.
If point is not inside a flashcard entry, an error is raised."
  `(save-excursion
     (org-fc-goto-entry-heading)
     ,@body))

(defmacro org-fc-with-point-at-back-heading (&rest body)
  "Execute BODY with point at the card's back heading.
If point is not inside a flashcard entry, an error is raised."
  `(if-let ((pos (org-fc-back-heading-position)))
       (save-excursion
         (goto-char pos)
         ,@body)))

;;; Diff

(defun org-fc-diff-subseq (a b start1 start2 end1 end2)
  "Find (index-a index-b len) of the longest matching subsequence in A and B.
Only parts of A in the range START1 to END1 and parts of B in the
range START2 to END2 are considered.
If there is no matching subsequence, nil is returned."
  (let ((best-length 0) (best-i 0) (best-j 0)
        ;; Longest matching subsequence starting at index j of B,
        ;; offset by one to handle the case j = 0
        (lengths (make-vector (1+ (length b)) 0)))
    (cl-loop for i from start1 to end1 do
             (let ((new-lengths (make-vector (1+ (length b)) 0)))
               (cl-loop for j from start2 to end2 do
                        (if (eql (aref a i) (aref b j))
                            (let ((length (+ 1 (aref lengths j))))
                              (aset new-lengths (1+ j) length)
                              (when (> length best-length)
                                (setq best-length length)
                                (setq best-i (1+ (- i length)))
                                (setq best-j (1+ (- j length)))))))
               (setq lengths new-lengths)))
    (if (> best-length 0)
        (list best-i best-j best-length))))

(defun org-fc-diff-matching-blocks (a b start1 start2 end1 end2)
  "Find matching blocks of A and B.
Only parts of A in the range START1 to END1 and parts of B in the
range START2 to END2 are considered."
  (if-let ((match (org-fc-diff-subseq a b start1 start2 end1 end2)))
      (cl-destructuring-bind (i j len) match
        (append
         (org-fc-diff-matching-blocks a b start1 start2 (1- i) (1- j))
         (list match)
         (org-fc-diff-matching-blocks a b (+ i len) (+ j len) end1 end2)))))

(defun org-fc-diff--propertize-got (got blocks expected-length)
  "Propertize the GOT answer given matching BLOCKS.
If it is shorter than EXPECTED-LENGTH, it is filled using
`org-fc-diff-filler'."
  (let ((last 0) res)
    ;; Prepend filler if text at start is missing
    (unless (null blocks)
      (cl-destructuring-bind (i j _len) (car blocks)
        (if (> j i)
            (setq res
                  (propertize
                   (make-string (- j i) org-fc-diff-filler)
                   'face 'org-fc-diff-wrong)))))
    (cl-loop for (i _ len) in blocks do
             (setq res
                   (concat
                    res
                    (propertize
                     (cl-subseq got last i)
                     'face 'org-fc-diff-wrong)
                    (propertize
                     (cl-subseq got i (+ i len))
                     'face 'org-fc-diff-correct)))
             (setq last (+ i len)))
    (setq res
          (concat
           res
           (propertize (cl-subseq got last) 'face 'org-fc-diff-wrong)))
    ;; Append filler if result is shorter than expected
    (if (< (length res) expected-length)
        (concat
         res
         (propertize
          (make-string (- expected-length (length res)) org-fc-diff-filler)
          'face 'org-fc-diff-wrong))
      res)))

(defun org-fc-diff--propertize-expected (expected blocks)
  "Propertize the EXPECTED answer, given matching BLOCKS."
  (let ((last 0) res)
    (cl-loop for (_ j len) in blocks do
             (setq res
                   (concat
                    res
                    (cl-subseq expected last j)
                    (propertize
                     (cl-subseq expected j (+ j len))
                     'face 'org-fc-diff-correct)))
             (setq last (+ j len)))
    (concat res (cl-subseq expected last))))

(defun org-fc-diff (got expected)
  "Generate a colored diff of the strings GOT and EXPECTED."
  (if (string= got expected)
      (cons (propertize got 'face 'org-fc-diff-correct) nil)
    (let ((blocks (org-fc-diff-matching-blocks
                   got expected
                   0 0
                   (1- (length got))
                   (1- (length expected)))))
      (cons
       (org-fc-diff--propertize-got got blocks (length expected))
       (org-fc-diff--propertize-expected expected blocks)))))

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

;;; Card Indexing (AWK)

(require 'org-fc-awk)

;;; Card Initialization

(defun org-fc--init-card (type)
  "Initialize the current card as a flashcard.
Should only be used by the init functions of card TYPEs."
  (if (org-fc-entry-p)
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

(require 'org-fc-type-normal)
(require 'org-fc-type-double)
(require 'org-fc-type-text-input)
(require 'org-fc-type-cloze)

;;; Working with Overlays / Hiding Text
;;;; Showing / Hiding Overlays

(defun org-fc-remove-overlays ()
  "Remove all org-fc overlays in the current buffer."
  (interactive)
  (remove-overlays (point-min) (point-max) 'category 'org-fc))

;; Based on `outline-flag-region'
(defun org-fc-hide-region (from to &optional text face)
  "Hide region FROM ... TO, optionally replacing it with TEXT.
FACE can be used to set the text face of the overlay, e.g. to
make it bold."
  ;; (remove-overlays from to 'category 'org-fc)
  (let ((o (make-overlay from to nil 'front-advance)))
    (overlay-put o 'category 'org-fc)
    (overlay-put o 'evaporate t)
    (if face (overlay-put o 'face face))
    (if (stringp text)
        (progn
          (overlay-put o 'invisible nil)
          (overlay-put o 'display text))
      (overlay-put o 'invisible t))
    o))

(defun org-fc-overlay-region (from to &optional face)
  "Wrap region FROM ... TO in an overlay for later hiding.
FACE can be used to set the text face of the overlay."
  ;; (remove-overlays from to 'category 'org-fc)
  (let ((o (make-overlay from to)))
    (overlay-put o 'evaporate t)
    (if face (overlay-put o 'face face))
    (overlay-put o 'invisible nil)
    (overlay-put o 'category 'org-fc)
    o))

(defun org-fc-make-overlay (begin end &rest props)
  "Create an overlay from BEGIN to END with PROPS."
  (let ((o (make-overlay begin end)))
    (overlay-put o 'category 'org-fc)
    (cl-loop for (prop value) on props by #'cddr do
             (overlay-put o prop value))
    o))

(defun org-fc-overlay-surround (o before after &optional face)
  "Surround O with strings BEFORE and AFTER with optional FACE."
  (overlay-put o 'before-string (propertize before 'face face))
  (overlay-put o 'after-string (propertize after 'face face))
  o)

;;;; Hiding Drawers

(defun org-fc-hide-keyword-times ()
  "Hide all timestamp keywords (e.g. DEADLINE) after point."
  (save-excursion
    (while (re-search-forward org-keyword-time-regexp nil t)
      (let ((start (1- (match-beginning 0)))
            (end (match-end 0)))
        (org-fc-hide-region start end)))))

(defun org-fc-hide-drawers ()
  "Hide all drawers except ones in `org-fc-drawer-whitelist' after point."
  (let ((bound (org-element-property :end (org-element-at-point))))
    (save-excursion
      (while (re-search-forward org-drawer-regexp bound t)
        (let ((start (1- (match-beginning 0)))
              (name (match-string 1))
              (end))
          (if (re-search-forward ":END:" bound t)
              (setq end (point))
            (error "No :END: found for drawer"))
          (if (member name org-fc-drawer-whitelist)
              (org-flag-drawer nil nil start end)
            (org-fc-hide-region start end)))))))

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

(defcustom org-fc-narrow-visibility 'ancestors
  "Visibility of the current heading during review.
See `org-show-set-visibility' for possible values"
  :group 'org-fc
  :type 'symbol
  :options '(ancestors lineage minimal local tree canonical))

(defun org-fc-narrow ()
  "Narrow the outline tree.
Only parent headings of the current heading remain visible."
  (interactive)
  (let* ((tags (org-get-tags nil 'local)))
    ;; Find the first heading with a :narrow: tag or the top level
    ;; ancestor of the current heading and narrow to its region
    (save-excursion
      (while (org-up-heading-safe))
      (org-narrow-to-subtree)
      (outline-hide-subtree))
    ;; Show only the ancestors of the current card
    (org-show-set-visibility org-fc-narrow-visibility)
    (if (member "noheading" tags) (org-fc-hide-heading))))

;;; Updating Cards

(defun org-fc-map-cards (fn &optional scope)
  "Call FN for each flashcard headline in SCOPE.
FN is called with point at the headline and no arguments.
If SCOPE is nil, it defaults to the full buffer.
Other useful values are:
- tree
- region"
  (org-map-entries
   (lambda () (if (org-fc-entry-p) (funcall fn)))
   nil
   scope))

;;;###autoload
(defun org-fc-update ()
  "Re-process the current flashcard."
  (interactive)
  (org-fc-with-point-at-entry
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
  (org-fc-with-point-at-entry
   (org-fc--add-tag org-fc-suspended-tag)))

;;;###autoload
(defun org-fc-suspend-tree ()
  "Suspend all cards in the subtree at point."
  (interactive)
  (org-fc-map-cards 'org-fc-suspend-card 'tree))

;;;###autoload
(defun org-fc-suspend-buffer ()
  "Suspend all cards in the current buffer."
  (interactive)
  (org-fc-map-cards 'org-fc-suspend-card))

;;;###autoload
(defun org-fc-suspend-region ()
  "Suspend all cards in the current region."
  (interactive)
  (org-fc-map-cards 'org-fc-suspend-card 'region))

;;;###autoload
(defun org-fc-unsuspend-card ()
  "Unsuspend the headline at point.
Checks if the headline is a suspended card first."
  (interactive)
  (org-fc--remove-tag org-fc-suspended-tag))

;;;###autoload
(defun org-fc-unsuspend-tree ()
  "Un-suspend all cards in the subtree at point."
  (interactive)
  (org-fc-map-cards 'org-fc-unsuspend-card 'tree))

;;;###autoload
(defun org-fc-unsuspend-buffer ()
  "Un-suspend all cards in the current buffer."
  (interactive)
  (org-fc-map-cards 'org-fc-unsuspend-card))

;;;###autoload
(defun org-fc-unsuspend-region ()
  "Un-suspend all cards in the current region."
  (interactive)
  (org-fc-map-cards 'org-fc-unsuspend-card 'region))

;;; Indexing Cards
;;;; Card Filters

(defun org-fc--compile-filter (filter)
  "Compile FILTER into a lambda function.
Filters can be combinations of the following expressions:

- `(and ex1 ex2 ...)'
- `(or ex1 ex2 ...)'
- `(not ex)'
- `(tag \"tag\")'
- `(type card-type)' or `(type \"card-type\")'

For example, to match all double cards with tag \"math\",
use `(and (type double) (tag \"math\"))'."
  (let ((card-var (gensym)))
    (cl-labels
        ((check-arity-exact
          (filter n)
          (unless (= (length filter) (1+ n))
            (error
             (format "Filter '%s' expects %d argument(s)" filter n))))
         (compile-inner
          (filter)
          (cl-case (car filter)
            ('and `(and ,@(mapcar #'compile-inner (cdr filter))))
            ('or `(or ,@(mapcar #'compile-inner (cdr filter))))
            ('not
             (check-arity-exact filter 1)
             `(not ,(compile-inner (cadr filter))))
            ('tag
             (check-arity-exact filter 1)
             `(member ,(cadr filter) (plist-get ,card-var :tags)))
            ('type
             (check-arity-exact filter 1)
             `(eq ',(if (stringp (cadr filter))
                        (intern (cadr filter))
                      (cadr filter))
                  (plist-get ,card-var :type))))))
      `(lambda (,card-var)
         ,(compile-inner filter)))))

(defun org-fc-index (context)
  "Create an index for review CONTEXT."
  (let ((paths (plist-get context :paths))
        (filter (plist-get context :filter)))
    ;; Handle path formats / symbols
    (cond
     ((or (null paths) (eq paths 'all)) (setq paths org-fc-directories))
     ((eq paths 'buffer) (setq paths (list (buffer-file-name))))
     ((stringp paths) (setq paths (list paths))))

    (if filter (setq filter (org-fc--compile-filter filter)))

    (funcall org-fc-index-function paths filter)))

(defun org-fc-index-flatten-card (card)
  "Flatten CARD into a list of positions.
Relevant data from the card is included in each position
element."
  (mapcar
   (lambda (pos)
     (list
      :filetitle (plist-get card :filetitle)
      :tags (plist-get card :tags)
      :path (plist-get card :path)
      :id (plist-get card :id)
      :type (plist-get card :type)
      :due (plist-get pos :due)
      :position (plist-get pos :position)))
   (plist-get card :positions)))

(defun org-fc-index-filter-due (index)
  "Filter INDEX to include only unsuspended due positions.
Cards with no positions are removed from the index."
  (let (res (now (current-time)))
    (dolist (card index)
      (unless (plist-get card :suspended)
        (let ((due
               (cl-remove-if-not
                (lambda (pos)
                  (time-less-p (plist-get pos :due) now))
                (plist-get card :positions))))
          (unless (null due)
            (plist-put
             card :positions
             (if (or (not org-fc-bury-siblings)
                     (member (plist-get card :cloze-type) '(single enumeration)))
                 due (list (car due))))
            (push card res)))))
    res))

(defun org-fc-index-positions (index)
  "Return all positions in INDEX."
  (mapcan (lambda (card) (org-fc-index-flatten-card card)) index))

(defun org-fc-index-shuffled-positions (index)
  "Return all positions in INDEX in random order.
Positions are shuffled in a way that preserves the order of the
  positions for each card."
  ;; 1. assign each position a random number
  ;; 2. flatten the list
  ;; 3. sort by the random number
  ;; 4. remove the random numbers from the result
  (let ((positions
         (mapcan
          (lambda (card)
            (let ((pos (org-fc-index-flatten-card card)))
              (org-fc-zip
               (org-fc-sorted-random (length pos))
               pos)))
          index)))
    (mapcar
     #'cdr
     (sort positions (lambda (a b) (> (car a) (car b)))))))

;;; Review & Spacing

(require 'org-fc-algo-sm2)

;;;; Demo Mode

;;;###autoload
(defun org-fc-demo ()
  "Start a review of the demo file."
  (interactive)
  (let ((path (expand-file-name "demo.org" org-fc-source-path)))
    (with-current-buffer (find-file path)
      (org-fc-review-buffer))))

;;;; Session Management

(defclass org-fc-review-session ()
  ((current-item :initform nil)
   (paused :initform nil :initarg :paused)
   (history :initform nil)
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

(defun org-fc-review-history-add (elements)
  "Add ELEMENTS to review history."
  (push
   elements
   (slot-value org-fc--session 'history)))

(defun org-fc-review-history-save ()
  "Save all history entries in the current session."
  (when (and org-fc--session (oref org-fc--session history))
    (append-to-file
     (concat
      (mapconcat
       (lambda (elements) (mapconcat #'identity elements "\t"))
       (reverse (oref org-fc--session history))
       "\n")
      "\n")
     nil
     org-fc-review-history-file)
    (setf (oref org-fc--session history) nil)))

;; Make sure the history is saved even if Emacs is killed
(add-hook 'kill-emacs-hook #'org-fc-review-history-save)

(defun org-fc-session-cards-pending-p (session)
  "Check if there are any cards in SESSION."
  (not (null (oref session cards))))

(defun org-fc-session-pop-next-card (session)
  "Remove and return one card from SESSION."
  (let ((card (pop (oref session cards))))
    (setf (oref session current-item) card)
    card))

(defun org-fc-session-append-card (session card)
  "Append CARD to the cards of SESSION."
  (with-slots (cards) session
    (setf cards (append cards (list card)))))

(defun org-fc-session-prepend-card (session card)
  "Prepend CARD to the cards of SESSION."
  (with-slots (cards) session
    (setf cards (cons card cards))))

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

(defvar org-fc--session nil
  "Current review session.")

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
  (if-let ((position (org-fc-review-data-position)))
      (org-with-point-at (car position)
        (cddr (org-table-to-lisp)))))

(defun org-fc-set-review-data (data)
  "Set the cards review data to DATA."
  (save-excursion
    (let ((position (org-fc-review-data-position 'create)))
      (kill-region (car position) (cdr position))
      (goto-char (car position))
      (insert "| position | ease | box | interval | due |\n")
      (insert "|-|-|-|-|-|\n")
      (dolist (datum data)
        (insert
         "| "
         (mapconcat (lambda (x) (format "%s" x)) datum " | ")
         " |\n"))
      (org-table-align))))

(defun org-fc-review-data-default (position)
  "Default review data for position POSITION."
  (case org-fc-algorithm
    ('sm2-v1 (org-fc-algo-sm2-initial-review-data position))
    ('sm2-v2 (org-fc-algo-sm2-initial-review-data position))))

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

;;;; Review Modes
;;;;; Header Line

(defun org-fc-set-header-line ()
  "Set the header-line for review."
  (let* ((remaining (1+ (length (oref org-fc--session cards))))
         (current (oref org-fc--session current-item))
         (title
          (unless (member "notitle" (plist-get current :tags))
            (plist-get current :filetitle))))
    (setq org-fc-original-header-line-format header-line-format)
    (setq-local
     header-line-format
     `((org-fc-review-flip-mode "Flip")
       (org-fc-review-rate-mode "Rate")
       (org-fc-review-edit-mode "Edit")
       ,(format " (%d) " remaining)
       ,title))))

(defun org-fc-reset-header-line ()
  "Reset the header-line to its original value."
  (setq-local header-line-format org-fc-original-header-line-format))

;;;;; Flip Mode

(defvar org-fc-review-flip-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'org-fc-review-flip)
    (define-key map (kbd "q") 'org-fc-review-quit)
    (define-key map (kbd "p") 'org-fc-review-edit)
    (define-key map (kbd "s") 'org-fc-review-suspend-card)
    map)
  "Keymap for `org-fc-flip-mode'.")

(define-minor-mode org-fc-review-flip-mode
  "Minor mode for flipping flashcards.

\\{org-fc-review-flip-mode-map}"
  :init-value nil
  :lighter " fc-flip"
  :keymap org-fc-review-flip-mode-map
  :group 'org-fc
  (when org-fc-review-flip-mode
    ;; Make sure only one of the modes is active at a time
    (org-fc-review-rate-mode -1)
    ;; Make sure we're in org mode and there is an active review session
    (unless (and (derived-mode-p 'org-mode) org-fc--session)
      (org-fc-review-flip-mode -1))))

;;;;; Rate Mode

(defvar org-fc-review-rate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'org-fc-review-rate-again)
    (define-key map (kbd "h") 'org-fc-review-rate-hard)
    (define-key map (kbd "g") 'org-fc-review-rate-good)
    (define-key map (kbd "e") 'org-fc-review-rate-easy)
    (define-key map (kbd "s") 'org-fc-review-suspend-card)
    (define-key map (kbd "p") 'org-fc-review-edit)
    (define-key map (kbd "q") 'org-fc-review-quit)
    map)
  "Keymap for `org-fc-rate-mode'.")

(define-minor-mode org-fc-review-rate-mode
  "Minor mode for rating flashcards.

\\{org-fc-review-rate-mode-map}"
  :init-value nil
  :lighter " fc-rate"
  :keymap org-fc-review-rate-mode-map
  :group 'org-fc
  (when org-fc-review-rate-mode
    ;; Make sure only one of the modes is active at a time
    (org-fc-review-flip-mode -1)
    ;; Make sure we're in org mode and there is an active review session
    (unless (and (derived-mode-p 'org-mode) org-fc--session)
      (org-fc-review-rate-mode -1))))

(defvar org-fc-review-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'org-fc-review-resume)
    (define-key map (kbd "C-c C-k") 'org-fc-review-quit)
    map)
  "Keymap for `org-fc-edit-mode'.")

(define-minor-mode org-fc-review-edit-mode
  "Minor mode for editing flashcards.

\\{org-fc-review-edit-mode-map}"
  :init-value nil
  :lighter " fc-edit"
  :keymap org-fc-review-edit-mode-map
  :group 'org-fc
  (when org-fc-review-edit-mode
    (org-fc-review-flip-mode -1)
    (org-fc-review-rate-mode -1)
    ;; Make sure we're in org mode and there is an active review session
    (unless (and (derived-mode-p 'org-mode) org-fc--session)
      (org-fc-review-edit-mode -1))))

;;;; Main Loop
;;
;; Cards are reviewed by
;; 1. opening the file they are in
;; 2. calling the setup function for the card type
;; 3. switch to review-flip-mode
;; 4. calling the flip function for the card type
;; 5. switch to review-rate-mode
;; 6. updating the review data based on the rating
;;

(defvar org-fc-custom-contexts '()
  "User-defined review contexts.")

(defvar org-fc-context-all '(:paths all)
  "Default context for all cards.")
(defvar org-fc-context-buffer '(:paths buffer)
  "Default context for the current buffer.")

(defun org-fc-contexts ()
  "List of all contexts."
  (cl-list*
   (cons 'all org-fc-context-all)
   (cons 'buffer org-fc-context-buffer)
   org-fc-custom-contexts))

(defun org-fc-select-context ()
  "Select a review context."
  (let ((context (completing-read
                  "Context: "
                  (mapcar (lambda (c) (car c)) (org-fc-contexts))
                  nil
                  :require-match)))
    (unless (string= context "")
      (alist-get (intern context) (org-fc-contexts)))))

;;;###autoload
(defun org-fc-review (context)
  "Start a review session for all cards in CONTEXT.
Called interactively, prompt for the context.
Valid contexts:
- 'all, all cards in `org-fc-directories'
- 'buffer, all cards in the current buffer
- a list of paths"
  (interactive (list (org-fc-select-context)))
  (if org-fc--session
      (message "Flashcards are already being reviewed")
    (let* ((index (org-fc-index context))
           (cards (org-fc-index-filter-due index)))
      (if org-fc-shuffle-positions
          (setq cards (org-fc-index-shuffled-positions cards))
        (setq cards (org-fc-index-positions cards)))
      (if (null cards)
          (message "No cards due right now")
        (progn
          (setq org-fc--session
                (org-fc-make-review-session cards))
          (run-hooks 'org-fc-before-review-hook)
          (org-fc-review-next-card))))))

(defun org-fc-review-resume ()
  "Resume review session, if it was paused."
  (interactive)
  (if org-fc--session
      (progn
        (org-fc-review-edit-mode -1)
        (org-fc-review-next-card 'resuming))
    (message "No session to resume to")))

;;;###autoload
(defun org-fc-review-buffer ()
  "Review due cards in the current buffer."
  (interactive)
  (org-fc-review org-fc-context-buffer))

;;;###autoload
(defun org-fc-review-all ()
  "Review all due cards."
  (interactive)
  (org-fc-review org-fc-context-all))

(defun org-fc-review-next-card (&optional resuming)
  "Review the next card of the current session.
If RESUMING is non-nil, some parts of the buffer setup are skipped."
  (if (org-fc-session-cards-pending-p org-fc--session)
      (condition-case err
          (let* ((card (org-fc-session-pop-next-card org-fc--session))
                 (path (plist-get card :path))
                 (id (plist-get card :id))
                 (type (plist-get card :type))
                 (position (plist-get card :position)))
            (let ((buffer (find-buffer-visiting path)))
              (with-current-buffer (find-file path)
                (unless resuming
                  ;; If buffer was already open, don't kill it after rating the card
                  (if buffer
                      (setq-local org-fc-reviewing-existing-buffer t)
                    (setq-local org-fc-reviewing-existing-buffer nil))
                  (org-fc-set-header-line))

                (goto-char (point-min))
                (org-fc-id-goto id path)

                (org-fc-indent)
                ;; Make sure the headline the card is in is expanded
                (org-reveal)
                (org-fc-narrow)
                (org-fc-hide-keyword-times)
                (org-fc-hide-drawers)
                (org-fc-show-latex)
                (org-display-inline-images)
                (run-hooks 'org-fc-before-setup-hook)

                (setq org-fc-timestamp (time-to-seconds (current-time)))
                (let ((step (funcall (org-fc-type-setup-fn type) position)))
                  (run-hooks 'org-fc-after-setup-hook)

                  ;; If the card has a no-noop flip function,
                  ;; skip to rate-mode
                  (let ((flip-fn (org-fc-type-flip-fn type)))
                    (if (or
                         (eq step 'rate)
                         (null flip-fn)
                         (eq flip-fn #'org-fc-noop))
                        (org-fc-review-rate-mode 1)
                      (org-fc-review-flip-mode 1)))))))
        (error
         (org-fc-review-quit)
         (signal (car err) (cdr err))))
    (message "Review Done")
    (org-fc-review-quit)))

(defmacro org-fc-review-with-current-item (var &rest body)
  "Evaluate BODY with the current card bound to VAR.
Before evaluating BODY, check if the heading at point has the
same ID as the current card in the session."
  (declare (indent defun))
  `(if org-fc--session
       (if-let ((,var (oref org-fc--session current-item)))
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
          (funcall (org-fc-type-flip-fn type))
          (org-fc-review-rate-mode)))
    (error
     (org-fc-review-quit)
     (signal (car err) (cdr err)))))

(defun org-fc-review-rate (rating)
  "Rate the card at point with RATING."
  (interactive)
  (condition-case err
      (org-fc-review-with-current-item card
        (let* ((path (plist-get card :path))
               (id (plist-get card :id))
               (position (plist-get card :position))
               (now (time-to-seconds (current-time)))
               (delta (- now org-fc-timestamp)))
          (org-fc-session-add-rating org-fc--session rating)
          (org-fc-review-update-data path id position rating delta)
          (org-fc-review-reset)

          (if (and (eq rating 'again) org-fc-append-failed-cards)
              (org-fc-session-append-card org-fc--session card))

          (save-buffer)
          (if org-fc-reviewing-existing-buffer
              (org-fc-review-reset)
            (kill-buffer))
          (org-fc-review-next-card)))
    (error
     (org-fc-review-quit)
     (signal (car err) (cdr err)))))

(define-obsolete-function-alias 'org-fc-review-rate-card 'org-fc-review-rate)

(defun org-fc-review-rate-again ()
  "Rate the card at point with 'again'."
  (interactive)
  (org-fc-review-rate 'again))

(defun org-fc-review-rate-hard ()
  "Rate the card at point with 'hard'."
  (interactive)
  (org-fc-review-rate 'hard))

(defun org-fc-review-rate-good ()
  "Rate the card at point with 'good'."
  (interactive)
  (org-fc-review-rate 'good))

(defun org-fc-review-rate-easy ()
  "Rate the card at point with 'easy'."
  (interactive)
  (org-fc-review-rate 'easy))

(defun org-fc-review-skip-card ()
  "Skip card and proceed to next."
  (interactive)
  (org-fc-review-reset)
  (org-fc-review-next-card))

(defun org-fc-review-suspend-card ()
  "Suspend card and proceed to next."
  (interactive)
  (org-fc-suspend-card)
  ;; Remove all other positions from review session
  (with-slots (current-item cards) org-fc--session
    (let ((id (plist-get current-item :id)))
      (setf cards
            (cl-remove-if
             (lambda (card)
               (string= id (plist-get card :id))) cards))))
  (org-fc-review-reset)
  (org-fc-review-next-card))

(defun org-fc-review-update-data (path id position rating delta)
  "Update the review data of the card.
Also add a new entry in the review history file.  PATH, ID,
POSITION identify the position that was reviewed, RATING is a
review rating and DELTA the time in seconds between showing and
rating the card."
  (org-fc-with-point-at-entry
   ;; If the card is marked as a demo card, don't log its reviews and
   ;; don't update its review data
   (unless (member org-fc-demo-tag (org-get-tags))
     (let* ((data (org-fc-get-review-data))
            (current (assoc position data #'string=)))
       (unless current
         (error "No review data found for this position"))
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
           (format "%.2f" delta)
           (symbol-name org-fc-algorithm)))
         (cl-destructuring-bind (next-ease next-box next-interval)
             (org-fc-sm2-next-parameters ease box interval rating)
           (setcdr
            current
            (list (format "%.2f" next-ease)
                  (number-to-string next-box)
                  (format "%.2f" next-interval)
                  (org-fc-timestamp-in next-interval)))
           (org-fc-set-review-data data)))))))

(defun org-fc-review-reset ()
  "Reset the buffer to its state before the review."
  (org-fc-review-rate-mode -1)
  (org-fc-review-flip-mode -1)
  (org-fc-review-edit-mode -1)
  (org-fc-reset-header-line)
  (org-fc-remove-overlays)
  (widen))

;;;###autoload
(defun org-fc-review-quit ()
  "Quit the review, remove all overlays from the buffer."
  (interactive)
  (org-fc-review-reset)
  (run-hooks 'org-fc-after-review-hook)
  (org-fc-review-history-save)
  (setq org-fc--session nil))

;;;###autoload
(defun org-fc-review-edit ()
  "Edit current flashcard.
Pauses the review, unnarrows the buffer and activates
`org-fc-edit-mode'."
  (interactive)
  (widen)
  (org-fc-remove-overlays)
  ;; Queue the current flashcard so it's reviewed a second time
  (org-fc-session-prepend-card
   org-fc--session
   (oref org-fc--session current-item))
  (setf (oref org-fc--session paused) t)
  (setf (oref org-fc--session current-item) nil)
  (org-fc-review-edit-mode 1))

;;; Dashboard

(require 'org-fc-dashboard)

;;; Cache

(require 'org-fc-cache)

;;; Footer

(provide 'org-fc)

;;; org-fc.el ends here
