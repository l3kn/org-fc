;;; org-fc-core.el --- Core functions of org-fc -*- lexical-binding: t; -*-

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
(require 'outline)
(require 'org-id)
(require 'org-indent)
(require 'org-element)
(require 'subr-x)

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

(defcustom org-fc-algo-property "FC_ALGO"
  "Property used to store the cards repetition spacing algorithm."
  :type 'string
  :group 'org-fc)

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

(defcustom org-fc-drawer-whitelist '()
  "Drawers that are not hidden during review."
  :type 'list
  :group 'org-fc)

(defcustom org-fc-stats-review-min-box 0
  "Minimum box for reviews to include in the review stats."
  :type 'integer
  :group 'org-fc)

(defcustom org-fc-back-heading-titles '("Back")
  "Allowed titles for the back headings of flashcards."
  :type '(list string)
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

(defun org-fc-timestamp-in (interval)
  "Generate an `org-mode' timestamp INTERVAL days from now."
  (let ((seconds (* interval 60 60 24))
        (now (time-to-seconds)))
    (format-time-string
     "%FT%TZ"
     (seconds-to-time (+ now seconds))
     "UTC0")))

(defun org-fc-show-latex ()
  "Show latex fragments of heading at point."
  (let ((inhibit-message t))
    (org-latex-preview 4)))

(defun org-fc-back-heading-position ()
  "Return point at the beginning of an entries `Back' subheading.
Return nil if there is no such heading.
This is expected to be called on an card entry heading."
  (let ((found nil)
        (level (cl-first (org-heading-components))))
    (org-map-entries
     (lambda ()
       (when (let ((comps (org-heading-components)))
               (and
		(member (cl-fifth comps) org-fc-back-heading-titles)
                (= (cl-first comps) (1+ level))))
         (setq found (point))))
     t 'tree)
    found))

(defun org-fc-has-back-heading-p ()
  "Check if the entry at point has a `Back' subheading.
Used to determine if a card uses the compact style."
  (not (null (org-fc-back-heading-position))))

(defun org-fc-sorted-random (n)
  "Generate a list of N sorted random numbers."
  (sort (cl-loop for i below n collect (cl-random 1.0)) #'>))

(defun org-fc-zip (as bs)
  "Zip two lists AS and BS."
  (cl-loop for a in as for b in bs collect (cons a b)))

;; File-scoped variant of `org-id-goto'
(defun org-fc-id-goto (id file)
  "Go to the heading with ID in FILE."
  (if-let ((position (org-id-find-id-in-file id file)))
      (goto-char (cdr position))
    (error "ID %s not found in %s" id file)))

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
  (when org-indent-mode
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

;;; Classes

(defclass org-fc-file ()
  ((path
    :initarg :path
    :type string
    :documentation "Location of the file.")
   (hash
    :initarg :hash
    :type (or null string)
    :documentation "Optional hash of the file, used for caching.")
   (title
    :initarg :title
    :type (or null string)
    :documentation "Title of the file.")
   (cards
    :initarg :cards
    :initform nil
    :type list
    :custom (repeat org-fc-card)
    :documentation "Flashcards in the file.")))

(defclass org-fc-card ()
  ((file
    :initarg :file
    :type org-fc-file
    :documentation "Parent file.")
   (id
    :initarg :id
    :type string
    :documentation "Org-mode ID of the card.")
   (title
    :initarg :title
    :type string
    :documentation "Title of the card.")
   (type
    :initarg :type
    :type symbol
    :documentation "Type of the card.")
   (algo
    :initarg :algo
    :type (or null symbol)
    :documentation "Algorithm of the card.")
   ;; TODO: Can both types be combined?
   (cloze-type
    :initarg :cloze-type
    :type (or null symbol)
    :documentation "Cloze-type of the card.")
   (created
    :initarg :created
    :initform nil
    :type list
    :custom (repeat integer)
    :documentation "Timestamp when this card was created.")
   (suspended
    :initarg :suspended
    :initform nil
    :type boolean
    :documentation "Suspension state of the card.")
   (tags
    :initarg :tags
    :initform nil
    :type list
    :custom (repeat string)
    :documentation "Tags of the card, both local and inherited.")
   (positions
    :initarg :positions
    :initform nil
    :type list
    :custom (repeat org-fc-position)
    :documentation "Positions of the card.")))

(defclass org-fc-position ()
  ((card
    :initarg :card
    :type org-fc-card
    :documentation "Parent card of this position.")
   (name
    :initarg :name
    :initform ""
    :type string
    :documentation "Name of the position, e.g. \"front\", \"back\" or \"0\", \"1\", ... .")
   (due
    :initarg :due
    :initform nil
    :type list
    :custom (repeat integer)
    :documentation "Timestamp when this position is due.")
   (data
    :initarg :data
    :initform nil
    :type list
    :documentation "Algorithm specific review data.")))

(defun org-fc-card-from-plist (plist file)
  (let* ((card
          (org-fc-card
           :file file
           :id (plist-get plist :id)
           :title (plist-get plist :title)
           :algo (plist-get plist :algo)
           :type (plist-get plist :type)
           :cloze-type (plist-get plist :cloze-type)
           :created (plist-get plist :created)
           :suspended (plist-get plist :suspended)
           :tags (plist-get plist :tags)))
         (positions
          (mapcar (lambda (plist) (org-fc-position-from-plist plist card))
                  (plist-get plist :positions))))
    (oset card positions positions)
    card))

(defun org-fc-position-from-plist (plist card)
  (org-fc-position
   :card card
   :name (plist-get plist :position)
   :due (plist-get plist :due)
   :data plist))

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

;;; Card Initialization

(defun org-fc--init-card (type)
  "Initialize the current card as a flashcard.
Should only be used by the init functions of card TYPEs."
  (when (org-fc-entry-p)
      (error "Headline is already a flashcard"))
  (org-back-to-heading)
  (org-set-property
   org-fc-created-property
   (org-fc-timestamp-in 0))
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
  (if-let ((entry (alist-get type org-fc-types nil nil #'string=)))
      (cl-first entry)
    (error "No such flashcard type: %s" type)))

(defun org-fc-type-flip-fn (type)
  "Get the flip function for a card of TYPE."
  (if-let ((entry (alist-get type org-fc-types nil nil #'string=)))
      (cl-second entry)
    (error "No such flashcard type: %s" type)))

(defun org-fc-type-update-fn (type)
  "Get the update function for a card of TYPE."
  (if-let ((entry (alist-get type org-fc-types nil nil #'string=)))
      (cl-third entry)
    (error "No such flashcard type: %s" type)))

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
    (when face (overlay-put o 'face face))
    (if (stringp text)
	(progn
	  (overlay-put o 'invisible nil)
	  (overlay-put o 'display text))
      (overlay-put o 'invisible t))
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
	      (org-fold-hide-drawer-toggle)
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
See `org-fold-show-set-visibility' for possible values"
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
    (org-fold-show-set-visibility org-fc-narrow-visibility)
    (when (member "noheading" tags) (org-fc-hide-heading))))

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
  "Suspend the headline at point if it is a flashcard.
If there is an active review session, all positions of
the now suspended card are removed from it."
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
	     (and `(and ,@(mapcar #'compile-inner (cdr filter))))
	     (or `(or ,@(mapcar #'compile-inner (cdr filter))))
	     (not
	      (check-arity-exact filter 1)
	      `(not ,(compile-inner (cadr filter))))
	     (tag
	      (check-arity-exact filter 1)
	      `(member ,(cadr filter) (oref ,card-var tags)))
	     (type
	      (check-arity-exact filter 1)
	      `(eq ',(if (stringp (cadr filter))
			 (intern (cadr filter))
		       (cadr filter))
		   (oref ,card-var type))))))
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

    (org-fc-index-flatten-file
     (funcall org-fc-index-function paths filter))))

(defun org-fc-index-flatten-file (index)
  "Flatten INDEX into a list of cards.
Relevant data from the file is included in each card element."
  (mapcan
   (lambda (file) (oref file cards))
   index))

(defun org-fc-index-filter-due (index)
  "Filter INDEX to include only unsuspended due positions.
Cards with no positions are removed from the index."
  (let (res (now (current-time)))
    (dolist (card index)
      (unless (oref card suspended)
        (let ((due
               (cl-remove-if-not
                (lambda (pos)
                  (time-less-p (oref pos due) now))
                (oref card positions))))
          (unless (null due)
            (oset card positions
                  (if (or (not org-fc-bury-siblings)
                          (member (oref card cloze-type) '(single enumeration)))
                      due (list (car due))))
            (push card res)))))
    (reverse res)))

;;; Demo Mode

;;;###autoload
(defun org-fc-demo ()
  "Start a review of the demo file."
  (interactive)
  (let ((path (expand-file-name "demo.org" org-fc-source-path)))
    (with-current-buffer (find-file path)
      ;; Prevent any changes to the demo file
      (read-only-mode 1)
      (org-fc-review '(:paths buffer :order ordered)))))

;;; Contexts

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
  "Select a review context.
The `buffer' context is resolved to the filename of the current
buffer immediately."
  (let* ((choice (completing-read
                  "Context: "
                  (mapcar (lambda (c) (car c)) (org-fc-contexts))
                  nil
                  :require-match))
         (context
          ;; If the result is empty, the user quit the prompt
          (unless (string= choice "")
            (alist-get (intern choice) (org-fc-contexts)))))
    ;; Resolve the `buffer' immediately so it will be valid even when
    ;; using the selected context from within another buffer,
    ;; e.g. when starting a review from the dashboard.
    (if (eq (plist-get context :paths) 'buffer)
        (list
         :paths (buffer-file-name)
         :filter (plist-get context :filter))
      context)))

;;; Footer

(provide 'org-fc-core)

;;; org-fc-core.el ends here
