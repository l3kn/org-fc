;;; org-fc-review.el --- Review mode for org-fc -*- lexical-binding: t; -*-

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
;; During review, due cards are presented one after another and the
;; user is asked to rate each card.
;;
;; Cards are reviewed by
;; 1. opening the file they are in
;; 2. calling the setup function for the card type
;; 3. switch to review-flip-mode
;; 4. calling the flip function for the card type
;; 5. switch to review-rate-mode
;; 6. updating the review data based on the rating
;;
;;; Code:

(require 'eieio)

(require 'org-fc-core)

;;; Hooks

(defcustom org-fc-before-setup-hook '()
  "Functions run before a card is set up for review."
  :type 'hook
  :group 'org-fc)

(defcustom org-fc-after-setup-hook '()
  "Functions run after a card is set up for review."
  :type 'hook
  :group 'org-fc)

(defcustom org-fc-after-flip-hook '()
  "Functions run after a card is flipped during review."
  :type 'hook
  :group 'org-fc)

(defcustom org-fc-after-rate-hook '()
  "Functions run after a card is rated during review."
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

(defcustom org-fc-review-hide-title-in-header-line nil
  "Whether or not to hide the file title in review header line.

Hide title for individual cards by adding the :notitle: tag."
  :type 'boolean
  :group 'org-fc)

;;; Variables

(defvar org-fc-review--session nil
  "Current review session.")

(defvar org-fc-review--timestamp nil
  "Time the last card was flipped.
Used to calculate the time needed for reviewing a card.")

(defvar org-fc-reviewing-existing-buffer nil
  "Track if the current buffer was open before the review.")
(make-variable-buffer-local 'org-fc-reviewing-existing-buffer)

;;; Main Review Functions

(defun org-fc-review-order-sequential (index)
  "Return all positions in INDEX."
  (mapcan
   #'org-fc-index-flatten-card
   (org-fc-index-flatten-file index)))

(defun org-fc-review-order-random (index)
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
          (org-fc-index-flatten-file index))))
    (mapcar
     #'cdr
     (sort positions (lambda (a b) (> (car a) (car b)))))))

;;;###autoload
(defun org-fc-review (context)
  "Start a review session for all cards in CONTEXT.
Called interactively, prompt for the context.
Valid contexts:
- 'all, all cards in `org-fc-directories'
- 'buffer, all cards in the current buffer
- a list of paths"
  (interactive (list (org-fc-select-context)))
  (if org-fc-review--session
      (when (yes-or-no-p "Flashcards are already being reviewed. Resume? ")
        (org-fc-review-resume))
    (let* ((index (org-fc-index-filter-due (org-fc-index context)))
           (order (or (plist-get context :order)
                      (if org-fc-shuffle-positions 'random 'ordered)))
           (positions
            (case order
              (sequential (org-fc-review-order-sequential index))
              (random     (org-fc-review-order-random index))
              (otherwise  (if (functionp order)
                              (funcall order index)
                            (error (format "Unknown review order: %s" order)))))))
      (if (null positions)
          (message "No positions due right now")
        (progn
          (setq org-fc-review--session (org-fc-make-review-session positions))
          (run-hooks 'org-fc-before-review-hook)
          (org-fc-review-next-card))))))

(defun org-fc-review-resume ()
  "Resume review session, if it was paused."
  (interactive)
  (if org-fc-review--session
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
  (if-let ((item (org-fc-review-pop-item org-fc-review--session)))
    (condition-case err (org-fc-review-item item resuming)
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
  `(if org-fc-review--session
       ;; TODO: Generalize to different items
       (if-let ((,var (oref org-fc-review--session current-item)))
           ;; (message ,var)
           (if (string= (oref (oref ,var card) id) (org-id-get))
               (progn ,@body)
             (message "Flashcard ID mismatch")
             (message (format "Expected %s" (oref (oref ,var card) id)))
             (message (format "Got %s" (org-id-get))))
         (message "No flashcard review is in progress"))))

(defun org-fc-review-flip ()
  "Flip the current flashcard."
  (interactive)
  (condition-case err
      (org-fc-review-with-current-item item
        (let ((type (oref (oref item card) type)))
          (funcall (org-fc-type-flip-fn type))
          (run-hooks 'org-fc-after-flip-hook)
          (org-fc-review-rate-mode)))
    (error
     (org-fc-review-quit)
     (signal (car err) (cdr err)))))

(defun org-fc-review-rate (rating)
  "Rate the card at point with RATING."
  (interactive)
  (condition-case err
      (org-fc-review-with-current-item item
        (org-fc-review-add-rating org-fc-review--session rating)
        (org-fc-review-update-data item rating)
        (org-fc-review-reset)
        (run-hooks 'org-fc-after-rate-hook)

        ;; TODO: With a failed box of 1, we can't guarantee timely
        ;; (not too early) reviews this way Alternative would be to
        ;; track a "failed" queue and once cards run out, see if one
        ;; of the cards there is due.
        ;;
        ;; (if (and (eq rating 'again) org-fc-append-failed-cards)
        ;;     (with-slots (cards) org-fc-review--session
        ;;       (setf cards (append cards (list card)))))

        (save-buffer)
        (if org-fc-reviewing-existing-buffer
            (org-fc-review-reset)
          (kill-buffer))
        (org-fc-review-next-card))
    (error
     (org-fc-review-quit)
     (signal (car err) (cdr err)))))

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
  (org-fc-review-reset)
  (org-fc-review-next-card))

(defun org-fc-review-update-data (position rating)
  "Update the review data of the card.
Also add a new entry in the review history file.  PATH, ID,
POSITION identify the position that was reviewed, RATING is a
review rating and DELTA the time in seconds between showing and
rating the card."
  (org-fc-with-point-at-entry
   ;; If the card is marked as a demo card, don't log its reviews and
   ;; don't update its review data
   (unless (member org-fc-demo-tag (org-get-tags))
     (let* ((now (time-to-seconds (current-time)))
            (algorithm
             ;; For compatibility reasons, default to SM2 for cards that
             ;; don't have the algorithm property set
             (if-let ((algo (org-entry-get (point) org-fc-algo-property)))
                 (intern algo)
               'sm2))

            (delta (- now org-fc-review--timestamp))
            (name (oref position name))
            (review-data (org-fc-review-data-parse
                          (org-fc-review-data-headers algorithm)))
            (current (org-fc-review-data-get-row review-data name)))
       (unless current
         (error "No review data found for this position"))

       (case algorithm
         (sm2
          (org-fc-algo-sm2-history-add position current rating delta))
         (fsrs
          (org-fc-algo-fsrs-history-add position current rating delta)))

       (org-fc-review-data-set-row
        review-data name
        (org-fc-plist-merge
         current

         (case algorithm
           (sm2
            (org-fc-algo-sm2-next-review-data current rating))
           (fsrs
            (org-fc-algo-fsrs-next-review-data current rating)))))
       (org-fc-review-data-write review-data)))))

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
  (setq org-fc-review--session nil))

;;;###autoload
(defun org-fc-review-edit ()
  "Edit current flashcard.
Pauses the review, unnarrows the buffer and activates
`org-fc-edit-mode'."
  (interactive)
  (widen)
  (org-fc-remove-overlays)
  ;; Queue the current flashcard so it's reviewed a second time
  (push
   (oref org-fc-review--session current-item)
   (oref org-fc-review--session items))
  (setf (oref org-fc-review--session paused) t)
  (setf (oref org-fc-review--session current-item) nil)
  (org-fc-review-edit-mode 1))

;;; Managing Review Data

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

(defun org-fc-review-data-default (algorithm)
  "Default review data for ALGORITHM."
  (cl-case algorithm
    (sm2 (org-fc-algo-sm2-initial-review-data))
    (fsrs (org-fc-algo-fsrs-initial-review-data))))

(defun org-fc-review-data-headers (algorithm)
  "Default review data headers for ALGORITHM."
  (cl-case algorithm
    (sm2 '(position ease box interval due))
    (fsrs '(position last-review state difficulty stability reps lapses due))))

(defun org-fc-review-data-update (positions)
  "Update review data to POSITIONS.
If a doesn't exist already, it is initialized with default
values.  Entries in the table not contained in POSITIONS are
removed."
  (let* ((algorithm
          ;; For compatibility reasons, default to SM2 for cards that
          ;; don't have the algorithm property set
          (if-let ((algo (org-entry-get (point) org-fc-algo-property)))
              (intern algo)
            'sm2))
         (review-data
          (org-fc-review-data-parse
           (org-fc-review-data-headers algorithm))))
    (org-fc-review-data-ensure-rows
     review-data
     positions
     (org-fc-review-data-default algorithm))
    (org-fc-review-data-write
     review-data)))

;;; Sessions

(defclass org-fc-review-session ()
  ((current-item :initform nil)
   (paused :initform nil :initarg :paused)
   (history :initform nil)
   (ratings :initform nil :initarg :ratings)
   (items :initform nil :initarg :items)))

(cl-defmethod org-fc-review-pop-item ((session org-fc-review-session))
  (when-let ((item (pop (oref session items))))
    (setf (oref session current-item) item)
    item))

(cl-defmethod org-fc-review-prepend-items ((session org-fc-review-session) items)
  (setf (oref session items) (append items (oref session items))))

(cl-defmethod org-fc-review-item ((item org-fc-position) resuming)
  (let* ((card (oref item card))
         (path (oref (oref card file) path))
         (id (oref card id))
         (type (oref card type))
         (name (oref item name)))
  (message "review item")
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

        (setq org-fc-review--timestamp (time-to-seconds (current-time)))
        (let ((step (funcall (org-fc-type-setup-fn type) name)))
          (run-hooks 'org-fc-after-setup-hook)

          ;; If the card has a no-noop flip function,
          ;; skip to rate-mode
          (let ((flip-fn (org-fc-type-flip-fn type)))
            (if (or
                 (eq step 'rate)
                 (null flip-fn)
                 (eq flip-fn #'org-fc-noop))
                (org-fc-review-rate-mode 1)
              (org-fc-review-flip-mode 1))))))))
(defun org-fc-make-review-session (items)
  "Create a new review session with ITEMS."
  (org-fc-review-session
   :ratings
   (if-let ((stats (org-fc-awk-stats-reviews)))
       (plist-get stats :day)
     '(:total 0 :again 0 :hard 0 :good 0 :easy 0))
   :items items))

(defun org-fc-review-history-add (elements)
  "Add ELEMENTS to review history."
  (append-to-file
   (format "%s\n" (mapconcat #'identity elements "\t"))
   nil
   org-fc-review-history-file))

(defun org-fc-review-add-rating (session rating)
  "Store RATING in the review history of SESSION."
  (with-slots (ratings) session
    (cl-case rating
      ('again (cl-incf (cl-getf ratings :again) 1))
      ('hard (cl-incf (cl-getf ratings :hard) 1))
      ('good (cl-incf (cl-getf ratings :good) 1))
      ('easy (cl-incf (cl-getf ratings :easy) 1)))
    (cl-incf (cl-getf ratings :total 1))))

;;; Header Line

(defvar org-fc-original-header-line-format nil
  "`header-line-format' before it was set by org-fc.")

(defun org-fc-set-header-line ()
  "Set the header-line for review."
  (let* ((remaining (1+ (length (oref org-fc-review--session items))))
         ;; TODO: Generalize to different items
         (card (oref (oref org-fc-review--session current-item) card))
         (title
          (unless (or org-fc-review-hide-title-in-header-line
                      (member "notitle" (oref card tags)))
            (oref (oref card file) title))))
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

;;; Modes

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
    (unless (and (derived-mode-p 'org-mode) org-fc-review--session)
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
    (unless (and (derived-mode-p 'org-mode) org-fc-review--session)
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
    (unless (and (derived-mode-p 'org-mode) org-fc-review--session)
      (org-fc-review-edit-mode -1))))

;;; Footer

(provide 'org-fc-review)

;;; org-fc-review.el ends here
