;;; org-fc-review.el --- Review mode for org-fc -*- lexical-binding: t; -*-

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
(require 'org-fc-review-data)
(require 'org-fc-scheduler)

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
    (let* ((index (org-fc-index context))
           (cards (org-fc-index-filter-due index))
	   (order
	    (or
	     (plist-get context :order)
	    (if org-fc-shuffle-positions 'shuffled 'ordered)))
	   (scheduler
	    (cl-case order
	      (ordered (org-fc-scheduler))
	      (shuffled (org-fc-scheduler-shuffled))
	      (t (error "Unknown review order %s" order)))))
      (if (null cards)
          (message "No cards due right now")
        (progn
	  (org-fc-scheduler-init scheduler cards)
          (setq org-fc-review--session
                (org-fc-make-review-session scheduler))
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

(cl-defmethod org-fc-review-item ((position org-fc-position) resuming)
  "Review logic for a POSITION of a card."
  (let* ((card (oref position card))
	 (path (oref (oref card file) path))
	 (id (oref card id))
	 (type (oref card type))
	 (name (oref position name))
	 (buffer (find-buffer-visiting path)))
    (with-current-buffer (find-file path)
      (unless resuming
	;; If buffer was already open, don't kill it after rating the card
	(if buffer
	    (setq-local org-fc-reviewing-existing-buffer t)
	  (setq-local org-fc-reviewing-existing-buffer nil))
	(org-fc-set-header-line))

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
	    (org-fc-review-flip-mode 1)))))))

(defun org-fc-review-next-card (&optional resuming)
  "Review the next card of the current session.
If RESUMING is non-nil, some parts of the buffer setup are skipped."
  (if-let ((pos
	    (org-fc-scheduler-next-position
	     (oref org-fc-review--session scheduler))))
      (condition-case err
	  (progn
	    (setf (oref org-fc-review--session current-item) pos)
	    (org-fc-review-item pos resuming))
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
  `(when org-fc-review--session
       (if-let ((,var (oref org-fc-review--session current-item)))
           (if (string= (oref (oref ,var card) id) (org-id-get))
               (progn ,@body)
             (message "Flashcard ID mismatch"))
         (message "No flashcard review is in progress"))))

(defun org-fc-review-flip ()
  "Flip the current flashcard."
  (interactive)
  (condition-case err
      (org-fc-review-with-current-item pos
        (let ((type (oref (oref pos card) type)))
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
      (org-fc-review-with-current-item pos
	(let* ((now (time-to-seconds (current-time)))
	       (delta (- now org-fc-review--timestamp)))

	  (org-fc-review-update-data pos rating delta)

	  (when (and (eq rating 'again) org-fc-append-failed-cards)
	    (org-fc-scheduler-push-position
	     (oref org-fc-review--session scheduler)
	     pos))

	  (save-buffer)
	  (if org-fc-reviewing-existing-buffer
	      (org-fc-review-reset)
	    (kill-buffer))
	  (org-fc-review-next-card)))
    (error
     (org-fc-review-quit)
     (signal (car err) (cdr err)))))

(defun org-fc-review-rate-again ()
  "Rate the card at point with `again'."
  (interactive)
  (org-fc-review-rate 'again))

(defun org-fc-review-rate-hard ()
  "Rate the card at point with `hard'."
  (interactive)
  (org-fc-review-rate 'hard))

(defun org-fc-review-rate-good ()
  "Rate the card at point with `good'."
  (interactive)
  (org-fc-review-rate 'good))

(defun org-fc-review-rate-easy ()
  "Rate the card at point with `easy'."
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
  (org-fc-review-with-current-item pos
    (org-fc-scheduler-remove-siblings
     (oref org-fc-review--session scheduler) pos))
  (org-fc-review-reset)
  (org-fc-review-next-card))

(defun org-fc-review-update-data (position rating delta)
  "Use the card's spacing algorithm to update the review data of a
POSITION.
RATING is a review rating and DELTA the time in seconds between
showing and rating the card."
  (org-fc-with-point-at-entry
   (let ((algo (oref (oref position card) algo)))
     (org-fc-algo-update-review-data algo position rating delta))))

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
  (org-fc-scheduler-push-position
   (oref org-fc-review--session scheduler)
   (oref org-fc-review--session current-item))
  (setf (oref org-fc-review--session paused) t)
  (setf (oref org-fc-review--session current-item) nil)
  (org-fc-review-edit-mode 1))

;;; Sessions

(defclass org-fc-review-session ()
  ((current-item :initform nil)
   (paused :initform nil :initarg :paused)
   (scheduler
    :initform (org-fc-scheduler)
    :initarg :scheduler)))

(defun org-fc-make-review-session (scheduler)
  "Create a new review session with SCHEDULER."
  (org-fc-review-session
   :scheduler scheduler))

;;; Header Line

(defvar org-fc-original-header-line-format nil
  "`header-line-format' before it was set by org-fc.")

(defun org-fc-set-header-line ()
  "Set the header-line for review."
  (let* ((remaining (1+ (length
			 (oref
			  (oref org-fc-review--session scheduler)
			  positions))))
         (current (oref org-fc-review--session current-item))
         (title
          (unless (or org-fc-review-hide-title-in-header-line
                      (member "notitle" (oref (oref current card) tags)))
            (oref (oref (oref current card) file) title))))
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
