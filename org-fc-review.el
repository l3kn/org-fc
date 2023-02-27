;;; org-fc-review.el --- Review mode for org-fc -*- lexical-binding: t; -*-

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
(require 'dash)

(require 'org-fc-awk)
(require 'org-fc-core)
(require 'org-fc-position)

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

(defcustom org-fc-review-new-limit -1
  "Limits the number of new positions shown per `org-fc-review-new-limit-schedule'.

-1 for unlimited."
  :type 'integer
  :group 'org-fc)

(defcustom org-fc-review-new-limit-schedule 'session
  "The schedule at which to limit the inclusion of new positions.

- `session': Each review session will include, at most, `org-fc-review-new-limit' new cards.
- `day': New cards will be limited to `org-fc-review-new-limit' across review sessions; resets at midnight."
  :type '(choice (const session)
                 (const day))
  :group 'org-fc)

(defvar org-fc-review-new-limit--new-seen-today -1
  "Remaining new cards for today's reviews.

Don't access directly! Use `org-fc-review-new-limit--get-remaining'.

Not persisted; resets when reloading Emacs!")

(defvar org-fc-review-new-limit--reset-day nil
  "The day number on which we should reset `org-fc-review-new-limit--new-seen-today'.

Not persisted; resets when reloading Emacs!")

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
           (cards (org-fc-index--to-cards index))
           (positions (org-fc-positions--filter-due
                       (org-fc-cards--to-positions cards)))
           (positions (if org-fc-shuffle-positions
                          (org-fc-shuffle positions)
                        positions))
           (positions (if (> org-fc-review-new-limit 0)
                          (let ((remaining-new (org-fc-review-new-limit--get-remaining)))
                            (cl-remove-if
                             (lambda (pos)
                               (cond
                                ((org-fc-position-new-p pos)
                                 (when (>= remaining-new 0)
                                   (cl-decf remaining-new))
                                 (< remaining-new 0))
                                (t
                                 nil)))
                             positions))
                        positions)))
      (if (null positions)
          (message "No positions due right now")
        (progn
          (setq org-fc-review--session
                (org-fc-review-session--create positions))
          (run-hooks 'org-fc-before-review-hook)
          (org-fc-review-next-position))))))

(defun org-fc-review-resume ()
  "Resume review session, if it was paused."
  (interactive)
  (if org-fc-review--session
      (progn
        (org-fc-review-edit-mode -1)
        (org-fc-review-next-position 'resuming))
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

(defun org-fc-review-next-position (&optional resuming)
  "Review the next card of the current session.
If RESUMING is non-nil, some parts of the buffer setup are skipped."
  (if (not (null (oref org-fc-review--session positions)))
      (condition-case err
          (let* ((pos (pop (oref org-fc-review--session positions)))
                 (card (oref pos card))
                 (path (oref card path))
                 (id (oref card id))
                 (type (oref card type)))
            (setf (oref org-fc-review--session current-item) pos)
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
                (let ((step (funcall (org-fc-type-setup-fn type) (oref pos pos))))
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
  `(if org-fc-review--session
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
        (let* ((card (oref pos card))
               (type (oref card type)))
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
        (let* ((card (oref pos card))
               (path (oref card path))
               (id (oref card id))
               (now (time-to-seconds (current-time)))
               (delta (- now org-fc-review--timestamp)))
          (when (org-fc-position-new-p pos)
            (cl-incf org-fc-review-new-limit--new-seen-today))
          (org-fc-review-add-rating org-fc-review--session rating)
          (org-fc-review-update-data path id pos rating delta)
          (org-fc-review-reset)

          (if (and (eq rating 'again) org-fc-append-failed-cards)
              (with-slots (positions) org-fc-review--session
                (setf positions (append positions (list pos)))))

          (save-buffer)
          (if org-fc-reviewing-existing-buffer
              (org-fc-review-reset)
            (kill-buffer))
          (org-fc-review-next-position)))
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
  (org-fc-review-next-position))

(defun org-fc-review-suspend-card ()
  "Suspend card and proceed to next."
  (interactive)
  (org-fc-suspend-card)
  ;; Remove all other positions from review session
  (with-slots (current-item positions) org-fc-review--session
    (let* ((card (oref current-item card))
           (id (oref card id)))
      (setf positions
            (cl-remove-if
             (lambda (card)
               (string= id (oref card id)))
             positions))))
  (org-fc-review-reset)
  (org-fc-review-next-position))

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
     (let* ((data (org-fc-review-data-get))
            (pos-pos (oref position pos))
            (current (assoc pos-pos
                            data
                            #'string=)))
       (unless current
         (error "No review data found for this position"))
       (let ((ease (oref position ease))
             (box (oref position box))
             (interval (oref position interval)))
         (org-fc-review-history-add
          (list
           (org-fc-timestamp-in 0)
           path
           id
           pos-pos
           (format "%.2f" ease)
           (format "%d" box)
           (format "%.2f" interval)
           (symbol-name rating)
           (format "%.2f" delta)
           (symbol-name org-fc-algorithm)))
         (cl-destructuring-bind (next-ease next-box next-interval)
             (org-fc-algo-sm2-next-parameters ease box interval rating)
           (setcdr
            current
            (list (format "%.2f" next-ease)
                  (number-to-string next-box)
                  (format "%.2f" next-interval)
                  (org-fc-timestamp-in next-interval)))
           (org-fc-review-data-set data)))))))

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
   (oref org-fc-review--session positions))
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

(defun org-fc-review-data-get ()
  "Get a cards review data as a Lisp object."
  (if-let ((position (org-fc-review-data-position)))
      (org-with-point-at (car position)
        (cddr (org-table-to-lisp)))))

(defun org-fc-review-data-set (data)
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
  (cl-case org-fc-algorithm
    ('sm2-v1 (org-fc-algo-sm2-initial-review-data position))
    ('sm2-v2 (org-fc-algo-sm2-initial-review-data position))))

(defun org-fc-review-data-update (positions)
  "Update review data to POSITIONS.
If a doesn't exist already, it is initialized with default
values.  Entries in the table not contained in POSITIONS are
removed."
  (let ((old-data (org-fc-review-data-get)))
    (org-fc-review-data-set
     (mapcar
      (lambda (pos)
        (or
         (assoc pos old-data #'string=)
         (org-fc-review-data-default pos)))
      positions))))

;;; Sessions

(defclass org-fc-review-session ()
  ((current-item
    :initform nil
    :initarg :current-item
    :documentation "The `org-fc-position' currently under review.")
   (paused
    :initform nil
    :initarg :paused
    :type boolean
    :documentation "t when the review session is paused; nil otherwise")
   (history
    :initform nil
    :initarg :paused)
   (ratings
    :initform nil
    :initarg :ratings
    :documentation "`org-fc-review-session-rating'."
    )
   (positions
    :initform nil
    :initarg :positions
    :documentation "List of `org-fc-position's.")))

(defclass org-fc-review-session-rating ()
  ((total
    :intiform 0
    :initarg :total
    :type integer)
   (again
    :intiform 0
    :initarg :again
    :type integer)
   (hard
    :intiform 0
    :initarg :hard
    :type integer)
   (good
    :intiform 0
    :initarg :good
    :type integer)
   (easy
    :intiform 0
    :initarg :easy
    :type integer)))

(cl-defmethod org-fc-awk-stats-reviews-as-rating (stats key)
  "Return the KEY of STATS as `org-fc-review-session-rating'."
  (cl-destructuring-bind
      (&key total again hard good easy &allow-other-keys)
      (pcase key
        ('all
         (plist-get stats :all))
        ('month
         (plist-get stats :month))
        ('week
         (plist-get stats :week))
        ('day
         (plist-get stats :day)))
    (org-fc-review-session-rating
     :total total
     :again again
     :hard hard
     :good good
     :easy easy)))

(defun org-fc-review-session--create (positions)
  "Create a new review session with POSITIONS."
  (let ((ratings (if-let ((stats (org-fc-awk-stats-reviews)))
                     (org-fc-awk-stats-reviews-as-rating stats 'day)
                   (org-fc-review-session-rating))))
    (org-fc-review-session
     :positions positions
     :ratings ratings)))

(defun org-fc-review-history-add (elements)
  "Add ELEMENTS to review history."
  (push
   elements
   (oref org-fc-review--session history)))

(defun org-fc-review-history-save ()
  "Save all history entries in the current session."
  (when (and org-fc-review--session (oref org-fc-review--session history))
    (append-to-file
     (concat
      (mapconcat
       (lambda (elements) (mapconcat #'identity elements "\t"))
       (reverse (oref org-fc-review--session history))
       "\n")
      "\n")
     nil
     org-fc-review-history-file)
    (setf (oref org-fc-review--session history) nil)))

;; Make sure the history is saved even if Emacs is killed
(add-hook 'kill-emacs-hook #'org-fc-review-history-save)

(defun org-fc-review-add-rating (session rating)
  "Store RATING in the review history of SESSION."
  (with-slots (ratings) session
    (cl-case rating
      ('again (cl-incf (oref ratings again) 1))
      ('hard (cl-incf (oref ratings hard) 1))
      ('good (cl-incf (oref ratings good) 1))
      ('easy (cl-incf (oref ratings easy) 1)))
    (cl-incf (oref ratings total) 1)))

;;; Header Line

(defvar org-fc-original-header-line-format nil
  "`header-line-format' before it was set by org-fc.")

(defun org-fc-set-header-line ()
  "Set the header-line for review."
  (let* ((remaining (1+ (length (oref org-fc-review--session positions))))
         (current-position (oref org-fc-review--session current-item))
         (current-card (oref current-position card))
         (title (unless (or org-fc-review-hide-title-in-header-line
                            (member "notitle" (oref current-card tags)))
                  (oref current-card filetitle))))
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


;;;; Daily limit

(defun org-fc-review-new-limit--get-remaining ()
  "Return the remaining new cards for the `org-fc-review-new-card-schedule'."
  (when (and org-fc-review-new-limit
             (> org-fc-review-new-limit 0))
    (cond
     ((eq 'session
          org-fc-review-new-limit-schedule)
      org-fc-review-new-limit)
     ((eq 'day
          org-fc-review-new-limit-schedule)
      (let ((current-day (time-to-days (current-time))))
        (when (or (not org-fc-review-new-limit--reset-day)
                  (= org-fc-review-new-limit--reset-day current-day))
          (setq org-fc-review-new-limit--reset-day (1+ current-day)
                org-fc-review-new-limit--new-seen-today 0))
        (- org-fc-review-new-limit
           org-fc-review-new-limit--new-seen-today))))))

;;; Footer

(provide 'org-fc-review)

;;; org-fc-review.el ends here
