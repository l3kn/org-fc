
;;; Configuration

(defcustom org-fc-review-data-drawer "REVIEW_DATA"
  "Name of the drawer used to store review data."
  :type 'string
  :group 'org-fc)

;;; Session Management

(defclass org-fc-review-session ()
  ((current-item :initform nil)
   (ratings :initform nil)
   (cards :initform nil)))

(defun org-fc-session-cards-pending-p (session)
  (not (null (oref session cards))))

(defun org-fc-session-pop-next-card (session)
  (let ((card (pop (oref session cards))))
    (setf (oref session current-item) card)
    card))

(defun org-fc-session-add-rating (session rating)
  (push rating (oref session ratings)))

(defun org-fc-session-stats-string (session)
  (with-slots (ratings) session
    (let ((len (length ratings)))
      (if (plusp len)
          (format "%.2f again, %.2f hard, %.2f good, %.2f easy"
                  (/ (* 100.0 (count 'again ratings)) len)
                  (/ (* 100.0 (count 'hard ratings)) len)
                  (/ (* 100.0 (count 'good ratings)) len)
                  (/ (* 100.0 (count 'easy ratings)) len))
        "No ratings yet"))))

(defvar org-fc-review--current-session nil
  "Current review session.")

;;; Helper Functions

(defun org-fc-review-next-time (next-interval)
  "Generate an org-mode timestamp NEXT-INTERVAL days from now"
  (let ((seconds (* next-interval 60 60 24))
        (now (time-to-seconds)))
    (format-time-string
     org-fc-timestamp-format
     (seconds-to-time (+ now seconds))
     "UTC0")))

(defun org-fc-id-goto (id file)
  "File-scoped variant of `org-id-goto'."
  (let ((position (org-id-find-id-in-file id file)))
    (if position
        (goto-char (cdr position))
      (error "ID %s not found in %s" id file))))

;;; Reviewing Cards

(defun org-fc-review--context (context)
  (let* ((session (make-instance 'org-fc-review-session))
         (cards (org-fc-due-positions context)))
    (if org-fc-review--current-session
        (message "Flashcards are already being reviewed")
      (if (null cards)
          (message "No cards due right now")
        (progn
          (setq org-fc-review--current-session session)
          (setf (oref session cards) cards)
          (org-fc-review-next-card))))))

(defun org-fc-review-buffer ()
  (interactive)
  (org-fc-review--context 'buffer))

(defun org-fc-review-all ()
  (interactive)
  (org-fc-review--context 'all))

(defun org-fc-review-next-card ()
  "Review the next card of the current session"
  (if (org-fc-session-cards-pending-p org-fc-review--current-session)
      (let* ((card (org-fc-session-pop-next-card org-fc-review--current-session))
             (path (plist-get card :path))
             (id (plist-get card :id))
             (type (plist-get card :type))
             (position (plist-get card :position)))
        (let ((buffer (find-buffer-visiting path)))
          (with-current-buffer (find-file path)
            ;; If buffer was already open, don't kill it after rating the card
            (if buffer (setq-local org-fc-reviewing-existing-buffer t))
            (goto-char (point-min))
            (org-fc-show-all)
            (org-fc-id-goto id path)
            ;; Make sure the headline the card is in is expanded
            (org-reveal)
            (org-fc-narrow-tree)
            (org-fc-hide-drawers)
            (org-fc-show-latex)
            (funcall (org-fc-type-setup-fn type) position))))
    (progn
      (message "Review Done")
      (setq org-fc-review--current-session nil)
      (org-fc-show-all))))

(defhydra org-fc-review-rate-hydra ()
  "
%(length (oref org-fc-review--current-session cards)) cards remaining
%s(org-fc-session-stats-string org-fc-review--current-session)

"
  ("a" (org-fc-review-rate-card 'again) "Rate as again" :exit t)
  ("h" (org-fc-review-rate-card 'hard) "Rate as hard" :exit t)
  ("g" (org-fc-review-rate-card 'good) "Rate as good" :exit t)
  ("e" (org-fc-review-rate-card 'easy) "Rate as easy" :exit t)
  ("q" org-fc-review-quit "Quit" :exit t))

(defhydra org-fc-review-flip-hydra ()
  "
%(length (oref org-fc-review--current-session cards)) cards remaining
%s(org-fc-session-stats-string org-fc-review--current-session)

"
  ("RET" org-fc-review-flip "Flip" :exit t)
  ;; Neo-Layout ergonomics
  ("n" org-fc-review-flip "Flip" :exit t)
  ("q" org-fc-review-quit "Quit" :exit t))

(defmacro org-fc-review-with-current-item (var &rest body)
  "Helper macro for functions that work with the current item of
a review session."
  (declare (indent defun))
  `(if org-fc-review--current-session
      (-if-let (,var (oref org-fc-review--current-session current-item))
        (if (string= (plist-get ,var :id) (org-id-get))
            (progn ,@body)
          (message "Flashcard ID mismatch"))
    (message "No flashcard review is in progress"))))

(defun org-fc-review-flip ()
  "Flip the current flashcard"
  (interactive)
  (org-fc-review-with-current-item card
    (let ((type (plist-get card :type)))
      (funcall (org-fc-type-flip-fn type)))))

;; TODO: Remove -card suffix
(defun org-fc-review-rate-card (rating)
  "Rate the card at point if it has the same id as the current
  card of the review session."
  (interactive)
  (org-fc-review-with-current-item card
    (let ((path (plist-get card :path))
          (id (plist-get card :id))
          (position (plist-get card :position)))
      (org-fc-session-add-rating org-fc-review--current-session rating)
      (org-fc-review-update-data path id position rating)
      (save-buffer)
      ;; TODO: Conditional kill
      (unless org-fc-reviewing-existing-buffer
        (kill-buffer))
      (org-fc-review-next-card))))

(defun org-fc-review-update-data (path id position rating)
  (save-excursion
    (org-fc-goto-entry-heading)
    (let* ((data (org-fc-get-review-data))
           (current (assoc position data #'string=)))
      (unless current
        (error "No review data found for this position"))
      (let ((ease (string-to-number (second current)))
            (box (string-to-number (third current)))
            (interval (string-to-number (fourth current))))
        (org-fc-review-history-add
         (list
          (org-fc-timestamp-now)
          path
          id
          position
          (format "%.2f" ease)
          (format "%d" box)
          (format "%.2f" interval)
          (symbol-name rating)))
        (destructuring-bind (next-ease next-box next-interval)
            (org-fc-sm2-next-parameters ease box interval rating)
          (setcdr
           current
           (list (format "%.2f" next-ease)
                 (number-to-string next-box)
                 (format "%.2f" next-interval)
                 (org-fc-review-next-time next-interval)))
          (org-fc-set-review-data data))))))

(defun org-fc-review-quit ()
  "Quit the review, remove all overlays from the buffer."
  (interactive)
  (setq org-fc-review--current-session nil)
  (org-fc-show-all))

;;; Writing Review History

(defun org-fc-review-history-add (elements)
  "Add ELEMENTS to the history csv file."
  (unless (and (boundp 'org-fc-demo-mode) org-fc-demo-mode)
    (append-to-file
     (concat
      (mapconcat #'identity elements "\t")
      "\n")
     nil
     org-fc-review-history-file)))

;;; Reading / Writing Review Data

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
  (let ((position (org-fc-review-data-position nil)))
    (if position
        (save-excursion
          (goto-char (car position))
          (cddr (org-table-to-lisp))))))

(defun org-fc-set-review-data (data)
  (save-excursion
    (let ((position (org-fc-review-data-position t)))
      (kill-region (car position) (cdr position))
      (goto-char (car position))
      (insert "| position | ease | box | interval | due |\n")
      (insert "|-|-|-|-|-|\n")
      (loop for datum in data do
            (insert
             "| "
             (mapconcat (lambda (x) (format "%s" x)) datum " | ")
             " |\n"))
      (org-table-align))))

(defun org-fc-review-data-default (position)
  (list position org-fc-sm2-initial-ease 0 0
        (org-fc-timestamp-now)))

(defun org-fc-review-data-update (positions)
  "Update review data to POSITIONS.
If a doesn't exist already, it is initialized with default
values.  Entries in the table not contained in POSITIONS are
removed."
  (unless (and (boundp 'org-fc-demo-mode) org-fc-demo-mode)
      (let ((old-data (org-fc-get-review-data)))
        (org-fc-set-review-data
         (mapcar
          (lambda (pos)
            (or
             (assoc pos old-data #'string=)
             (org-fc-review-data-default pos)))
          positions)))))

;;; Exports

(provide 'org-fc-review)
