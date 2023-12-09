;; Ratings: again, hard, good, easy
;; States: new, learning, relearning, review

(defcustom org-fc-algo-fsrs-history-file (expand-file-name "org-fc-reviews-fsrs.tsv" user-emacs-directory)
  "File to store review history in."
  :type 'string
  :group 'org-fc)

(defclass org-fc-algo-fsrs-parameters ()
  ((weights
    :initarg :weights
    :type vector
    :custom (vector float)
    :documentation "List of weights")
   (request-retention
    :initarg :request-retention
    :type float
    :documentation "Desired retention rate")
   (maximum-interval
    :initarg :maximum-interval
    :initform 36500
    :type (or number float)
    :documentation "Maximum spacing interval in days")))

(defclass org-fc-algo-fsrs-scheduler ()
  ((parameters
    :initarg :parameters
    :type org-fc-algo-fsrs-parameters
    :documentation "Algorithm parameters")))

(setq org-fc-algo-fsrs-default-parameters
      (org-fc-algo-fsrs-parameters
       :weights
       [0.4 0.6 2.4 5.8 4.93 0.94 0.86 0.01 1.49 0.14 0.94 2.18 0.05 0.34 1.26 0.29 2.61]
       :request-retention 0.90))

(setq org-fc-algo-fsrs-default-scheduler
      (org-fc-algo-fsrs-scheduler
       :parameters org-fc-algo-fsrs-default-parameters))

(defclass org-fc-algo-fsrs-scheduling-positions ()
  ((again
    :initarg :again
    :type org-fc-algo-fsrs-position)
   (hard
    :initarg :hard
    :type org-fc-algo-fsrs-position)
   (good
    :initarg :good
    :type org-fc-algo-fsrs-position)
   (easy
    :initarg :easy
    :type org-fc-algo-fsrs-position)))

(cl-defmethod org-fc-algo-fsrs-update-state ((s org-fc-algo-fsrs-scheduling-positions) state)
  (cl-case state
    (new
     (oset (oref s again) state 'learning)
     (oset (oref s hard)  state 'learning)
     (oset (oref s good)  state 'learning)
     (oset (oref s easy)  state 'review))
    ((learning relearning)
     (oset (oref s again) state state)
     (oset (oref s hard)  state state)
     (oset (oref s good)  state 'review)
     (oset (oref s easy)  state 'review))
    (review
     (oset (oref s again) state 'relearning)
     (oset (oref s hard)  state 'review)
     (oset (oref s good)  state 'review)
     (oset (oref s easy)  state 'review))))

(cl-defmethod org-fc-algo-fsrs-schedule
  ((s org-fc-algo-fsrs-scheduling-positions)
   now
   hard-interval
   good-interval
   easy-interval)
  (with-slots (again hard good easy) s
    (oset again due (org-fc-algo-fsrs--add-time now :minute 5))
    (oset hard due
          (if (> hard-interval 0)
              (org-fc-algo-fsrs--add-time now :day hard-interval)
              (org-fc-algo-fsrs--add-time now :minute 10)))
    (oset good due (org-fc-algo-fsrs--add-time now :day good-interval))
    (oset easy due (org-fc-algo-fsrs--add-time now :day easy-interval))))

(cl-defmethod org-fc-algo-fsrs-init-ds
  ((s org-fc-algo-fsrs-scheduling-positions)
   (scheduler org-fc-algo-fsrs-scheduler))
  (dolist (rating '(again hard good easy))
    (oset (eieio-oref s rating) difficulty
          (org-fc-algo-fsrs-init-difficulty scheduler rating))
    (oset (eieio-oref s rating) stability
          (org-fc-algo-fsrs-init-stability  scheduler rating))))

(cl-defmethod org-fc-algo-fsrs-next-ds
  ((s org-fc-algo-fsrs-scheduling-positions)
   (scheduler org-fc-algo-fsrs-scheduler)
   last-d
   last-s
   retrievability)

  ;; Set parameters for failed review
  (oset (oref s again) difficulty
        (org-fc-algo-fsrs-next-difficulty scheduler last-d 'again))
  (oset (oref s again) stability
        (org-fc-algo-fsrs-next-forget-stability scheduler last-d last-s retrievability))

  ;; Set parameters for successful reviews
  (dolist (rating '(hard good easy))
    (oset (eieio-oref s rating) difficulty
          (org-fc-algo-fsrs-next-difficulty scheduler last-d rating))
    (oset (eieio-oref s rating) stability
          (org-fc-algo-fsrs-next-recall-stability scheduler last-d last-s retrievability rating))))

(defclass org-fc-algo-fsrs-position ()
  ((state
    :initarg :state
    :initform 'new
    :type symbol
    :documentation "new/learning/review/relearning"
    )
   (last-review
    :initarg :last-review
    :initform nil
    :type (or null list)
    ;; :custom (or null (repeat integer))
    ;; :documentation "Timestamp of last repetition"
    )
   (due
    :initarg :due
    :type list
    ;; :custom (repeat integer)
    ;; :documentation "Timestamp of the due date"
    )
   (stability
    :initarg :stability
    :initform 0
    ;; :type float
    ;; :documentation "Memory stability"
    )
   (difficulty
    :initarg :difficulty
    :initform 0
    ;; :type float
    ;; :documentation "Item difficulty"
    )
   (reps
    :initarg :reps
    :initform 0
    :type integer
    :documentation "Number of reviews"
    )
   (lapses
    :initarg :lapses
    :initform 0
    :type integer
    :documentation "Number failed reviews in `review' state"
    )
   ))

(defun org-fc-algo-fsrs--rating-to-number (rating)
  (cl-case rating
    (again 1)
    (hard  2)
    (good  3)
    (easy  4)))

(defun org-fc-algo-fsrs--add-time (now &rest args)
  (decoded-time-add now (apply #'make-decoded-time args)))

(cl-defmethod org-fc-algo-fsrs-rate-position ((scheduler org-fc-algo-fsrs-scheduler) (position org-fc-algo-fsrs-position) now)
  (let* ((elapsed-days
          (if (eq (oref position state) 'new)
              0
            (-
             (time-to-days (encode-time now))
             (time-to-days (encode-time (oref position last-review)))
             )))
         (reps (oref position reps))
         (scheduling-positions
          (org-fc-algo-fsrs-scheduling-positions
           :again (clone position :last-review now :reps (1+ reps))
           :hard  (clone position :last-review now :reps (1+ reps))
           :good  (clone position :last-review now :reps (1+ reps))
           :easy  (clone position :last-review now :reps (1+ reps)))))
    (org-fc-algo-fsrs-update-state scheduling-positions (oref position state))
    (cl-case (oref position state)
      (new
       (org-fc-algo-fsrs-init-ds scheduling-positions scheduler)
       (with-slots (again hard good easy) scheduling-positions
         (oset again due (org-fc-algo-fsrs--add-time now :minute 1))
         (oset hard  due (org-fc-algo-fsrs--add-time now :minute 5))
         (oset good  due (org-fc-algo-fsrs--add-time now :minute 10))
         (oset easy  due
               (org-fc-algo-fsrs--add-time
                now
                :day (org-fc-algo-fsrs-next-interval scheduler (oref easy stability))))))
      ((learning relearning)
       (let* ((hard-interval 0)
              (good-interval
               (org-fc-algo-fsrs-next-interval
                scheduler
                (oref (oref scheduling-positions good) stability)))
              (easy-interval
               (org-fc-algo-fsrs-next-interval
                scheduler
                (max
                 (oref (oref scheduling-positions easy) stability)
                 (+ 1 good-interval)))))
         (org-fc-algo-fsrs-schedule
          scheduling-positions
          now
          hard-interval
          good-interval
          easy-interval)))
      (review
       (let* ((last-d (oref position difficulty))
              (last-s (oref position stability))
              (retrievability (/ 1 (+ 1 (/ elapsed-days (* 9 last-s))))))
         (org-fc-algo-fsrs-next-ds
          scheduling-positions
          scheduler
          last-d
          last-s
          retrievability)
         ;; First compute preliminary intervals, then ensure their
         ;; duration increases the better the rating is.
         (let* ((hard-interval-pre
                 (org-fc-algo-fsrs-next-interval
                  scheduler
                  (oref (oref scheduling-positions hard) stability)))
                (good-interval-pre
                 (org-fc-algo-fsrs-next-interval
                  scheduler
                  (oref (oref scheduling-positions good) stability)))
                (easy-interval-pre
                 (org-fc-algo-fsrs-next-interval
                  scheduler
                  (oref (oref scheduling-positions easy) stability)))
                (hard-interval
                 (min hard-interval-pre good-interval-pre))
                (good-interval
                 (max good-interval-pre (+ 1 hard-interval)))
                (easy-interval
                 (max easy-interval-pre (+ 1 good-interval))))
           (org-fc-algo-fsrs-schedule
            scheduling-positions
            now
            hard-interval
            good-interval
            easy-interval)
           ;; Increment lapses of failed card in review state
           (let ((s-again (oref scheduling-positions again)))
             (oset
              s-again lapses
              (1+ (oref s-again lapses))))))))
    scheduling-positions))

(cl-defmethod org-fc-algo-fsrs-init-stability ((scheduler org-fc-algo-fsrs-scheduler) rating)
  (let ((parameters (oref scheduler parameters))
        (r (org-fc-algo-fsrs--rating-to-number rating)))
    (max
     (aref (oref parameters weights) (- r 1))
     0.1)))

(cl-defmethod org-fc-algo-fsrs-init-difficulty ((scheduler org-fc-algo-fsrs-scheduler) rating)
  (let* ((parameters (oref scheduler parameters))
         (weights (oref parameters weights))
         (r (org-fc-algo-fsrs--rating-to-number rating)))
    (min
     (max
      (-
       (aref weights 4)
       (* (aref weights 5) (- r 3)))
      1)
     10)))

(cl-defmethod org-fc-algo-fsrs-next-interval ((scheduler org-fc-algo-fsrs-scheduler) s)
  (let* ((parameters (oref scheduler parameters))
         (new-interval
          (* s 9 (- (/ 1 (oref parameters request-retention)) 1))))
    (min
     (max (round new-interval) 1)
     (oref parameters maximum-interval))))

(cl-defmethod org-fc-algo-fsrs-next-difficulty ((scheduler org-fc-algo-fsrs-scheduler) d rating)
  (let* ((r (org-fc-algo-fsrs--rating-to-number rating))
         (parameters (oref scheduler parameters))
         (weights (oref parameters weights))
         (next-d (- d (* (aref weights 6) (- r 3)))))
    (min
     (max
      (org-fc-algo-fsrs-mean-reversion scheduler (aref weights 4) next-d)
      1)
     10)))

(cl-defmethod org-fc-algo-fsrs-mean-reversion ((scheduler org-fc-algo-fsrs-scheduler) init current)
  (let* ((parameters (oref scheduler parameters))
         (weights (oref parameters weights)))
    (+
     (* (aref weights 7) init)
     (* (- 1 (aref weights 7)) current))))

(cl-defmethod org-fc-algo-fsrs-next-recall-stability ((scheduler org-fc-algo-fsrs-scheduler) d s retrievability rating)
  (let* ((parameters (oref scheduler parameters))
         (weights (oref parameters weights))
         (hard-penalty
          (if (eq rating 'hard) (aref weights 15) 1))
         (easy-bonus
          (if (eq rating 'easy) (aref weights 16) 1)))
    (* s
       (+ 1
          (* (exp (aref weights 8))
             (- 11 d)
             (expt s (- (aref weights 9)))
             (- (exp (* (- 1 retrievability) (aref weights 10))) 1)
             hard-penalty
             easy-bonus)))))

(cl-defmethod org-fc-algo-fsrs-next-forget-stability ((scheduler org-fc-algo-fsrs-scheduler) d s retrievability)
  (let* ((parameters (oref scheduler parameters))
         (weights (oref parameters weights)))
    (* (aref weights 11)
       (expt d (- (aref weights 12)))
       (- (expt (+ s 1) (aref weights 13)) 1)
       (exp (* (- 1 retrievability) (aref weights 14))))))

(defun org-fc-algo-fsrs-initial-review-data ()
  "Initial FSRS review data for any position."
  (list
   'last-review "nil"
   'state "new"
   'difficulty "0"
   'stability "0"
   'reps "0"
   'lapses "0"
   'due (org-fc-timestamp-in 0)))

(defun org-fc-algo-fsrs-next-review-data (old-data rating)
  (let* ((last-review
          (when (not (string= (plist-get old-data 'last-review) "nil"))
            (decode-time
             (parse-iso8601-time-string
              (plist-get old-data 'last-review))
             "UTC0")))
         (due
          (decode-time
           (parse-iso8601-time-string
            (plist-get old-data 'due))
           "UTC0"))

         (state (intern (plist-get old-data 'state)))
         (difficulty (string-to-number (plist-get old-data 'difficulty)))
         (stability (string-to-number (plist-get old-data 'stability)))
         (reps (string-to-number (plist-get old-data 'reps)))
         (lapses (string-to-number (plist-get old-data 'lapses)))

         (now (decode-time))

         (position
          (org-fc-algo-fsrs-position
           :state state
           :last-review last-review
           :due due
           :stability stability
           :difficulty difficulty
           :reps reps
           :lapses lapses))

         (sched (org-fc-algo-fsrs-rate-position
                 org-fc-algo-fsrs-default-scheduler
                 position
                 now))
         (new-position (eieio-oref sched rating)))

    (list
     'last-review
     (format-time-string "%FT%TZ" (encode-time now) "UTC0")
     'due
     (format-time-string "%FT%TZ" (encode-time (oref new-position due)) "UTC0")
     'state
     (symbol-name (oref new-position state))
     'stability
     (format "%.6f" (oref new-position stability))
     'difficulty
     (format "%.6f" (oref new-position difficulty))
     'reps
     (number-to-string (oref new-position reps))
     'lapses
     (number-to-string (oref new-position lapses)))))

(defun org-fc-algo-fsrs-history-add (position old-data rating delta)
  (let* ((card (oref position card))
         (path (oref (oref card file) path))
         (id (oref card id))
         (name (oref position name))

         (state (plist-get old-data 'state))

         (elements
          (list
           (org-fc-timestamp-in 0)
           path
           id
           name
           state
           (symbol-name rating)
           (format "%.2f" delta))))

    (append-to-file
     (format "%s\n" (mapconcat #'identity elements "\t"))
     nil
     org-fc-algo-fsrs-history-file)))

(provide 'org-fc-algo-fsrs)
