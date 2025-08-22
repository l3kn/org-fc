(require 'org-fc)
(require 'org-fc-test-helper)
(require 'org-fc-algo-fsrs)
(require 'org-fc-algo-fsrs-test-reference)
(require 'ert)

;;; CLI tests

(ert-deftest org-fc-algo-fsrs-test-initial ()
  (org-fc-test-with-overwrites
   (org-fc-test-overwrite-fun time-to-seconds (lambda () 0))
   (assert
    (equal (org-fc-algo-initial-review-data (org-fc-algo-fsrs6) "name")
           '(position "name" state 1 step 0 stability nil difficulty nil due "1970-01-01T00:00:00Z" last-review nil)))))

;; Test the progression of the state of a single card via the CLI
(ert-deftest org-fc-algo-fsrs-test-step-many ()
  (let ((org-fc-algo-fsrs6-desired-retention 0.9)
        (org-fc-algo-fsrs6-enable-fuzzing nil))
    (cl-loop
     for history on org-fc-algo-fsrs-test--history-single
     while (cadr history)
     do
     (cl-destructuring-bind (c1-card-id c1-pos-name c1-plist c1-alist c1-date c1-rating) (car history)
       (cl-destructuring-bind (c2-card-id c2-pos-name c2-plist c2-alist c2-date c2-rating) (cadr history)
         (assert (equal (org-fc-algo-fsrs6--cli-get-next c1-alist c1-rating c1-date) c2-plist)))))))

;;; File-based tests

(ert-deftest org-fc-algo-fsrs6-test-card-init-normal ()
  (ert-test-erts-file
   (org-fc-test-fixture "erts/card_init_normal_fsrs6.erts")
   (lambda ()
     (org-fc-test-with-overwrites
      (org-fc-test-overwrite-fun
       org-fc-select-algo
       (lambda () "fsrs6"))
      (org-fc-test-overwrite-fun
       time-to-seconds
       (lambda () 0))
      (org-fc-test-overwrite-fun
       org-id-get
       (lambda (&rest _args)
	       (org-entry-put (point) "ID" "dummy-id")
	       "dummy-id"))
      (org-mode)
      (goto-char (point-min))
      (org-fc-type-normal-init)))))

(ert-deftest org-fc-algo-fsrs6-test-card-rate-normal ()
  (let ((org-fc-review-history-file (make-temp-file "org-fc-test" nil ".tsv")))
    (ert-test-erts-file
     (org-fc-test-fixture "erts/card_rate_normal_fsrs6.erts")
     (lambda ()
       (let* ((file (org-fc-file :path "mock-path"))
              (card (org-fc-card :file file :id "mock-id" :algo (org-fc-algo-fsrs6)))
              (position (org-fc-position :card card :name "front"))
              (now (time-to-seconds (date-to-time "2000-01-01T12:34:56Z")))
              (org-fc-algo-fsrs6-desired-retention 0.8)
              (org-fc-algo-fsrs6-enable-fuzzing nil))
         (org-fc-test-with-overwrites
          (org-fc-test-overwrite-fun time-to-seconds (lambda () now))
          (org-mode)
          (goto-char (point-min))
          (org-fc-review-update-data position 'good 0)))))

    (assert
     (equal
      (mapcar (lambda (l) (split-string l "\t")) (split-string (org-file-contents org-fc-review-history-file) "\n" 'omit-nulls))
      '(("2000-01-01T12:34:56Z" "mock-path" "mock-id" "front" "" "" "" "good" "0.00" "fsrs6")
        ("2000-01-01T12:34:56Z" "mock-path" "mock-id" "front" "" "" "" "good" "0.00" "fsrs6")
        ("2000-01-01T12:34:56Z" "mock-path" "mock-id" "front" "" "" "" "good" "0.00" "fsrs6")
        ("2000-01-01T12:34:56Z" "mock-path" "mock-id" "front" "" "" "" "good" "0.00" "fsrs6")
        ("2000-01-01T12:34:56Z" "mock-path" "mock-id" "front" "" "" "" "good" "0.00" "fsrs6"))))))

;;; E2E Tests

;; Create two fresh cards, one double and one normal
(defun org-fc-algo-fsrs6-test--create-mock-cards ()
  (insert "* card-id1")
  (org-set-property "ID" "id1")
  (org-fc-type-double-init)

  (goto-char (point-max))
  (insert "* card-id2")
  (org-set-property "ID" "id2")
  (org-fc-type-normal-init))

(defun org-fc-algo-fsrs6-test--replay-reviews ()
  (cl-loop
   for (card-id pos-name new-now rating) in reviews do
   ;; We need to create these manually because we can't index the temp file here
   (let* ((card (org-fc-card :file file :id card-id :algo (org-fc-algo-fsrs6)))
          (position (org-fc-position :card card :name pos-name)))
     (setq mock-now new-now)
     (goto-char (point-min))
     (search-forward (format "card-%s" card-id))
     (org-fc-review-update-data position (intern rating) 0))))

;; Currently setting up everything we need to simulate reviews at
;; different times requires a lot of effort, we do it once and run
;; multiple tests afterwards.
;;
;; 1. Test if the review history is written correctly
;; 2. Test if recomputing review data from the history gives the
;; expected result
(ert-deftest org-fc-algo-fsrs6-test-from-history ()
  (let* ((org-fc-review-history-file (make-temp-file "org-fc-test" nil ".tsv"))
         (org-fc-algo-fsrs6-enable-fuzzing nil)
         (org-fc-algo-fsrs6-desired-retention 0.9)
         (mock-now 0)
         (file (org-fc-file :path "mock-path"))
         ;; Make all calls to `time-to-seconds' now because we will overwrite it
         (reviews
          (cl-loop
           for (card-id pos-name plist alist date rating) in org-fc-algo-fsrs-test--history-multi
           collect (list card-id pos-name (time-to-seconds (date-to-time date)) rating))))

    (with-temp-buffer
      (org-fc-test-with-overwrites
       (org-fc-test-overwrite-fun time-to-seconds (lambda () mock-now))
       (org-fc-test-overwrite-fun org-fc-select-algo (lambda () "fsrs6"))

       (org-mode)
       (org-fc-algo-fsrs6-test--create-mock-cards)
       (org-fc-algo-fsrs6-test--replay-reviews reviews)))

    ;; Make sure the review history looks like we would expect
    (assert
     (equal
      (mapcar
       (lambda (l) (split-string l "\t"))
       (split-string (org-file-contents org-fc-review-history-file) "\n" 'omit-nulls))
      (mapcar
       (lambda (l) (list (nth 4 l) "mock-path" (nth 0 l) (nth 1 l) "" "" "" (nth 5 l) "0.00" "fsrs6"))
       org-fc-algo-fsrs-test--history-multi)))

    ;; Try to recompute the card state from the review history,
    ;; then compare it to the expected result
    (cl-loop
     for (card-id pos-name card-plist card-alist) in org-fc-algo-fsrs-test--results-multi do
     (let ((output (aref (org-fc-algo-fsrs6--cli-from-history card-id `(,pos-name)) 0)))
       ;; The CLI output includes an extra position entry not present in the data
       ;; so we need to do a bit of cleaning up before comparing
       (cl-remf output 'position)
       (should (equalp card-plist output))))))
