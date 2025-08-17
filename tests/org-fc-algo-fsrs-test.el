(require 'org-fc)
(require 'org-fc-test-helper)
(require 'org-fc-algo-fsrs)
(require 'ert)

;;; Data

(defvar org-fc-algo-fsrs-test--data
  "Reference data, with cards in both plist and alist format,
together with the time of review and the review rating."
  '(
    (
     (state 1 step 0 stability nil difficulty nil due "2025-08-17T14:09:05Z" last-review nil)
     ((state . 1) (step . 0) (stability . nil) (difficulty . nil) (due . "2025-08-17T14:09:05Z") (last-review . nil))
     "2000-01-16T12:34:56Z"
     "good")
    (
     (state 1 step 1 stability 3.2602 difficulty 4.884631634813845 due "2000-01-16T12:44:56Z" last-review "2000-01-16T12:34:56Z")
     ((state . 1) (step . 1) (stability . 3.2602) (difficulty . 4.884631634813845) (due . "2000-01-16T12:44:56Z") (last-review . "2000-01-16T12:34:56Z"))
     "2000-02-02T12:34:56Z"
     "again")
    (
     (state 1 step 0 stability 1.6210824546714064 difficulty 7.234918643089044 due "2000-02-02T12:35:56Z" last-review "2000-02-02T12:34:56Z")
     ((state . 1) (step . 0) (stability . 1.6210824546714064) (difficulty . 7.234918643089044) (due . "2000-02-02T12:35:56Z") (last-review . "2000-02-02T12:34:56Z"))
     "2000-02-27T12:34:56Z"
     "easy")
    (
     (state 2 step nil stability 40.3268038275737 difficulty 6.562430039939845 due "2000-04-07T12:34:56Z" last-review "2000-02-27T12:34:56Z")
     ((state . 2) (step . nil) (stability . 40.3268038275737) (difficulty . 6.562430039939845) (due . "2000-04-07T12:34:56Z") (last-review . "2000-02-27T12:34:56Z"))
     "2000-03-22T12:34:56Z"
     "again")
    (
     (state 3 step 0 stability 3.8339700142386626 difficulty 8.124829084009125 due "2000-03-22T12:44:56Z" last-review "2000-03-22T12:34:56Z")
     ((state . 3) (step . 0) (stability . 3.8339700142386626) (difficulty . 8.124829084009125) (due . "2000-03-22T12:44:56Z") (last-review . "2000-03-22T12:34:56Z"))
     "2000-04-17T12:34:56Z"
     "again")
    (
     (state 3 step 0 stability 1.822599970491401 difficulty 8.953531279667958 due "2000-04-17T12:44:56Z" last-review "2000-04-17T12:34:56Z")
     ((state . 3) (step . 0) (stability . 1.822599970491401) (difficulty . 8.953531279667958) (due . "2000-04-17T12:44:56Z") (last-review . "2000-04-17T12:34:56Z"))
     "2000-05-14T12:34:56Z"
     "hard")
    (
     (state 3 step 0 stability 3.503449399852394 difficulty 9.150979354914503 due "2000-05-14T12:49:56Z" last-review "2000-05-14T12:34:56Z")
     ((state . 3) (step . 0) (stability . 3.503449399852394) (difficulty . 9.150979354914503) (due . "2000-05-14T12:49:56Z") (last-review . "2000-05-14T12:34:56Z"))
     "2000-06-11T12:34:56Z"
     "easy")
    (
     (state 2 step nil stability 32.67117621872323 difficulty 8.908547057240733 due "2000-07-14T12:34:56Z" last-review "2000-06-11T12:34:56Z")
     ((state . 2) (step . nil) (stability . 32.67117621872323) (difficulty . 8.908547057240733) (due . "2000-07-14T12:34:56Z") (last-review . "2000-06-11T12:34:56Z"))
     "2000-07-10T12:34:56Z"
     "good")))

;;; CLI tests

(ert-deftest org-fc-algo-fsrs-test-initial ()
  (org-fc-test-with-overwrites
   (org-fc-test-overwrite-fun time-to-seconds (lambda () 0))
   (assert
    (equal (org-fc-algo-initial-review-data (org-fc-algo-fsrs6) "name")
           '(position "name" state 1 step 0 stability nil difficulty nil due "1970-01-01T00:00:00Z" last-review nil)))))

(ert-deftest org-fc-algo-fsrs-test-step-many ()
  (let ((org-fc-algo-fsrs6-desired-retention 0.9)
        (org-fc-algo-fsrs6-enable-fuzzing nil))
    (cl-loop
     for history on org-fc-algo-fsrs-test--data
     while (cadr history)
     do
     (cl-destructuring-bind (c1-plist c1-alist c1-date c1-rating) (car history)
       (cl-destructuring-bind (c2-plist c2-alist c2-date c2-rating) (cadr history)
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
  (ert-test-erts-file
   (org-fc-test-fixture "erts/card_rate_normal_fsrs6.erts")
   (lambda ()
     (let* ((file (org-fc-file :path "mock-path"))
            (card (org-fc-card :file file :id "mock-id" :algo (org-fc-algo-fsrs6)))
            (position (org-fc-position :card card :name "front"))
            (now (time-to-seconds (date-to-time "2000-01-01T12:34:56Z")))
            (org-fc-algo-fsrs6-desired-retention 0.9)
            (org-fc-algo-fsrs6-enable-fuzzing nil))
       (org-fc-test-with-overwrites
        (org-fc-test-overwrite-fun
         time-to-seconds
         (lambda () now))
        (org-fc-test-overwrite-fun
         org-fc-review-history-add
         (lambda (data) nil))

        (org-mode)
        (goto-char (point-min))
        (org-fc-review-update-data position 'good 0))))))
