(require 'org-fc)
(require 'org-fc-test-helper)
(require 'org-fc-algo-fsrs)
(require 'ert)

(ert-deftest org-fc-algo-fsrs-test-initial ()
  (org-fc-test-with-overwrites
   (org-fc-test-overwrite-fun time-to-seconds (lambda () 0))
   (assert
    (equal (org-fc-algo-initial-review-data (org-fc-algo-fsrs6) "name")
           '(position "name" state 1 step 0 stability nil difficulty nil due "1970-01-01T00:00:00Z" last-review nil)))))

(ert-deftest org-fc-algo-fsrs-test-step ()
  (org-fc-test-with-overwrites
   (org-fc-test-overwrite-fun time-to-seconds (lambda () 0))
   (assert
    (equal
     (org-fc-algo-fsrs6--cli-get-next
      '((position . "name") (state . 1) (step . 0) (stability . nil) (difficulty . nil) (due . "1970-01-01T00:00:00Z") (last-review . nil))
      "good")
     '(state 1 step 1 stability 3.2602 difficulty 4.884631634813845 due "1970-01-01T00:10:00Z" last-review "1970-01-01T00:00:00Z")))))

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
	    (position (org-fc-position :card card :name "front")))
       (org-fc-test-with-overwrites
	(org-fc-test-overwrite-fun
	 time-to-seconds
	 (lambda () 0))
	(org-fc-test-overwrite-fun
	 org-fc-review-history-add
	 (lambda (data) nil))

	(org-mode)
	(goto-char (point-min))
	(org-fc-review-update-data position 'good 0))))))
