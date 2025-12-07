(require 'org-fc)
(require 'org-fc-test-helper)
(require 'ert)

(ert-deftest org-fc-test-card-init-normal ()
  (ert-test-erts-file
   (org-fc-test-fixture "erts/card_init_normal_sm2.erts")
   (lambda ()
     (org-fc-test-with-overwrites
      (org-fc-test-overwrite-fun
       org-fc-select-algo
       (lambda () "sm2"))
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

(ert-deftest org-fc-test-card-rate-normal ()
  (let ((org-fc-review-history-file (make-temp-file "org-fc-test" nil ".tsv")))
    (ert-test-erts-file
     (org-fc-test-fixture "erts/card_rate_normal_sm2.erts")
     (lambda ()
       (let* ((file (org-fc-file :path "mock-path"))
	            (card (org-fc-card :file file :id "mock-id" :algo (org-fc-algo-sm2)))
	            (position (org-fc-position :card card :name "front")))
         (org-fc-test-with-overwrites
	        (org-fc-test-overwrite-fun
	         time-to-seconds
	         (lambda () 0))

	        (org-mode)
	        (goto-char (point-min))
	        (org-fc-review-update-data position 'good 0)))))))

(ert-deftest org-fc-test-card-init-double ()
  (ert-test-erts-file
   (org-fc-test-fixture "erts/card_init_double_sm2.erts")
   (lambda ()
     (org-fc-test-with-overwrites
      (org-fc-test-overwrite-fun
       org-fc-select-algo
       (lambda () "sm2"))
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
      (org-fc-type-double-init)))))

(ert-deftest org-fc-test-card-rate-double ()
  (let ((org-fc-review-history-file (make-temp-file "org-fc-test" nil ".tsv")))
    (ert-test-erts-file
     (org-fc-test-fixture "erts/card_rate_double_sm2.erts")
     (lambda ()
       (let* ((file (org-fc-file :path "mock-path"))
              (card (org-fc-card :file file :id "mock-id" :algo (org-fc-algo-sm2)))
              (position (org-fc-position :card card :name "front")))
         (org-fc-test-with-overwrites
          (org-fc-test-overwrite-fun
           time-to-seconds
           (lambda () 0))

          (org-mode)
          (goto-char (point-min))
          (org-fc-review-update-data position 'easy 0)))))))

(ert-deftest org-fc-test-card-history ()
  (let* ((org-file (make-temp-file "org-fc-" nil ".org"))
	       (log-file (make-temp-file "org-fc-" nil ".tsv"))
	       (org-fc-directories (list org-file))
	       (org-fc-review-history-file log-file)
	       (org-fc-append-failed-cards nil)
	       (org-fc-review-card-filters (list))
	       (mock-id 0)
	       (mock-time 0)
	       (buffer-string ""))

    (org-fc-test-with-overwrites
     (org-fc-test-overwrite-fun
      time-to-seconds
      (lambda (&rest _args) (cl-incf mock-time 1)))
     (org-fc-test-overwrite-fun
      org-id-get
      (lambda (&rest _args)
	      (let ((cur-id (org-entry-get nil "ID"))
	            (new-id (format "dummy-id-%d" mock-id)))
	        (if cur-id
	            cur-id
	          (cl-incf mock-id 1)
	          (org-entry-put (point) "ID" new-id)
	          new-id))))

     (with-current-buffer (find-file-noselect org-file)
       (erase-buffer)
       (org-mode)

       (insert-file-contents
	      (org-fc-test-fixture "card/before_review.org"))

       (save-buffer)

       (org-fc-review '(:paths buffer :order ordered))
       (org-fc-review-flip)
       (org-fc-review-rate-easy)
       (org-fc-review-flip)
       (org-fc-review-rate-good)
       (org-fc-review-flip)
       (org-fc-review-rate-again)
       (setq
	      buffer-string
	      (buffer-substring-no-properties (point-min) (point-max)))
       (save-buffer)
       (kill-buffer)))

    (should (equal
	           buffer-string
	           (org-file-contents
	            (org-fc-test-fixture "card/after_review.org"))))
    (should (equal
	           (org-file-contents log-file)
	           (string-replace
	            "<path>"
	            org-file
	            (org-file-contents
	             (org-fc-test-fixture "card/after_review.tsv")))))))
