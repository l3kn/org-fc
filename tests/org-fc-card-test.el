(require 'org-fc)
(require 'org-fc-test-helper)
(require 'ert)

(ert-deftest org-fc-test-card-init-normal ()
  (ert-test-erts-file
   (org-fc-test-fixture "erts/card_init_normal.erts")
   (lambda ()
     (org-fc-test-with-overwrites
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
  (ert-test-erts-file
   (org-fc-test-fixture "erts/card_rate_normal.erts")
   (lambda ()
     (org-fc-test-with-overwrites
      (org-fc-test-overwrite-fun
       time-to-seconds
       (lambda () 0))
      (org-fc-test-overwrite-fun
       org-fc-review-history-add
       (lambda (data) nil))

      (org-mode)
      (goto-char (point-min))
      (org-fc-review-update-data "" "" "front" 'good 0)))))

(ert-deftest org-fc-test-card-init-double ()
  (ert-test-erts-file
   (org-fc-test-fixture "erts/card_init_double.erts")
   (lambda ()
     (org-fc-test-with-overwrites
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
  (ert-test-erts-file
   (org-fc-test-fixture "erts/card_rate_double.erts")
   (lambda ()
     (org-fc-test-with-overwrites
      (org-fc-test-overwrite-fun
       time-to-seconds
       (lambda () 0))
      (org-fc-test-overwrite-fun
       org-fc-review-history-add
       (lambda (data) nil))

      (org-mode)
      (goto-char (point-min))
      (org-fc-review-update-data "" "" "front" 'easy 0)))))
