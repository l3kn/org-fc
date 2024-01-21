(require 'cl-lib)
(require 'org-fc)
(require 'org-fc-test-helper)
(require 'ert)
(require 'el-mock)

(ert-deftest org-fc-test-review-data-init ()
  (let* ((algo (org-fc-algo-sm2))
	 (review-data
	  (org-fc-review-data :headers (org-fc-algo-headers algo))))

    (should (equal (org-fc-review-data-names review-data) nil))

    (org-fc-review-data-ensure-rows review-data '("front" "back"))
    (should (equal (org-fc-review-data-names review-data) '("front" "back")))))

(ert-deftest org-fc-test-review-data-update ()
  (ert-test-erts-file
   (org-fc-test-fixture "erts/review_data_update.erts")
   (lambda ()
     (with-mock
       (mock (time-to-seconds) => 0)
       (org-mode)
       (org-fc-review-data-update '("front" "back"))))))

(ert-deftest org-fc-test-review-data-update-row ()
  (let ((review-data
	 (org-fc-review-data
	  :headers '(a b c)
	  ;; Note: The test will randomly fail if we use a quoted
	  ;; expression, probably because the same object is reused
	  ;; between tests. The main org-fc code never does this so we
	  ;; only need to be careful to work around it here.
	  :rows (list (cons "1" '(a 1 b 2 c 3))))))
    (should (equal (org-fc-review-data-get-row review-data "1")
		   '(a 1 b 2 c 3)))

    ;; Update of existing single column
    (org-fc-review-data-update-row review-data "1" '(a 4))
    (should (equal (org-fc-review-data-get-row review-data "1")
		   '(a 4 b 2 c 3)))

    ;; Update of non-existing column
    (org-fc-review-data-update-row review-data "1" '(x 4))
    (should (equal (org-fc-review-data-get-row review-data "1")
		   '(a 4 b 2 c 3)))

    ;; Update of multiple columns
    (org-fc-review-data-update-row review-data "1" '(a 8 b 9 c 10 d 11))
    (should (equal (org-fc-review-data-get-row review-data "1")
		   '(a 8 b 9 c 10)))))

(ert-deftest org-fc-test-review-data ()
  (let* ((result
	  (org-fc-awk-index
	   (list
	    (org-fc-test-fixture "index/review_data.org"))))
	 (file (car result))
	 (card1 (nth 0 (oref file cards)))
	 (card2 (nth 1 (oref file cards)))
	 (data1 (oref (nth 0 (oref card1 positions)) data))
	 (data2 (oref (nth 0 (oref card2 positions)) data)))
    (org-fc-test-check-structure
     '((cards ((id "f8cc05c7-aa3a-4a21-aa71-38178477e619"
		   positions
		   ((name "front"
			  due (24485 10257)
			  data (:ease 2.5 :box 0 :interval 0))
		    (name "back"
			  due (24485 10260)
			  data (:ease 2.8 :box 2 :interval 123.4))))

	       (id "404557e5-ec07-4ee1-a000-3f0e8a94eaa0"
		   positions
		   ((name "front"
			  due (24485 13305)
			  data (:custom1 1.0 :custom2 3))
		    (name "back"
			  due (24485 13310)
			  data (:custom1 2.0 :custom2 4)))))))
     result)

    (should
     (equal
      (cl-loop
       for pair on data1 by #'cddr collecting
       (car pair))
      '(:position :ease :box :interval :due)))))
