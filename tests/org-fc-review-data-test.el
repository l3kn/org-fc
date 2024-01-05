(require 'org-fc)
(require 'org-fc-test-helper)
(require 'ert)

(ert-deftest org-fc-test-review-data ()
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
   (org-fc-awk-index
    (list
     (org-fc-test-fixture "index/review_data.org")))))
