(require 'org-fc)
(require 'org-fc-test-helper)
(require 'ert)

(ert-deftest org-fc-test-index-malformed ()
  (let ((files
	 (mapcar
	  #'org-fc-test-fixture
	  '("malformed/no_review_data.org"
	    "malformed/no_properties.org"
	    "malformed/normal_swapped_drawers.org"
	    "malformed/unclosed_drawer1.org"
	    "malformed/unclosed_drawer2.org"))))
    (dolist (file files)
      (org-fc-test-check-structure '()
       (org-fc-awk-index (list file))))))

(ert-deftest org-fc-test-escaping ()
  (org-fc-test-check-structure
   '((:cards ((:id "33645f3a-384d-44ed-aed2-a2d56b973800"))))
   (org-fc-awk-index
    (list (org-fc-test-fixture "escaping/spaces in filename.org")))))

(ert-deftest org-fc-test-index-keywords ()
  (org-fc-test-check-structure
   '((:title "File Title Uppercase"
	     :cards ((:tags ("tag1" "tag2" "fc"))))
     (:title "File Title Lowercase"
	     :cards ((:tags ("tag3" "tag4" "fc")))))
   (org-fc-awk-index
    (list (org-fc-test-fixture "index/uppercase.org")
          (org-fc-test-fixture "index/lowercase.org")))))

(ert-deftest org-fc-test-index ()
  (let ((algo-sm2 (org-fc-algo-sm2))
	(algo-noop (org-fc-algo-noop)))
    (org-fc-test-check-structure
     `((:cards
	((:id "edee8940-5c9a-4c70-b1c4-f45c194c0c97"
	      :tags ("fc" "tag1")
	      :title "Headline"
	      :algo ,algo-sm2)
	 (:id "59b3b102-aebd-44ba-a1fd-6dc912c34fcf"
	      :tags ("fc" "tag2")
	      :title "Headline 2"
	      :algo ,algo-sm2)
	 (:id "a7ed2686-73e6-4780-825d-78cf4b2e5374"
	      :tags ("fc" "tag3")
	      :title "Headline 3:not_a_tag:"
	      :algo ,algo-noop))))
     (org-fc-awk-index (list (org-fc-test-fixture "index/test.org"))))))
