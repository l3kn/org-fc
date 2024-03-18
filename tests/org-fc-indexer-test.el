(require 'org-fc)
(require 'org-fc-cache)
(require 'org-fc-test-helper)
(require 'ert)

;; TODO: Ideally we would test indexing multiple files at once but
;; currently there's no way to ignore the order of the results.
(setq
 org-fc-test-index-expectations
 (let ((algo-sm2 (org-fc-algo-sm2))
       (algo-noop (org-fc-algo-noop)))
   `((("malformed/no_review_data.org") . ())
     ;; (("malformed/no_properties.org") . ())
     (("malformed/normal_swapped_drawers.org") . ())
     (("malformed/unclosed_drawer1.org") . ())
     (("malformed/unclosed_drawer2.org") . ())
     (("escaping/spaces in filename.org")
      .
      ((:cards ((:id "33645f3a-384d-44ed-aed2-a2d56b973800")))))
     (("index/uppercase.org")
      .
      ((:title "File Title Uppercase"
	       :cards ((:tags ("tag1" "tag2" "fc"))))))
     (("index/lowercase.org")
      .
      ((:title "File Title Lowercase"
	       :cards ((:tags ("tag3" "tag4" "fc"))))))
     (("index/test.org")
      .
      ((:cards
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
	      :algo ,algo-noop))))))))

(ert-deftest org-fc-test-awk-index ()
  (dolist (expectation org-fc-test-index-expectations)
    (let ((files
	   (mapcar #'org-fc-test-fixture (car expectation))))
      (org-fc-test-check-structure
       (cdr expectation)
       (org-fc-awk-index files)))))

(ert-deftest org-fc-test-cache-index ()
  (dolist (expectation org-fc-test-index-expectations)
    (let* ((files
	    (mapcar #'org-fc-test-fixture (car expectation)))
	   (org-fc-directories files)
	   (org-fc-test-cache (symbol-value 'org-fc-cache)))
      (setq org-fc-cache (make-hash-table :test #'equal))
      (org-fc-cache-build)
      (org-fc-cache-update)
      (org-fc-test-check-structure
       (cdr expectation)
       (org-fc-cache-index files))
      (setq org-fc-cache org-fc-test-cache))))

(ert-deftest org-fc-test-cache-hashes-empty ()
  (should (equal (hash-table-keys (org-fc-cache-hashes '())) '())))

(ert-deftest org-fc-test-cache-hashes-index ()
  (let ((hashes
	 (org-fc-cache-hashes (list (org-fc-test-fixture "index"))))
	(expected
	 '(("index/review_data.org" . "eb36df0667b132c50e0e9536371a57c79260ca3b")
	   ("index/test.org" . "01e1017d7de5dc08b740f43cbb3e42d1477357dd")
	   ("index/uppercase.org" . "41fea85ec0451c7e68510f9e60164cb42117ccba")
	   ("index/lowercase.org" . "7d15fe8692cc585f2fd4fb7cd6e947d939cc781c"))))
    (should (equal (hash-table-count hashes) 4))
    (dolist (expectation expected)
      (should (equal (gethash (org-fc-test-fixture (car expectation)) hashes)
		     (cdr expectation))))))
