(require 'org-fc)
(require 'org-fc-test-helper)
(require 'ert)

(ert-deftest org-fc-test-review-data ()
  (let ((index (org-fc-awk-index-paths
                (list
                 (org-fc-test-fixture "index/review_data.org")))))
    (should (eq (length index) 2))
    (let ((card1 (car index))
          (card2 (cadr index)))

      (should
       (equal (plist-get card1 :id)
              "f8cc05c7-aa3a-4a21-aa71-38178477e619"))
      (should
       (eq (length (plist-get card1 :positions)) 2))

      (let* ((poss (plist-get card1 :positions))
             (pos1 (car poss))
             (pos2 (cadr poss)))

        (should (equal (plist-get pos1 :position) "front"))
        (should (equal (plist-get pos1 :ease) 2.5))
        (should (equal (plist-get pos1 :box) 0))
        (should (equal (plist-get pos1 :interval) 0))
        (should (equal (plist-get pos1 :due) '(24485 10257)))

        (should (equal (plist-get pos2 :position) "back"))
        (should (equal (plist-get pos2 :ease) 2.8))
        (should (equal (plist-get pos2 :box) 2))
        (should (equal (plist-get pos2 :interval) 123.4))
        (should (equal (plist-get pos2 :due) '(24485 10260))))

      (should
       (equal (plist-get card2 :id)
              "404557e5-ec07-4ee1-a000-3f0e8a94eaa0"))
      (should
       (eq (length (plist-get card2 :positions)) 2))

      (let* ((poss (plist-get card2 :positions))
             (pos1 (car poss))
             (pos2 (cadr poss)))

        (should (equal (plist-get pos1 :position) "front"))
        (should (equal (plist-get pos1 :due) '(24485 13305)))
        (should (equal (plist-get pos1 :custom1) 1.0))
        (should (equal (plist-get pos1 :custom2) 3))

        (should (equal (plist-get pos2 :position) "back"))
        (should (equal (plist-get pos2 :due) '(24485 13310)))
        (should (equal (plist-get pos2 :custom1) 2.0))
        (should (equal (plist-get pos2 :custom2) 4))))))
