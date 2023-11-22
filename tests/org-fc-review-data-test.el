(require 'org-fc)
(require 'org-fc-test-helper)
(require 'ert)

(ert-deftest org-fc-test-review-data ()
  (let ((index
         (org-fc-index-flatten-file
          (org-fc-awk-index
           (list
            (org-fc-test-fixture "index/review_data.org"))))))
    (should (eq (length index) 2))
    (let ((card1 (car index))
          (card2 (cadr index)))

      (should
       (equal (oref card1 id)
              "f8cc05c7-aa3a-4a21-aa71-38178477e619"))
      (should
       (eq (length (oref card1 positions)) 2))

      (let* ((poss (oref card1 positions))
             (pos1 (car poss))
             (pos2 (cadr poss)))

        (should (equal (oref pos1 name) "front"))
        (should (equal (oref pos1 due) '(24485 10257)))
        (should (equal (plist-get (oref pos1 data) :ease) 2.5))
        (should (equal (plist-get (oref pos1 data) :box) 0))
        (should (equal (plist-get (oref pos1 data) :interval) 0))

        (should (equal (oref pos2 name) "back"))
        (should (equal (oref pos2 due) '(24485 10260)))

        (should (equal (plist-get (oref pos2 data) :ease) 2.8))
        (should (equal (plist-get (oref pos2 data) :box) 2))
        (should (equal (plist-get (oref pos2 data) :interval) 123.4)))

      (should
       (equal (oref card2 id)
              "404557e5-ec07-4ee1-a000-3f0e8a94eaa0"))
      (should
       (eq (length (oref card2 positions)) 2))

      (let* ((poss (oref card2 positions))
             (pos1 (car poss))
             (pos2 (cadr poss)))

        (should (equal (oref pos1 name) "front"))
        (should (equal (oref pos1 due) '(24485 13305)))

        (should (equal (plist-get (oref pos1 data) :custom1) 1.0))
        (should (equal (plist-get (oref pos1 data) :custom2) 3))

        (should (equal (oref pos2 name) "back"))
        (should (equal (oref pos2 due) '(24485 13310)))

        (should (equal (plist-get (oref pos2 data) :custom1) 2.0))
        (should (equal (plist-get (oref pos2 data) :custom2) 4))))))
