(require 'org-fc)
(require 'org-fc-test-helper)
(require 'ert)

(ert-deftest org-fc-test-review-data-get-set ()
  (let ((review-data
         (org-fc-review-data :headers '(foo bar))))

    ;; Initially empty
    (should (eq (org-fc-review-data-get-row review-data "a") nil))
    (should (eq (org-fc-review-data-get-row review-data "b") nil))

    ;; Setting one key
    (org-fc-review-data-set-row review-data "a" '(foo 1 bar 2))
    (let ((entry-a (org-fc-review-data-get-row review-data "a"))
          (entry-b (org-fc-review-data-get-row review-data "b")))
      (org-fc-test-check-structure '(foo 1 bar 2) entry-a)
      (should (eq entry-b nil)))

    ;; Setting a second key
    (org-fc-review-data-set-row review-data "b" '(foo 3 bar 4))
    (let ((entry-a (org-fc-review-data-get-row review-data "a"))
          (entry-b (org-fc-review-data-get-row review-data "b")))
      (org-fc-test-check-structure '(foo 1 bar 2) entry-a)
      (org-fc-test-check-structure '(foo 3 bar 4) entry-b))))

(ert-deftest org-fc-test-review-data-ensure ()
  (let ((review-data
         (org-fc-review-data :headers '(foo bar))))

    ;; Initially empty
    (should (eq (org-fc-review-data-get-row review-data "a") nil))
    (should (eq (org-fc-review-data-get-row review-data "b") nil))

    ;; Initializing defaults for some rows
    (org-fc-review-data-ensure-rows
     review-data '("a" "b") '(foo 1 bar 2))

    (let ((entry-a (org-fc-review-data-get-row review-data "a"))
          (entry-b (org-fc-review-data-get-row review-data "b")))
      (org-fc-test-check-structure '(foo 1 bar 2) entry-a)
      (org-fc-test-check-structure '(foo 1 bar 2) entry-b))

    ;; Change values of one entry
    (let ((entry-a (org-fc-review-data-get-row review-data "a")))
      (org-fc-review-data-set-row
       review-data "a"
       (plist-put entry-a 'foo 3)))

    (let ((entry-a (org-fc-review-data-get-row review-data "a"))
          (entry-b (org-fc-review-data-get-row review-data "b")))
      (org-fc-test-check-structure '(foo 3 bar 2) entry-a)
      (org-fc-test-check-structure '(foo 1 bar 2) entry-b))

    ;; Initializing new defaults for some different rows
    (org-fc-review-data-ensure-rows
     review-data '("a" "c") '(foo 5 bar 6))
    (let ((entry-a (org-fc-review-data-get-row review-data "a"))
          (entry-b (org-fc-review-data-get-row review-data "b"))
          (entry-c (org-fc-review-data-get-row review-data "c")))
      (org-fc-test-check-structure '(foo 3 bar 2) entry-a)
      (should (eq entry-b nil))
      (org-fc-test-check-structure '(foo 5 bar 6) entry-c))))

(ert-deftest org-fc-test-review-data-parse-write ()
  (let (review-data buffer-string1 buffer-string2 buffer-string3)
    ;; Use a temporary buffer to make sure we don't mess up a test
    ;; fixture, then parse the data of one heading and store the raw
    ;; text of the drawer.
    (with-temp-buffer
      (insert-file-contents
       (org-fc-test-fixture "index/review_data.org"))
      (goto-char (point-min))
      (org-mode)

      (search-forward "404557e5-ec07-4ee1-a000-3f0e8a94eaa0")
      (setq review-data
            (org-fc-review-data-parse '(position custom1 custom2 due)))

      (org-fc-test-check-structure
       '(custom1 "1.0" custom2 "3" due "2020-11-06T11:31:05Z")
       (org-fc-review-data-get-row review-data "front"))
      (org-fc-test-check-structure
       '(custom1 "2.0" custom2 "4" due "2020-11-06T11:31:10Z")
       (org-fc-review-data-get-row review-data "back"))

      (let ((pos (org-fc-review-data-position)))
        (setq buffer-string1
              (buffer-substring-no-properties
               (car pos)
               (cdr pos)))))

    ;; In a second temporary buffer, create a dummy heading,
    ;; write the review data and store the raw text of the drawer
    (with-temp-buffer
      (org-mode)
      (insert "* Heading\n")
      (org-fc-review-data-write review-data)

      (let ((pos (org-fc-review-data-position)))
        (setq buffer-string2
              (buffer-substring-no-properties
               (car pos)
               (cdr pos)))))

    ;; Now create the same review data from scratch, write it out and
    ;; store the raw text
    (let ((review-data
           (org-fc-review-data :headers '(position custom1 custom2 due))))

      (org-fc-review-data-set-row
       review-data "front"
       '(custom1 "1.0" custom2 "3" due "2020-11-06T11:31:05Z"))

      ;; Leave out the position part this time
      (org-fc-review-data-set-row
       review-data "back"
       '(custom1 "2.0" custom2 "4" due "2020-11-06T11:31:10Z"))

      (with-temp-buffer
        (org-mode)
        (insert "* Heading\n")
        (org-fc-review-data-write review-data)

        (let ((pos (org-fc-review-data-position)))
          (setq buffer-string3
                (buffer-substring-no-properties
                 (car pos)
                 (cdr pos))))))

    ;; Make sure parsing and writing didn't change the text
    (should (equal buffer-string1 buffer-string2))

    ;; Make sure creating and writing didn't change the text
    (should (equal buffer-string1 buffer-string3))))

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
