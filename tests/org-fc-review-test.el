(require 'cl-lib)
(require 'org-fc)
(require 'ert)

(cl-defun org-fc-review-test--make-card (&key (num-positions 1))
  "Create a test card with a number of NUM-POSITIONS."
  (let ((card (org-fc-card)))
    (oset card positions
          (cl-loop for i from 1 to num-positions
                   collect (org-fc-position :card card :name (format "pos%d" i))))
    card))

(ert-deftest org-fc-review--maybe-bury-siblings-enabled ()
  "Test `org-fc-review--maybe-bury-siblings' with `org-fc-bury-siblings' enabled."
  (let* ((cards (list (org-fc-review-test--make-card :num-positions 3)
                      (org-fc-review-test--make-card :num-positions 1)
                      (org-fc-review-test--make-card :num-positions 5)))
         (org-fc-bury-siblings t)
         (result (org-fc-review--maybe-bury-siblings cards)))
    (should (equal 3 (length result)))
    (dolist (card result)
      (should (= 1 (length (oref card positions))))
      (should (equal "pos1" (oref (nth 0 (oref card positions)) name))))))

(ert-deftest org-fc-review--maybe-bury-siblings-disabled ()
  "Test `org-fc-review--maybe-bury-siblings' with `org-fc-bury-siblings' disabled."
  (let* ((cards (list (org-fc-review-test--make-card :num-positions 3)
                      (org-fc-review-test--make-card :num-positions 1)
                      (org-fc-review-test--make-card :num-positions 5)))
         (org-fc-bury-siblings nil)
         (result (org-fc-review--maybe-bury-siblings cards)))
    (should (eq cards result))
    (should (equal 3 (length (oref (nth 0 result) positions))))
    (should (equal 1 (length (oref (nth 1 result) positions))))
    (should (equal 5 (length (oref (nth 2 result) positions))))))

(defun org-fc--make-cloze-test-card (cloze-type num-positions)
  "Create a mock `org-fc-card' for testing.
It has CLOZE-TYPE and NUM-POSITIONS positions."
  (let ((card (org-fc-card :type 'cloze :cloze-type cloze-type)))
    (oset card positions
          (cl-loop for i from 1 to num-positions
                   collect (org-fc-position :card card :name (format "pos%d" i))))
    card))

(ert-deftest org-fc-review--bury-siblings-for-cloze-single ()
  "Test burying siblings for `single' cloze type.
Should not bury any siblings."
  (let* ((card (org-fc--make-cloze-test-card 'single 3))
         (cards (list card))
         (result (org-fc-review--bury-siblings-for-deletion-and-context cards)))
    (should (= (length (oref (car result) positions)) 3))))

(ert-deftest org-fc-review--bury-siblings-for-cloze-enumeration ()
  "Test burying siblings for `enumeration' cloze type.
Should not bury any siblings."
  (let* ((card (org-fc--make-cloze-test-card 'enumeration 3))
         (cards (list card))
         (result (org-fc-review--bury-siblings-for-deletion-and-context cards)))
    (should (= (length (oref (car result) positions)) 3))))

(ert-deftest org-fc-review--bury-siblings-for-cloze-deletion ()
  "Test burying siblings for `deletion' cloze type.
Should bury all siblings, leaving one position."
  (let* ((card (org-fc--make-cloze-test-card 'deletion 3))
         (cards (list card))
         (result (org-fc-review--bury-siblings-for-deletion-and-context cards)))
    (should (= (length (oref (car result) positions)) 1))))

(ert-deftest org-fc-review--bury-siblings-for-cloze-context ()
  "Test burying siblings for `context' cloze type.
Should bury all siblings, leaving one position."
  (let* ((card (org-fc--make-cloze-test-card 'context 3))
         (cards (list card))
         (result (org-fc-review--bury-siblings-for-deletion-and-context cards)))
    (should (= (length (oref (car result) positions)) 1))))
