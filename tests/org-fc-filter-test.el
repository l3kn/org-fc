(require 'org-fc)
(require 'org-fc-test-helper)
(require 'ert)

(defvar
  org-fc-filter-test-ids
  '((a-normal . "9c5e1c79-e0a0-4cf9-9ddd-a2556da0d755")
    (a-double . "dc8340e2-8725-40b7-8e88-4c7fec938fc0")
    (b-normal1 . "b5686029-f41a-4ff8-a8ed-2629d2f47b81")
    (b-normal2 . "3abe7213-33fd-4b90-8a8b-e323682e19ee")
    (c-double . "d96b7d6a-c719-4696-aafc-da88b23e1dcb")
    (c-cloze . "54625832-2d72-46f6-8f26-862eba4e4651")))

(defun org-fc-test-compare-ids (index expected)
  (let ((ids1 (mapcar (lambda (card) (oref card id)) index))
        (ids2
         (mapcar
          (lambda (ex) (alist-get ex org-fc-filter-test-ids))
          expected)))
    (equal (sort ids1 #'string-lessp)
           (sort ids2 #'string-lessp))))

(ert-deftest org-fc-filter-test ()
  (let* ((paths (list (org-fc-test-fixture "filter/")))
	 (org-fc-index-function #'org-fc-awk-index))
    ;; Index of all cards
    (should (org-fc-test-compare-ids
	     (org-fc-index `(:paths ,paths))
             '(a-normal a-double b-normal1 b-normal2 c-double c-cloze)))

    ;; Filter by type
    (should
     (org-fc-test-compare-ids
      (org-fc-index `(:paths ,paths :filter (type double)))
      '(a-double c-double)))

    ;; Filter by type, or
    (should
     (org-fc-test-compare-ids
      (org-fc-index `(:paths ,paths :filter (or (type cloze) (type double))))
      '(a-double c-double c-cloze)))

    ;; Filter by tag, direct
    (should
     (org-fc-test-compare-ids
      (org-fc-index `(:paths ,paths :filter (tag "tag1")))
      '(a-normal a-double)))

    ;; Filter by tag, inherited
    (should
     (org-fc-test-compare-ids
      (org-fc-index `(:paths ,paths :filter (tag "tag2")))
      '(a-double b-normal1)))

    ;; Filter by tag, filetag
    (should
     (org-fc-test-compare-ids
      (org-fc-index `(:paths ,paths :filter (and (tag "file1")
						 (tag "file2")
						 (tag "file3"))))
      '(c-double c-cloze)))

    ;; Negation
    (should
     (org-fc-test-compare-ids
      (org-fc-index `(:paths ,paths :filter (not (type normal))))
      '(a-double c-double c-cloze)))

    ;; Combined
    (should
     (org-fc-test-compare-ids
      (org-fc-index `(:paths ,paths :filter (and (not (type normal))
						 (tag "file1"))))
      '(c-double c-cloze)))))
