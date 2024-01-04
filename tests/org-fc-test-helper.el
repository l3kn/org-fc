(require 'org-fc-core)
(require 'cl-lib)
(require 'ert)

(defun org-fc-test-fixture (name)
  "Return the full path of fixture file NAME."
  (expand-file-name
   name
   (expand-file-name "tests/" org-fc-source-path)))

(defun org-fc-test-index-ids (index)
  "Return a list of IDs in INDEX."
  (mapcar
   (lambda (card) (plist-get card :id))
   index))

(defun org-fc-test-check-structure (expected got)
  "Check structural equality of parts of larger objects.
For plists, values of all keys in EXPECTED are compared,
lists are compared element-by-element,
everything else is checked for equality."
  (cond
   ;; plist
   ((and (listp expected)
         expected
         (symbolp (car expected)))
    (let ((keys
           (cl-loop for key in expected by #'cddr collecting key)))
      (dolist (key keys)
        (org-fc-test-check-structure
         (plist-get expected key)
         (plist-get got key)))))
   ;; Normal list
   ((listp expected)
    (should (eq (length expected) (length got)))
    (cl-loop for e in expected for g in got do
             (org-fc-test-check-structure e g)))
   ;; Anything else
   (t (should (equal expected got)))))

(provide 'org-fc-test-helper)
