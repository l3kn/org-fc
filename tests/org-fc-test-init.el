;; Keep track of where the file was loaded from
(defvar org-fc-test-package-path
  (file-name-parent-directory
   (file-name-parent-directory
    (file-truename load-file-name))))

(defun org-fc-test-entrypoint ()
  (add-to-list 'load-path org-fc-test-package-path)
  (add-to-list 'load-path (expand-file-name "tests/" org-fc-test-package-path))
  (require 'org-fc)
  (require 'org-fc-test-helper)

  ;; TODO: Find better way to load the tests
  (load "tests/org-fc-indexer-test.el")
  (load "tests/org-fc-filter-test.el")
  (load "tests/org-fc-review-data-test.el")
  (load "tests/org-fc-review-test.el")
  (load "tests/org-fc-card-test.el")
  (load "tests/org-fc-algo-fsrs-test.el")
  (load "tests/org-fc-algo-fsrs-test-reference.el")
  (ert-run-tests-batch-and-exit))
