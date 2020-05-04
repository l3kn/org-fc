(require 'org-fc)
(require 'org-fc-test-helper)
(require 'ert)

(ert-deftest org-fc-test-malformed ()
  (should (null (org-fc-awk-index-paths
                 (list (org-fc-test-fixture "malformed/no_review_data.org")))))
  (should (null (org-fc-awk-index-paths
                 (list (org-fc-test-fixture "malformed/no_properties.org")))))
  (should (null (org-fc-awk-index-paths
                 (list (org-fc-test-fixture "malformed/normal_swapped_drawers.org")))))
  (should (null (org-fc-awk-index-paths
                 (list (org-fc-test-fixture "malformed/unclosed_drawer1.org")))))
  (should (null (org-fc-awk-index-paths
                 (list (org-fc-test-fixture "malformed/unclosed_drawer2.org"))))))
