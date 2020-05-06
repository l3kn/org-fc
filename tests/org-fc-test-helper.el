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

(provide 'org-fc-test-helper)
