(defun org-fc-type-text-input-init ()
  (interactive)
  (org-fc--init-card "text-input")
  (org-fc-review-data-update '("front")))

(defun org-fc-type-text-input-review (_position)
  (org-show-subtree)
  (let ((answer (org-entry-get (point) "ANSWER"))
        (user-answer (read-string "Answer: ")))
    (goto-char (point-max))
    ;; Overlays need to be of at least size 1 to be visible
    (let ((ovl (make-overlay (- (point) 1) (point))))
      (overlay-put ovl 'category 'org-fc-additional-text-overlay)
      (overlay-put ovl 'priority 9999)
      (overlay-put ovl 'face 'default)
      (overlay-put ovl 'display
                   (concat "\n\n\nExpected: " answer
                           "\nGot:      " user-answer)))))

;; No-op
(defun org-fc-type-text-input-update ())

;; TODO: Implement real handler
(org-fc-register-type
 'text-input
 'org-fc-type-normal-setup
 'org-fc-type-normal-flip
 'org-fc-type-normal-update)

(provide 'org-fc-type-text-input)
