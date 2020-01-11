(defvar org-fc-type-double-hole-re
  (rx "{{" (group (* (not (any "}")))) "}}"))

(defvar org-fc-type-double--overlay '())

(defun org-fc-type-double-init ()
  (interactive)
  (org-fc--init-card "double")
  (org-fc-review-data-update '("front" "back")))

(defun org-fc-type-double-setup (position)
  (pcase position
    ("front" (org-fc-type-normal-setup position))
    ("back" (org-fc-type-double-setup-back))
    (_ (error "Invalid double position %s" position))))

(defun org-fc-type-double-setup-back ()
  (org-show-subtree)
  (if (org-fc-has-back-heading-p)
      (setq org-fc-type-double--overlay (org-fc-hide-content "[...]\n"))
      (setq org-fc-type-double--overlay (org-fc-hide-heading "[...]")))
  (org-fc-review-flip-hydra/body))

(defun org-fc-type-double-flip ()
  (message "double flip")
  (pp org-fc-type-double--overlay)
  (if org-fc-type-double--overlay
      (delete-overlay org-fc-type-double--overlay))
  (org-show-subtree)
  (org-fc-review-rate-hydra/body))

;; No-op
(defun org-fc-type-double-update ())

(org-fc-register-type
 'double
 'org-fc-type-double-setup
 'org-fc-type-double-flip
 'org-fc-type-double-update)

(provide 'org-fc-type-double)
