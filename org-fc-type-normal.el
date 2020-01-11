(defun org-fc-type-normal-init ()
  (interactive)
  (org-fc--init-card "normal")
  (org-fc-review-data-update '("front")))

(defvar org-fc-type-normal--hidden '())

(defun org-fc-type-normal-setup (_position)
  (interactive)
  (if (org-fc-has-back-heading-p)
      (progn
        (org-show-subtree)
        (setq org-fc-type-normal--hidden (org-fc-hide-subheading "Back"))))
  (org-fc-review-flip-hydra/body))

(defun org-fc-type-normal-flip ()
  (interactive)
  (save-excursion
    (org-show-subtree)
    (dolist (pos org-fc-type-normal--hidden)
      (goto-char pos)
      (org-show-subtree)))
  (org-fc-review-rate-hydra/body))

;; No-op
(defun org-fc-type-normal-update ())

(org-fc-register-type
 'normal
 'org-fc-type-normal-setup
 'org-fc-type-normal-flip
 'org-fc-type-normal-update)

(provide 'org-fc-type-normal)
