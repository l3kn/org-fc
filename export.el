;; Don’t show the startup screen
(setq load-prefer-newer t)
(setq inhibit-startup-screen t)
;; (tool-bar-mode -1)
;; (menu-bar-mode -1)
;; (scroll-bar-mode -1)


;; NOTE: This is also defined in org-fc-core.el
(defvar org-fc-source-path
  (file-name-parent-directory
   (file-truename (or load-file-name (buffer-file-name)))))

(defun org-fc-save-svg (file)
  "Save the current frame as an svg image into FILE."
  (let ((frame (selected-frame)))
    (setq mode-line-format nil)
    (force-mode-line-update)
    (set-frame-size frame 600 400 'pixelwise)
    (set-window-fringes (selected-window) 0 0)
    (set-window-fringes (minibuffer-window) 0 0)
    (with-temp-file file
      (insert (x-export-frames (list frame) 'svg)))))

(add-to-list 'load-path org-fc-source-path)

(require 'org-fc)
(require 'org-fc-keymap-hint)
;; (require 'org-fc-algo-fsrs)

(let* ((tempdir (make-temp-file "org-fc-test" 'dir-flag))
       (tempfile (expand-file-name "test.org" tempdir))
       (org-fc-review-history-file (make-temp-file "org-fc-test" nil ".tsv"))
       (org-fc-directories (list tempdir))
       (org-fc-bury-siblings t))
  (with-current-buffer (find-file tempfile)
    (insert-file-contents (file-name-concat org-fc-source-path "demo/demo.org"))
    (org-mode)

    (message "")
    (org-fc-save-svg  (file-name-concat org-fc-source-path "images/create.svg"))

    (org-fc-type-cloze-init 'deletion)
    (save-buffer)

    (message "")
    (org-fc-save-svg  (file-name-concat org-fc-source-path "images/mark.svg"))

    (org-fc-review 'buffer)

    (org-fc-save-svg  (file-name-concat org-fc-source-path "images/review.svg"))

    (org-fc-review-flip)

    (org-fc-save-svg  (file-name-concat org-fc-source-path "images/review_flip.svg"))

    (org-fc-review-rate 'good)

    (org-fc-save-svg  (file-name-concat org-fc-source-path "images/review_rate.svg"))

    (org-fc-review-quit)

    (org-fc-dashboard 'all)

    (org-fc-save-svg  (file-name-concat org-fc-source-path "images/dashboard.svg"))))

;; Normal card type
(let* ((tempdir (make-temp-file "org-fc-test" 'dir-flag))
       (tempfile (expand-file-name "test.org" tempdir))
       (org-fc-review-history-file (make-temp-file "org-fc-test" nil ".tsv"))
       (org-fc-directories (list tempdir))
       (org-fc-bury-siblings t))
  (with-current-buffer (find-file tempfile)
    (insert-file-contents (file-name-concat org-fc-source-path "demo/card_type_normal.org"))
    (org-mode)

    (message "")
    (org-fc-save-svg  (file-name-concat org-fc-source-path "images/card_type_normal_create.svg"))

    (org-fc-type-normal-init)
    (save-buffer)

    (message "")
    (org-fc-save-svg  (file-name-concat org-fc-source-path "images/card_type_normal_init.svg"))

    (org-fc-review 'buffer)

    (org-fc-save-svg  (file-name-concat org-fc-source-path "images/card_type_normal_review.svg"))

    (org-fc-review-flip)

    (org-fc-save-svg  (file-name-concat org-fc-source-path "images/card_type_normal_review_flip.svg"))
    (org-fc-review-quit)
    ))

(let* ((tempdir (make-temp-file "org-fc-test" 'dir-flag))
       (tempfile (expand-file-name "test.org" tempdir))
       (org-fc-review-history-file (make-temp-file "org-fc-test" nil ".tsv"))
       (org-fc-directories (list tempdir)))
  (with-current-buffer (find-file tempfile)
    (insert-file-contents (file-name-concat org-fc-source-path "demo/card_type_double.org"))
    (org-mode)
    (outline-next-heading)

    (message "")
    (org-fc-save-svg  (file-name-concat org-fc-source-path "images/card_type_double_create.svg"))

    (org-fc-type-double-init)
    (save-buffer)

    (message "")
    (org-fc-save-svg  (file-name-concat org-fc-source-path "images/card_type_double_init.svg"))

    (org-fc-review 'buffer)

    (org-fc-save-svg  (file-name-concat org-fc-source-path "images/card_type_double_review.svg"))
    (org-fc-review-flip)
    (org-fc-save-svg  (file-name-concat org-fc-source-path "images/card_type_double_review_flip.svg"))
    (org-fc-review-rate 'good)
    (org-fc-save-svg  (file-name-concat org-fc-source-path "images/card_type_double_review2.svg"))
    (org-fc-review-flip)
    (org-fc-save-svg  (file-name-concat org-fc-source-path "images/card_type_double_review2_flip.svg"))
    (org-fc-review-quit)
    ))

(let* ((tempdir (make-temp-file "org-fc-test" 'dir-flag))
       (tempfile (expand-file-name "test.org" tempdir))
       (org-fc-review-history-file (make-temp-file "org-fc-test" nil ".tsv"))
       (org-fc-directories (list tempdir)))
  (with-current-buffer (find-file tempfile)
    (insert-file-contents (file-name-concat org-fc-source-path "demo/card_type_cloze_deletion.org"))
    (org-mode)
    (outline-next-heading)

    (message "")
    (org-fc-save-svg  (file-name-concat org-fc-source-path "images/card_type_cloze_create.svg"))

    (org-fc-type-cloze-init 'deletion)
    (save-buffer)

    (message "")
    (org-fc-save-svg  (file-name-concat org-fc-source-path "images/card_type_cloze_init.svg"))

    (org-fc-review 'buffer)

    (org-fc-save-svg  (file-name-concat org-fc-source-path "images/card_type_cloze_review.svg"))
    (org-fc-review-flip)
    (org-fc-save-svg  (file-name-concat org-fc-source-path "images/card_type_cloze_review_flip.svg"))
    (org-fc-review-quit)
    ))

(kill-emacs)
;;; Tests

;; (add-to-list 'load-path (file-name-concat org-fc-source-path "tests"))
;; (require 'org-fc-test-helper)
;; (require 'ert)
;; (require 'org-fc-algo-fsrs-test)

;; (ert)
