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
;; (require 'org-fc-algo-fsrs)

(setq org-fc-review-history-file (make-temp-file "org-fc-test" nil ".tsv"))
(setq org-fc-directories (list (file-name-concat org-fc-source-path "demo")))

(with-current-buffer (find-file (make-temp-file "org-fc-test" nil ".org"))
  (insert-file-contents (file-name-concat org-fc-source-path "demo/demo.org"))
  (org-mode)

  (org-fc-save-svg  (file-name-concat org-fc-source-path "images/file.svg"))

  (org-fc-type-cloze-init 'deletion)

  (save-buffer)

  (org-fc-review 'buffer)

  (org-fc-save-svg  (file-name-concat org-fc-source-path "images/review.svg"))

  (org-fc-review-quit)

  (org-fc-dashboard 'all)

  (org-fc-save-svg  (file-name-concat org-fc-source-path "images/dashboard.svg")))

(kill-emacs)
;;; Tests

;; (add-to-list 'load-path (file-name-concat org-fc-source-path "tests"))
;; (require 'org-fc-test-helper)
;; (require 'ert)
;; (require 'org-fc-algo-fsrs-test)

;; (ert)
