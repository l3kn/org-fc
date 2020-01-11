(require 'org-fc-review)
(require 'org-fc-awk)

;;; Configuration

(defcustom org-fc-dashboard-bar-chart-width 400
  "Width of the svg generated to display review statistics."
  :type 'integer
  :group 'org-fc)
(defcustom org-fc-dashboard-bar-chart-height 20
  "Height of the svg generated to display review statistics."
  :type 'integer
  :group 'org-fc)

(defcustom org-fc-dashboard-buffer-name "*org-fc Main*"
  "Name of the buffer to use for displaying the dashboard view."
  :type 'string
  :group 'org-fc)

;;; Bar-Chart Generation

(defun org-fc-dashboard-bar-chart (stat)
  "Generate a svg bar-chart for the plist STAT"
  (let* ((width org-fc-dashboard-bar-chart-width)
         (height org-fc-dashboard-bar-chart-height)
         (values
          `((,(or (plist-get stat :again) 0.0) . "red")
            (,(or (plist-get stat :hard) 0.0) . "yellow")
            (,(or (plist-get stat :good) 0.0) . "green")
            (,(or (plist-get stat :easy) 0.0) . "darkgreen")))
         (svg (svg-create width height)))
    (do ((values values (cdr values))
         (pos 0 (+ pos (* width (caar values)))))
        ((null values) '())
      (svg-rectangle svg pos 0 (* width (caar values)) height :fill (cdar values)))
    (svg-image svg)))

(defun org-fc-dashboard-percent-right (stats)
  (format "  %5.2f | %5.2f | %5.2f | %5.2f"
          (or (* 100 (plist-get stats :again)) 0.0)
          (or (* 100 (plist-get stats :hard)) 0.0)
          (or (* 100 (plist-get stats :good)) 0.0)
          (or (* 100 (plist-get stats :easy)) 0.0)))

;;; Main View

;; Based on `mu4e-main-view-real'
(defun org-fc-dashboard-view (_ignore-auto _noconfirm)
  (let* ((buf (get-buffer-create org-fc-dashboard-buffer-name))
         (inhibit-read-only t)
         (cards-stats (org-fc-awk-stats-cards))
         (positions-stats (org-fc-awk-stats-positions))
         (reviews-stats (org-fc-awk-stats-reviews)))
    (with-current-buffer buf
      (erase-buffer)
      (insert
       (propertize "Flashcards\n\n" 'face 'org-level-1))

      (insert
       (propertize "  Card Statistics\n\n" 'face 'org-level-1))

      (insert (format "    New: %d (day) %d (week) %d (month) \n"
                      (plist-get cards-stats :created-day)
                      (plist-get cards-stats :created-week)
                      (plist-get cards-stats :created-month)))

      (insert "\n")
      (insert (format
               "    %6d Cards, %d suspended\n"
               (plist-get cards-stats :total)
               (plist-get cards-stats :suspended)))
      (dolist (position '((:type-normal . "Normal")
                          (:type-double . "Double")
                          (:type-text-input . "Text Input")
                          (:type-cloze . "Cloze")))
        (insert
         (format "    %6d %s\n"
                 (plist-get cards-stats (car position))
                 (cdr position))))

      (insert "\n")
      (insert
       (propertize "  Position Statistics\n\n" 'face 'org-level-1))

      (insert (format "    %6d Due Now\n\n" (plist-get positions-stats :due)))

      (dolist (position '((:avg-ease . "Avg. Ease")
                          (:avg-box . "Avg. Box")
                          (:avg-interval . "Avg. Interval (days)")))
        (insert
         (format "    %6.2f %s\n"
                 (plist-get positions-stats (car position))
                 (cdr position))))

      (insert "\n")

      (insert
       (propertize "  Review Statistics\n\n" 'face 'org-level-1))

      (dolist (scope '((:day . "Day")
                       (:week . "Week")
                       (:month . "Month")
                       (:all . "All")))
        (when-let (stat (plist-get reviews-stats (car scope)))
          (when (plusp (plist-get stat :reviews))
            (insert (propertize (format "    %s (%d)\n" (cdr scope) (plist-get stat :reviews)) 'face 'org-level-1))
            (insert "    ")
            (insert-image (org-fc-dashboard-bar-chart stat))
            (insert (org-fc-dashboard-percent-right stat))
            (insert "\n\n"))))

      (insert "\n")
      (insert
       (propertize "  [r] Review\n" 'face 'org-level-1))
      (insert
       (propertize "  [q] Quit\n" 'face 'org-level-1)))))

(defvar org-fc-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'org-fc-review-all)
    (define-key map (kbd "q") 'quit-window)
    map))

(define-derived-mode org-fc-dashboard-mode special-mode "org-fc main"
  "Major mode providing an overview of the flashcard system"
  (set (make-local-variable 'revert-buffer-function) #'org-fc-dashboard-view)
  (setq-local cursor-type nil))

(defun org-fc-dashboard ()
  (interactive)
  (org-fc-dashboard-view nil nil)
  (switch-to-buffer org-fc-dashboard-buffer-name)
  (goto-char (point-min))
  (org-fc-dashboard-mode))

;;; Exports

(provide 'org-fc-dashboard)
