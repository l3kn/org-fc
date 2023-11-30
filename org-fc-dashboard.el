;;; org-fc-dashboard.el --- Dashboard buffer for org-fc -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Leon Rische

;; Author: Leon Rische <emacs@leonrische.me>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The dashboard shows an overview over all cards in the system (or in
;; the current context.
;;
;;; Code:

(require 'svg)

(require 'org-fc-core)
(require 'org-fc-awk)
(require 'org-fc-review)

;;; Customization

(defcustom org-fc-dashboard-bar-chart-width 400
  "Width of the svg generated to display review statistics."
  :type 'integer
  :group 'org-fc)

(defcustom org-fc-dashboard-bar-chart-height 20
  "Height of the svg generated to display review statistics."
  :type 'integer
  :group 'org-fc)

(defcustom org-fc-dashboard-text-bar-chart-width 40
  "Width of the text-bar chart used to display review statistics.
This is only used org-fc is run in a non-graphic display
environment without svg support."
  :type 'integer
  :group 'org-fc)

(defcustom org-fc-dashboard-buffer-name "*org-fc Dashboard*"
  "Name of the buffer to use for displaying the dashboard view."
  :type 'string
  :group 'org-fc)

;;; Variables

(defvar org-fc-dashboard-context org-fc-context-all
  "Context of the current dashboard buffer.")

(defvar org-fc-dashboard-view nil
  "View of the current dashboard buffer.")

;;; Helper Functions

(defun org-fc-dashboard--hashtable-to-alist (ht)
  "Convert a hash-table HT to an alist."
  (let (res)
    (dolist (key (hash-table-keys ht))
      (push (cons key (gethash key ht)) res))
    res))

(defun org-fc-dashboard--hashtable-normalize (ht)
  "Convert a hash-table HT to a list of relative values."
  (let ((keys (sort (hash-table-keys ht) #'<))
        (sum (apply #'max (hash-table-values ht))))
    (mapcar (lambda (key)
              (cons key (/ (* 1.0 (gethash key ht)) sum)))
            keys)))

;;; Bar Chart Generation

(defun org-fc-dashboard-bar-chart (stat)
  "Generate a svg bar-chart for the plist STAT."
  (let* ((width org-fc-dashboard-bar-chart-width)
         (height org-fc-dashboard-bar-chart-height)
         (fontsize (floor (* height 0.6)))
         (total (float (plist-get stat :total)))
         (pos 0)
         (values
          `((,(/ (plist-get stat :again) total) . "red")
            (,(/ (plist-get stat :hard) total) . "yellow")
            (,(/ (plist-get stat :good) total) . "green")
            (,(/ (plist-get stat :easy) total) . "darkgreen")))
         (svg (svg-create width height)))
    (dolist (value values)
      (svg-rectangle svg pos 0 (* width (car value)) height :fill (cdr value))
      (if (> (* width (car value)) (* 2 fontsize))
          (svg-text svg (format "%.1f" (* 100 (car value)))
                    :font-size fontsize
                    :fill "white"
                    :font-weight "bold"
                    :font-family "sans-serif"
                    :x (+ pos 5)
                    :y (+ fontsize (floor (- height fontsize) 2))))
      (setq pos (+ pos (* width (car value)))))
    (svg-image svg :ascent 'center)))

(defun org-fc-dashboard-text-bar-chart (stat)
  "Generate a text bar-chart for the plist STAT."
  (cl-flet ((colored-bar (length color)
                         (propertize
                          (make-string length ?\s)
                          'font-lock-face `(:background ,color))))
    (let* ((width org-fc-dashboard-text-bar-chart-width)
           (total (float (plist-get stat :total)))
           (again (floor (* width (/ (plist-get stat :again) total))))
           (hard (floor (* width (/ (plist-get stat :hard) total))))
           (good (floor (* width (/ (plist-get stat :good) total))))
           ;; Make sure to use the total width
           (easy (- width again hard good)))
      (concat
       (colored-bar again "red")
       (colored-bar hard "yellow")
       (colored-bar good "green")
       (colored-bar easy "blue")))))

(defun org-fc-dashboard-bar (relative)
  (let* ((width org-fc-dashboard-bar-chart-width)
         (height org-fc-dashboard-bar-chart-height)
         (svg (svg-create width height)))
    (svg-rectangle svg 0 0 (* width relative) height :fill "darkblue")
    (svg-image svg :ascent 'center)))

(defun org-fc-dashboard-text-bar (relative)
  (let ((length (floor (* org-fc-dashboard-text-bar-chart-width relative))))
    (propertize
     (make-string length ?\s)
     'font-lock-face '(:background "blue"))))

;;; Sections

(defun org-fc-dashboard-insert-card-stats (cards)
  (let* ((total 0)
         (suspended 0)
         ;; NOTE: This has to use `list' so incf + getf works as
         ;; expected
         (created (list :day 0 :week 0 :month 0))
         (by-type (make-hash-table))
         (now (current-time))
         (minus-day (time-subtract now (* 24 60 60)))
         (minus-week (time-subtract now (* 7 24 60 60)))
         (minus-month (time-subtract now (* 30 24 60 60))))

    (dolist (card cards)
      (cl-incf total 1)
      (if (oref card suspended)
          (cl-incf suspended 1)
        (let ((card-created (oref card created)))
          (when (time-less-p minus-day card-created)
            (cl-incf (cl-getf created :day) 1))
          (when (time-less-p minus-week card-created)
            (cl-incf (cl-getf created :week) 1))
          (when (time-less-p minus-month card-created)
            (cl-incf (cl-getf created :month) 1))))
      (cl-incf (gethash (oref card type) by-type 0) 1))

    (insert (format "  New: %d (day) %d (week) %d (month) \n"
                    (plist-get created :day)
                    (plist-get created :week)
                    (plist-get created :month)))

    (insert "\n")
    (insert (format
             "  %6d Cards, %d suspended\n"
             total
             suspended))
    (dolist (pair (org-fc-dashboard--hashtable-to-alist by-type))
      (insert (format "  %6d %s\n" (cdr pair) (car pair))))
    (insert "\n")))

(defun org-fc-dashboard-insert-position-stats (cards)
  (let* ((n-pos 0)
         (due (list :now 0 :day 0 :week 0 :month 0))
         (now (current-time))
         (plus-day (time-add now (* 24 60 60)))
         (plus-week (time-add now (* 7 24 60 60)))
         (plus-month (time-add now (* 30 24 60 60))))

    (dolist (card cards)
      (when (not (oref card suspended))
        (dolist (pos (oref card positions))
          (cl-incf n-pos 1)

          (let ((pos-due (oref pos due)))
            (when (time-less-p pos-due now)
              (cl-incf (cl-getf due :now) 1))
            (when (time-less-p pos-due plus-day)
              (cl-incf (cl-getf due :day) 1))
            (when (time-less-p pos-due plus-week)
              (cl-incf (cl-getf due :week) 1))
            (when (time-less-p pos-due plus-month)
              (cl-incf (cl-getf due :month) 1)))

          )))

    (insert (format "  Total: %d\n\n" n-pos))
    (insert (format "  Due: %d (now) %d (day) %d (week) %d (month)\n\n"
                    (plist-get due :now)
                    (plist-get due :day)
                    (plist-get due :week)
                    (plist-get due :month)))

    (insert "\n")))
(defun org-fc-dashboard-insert-sm2-overview (cards)
  (let* ((n-pos 0)
         (avg-ease 0.0) (avg-box 0.0) (avg-interval 0.0)
         (now (current-time)))

    (dolist (card cards)
      (when (and (not (oref card suspended))
                  (or (null (oref card algo))
                      (eq (oref card algo) 'sm2)))
        (dolist (pos (oref card positions))
          (cl-incf n-pos 1)
          (cl-incf
           avg-ease
           (plist-get (oref pos data) :ease))
          (cl-incf
           avg-box
           (plist-get (oref pos data) :box))
          (cl-incf
           avg-interval
           (plist-get (oref pos data) :interval)))))

    (setq avg-ease (/ avg-ease n-pos))
    (setq avg-box (/ avg-box n-pos))
    (setq avg-interval (/ avg-interval n-pos))

    (insert (format "  Total: %d\n\n" n-pos))

    (insert (format "  %6.2f Avg. Ease\n" avg-ease))
    (insert (format "  %6.2f Avg. Box\n" avg-box))
    (insert (format "  %6.2f Avg. Interval (days)\n" avg-interval))
    (insert "\n")))

(defun org-fc-dashboard-insert-sm2-review-stats (_cards)
  (when-let ((reviews-stats (org-fc-awk-sm2-review-stats)))
    (dolist (scope '((:day . "Day")
                     (:week . "Week")
                     (:month . "Month")
                     (:all . "All")))
      (when-let (stat (plist-get reviews-stats (car scope)))
        (when (> (plist-get stat :total) 0)
          (insert "    ")
          (if (and (display-graphic-p)
                   (memq 'svg (and (boundp 'image-types) image-types)))
              (insert-image (org-fc-dashboard-bar-chart stat))
            (insert (org-fc-dashboard-text-bar-chart stat)))
          (insert (propertize (format " %s (%d)\n\n" (cdr scope) (plist-get stat :total)) 'face 'org-level-1)))))
    (insert "\n")))


;;;; FSRS

(defun org-fc-dashboard-insert-fsrs-overview (cards)
  (let ((sum-days 0.0)
        (sum-stability 0.0)
        (count 0.0))
    (dolist (card cards)
      (unless (or (oref card suspended)
                  (not (eq (oref card algo) 'fsrs)))
        (dolist (pos (oref card positions))
          (when (equal (plist-get (oref pos data) :state)
                       'review)
            (let* ((data (oref pos data))
                   (last-review (plist-get data :last-review))
                   (due (oref pos due))
                   (span
                    (-
                     (time-to-days due)
                     (time-to-days last-review))))
              (cl-incf sum-days span)
              (cl-incf sum-stability (plist-get data :stability))
              (cl-incf count 1))))))
    (insert
     (format "  Avg. span (review): %.2f days\n"
             (/ sum-days count)))
    (insert
     (format "  Avg. stability (review): %.2f days\n\n"
             (/ sum-stability count)))))

(defun org-fc-dashboard-insert-fsrs-state-stats (cards)
  (let ((now (current-time))
        (res-state (make-hash-table :test #'equal))
        (res-state-due (make-hash-table :test #'equal))
        (states '(new learning relearning review)))
    (dolist (card cards)
      (unless (or (oref card suspended)
                  (not (eq (oref card algo) 'fsrs)))
        (dolist (pos (oref card positions))
          (let ((state (plist-get (oref pos data) :state)))
            (puthash state (1+ (gethash state res-state 0)) res-state)
            (when (time-less-p (oref pos due) now)
              (puthash state (1+ (gethash state res-state-due 0)) res-state-due))))))
    (dolist (state states)
      (insert
       (format
        "  %s %s (%s due)\n"
        (symbol-name state)
        (gethash state res-state 0)
        (gethash state res-state-due 0))))
    (insert "\n")))

(defun org-fc-dashboard-insert-fsrs-review-stats (_cards)
  (when-let ((reviews-stats-all (org-fc-awk-fsrs-review-stats)))
    (dolist (state '(new learning relearning review))
      (let* ((reviews-stats (plist-get reviews-stats-all state)))
        (insert (format "  %s\n" state))
        (if (> (plist-get (plist-get reviews-stats :all) :total) 0)
            (progn
              (dolist (scope '((:day . "Day")
                               ;; (:week . "Week")
                               ;; (:month . "Month")
                               (:all . "All")))
                (when-let (stat (plist-get reviews-stats (car scope)))
                  (when (> (plist-get stat :total) 0)
                    (insert "    ")
                    (if (and (display-graphic-p)
                             (memq 'svg (and (boundp 'image-types) image-types)))
                        (insert-image (org-fc-dashboard-bar-chart stat))
                      (insert (org-fc-dashboard-text-bar-chart stat)))
                    (insert (propertize (format " %s (%d)\n" (cdr scope) (plist-get stat :total)) 'face 'org-level-1)))))
              (insert "\n"))
          (insert "    No review yet\n\n"))))))


;; TODO: sort by count, show at most 10/20 tags
;; Filter out internal tags (fc, notitle, noheading, suspended)

(defun org-fc-dashboard--insert-table (headers rows)
  (let* ((all-rows (cons headers rows))
         (lengths
          (mapcar
           (lambda (col)
             (apply #'max (mapcar #'length col)))
           (apply #'cl-mapcar #'list all-rows)))
         (padded
          (mapcar (lambda (row)
                    (cl-mapcar
                     (lambda (str len)
                       (string-pad str len nil 'start))
                     row
                     lengths)) all-rows)))
    (insert "|")
    (dolist (header (car padded))
      (insert (format " %s |" header)))
    (insert "\n")

    (insert "|")
    (dolist (len lengths)
      (insert (format "%s|" (make-string (+ len 2) ?-))))
    (insert "\n")

    (dolist (row (cdr padded))
      (insert "|")
      (dolist (cell row)
        (insert (format " %s |" cell)))
      (insert "\n"))))

;;; Main View

(setq org-fc-dashboard-views
      (list
       '("Overview"
         ("Card Statistics" . card-stats)
         ("Position Statistics" . position-stats))
       '("SM2"
         ("Overview" . sm2-overview)
         ("Review Statistics (All Cards)" . sm2-review-stats))
       '("FSRS"
         ("Overview" . fsrs-overview)
         ("States" . fsrs-state-stats)
         ("Review Statistics (All Cards)" . fsrs-review-stats))))

;; Based on `mu4e-main-view-real'
(defun org-fc-dashboard-view (context)
  "Show the dashboard view for CONTEXT in the current buffer."
  (let* ((buf (get-buffer-create org-fc-dashboard-buffer-name))
         (inhibit-read-only t)
         (cards (org-fc-index-flatten-file (org-fc-index context))))
    (with-current-buffer buf
      (erase-buffer)
      (insert "\n")
      (dolist (view org-fc-dashboard-views)
        (if (string= (car view) org-fc-dashboard-view)
            (insert (format "*%s* " (car view)))
          (insert (format "%s " (car view)))))
      (insert "\n\n")
      (insert
       (propertize "[v] Change View  " 'face 'org-level-1))
      (insert
       (propertize "[r] Review  " 'face 'org-level-1))
      (insert
       (propertize "[q] Quit\n" 'face 'org-level-1))
      (insert "\n")
      (let ((view (alist-get
                   org-fc-dashboard-view
                   org-fc-dashboard-views
                   nil nil #'string=)))
        (dolist (section view)
          (let ((fn (intern (concat "org-fc-dashboard-insert-" (symbol-name (cdr section))))))
            (when (fboundp fn)
              (insert
               (propertize (format "%s\n\n" (car section)) 'face 'org-level-1))
              (funcall fn cards)))))
      )))

;;; Dashboard Mode

(defun org-fc-dashboard-change-view ()
  "Review the context of the current dashboard."
  (interactive)
  (let* ((names (mapcar #'car org-fc-dashboard-views))
         (current (or org-fc-dashboard-view (car names))))
    (setq org-fc-dashboard-view
          (nth (mod (1+ (position current names :test #'string=)) (length names)) names)))
  (org-fc-dashboard-view org-fc-dashboard-context))

(defun org-fc-dashboard-review ()
  "Review the context of the current dashboard."
  (interactive)
  (org-fc-review org-fc-dashboard-context))

(defun org-fc-dashboard-revert (_ignore-auto _noconfirm)
  "Reload the current dashboard."
  (interactive)
  (org-fc-dashboard-view org-fc-dashboard-context))

(defvar org-fc-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "v") 'org-fc-dashboard-change-view)
    (define-key map (kbd "r") 'org-fc-dashboard-review)
    (define-key map (kbd "q") 'quit-window)
    map))

(define-derived-mode org-fc-dashboard-mode special-mode "org-fc main"
  "Major mode providing an overview of the flashcard system"
  (set (make-local-variable 'revert-buffer-function) #'org-fc-dashboard-revert)
  (setq-local cursor-type nil))

;;;###autoload
(defun org-fc-dashboard (context)
  "Open a buffer showing the dashboard view for CONTEXT."
  (interactive (list (org-fc-select-context)))
  (setq org-fc-dashboard-context context)
  (setq org-fc-dashboard-view (car (car org-fc-dashboard-views)))
  (org-fc-dashboard-view context)
  (switch-to-buffer org-fc-dashboard-buffer-name)
  (goto-char (point-min))
  (org-fc-dashboard-mode))

;;; Footer

(provide 'org-fc-dashboard)

;;; org-fc-dashboard.el ends here
