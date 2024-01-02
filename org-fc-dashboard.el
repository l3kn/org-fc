;;; org-fc-dashboard.el --- Dashboard buffer for org-fc -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024  Leon Rische

;; Author: Leon Rische <emacs@leonrische.me>
;; Url: https://www.leonrische.me/pages/org_flashcards.html
;; Package-requires: ((emacs "26.3") (org "9.3"))
;; Version: 0.2.0

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
  "Context of the current dashboard view.")

;;; Helper Functions

(defun org-fc-dashboard--hashtable-to-alist (ht)
  "Convert a hash-table HT to an alist."
  (let (res)
    (dolist (key (hash-table-keys ht))
      (push (cons key (gethash key ht)) res))
    res))



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

;;; Section Definitions

(defun org-fc-dashboard-insert-overview (cards)
  (let* ((card-count (length cards)) (suspended 0) (pos-count 0)
         (created (list :day 0 :week 0 :month 0))
         (due (list :now 0 :day 0 :week 0 :month 0))
         (now (current-time))
         (minus-day (time-subtract now (* 24 60 60)))
         (minus-week (time-subtract now (* 7 24 60 60)))
         (minus-month (time-subtract now (* 30 24 60 60)))
         (plus-day (time-add now (* 24 60 60)))
         (plus-week (time-add now (* 7 24 60 60)))
         (plus-month (time-add now (* 30 24 60 60))))
    (dolist (card cards)
      (if (plist-get card :suspended)
          (cl-incf suspended 1)
        (let ((card-created (plist-get card :created)))

          (if (time-less-p minus-day card-created)
              (cl-incf (cl-getf created :day) 1))
          (if (time-less-p minus-week card-created)
              (cl-incf (cl-getf created :week) 1))
          (if (time-less-p minus-month card-created)
              (cl-incf (cl-getf created :month) 1))

          (dolist (pos (plist-get card :positions))
            (cl-incf pos-count)

            (let ((pos-due (plist-get pos :due)))
              (if (time-less-p pos-due now)
                  (cl-incf (cl-getf due :now) 1))
              (if (time-less-p pos-due plus-day)
                  (cl-incf (cl-getf due :day) 1))
              (if (time-less-p pos-due plus-week)
                  (cl-incf (cl-getf due :week) 1))
              (if (time-less-p pos-due plus-month)
                  (cl-incf (cl-getf due :month) 1)))))))

    (insert
     (propertize "Overview\n\n" 'face 'org-level-1))

    (insert (format
	     "  %d cards, %d suspended\n"
	     card-count suspended))
    (insert (format "  new: %d (day) %d (week) %d (month) \n"
		    (plist-get created :day)
		    (plist-get created :week)
		    (plist-get created :month)))

    (insert "\n")

    (insert (format "  %d positions\n" pos-count))

    (when (plusp pos-count)
      (insert (format "  due: %d (now) %d (day) %d (week) %d (month)\n\n"
		      (plist-get due :now)
		      (plist-get due :day)
		      (plist-get due :week)
		      (plist-get due :month))))))

(defun org-fc-dashboard-insert-cards-by-type (cards)
  (let* ((by-type (make-hash-table)))
    (dolist (card cards)
      (unless (plist-get card :suspended)
	(cl-incf (gethash (plist-get card :type) by-type 0) 1)))

    (insert
     (propertize "Cards by Type\n\n" 'face 'org-level-1))
    (dolist (pair (org-fc-dashboard--hashtable-to-alist by-type))
      (insert (format "  %6d %s\n" (cdr pair) (car pair))))
    (insert "\n")))

(defun org-fc-dashboard-insert-review-stats (_cards)
  (let ((reviews-stats (org-fc-awk-stats-reviews)))
    (insert
     (propertize "Review Statistics (All Cards)\n\n" 'face 'org-level-1))
    (if reviews-stats
	(progn
	  (dolist (scope '((:day . "Day")
			   (:week . "Week")
			   (:month . "Month")
			   (:all . "All")))
	    (when-let (stat (plist-get reviews-stats (car scope)))
	      (when (plusp (plist-get stat :total))
		(insert "  ")
		(if (and (display-graphic-p)
			 (memq 'svg (and (boundp 'image-types) image-types)))
		    (insert-image (org-fc-dashboard-bar-chart stat))
		  (insert (org-fc-dashboard-text-bar-chart stat)))
		(insert (propertize (format " %s (%d)\n" (cdr scope) (plist-get stat :total)) 'face 'org-level-1)))))
	  (insert "\n"))
      (insert "  No reviews yet\n\n"))))

(defun org-fc-dashboard-insert-sm2-stats (cards)
  (let* ((pos-count 0)
         (avg-ease 0.0) (avg-box 0.0) (avg-interval 0.0))
    (dolist (card cards)
      (unless (plist-get card :suspended)
        (dolist (pos (plist-get card :positions))
          (cl-incf pos-count 1)
          (cl-incf avg-ease (plist-get pos :ease))
          (cl-incf avg-box (plist-get pos :box))
          (cl-incf avg-interval (plist-get pos :interval)))))

    (insert
     (propertize "SM2 Statistics\n\n" 'face 'org-level-1))

    (when (plusp pos-count)
      (insert (format "  %6.2f avg. ease\n" (/ avg-ease pos-count)))
      (insert (format "  %6.2f avg. box\n" (/ avg-box pos-count)))
      (insert (format "  %6.2f avg. interval (days)\n\n" (/ avg-interval pos-count))))))

;;; Main View

;; Based on `mu4e-main-view-real'
(defun org-fc-dashboard-view (context)
  "Show the dashboard view for CONTEXT in the current buffer."
  (let* ((buf (get-buffer-create org-fc-dashboard-buffer-name))
         (inhibit-read-only t)
         (index (org-fc-index context)))
    (with-current-buffer buf
      (erase-buffer)

      (org-fc-dashboard-insert-overview index)
      (org-fc-dashboard-insert-cards-by-type index)
      (org-fc-dashboard-insert-sm2-stats index)
      (org-fc-dashboard-insert-review-stats index)

      (insert
       (propertize "[r] Review\n" 'face 'org-level-1))
      (insert
       (propertize "[q] Quit\n" 'face 'org-level-1)))))

;;; Dashboard Mode

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
  (org-fc-dashboard-view context)
  (switch-to-buffer org-fc-dashboard-buffer-name)
  (goto-char (point-min))
  (org-fc-dashboard-mode))

;;; Footer

(provide 'org-fc-dashboard)

;;; org-fc-dashboard.el ends here
