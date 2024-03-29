;;; org-fc-dashboard.el --- Dashboard buffer for org-fc -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024  Leon Rische

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

(require 'cl-lib)
(require 'eieio)
(require 'svg)

(require 'org-fc-core)
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

(defvar-local org-fc-dashboard-context org-fc-context-all
  "Context of the current dashboard view.")

(defvar-local org-fc-dashboard-cards '()
  "Cards of the current dashboard view/context.")

(defvar-local org-fc-dashboard-section--current 0
  "Index of the current dashboard section.")

;;; Helper Functions

(defun org-fc-dashboard--hashtable-to-alist (ht)
  "Convert a hash-table HT to an alist."
  (let (res)
    (dolist (key (hash-table-keys ht))
      (push (cons key (gethash key ht)) res))
    res))

(defun org-fc-dashboard-buffer ()
  (get-buffer-create org-fc-dashboard-buffer-name))

;;; Sections and Folding

(defclass org-fc-dashboard-section ()
  ((title
    :initarg :title)
   (inserter
    :initarg :inserter)
   (start-visible
    :initarg :start-visible
    :initform nil)
   (visible
    :initarg :visible
    :initform nil)
   (expanded
    :initarg :expanded
    :initform nil)
   (heading-bounds
    :initarg :heading-bounds
    :initform nil)
   (content-bounds
    :initarg :content-bounds
    :initform nil)))

(cl-defmethod org-fc-dashboard-section-offset-bounds ((section org-fc-dashboard-section) delta)
  "Offset all bounds of a SECTION by DELTA characters."
  (with-slots (heading-bounds content-bounds) section
    (oset
     section heading-bounds
     (cons (+ (car heading-bounds) delta)
           (+ (cdr heading-bounds) delta)))
    (oset
     section content-bounds
     (cons (+ (car content-bounds) delta)
           (+ (cdr content-bounds) delta)))))

(defun org-fc-dashboard-section-goto (n &optional is-first)
  (let ((inhibit-read-only t))
    ;; If there was a previous section, change the cursor-marker
    (unless is-first
      (let ((section-old
	     (nth
	      org-fc-dashboard-section--current
	      org-fc-dashboard-sections)))
	(goto-char (car (oref section-old heading-bounds)))
	(delete-char 1)
	(if (oref section-old visible)
	    (insert (propertize "-" 'face 'org-level-1))
	  (insert (propertize "+" 'face 'org-level-1)))))

    (let ((section-new (nth n org-fc-dashboard-sections)))
      (goto-char (car (oref section-new heading-bounds)))
      (delete-char 1)
      (insert (propertize "o" 'face 'org-level-1)))
    (setq org-fc-dashboard-section--current n)))

(cl-defmethod org-fc-dashboard-section-close ((section org-fc-dashboard-section))
  "Close a dashboard SECTION."
  (when (oref section visible)
    (let ((inhibit-read-only t))
      (goto-char (car (oref section heading-bounds)))
      (unless (= (following-char) ?o)
	(delete-char 1)
	(insert (propertize "+" 'face 'org-level-1)))
      (org-fc-hide-region
       (car (oref section content-bounds))
       (cdr (oref section content-bounds)))
      (oset section visible nil)
      ;; In case point is inside the section,
      ;; jump to the “headline” when collapsing
      (goto-char (car (oref section heading-bounds))))))

(cl-defmethod org-fc-dashboard-section-open ((section org-fc-dashboard-section))
  "Open a dashboard SECTION."
  (unless (oref section visible)
    (let ((inhibit-read-only t))
      (save-excursion
	(goto-char (car (oref section heading-bounds)))
	(unless (= (following-char) ?o)
	  (delete-char 1)
	  (insert (propertize "-" 'face 'org-level-1))))
      (if (oref section expanded)
	  (progn
	    (remove-overlays
	     (car (oref section content-bounds))
	     (cdr (oref section content-bounds))
	     'category 'org-fc)
	    (oset section visible t))
	(save-excursion
	  (goto-char (car (oref section content-bounds)))
	  (let ((cur (point)))
	    (funcall (oref section inserter) org-fc-dashboard-cards)
	    (oset section content-bounds (cons cur (point)))
	    (oset section expanded t)
	    (oset section visible t)

	    (let ((delta (- (point) cur)))
	      (dolist (next-section org-fc-dashboard-sections)
		(when (and (not (equal section next-section))
			   (<= cur (car (oref next-section heading-bounds))))
		  (org-fc-dashboard-section-offset-bounds next-section delta))))))))))

(defun org-fc-dashboard-section-toggle ()
  "Toggle the current dashboard section."
  (interactive)
  (let ((section (nth org-fc-dashboard-section--current org-fc-dashboard-sections)))
    (if (oref section visible)
	(org-fc-dashboard-section-close section)
      (org-fc-dashboard-section-open section))))

(defun org-fc-dashboard-section-previous ()
  (interactive)
  (if (= 0 org-fc-dashboard-section--current)
      (org-fc-dashboard-section-goto (- (length org-fc-dashboard-sections) 1))
    (org-fc-dashboard-section-goto (- org-fc-dashboard-section--current 1))))

(defun org-fc-dashboard-section-next ()
  (interactive)
  (org-fc-dashboard-section-goto (mod (+ org-fc-dashboard-section--current 1) (length org-fc-dashboard-sections))))

(defvar org-fc-dashboard-sections
  (list
   (org-fc-dashboard-section
    :title "Hotkeys"
    :start-visible t
    :inserter #'org-fc-dashboard-insert-hotkeys)
   (org-fc-dashboard-section
    :title "Overview"
    :start-visible t
    :inserter #'org-fc-dashboard-insert-overview)
   (org-fc-dashboard-section
    :title "Cards by Type"
    :inserter #'org-fc-dashboard-insert-cards-by-type)))

(defun org-fc-dashboard-add-section (section)
  (setq org-fc-dashboard-sections
	(append org-fc-dashboard-sections (list section))))

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

(defun org-fc-dashboard-insert-hotkeys (_cards)
  (insert "\n")
  (insert "  [r] review [q] quit\n")
  (insert "  [n] next section [p] previous section [Tab] open/close section\n\n"))

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
      (if (oref card suspended)
	  (cl-incf suspended 1)
	(let ((card-created (oref card created)))

	  (when (time-less-p minus-day card-created)
	    (cl-incf (cl-getf created :day) 1))
	  (when (time-less-p minus-week card-created)
	    (cl-incf (cl-getf created :week) 1))
	  (when (time-less-p minus-month card-created)
	    (cl-incf (cl-getf created :month) 1))

	  (dolist (pos (oref card positions))
	    (cl-incf pos-count)

	    (let ((pos-due (oref pos due)))
	      (when (time-less-p pos-due now)
		(cl-incf (cl-getf due :now) 1))
	      (when (time-less-p pos-due plus-day)
		(cl-incf (cl-getf due :day) 1))
	      (when (time-less-p pos-due plus-week)
		(cl-incf (cl-getf due :week) 1))
	      (when (time-less-p pos-due plus-month)
		(cl-incf (cl-getf due :month) 1)))))))

    (insert "\n")
    (insert (format
	     "  %d cards, %d suspended\n"
	     card-count suspended))
    (insert (format "  new: %d (day) %d (week) %d (month) \n"
		    (plist-get created :day)
		    (plist-get created :week)
		    (plist-get created :month)))

    (insert "\n")

    (insert (format "  %d positions\n" pos-count))

    (when (cl-plusp pos-count)
      (insert (format "  due: %d (now) %d (day) %d (week) %d (month)\n"
		      (plist-get due :now)
		      (plist-get due :day)
		      (plist-get due :week)
		      (plist-get due :month))))
    (insert "\n")))

(defun org-fc-dashboard-insert-cards-by-type (cards)
  (insert "\n")
  (let* ((by-type (make-hash-table)))
    (dolist (card cards)
      (unless (oref card suspended)
	(cl-incf (gethash (oref card type) by-type 0) 1)))

    (if (cl-plusp (hash-table-count by-type))
	(dolist (pair (org-fc-dashboard--hashtable-to-alist by-type))
	  (insert (format "  %6d %s\n" (cdr pair) (car pair))))
      (insert "  No cards yet\n"))
    (insert "\n")))

;;; Main View

;; Based on `mu4e-main-view-real'
(defun org-fc-dashboard-view (context)
  "Show the dashboard view for CONTEXT in the current buffer."
  (let* ((inhibit-read-only t)
         (index (org-fc-index context)))
    (with-current-buffer (org-fc-dashboard-buffer)
      (setq org-fc-dashboard-context context)
      (setq org-fc-dashboard-cards index)

      (erase-buffer)

      (dolist (section org-fc-dashboard-sections)
        (oset section expanded nil)
        (oset section visible nil)
        (let ((heading-beginning (point))
              (heading-end nil)
              (content-end nil))
          (insert
           (propertize
            (format "+ %s\n" (oref section title))
            'face 'org-level-1))
          (setq heading-end (point))
          (oset section heading-bounds (cons heading-beginning heading-end))
          (setq content-end (point))
          (oset section content-bounds (cons heading-end content-end))))

      (dolist (section org-fc-dashboard-sections)
	(when (oref section start-visible)
	  (org-fc-dashboard-section-open section)
	  (goto-char (cdr (oref section content-bounds)))))

      (org-fc-dashboard-section-goto 0 'is-first))))

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

    (define-key map (kbd "n") 'org-fc-dashboard-section-next)
    (define-key map (kbd "p") 'org-fc-dashboard-section-previous)
    (define-key map (kbd "TAB") 'org-fc-dashboard-section-toggle)

    map))

(define-derived-mode org-fc-dashboard-mode special-mode "org-fc dashboard"
  "Major mode providing an overview of the flashcard system"
  (setq-local revert-buffer-function #'org-fc-dashboard-revert)
  (setq-local cursor-type nil))

;;;###autoload
(defun org-fc-dashboard (context)
  "Open a buffer showing the dashboard view for CONTEXT."
  (interactive (list (org-fc-select-context)))
  (with-current-buffer (org-fc-dashboard-buffer)
    (org-fc-dashboard-mode))
  (org-fc-dashboard-view context)
  (switch-to-buffer (org-fc-dashboard-buffer)))

;;; Footer

(provide 'org-fc-dashboard)

;;; org-fc-dashboard.el ends here
