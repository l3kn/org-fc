;;; org-fc-algo-fsrs.el --- Interface to python-based FSRS algorithm -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Cash Prokop-Weaver
;; Copyright (C) 2025  Leon Rische

;; Author: Cash Prokop-Weaver <cash@cashpw.com>
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
;;
;;
;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'eieio-base)

(require 'org-fc-core)
(require 'org-fc-awk)
(require 'org-fc-dashboard)


;;;; Configuration

(defcustom org-fc-algo-fsrs6-parameters
  '(0.2172
    1.1771
    3.2602
    16.1507
    7.0114
    0.57
    2.0966
    0.0069
    1.5261
    0.112
    1.0178
    1.849
    0.1133
    0.3127
    2.2934
    0.2191
    3.0004
    0.7536
    0.3332
    0.1437
    0.2)
  "The model weights of the FSRS scheduler."
  :type '(repeat float)
  :group 'org-fc)

(defcustom org-fc-algo-fsrs6-desired-retention 0.8
  "The desired retention rate of cards scheduled with the scheduler."
  :type 'float
  :group 'org-fc)

(defcustom org-fc-algo-fsrs6-learning-steps
  '(60 600)
  "Small time intervals that schedule cards in the Learning state, in seconds."
  :type '(repeat sexp)
  :group 'org-fc)

(defcustom org-fc-algo-fsrs6-relearning-steps '(600)
  "Small time intervals that schedule cards in the Relearning state, in seconds."
  :type '(repeat sexp)
  :group 'org-fc)

(defcustom org-fc-algo-fsrs6-maximum-interval (* 100 365)
  "The maximum number of days a Review-state card can be scheduled into the future"
  :type 'number
  :group 'org-fc)

(defcustom org-fc-algo-fsrs6-enable-fuzzing t
  "Whether to apply a small amount of random 'fuzz' to calculated intervals."
  :type 'boolean
  :group 'org-fc)

;;;; Helper Functions

(defun org-fc-algo-fsrs6--plist-to-alist (plist)
  (cl-loop for (key val) on plist by #'cddr collect (cons key val)))

;;;; Python CLI Interaction

(defun org-fc-algo-fsrs6--scheduler-alist ()
  `((parameters . ,(vconcat org-fc-algo-fsrs6-parameters))
    (desired-retention . ,org-fc-algo-fsrs6-desired-retention)
    (relearning-steps . ,(vconcat org-fc-algo-fsrs6-relearning-steps))
    (learning-steps . ,(vconcat org-fc-algo-fsrs6-learning-steps))
    (maximum-interval . ,org-fc-algo-fsrs6-maximum-interval)
    (enable-fuzzing . ,org-fc-algo-fsrs6-enable-fuzzing)))

(defun org-fc-algo-fsrs6--cli-wrap-json (request args)
  "Run the python interface with json-encoded REQUEST as stdin,
then return the parsed json response."
  (let* ((default-directory
          (file-name-as-directory
           (expand-file-name "python" org-fc-source-path)))
         (json-object-type 'plist)
         (json-key-type 'symbol)
         (return-data
          (with-temp-buffer
            (insert (json-encode request))
            (save-excursion
              (apply
               #'call-process-region
               (point-min) (point-max)
               "python" nil t nil "algo_fsrs6.py"
               args))
            (json-read))))
    return-data))

(defun org-fc-algo-fsrs6--cli-get-initial ()
  (org-fc-algo-fsrs6--cli-wrap-json
   nil
   (list "initial" "--now" (org-fc-timestamp-in 0))))

(defun org-fc-algo-fsrs6--cli-get-next (current rating now)
  ;; Note: json-encode expects alists while other parts of org-fc use
  ;; plists, so we need to convert here.
  ;; When reading json, this can be configured but apparently not when
  ;; writing.
  (org-fc-algo-fsrs6--cli-wrap-json
   `((scheduler . ,(org-fc-algo-fsrs6--scheduler-alist))
     (card . ,current)
     (rating . ,rating))
   (list "review" "--now" now)))

(defun org-fc-algo-fsrs6--cli-from-history (card-id positions &optional quantize)
  (org-fc-algo-fsrs6--cli-wrap-json
   `((scheduler . ,(org-fc-algo-fsrs6--scheduler-alist))
     (card-id . ,card-id)
     (positions . ,positions))
   `("from_history"
     "--history_file"
     ,(expand-file-name org-fc-review-history-file)
     ,@(if quantize '("--quantize")))))

;;;; Main Algorithm Interface

(defclass org-fc-algo-fsrs6 (eieio-singleton org-fc-algo) ())

(cl-defmethod org-fc-algo-headers ((_algo org-fc-algo-fsrs6))
  '(position state step stability difficulty due last-review))

(cl-defmethod org-fc-algo-initial-review-data ((_algo org-fc-algo-fsrs6) name)
  "Initial FSRS_6 review data for position NAME."
  (list*
   'position
   name
   (org-fc-algo-fsrs6--cli-get-initial)))

(defun org-fc-algo-fsrs--parse-optional-str (v)
  (if (equalp v "nil") nil v))

(defun org-fc-algo-fsrs--parse-optional-number (v)
  (if (equalp v "nil") nil (string-to-number v)))

(defun org-fc-algo-fsrs--format-review-data (data &optional include-position)
  (let* ((stability (plist-get data 'stability))
         (difficulty (plist-get data 'difficulty))
         (result (list
                  'state (format "%d" (plist-get data 'state))
                  'step (format "%s" (plist-get data 'step))
                  'stability (if stability (format "%.6f" stability) "nil")
                  'difficulty (if difficulty (format "%.6f" difficulty) "nil")
                  'due (plist-get data 'due)
                  'last-review (plist-get data 'last-review))))
    (if include-position
        (list* 'position (plist-get data 'position) result)
      result)))

(cl-defmethod org-fc-algo-next-review-data
  ((_algo org-fc-algo-fsrs6) current rating)
  "Calculate the next parameters, given the CURRENT parameters and a RATING."
  (let ((next-data (org-fc-algo-fsrs6--cli-get-next
                    `((state . ,(string-to-number (plist-get current 'state)))
                      (step . ,(org-fc-algo-fsrs--parse-optional-number (plist-get current 'step)))
                      (stability . ,(org-fc-algo-fsrs--parse-optional-number (plist-get current 'stability)))
                      (difficulty . ,(org-fc-algo-fsrs--parse-optional-number (plist-get current 'difficulty)))
                      (due . ,(plist-get current 'due))
                      (last-review . ,(org-fc-algo-fsrs--parse-optional-str (plist-get current 'last-review))))
                    rating
                    (org-fc-timestamp-in 0))))
    (org-fc-algo-fsrs--format-review-data next-data)))

(cl-defmethod org-fc-algo-log-review ((_algo org-fc-algo-fsrs6) (position org-fc-position) current rating delta)
  (let* ((card (oref position card))
	       (file (oref card file))
	       (elements
	        (list
	         (org-fc-timestamp-in 0)
	         (oref file path)
	         (oref card id)
	         (oref position name)
           ;; Leave columns used by SM2 algorithm empty
           ""
           ""
           ""
	         (symbol-name rating)
	         (format "%.2f" delta)
	         (symbol-name 'fsrs6))))
    (append-to-file
     (format "%s\n" (mapconcat #'identity elements "\t"))
     nil
     org-fc-review-history-file)))

;; NOTE: This is a full duplicate of the version in SM2
(cl-defmethod org-fc-algo-update-review-data ((algo org-fc-algo-fsrs6) (position org-fc-position) rating delta)
  "Update the review data of a POSITION.
Also add a new entry in the review history file. RATING is a
review rating and DELTA the time in seconds between showing and
rating the card."
  (let* ((name (oref position name))
	       (review-data (org-fc-review-data-parse (org-fc-algo-headers algo)))
	       (current (org-fc-review-data-get-row review-data name)))

    (unless current
      (error "No review data row found for this position"))

    (org-fc-algo-log-review algo position current rating delta)

    (org-fc-review-data-update-row
     review-data name
     (org-fc-algo-next-review-data algo current rating))
    (org-fc-review-data-write review-data)))

;; NOTE: This is a full duplicate of the version in SM2
(cl-defmethod org-fc-algo-review-history ((_algo org-fc-algo-fsrs6) card-id position-name)
  "Review history for a given CARD-ID and POSITION name.
Returns nil if there is no history file."
  (when (file-exists-p org-fc-review-history-file)
    (let ((output
           (shell-command-to-string
            (org-fc-awk--command
             ;; NOTE: I expected to use different history files for different algorithms
             ;; but sharing a file and leaving columns empty seems more elegant now.
             ;; TODO: Rename the file again
             "awk/review_history_sm2.awk"
             :input org-fc-review-history-file
	           :variables `(("filter_card_id" . ,(or card-id "any"))
			                    ("filter_position_name" . ,(or position-name "any")))))))
      (if (string-prefix-p "(" output)
          (read output)
        (error "Org-fc shell error: %s" output)))))

(org-fc-register-algo 'fsrs6 org-fc-algo-fsrs6)

;;; Card Migration

(defun org-fc-algo-fsrs6-migrate ()
  (interactive)
  (unless (org-fc-entry-p)
    (error "Only flashcard entries can be migrated"))
  (let* ((card-id (org-id-get))
         (review-data (org-fc-review-data-parse (org-fc-algo-headers (org-fc-algo-fsrs6))))
         (position-names
          (mapcar
           (lambda (r) `((name . ,(car r))
                    (original-due . ,(plist-get (cdr r) 'due))))
           (oref review-data rows)))
         (next-data (org-fc-algo-fsrs6--cli-from-history card-id position-names 'quantize))
         (formatted-data (mapcar (lambda (row) (org-fc-algo-fsrs--format-review-data row 'include-position)) next-data))
         (review-data (org-fc-review-data
                       :headers (org-fc-algo-headers (org-fc-algo-fsrs6))
                       :rows (mapcar (lambda (row) (cons (plist-get row 'position) row)) formatted-data))))
    (org-fc-review-data-write review-data)
    (org-set-property org-fc-algo-property "fsrs6")))

;;; Footer

(provide 'org-fc-algo-fsrs)

;;; org-fc-algo-fsrs.el ends here
