(require 'org)

(require 'org-fc-algo-sm2)

(defcustom org-fc-review-data-drawer "REVIEW_DATA"
  "Name of the drawer used to store review data."
  :type 'string
  :group 'org-fc)

;; Based on `org-log-beginning'
(defun org-fc-review-data-position (&optional create)
  "Return (BEGINNING . END) points of the review data drawer.
When optional argument CREATE is non-nil, the function creates a
drawer, if necessary.  Returned position ignores narrowing.

BEGINNING is the start of the first line inside the drawer,
END is the start of the line with :END: on it."
  (org-with-wide-buffer
   (org-end-of-meta-data)
   (let ((regexp (concat "^[ \t]*:" (regexp-quote org-fc-review-data-drawer) ":[ \t]*$"))
         (end (if (org-at-heading-p) (point)
                (save-excursion (outline-next-heading) (point))))
         (case-fold-search t))
     (catch 'exit
       ;; Try to find existing drawer.
       (while (re-search-forward regexp end t)
         (let ((element (org-element-at-point)))
           (when (eq (org-element-type element) 'drawer)
             (throw 'exit
                    (cons (org-element-property :contents-begin element)
                          (org-element-property :contents-end element))))))
       ;; No drawer found.  Create one, if permitted.
       (when create
         (unless (bolp) (insert "\n"))
         (let ((beg (point)))
           (insert ":" org-fc-review-data-drawer ":\n:END:\n")
           (org-indent-region beg (point)))
         (cons
          (line-beginning-position 0)
          (line-beginning-position 0)))))))

(defun org-fc-review-data-get ()
  "Get a cards review data as a Lisp object."
  (if-let ((position (org-fc-review-data-position)))
      (org-with-point-at (car position)
        (cddr (org-table-to-lisp)))))

(defun org-fc-review-data-set (data)
  "Set the cards review data to DATA."
  (save-excursion
    (let ((position (org-fc-review-data-position 'create)))
      (delete-region (car position) (cdr position))
      (goto-char (car position))
      (insert "| position | ease | box | interval | due |\n")
      (insert "|-|-|-|-|-|\n")
      (dolist (datum data)
        (insert
         "| "
         (mapconcat (lambda (x) (format "%s" x)) datum " | ")
         " |\n"))
      (org-table-align))))

(defun org-fc-review-data-default (position)
  "Default review data for position POSITION."
  (cl-case org-fc-algorithm
    (sm2-v1 (org-fc-algo-sm2-initial-review-data position))
    (sm2-v2 (org-fc-algo-sm2-initial-review-data position))))

(defun org-fc-review-data-update (positions)
  "Update review data to POSITIONS.
If a doesn't exist already, it is initialized with default
values.  Entries in the table not contained in POSITIONS are
removed."
  (let ((old-data (org-fc-review-data-get)))
    (org-fc-review-data-set
     (mapcar
      (lambda (pos)
        (or
         (assoc pos old-data #'string=)
         (org-fc-review-data-default pos)))
      positions))))

(provide 'org-fc-review-data)
