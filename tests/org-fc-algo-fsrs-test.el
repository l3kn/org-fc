(setq org-fc-algo-fsrs-test-parameters
      (org-fc-algo-fsrs-parameters
       :weights
       '(0.4 0.6 2.4 5.8 4.93 0.94 0.86 0.01 1.49 0.14 0.94 2.18 0.05 0.34 1.26 0.29 2.61)
       :request-retention 0.90))

(setq org-fc-algo-fsrs-test-scheduler
      (org-fc-algo-fsrs-scheduler
       :parameters org-fc-algo-fsrs-test-parameters))

(defun org-fc-algo-fsrs--trace-schedule (position depth)
  (if (> depth 0)
      (let ((sched (org-fc-algo-fsrs-rate-position
                    org-fc-algo-fsrs-test-scheduler
                    position
                    (oref position due))))
        (cons
         position
         (mapcan (lambda (pos)
                   (org-fc-algo-fsrs--trace-schedule (eieio-oref sched pos) (- depth 1)))
                 '(again hard good easy))))
    (list position)))


(format-time-string
 "%FT%TZ"
 (encode-time
  (decode-time
   (parse-iso8601-time-string "2000-01-01T00:00:00Z")
   "UTC0"))
 "UTC0")

(defun org-fc-algo-fsrs-test-updates ()
  (let* ((now (decode-time (parse-iso8601-time-string "2000-01-01T00:00:00Z")))
         (position (org-fc-algo-fsrs-position :due now))
         (expected
          (with-temp-buffer
            (insert-file-contents
             "/home/leon/src/org-fc/fsrs/reference.tsv")
            (mapcar
             (lambda (line)
               (split-string line "\t"))
             (split-string
              (buffer-string)
              "\n" 'omit-nulls))))
         (got
          (mapcar
           (lambda (pos)
             (list
              (format-time-string "%FT%TZ" (encode-time (oref pos due)) t)
              (format "%s" (oref pos stability))
              (format "%s" (oref pos difficulty))
              (format "%s" (oref pos state))
              (format "%s" (oref pos reps))
              (format "%s" (oref pos lapses))))
           (org-fc-algo-fsrs--trace-schedule position 6))))
    (should (eq (length expected) (length got)))
    (should (equal expected got))
    ;; (mapcar (lambda (pair)
    ;;           (should (equal (car pair) (cdr pair))))
    ;;         (org-fc-zip expected got))
    ))
