(require 'eieio)

(require 'org-fc-core)

(defclass org-fc-scheduler ()
  ((positions
    :initarg :positions
    :initform nil)))

(defclass org-fc-scheduler-shuffled (org-fc-scheduler)
  ())

(cl-defmethod org-fc-scheduler-init ((scheduler org-fc-scheduler) cards)
  (oset
   scheduler
   positions
   (mapcan (lambda (card) (oref card positions)) cards)))

(cl-defmethod org-fc-scheduler-init ((scheduler org-fc-scheduler-shuffled)
				     cards)
  ;; 1. assign each position a random number
  ;; 2. flatten the list
  ;; 3. sort by the random number
  ;; 4. remove the random numbers from the result
  (let ((positions
	 (mapcan
	  (lambda (card)
	    (let ((positions (oref card positions)))
	      (org-fc-zip
	       (org-fc-sorted-random (length positions))
	       positions)))
	  index)))
    (oset
     scheduler
     positions
     (mapcar
      #'cdr
      (sort positions (lambda (a b) (> (car a) (car b))))))))

(cl-defmethod org-fc-scheduler-next-position ((scheduler org-fc-scheduler))
  (pop (oref scheduler positions)))

(cl-defmethod org-fc-scheduler-push-position ((scheduler org-fc-scheduler) position)
  (with-slots (positions) scheduler
    (setf positions (append positions (list position)))))

(provide 'org-fc-scheduler)
