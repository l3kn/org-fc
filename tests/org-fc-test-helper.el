(require 'cl-lib)
(require 'eieio)
(require 'ert)

(require 'org-fc-core)

(defun org-fc-test-fixture (name)
  "Return the full path of fixture file NAME."
  (expand-file-name
   name
   (expand-file-name "tests/" org-fc-source-path)))

(defun org-fc-test-index-ids (index)
  "Return a list of IDs in INDEX."
  (mapcar
   (lambda (card) (plist-get card :id))
   index))

(defun org-fc-test-check-structure (expected got)
  "Check structural equality of parts of larger objects.
For plists, values of all keys in EXPECTED are compared,
lists are compared element-by-element,
everything else is checked for equality."
  (cond
   ;; plist
   ((and (listp expected)
         expected
         (symbolp (car expected)))
    (let ((keys
           (cl-loop for key in expected by #'cddr collecting key)))
      (dolist (key keys)
        (org-fc-test-check-structure
         (plist-get expected key)
         (if (eieio-object-p got)
             (slot-value got key)
           (plist-get got key))))))
   ;; Normal list
   ((listp expected)
    (should (eq (length expected) (length got)))
    (cl-loop for e in expected for g in got do
             (org-fc-test-check-structure e g)))
   ;; Anything else
   (t (should (equal expected got)))))

;;; Minimal Mocking Mechanism

(defvar org-fc-test--allow-overwrite nil)

(defmacro org-fc-test-with-overwrites (&rest body)
  `(let (org-fc-test--overwritten-functions
	 (org-fc-test--allow-overwrite t))
     (unwind-protect
	 (progn ,@body)
       ;; Undo overwrites
       (dolist (name org-fc-test--overwritten-functions)
	 (if-let ((func (function-get name 'org-fc-test-original-function)))
	     (fset name func)
	   (fmakunbound name))))))

(defmacro org-fc-test-overwrite-fun (name fun)
  `(progn
     (unless org-fc-test--allow-overwrite
       (error "Can't overwrite functions outside of `org-fc-test-with-overwrites'"))
     (when (fboundp ',name)
       (function-put ',name 'org-fc-test-original-function (symbol-function ',name)))
     (push ',name org-fc-test--overwritten-functions)
     (fset ',name ,fun)))


(provide 'org-fc-test-helper)
