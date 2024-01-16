;;; org-fc-diff.el --- String diff functions -*- lexical-binding: t; -*-

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
;; Diff functions for text-input cards.
;;
;; Expected and given input are compared using a longest matching
;; subsequence algorithm and matching / differing sections are colored.
;;
;;; Code:

(require 'cl-lib)

(require 'org-fc-core)

(defcustom org-fc-diff-filler ?-
  "Character for filling diffs when the input was too short."
  :type 'character
  :group 'org-fc)

;; Based on `magit-diff-added'
(defface org-fc-diff-correct
  `((((class color) (background light))
     :background "#ddffdd"
     :foreground "#22aa22")
    (((class color) (background dark))
     :background "#335533"
     :foreground "#ddffdd"))
  "Face for correct parts of a diff."
  :group 'org-fc)

;; Based on `magit-diff-removed'
(defface org-fc-diff-wrong
  `((((class color) (background light))
     :background "#ffdddd"
     :foreground "#aa2222")
    (((class color) (background dark))
     :background "#553333"
     :foreground "#ffdddd"))
  "Face for wrong parts of a diff."
  :group 'org-fc)

(defun org-fc-diff-subseq (a b start1 start2 end1 end2)
  "Find (index-a index-b len) of the longest matching subsequence in A and B.
Only parts of A in the range START1 to END1 and parts of B in the
range START2 to END2 are considered.
If there is no matching subsequence, nil is returned."
  (let ((best-length 0) (best-i 0) (best-j 0)
	;; Longest matching subsequence starting at index j of B,
	;; offset by one to handle the case j = 0
	(lengths (make-vector (1+ (length b)) 0)))
    (cl-loop for i from start1 to end1 do
	     (let ((new-lengths (make-vector (1+ (length b)) 0)))
	       (cl-loop for j from start2 to end2 do
			(if (eql (aref a i) (aref b j))
			    (let ((length (+ 1 (aref lengths j))))
			      (aset new-lengths (1+ j) length)
			      (when (> length best-length)
				(setq best-length length)
				(setq best-i (1+ (- i length)))
				(setq best-j (1+ (- j length)))))))
	       (setq lengths new-lengths)))
    (when (> best-length 0)
      (list best-i best-j best-length))))

(defun org-fc-diff-matching-blocks (a b start1 start2 end1 end2)
  "Find matching blocks of A and B.
Only parts of A in the range START1 to END1 and parts of B in the
range START2 to END2 are considered."
  (if-let ((match (org-fc-diff-subseq a b start1 start2 end1 end2)))
      (cl-destructuring-bind (i j len) match
	(append
	 (org-fc-diff-matching-blocks a b start1 start2 (1- i) (1- j))
	 (list match)
	 (org-fc-diff-matching-blocks a b (+ i len) (+ j len) end1 end2)))))

(defun org-fc-diff--propertize-got (got blocks expected-length)
  "Propertize the GOT answer given matching BLOCKS.
If it is shorter than EXPECTED-LENGTH, it is filled using
`org-fc-diff-filler'."
  (let ((last 0) res)
    ;; Prepend filler if text at start is missing
    (unless (null blocks)
      (cl-destructuring-bind (i j _len) (car blocks)
	(when (> j i)
	  (setq res
		(propertize
		 (make-string (- j i) org-fc-diff-filler)
		 'face 'org-fc-diff-wrong)))))
    (cl-loop for (i _ len) in blocks do
	     (setq res
		   (concat
		    res
		    (propertize
		     (cl-subseq got last i)
		     'face 'org-fc-diff-wrong)
		    (propertize
		     (cl-subseq got i (+ i len))
		     'face 'org-fc-diff-correct)))
	     (setq last (+ i len)))
    (setq res
	  (concat
	   res
	   (propertize (cl-subseq got last) 'face 'org-fc-diff-wrong)))
    ;; Append filler if result is shorter than expected
    (if (< (length res) expected-length)
	(concat
	 res
	 (propertize
	  (make-string (- expected-length (length res)) org-fc-diff-filler)
	  'face 'org-fc-diff-wrong))
      res)))

(defun org-fc-diff--propertize-expected (expected blocks)
  "Propertize the EXPECTED answer, given matching BLOCKS."
  (let ((last 0) res)
    (cl-loop for (_ j len) in blocks do
             (setq res
                   (concat
                    res
                    (cl-subseq expected last j)
                    (propertize
                     (cl-subseq expected j (+ j len))
                     'face 'org-fc-diff-correct)))
             (setq last (+ j len)))
    (concat res (cl-subseq expected last))))

(defun org-fc-diff (got expected)
  "Generate a colored diff of the strings GOT and EXPECTED."
  (if (string= got expected)
      (cons (propertize got 'face 'org-fc-diff-correct) nil)
    (let ((blocks (org-fc-diff-matching-blocks
                   got expected
                   0 0
                   (1- (length got))
                   (1- (length expected)))))
      (cons
       (org-fc-diff--propertize-got got blocks (length expected))
       (org-fc-diff--propertize-expected expected blocks)))))

;;; Footer

(provide 'org-fc-diff)

;;; org-fc-diff.el ends here
