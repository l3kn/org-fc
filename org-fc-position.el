;;; org-fc-position.el --- Representation of a position -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Cash Weaver
;;
;; Author: Cash Weaver <cashbweaver@gmail.com>
;; Maintainer: Cash Weaver <cashbweaver@gmail.com>
;; Created: September 26, 2022
;; Modified: September 26, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/cashweaver/org-fc-position
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; A position is a single "view" of a card.
;;
;;; Code:

(require 'eieio)
(require 'dash)

(defclass org-fc-position ()
  ((box
    :initform 0
    :initarg :box
    :type (integer -1 *)
    :documentation "The positions's current Leitner system box.")
   (card
    :initform nil
    :initarg :card
    :documentation "The `org-fc-card' which contains this position.")
   (due
    :initform nil
    :initarg :due
    :type list
    :custom (repeat integer)
    :documentation "The time at which this position is due.")
   (ease
    :initform 0
    :initarg :ease
    :type (or float integer)
    :documentation "The ease factor.")
   (interval
    :initform 0
    :initarg :interval
    :type (or float integer)
    :documentation "The interval, in days.")
   (pos
    :initform ""
    :initarg :pos
    :type (or integer string)
    :documentation "The name of the position (e.g. \"front\" for double/normal or \"0\" for cloze)."))
  "Represents a single position.")

(cl-defmethod org-fc-position--is-due ((pos org-fc-position))
  "Return t if POS is due; else nil."
  (time-less-p (oref pos due) (current-time)))

(cl-defmethod org-fc-position-new-p ((pos org-fc-position))
  "Return t if the provided POS ition is new; nil otherwise."
  (eq -1 (oref pos box)))

(defun org-fc-positions--filter-due (positions)
  "Filter POSITIONS to include only enabled and due positions."
  (let ((due-enabled-positions (cl-loop for pos in positions
                                        when (and (not (oref (oref pos card) suspended))
                                                  (org-fc-position--is-due pos))
                                        collect pos)))
    (if org-fc-bury-siblings
        (org-fc-positions--one-per-card due-enabled-positions)
      due-enabled-positions)))

(defun org-fc-positions--one-per-card (positions)
  "Return filtered list of POSITIONS; only one position per card."
  (let ((-compare-fn (lambda (a b)
                       (equal (oref (oref a card) id)
                              (oref (oref b card) id)))))
    (-distinct positions)))


(provide 'org-fc-position)
;;; org-fc-position.el ends here
