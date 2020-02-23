;;; org-fc-overlay.el --- Functions for hidings parts of org buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Leon Rische

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
;; During review, parts of the cards buffer should be hidden.
;; This file provides a number of functions for doing so.
;;
;;; Code:

(require 'outline)
(require 'org)
(require 'org-element)

;;;; Finding Positions in the Buffer

(defun org-fc-overlay--point-at-end-of-previous ()
  "Value of point at the end of the previous line.
Returns nil if there is no previous line."
  (save-excursion
    (beginning-of-line)
    (if (bobp)
        nil
      (progn (backward-char)
             (point)))))

(defun org-fc-overlay--point-after-title ()
  "Value of point at the first line after the title keyword.
Returns nil if there is no title keyword."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (rx bol "#+TITLE:") nil t)
      (point-at-eol))))

;;; Showing / Hiding Regions

(defun org-fc-show-all ()
  "Remove all org-fc overlays in the current buffer."
  (interactive)
  (remove-overlays (point-min) (point-max) 'category 'org-fc-hidden)
  (remove-overlays (point-min) (point-max) 'category 'org-fc-visible))

;; Based on `outline-flag-region'
(defun org-fc-hide-region (from to &optional text face)
  "Hide region FROM ... TO, optionally replacing it with TEXT.
FACE can be used to set the text face of the overlay, e.g. to
make it bold."
  ;; (remove-overlays from to 'category 'org-fc-hidden)
  (let ((o (make-overlay from to nil 'front-advance)))
    (overlay-put o 'display-original (overlay-get o 'display))
    (overlay-put o 'category 'org-fc-hidden)
    (overlay-put o 'evaporate t)
    (if (stringp text)
        (progn
          (overlay-put o 'invisible nil)
          (if face (overlay-put o 'face face))
          (overlay-put o 'display text))
      (overlay-put o 'invisible t))
    o))

(defun org-fc-overlay-region (from to &optional face)
  "Wrap region FROM ... TO in an overlay for later hiding.
FACE can be used to set the text face of the overlay."
  ;; (remove-overlays from to 'category 'org-fc-hidden)
  (let ((o (make-overlay from to)))
    (overlay-put o 'evaporate t)
    (if face (overlay-put o 'face face))
    (overlay-put o 'invisible nil)
    (overlay-put o 'category 'org-fc-visible)
    o))

(defun org-fc-hide-overlay (o)
  "Hide the overlay O."
  (overlay-put o 'category 'org-fc-hidden)
  (overlay-put o 'invisible t)
  (overlay-put o 'display ""))

(defun org-fc-show-overlay (o &optional face)
  "Show the overlay O using an optional font FACE."
  (overlay-put o 'category 'org-fc-hidden)
  (overlay-put o 'invisible nil)
  (if face
      (overlay-put o 'face face)))

;;;; Hiding Drawers

(defun org-fc-hide-drawers ()
  "Hide all drawers after point."
  (save-excursion
    (while (re-search-forward org-drawer-regexp nil t)
      (let ((start (1- (match-beginning 0)))
            (end))
        (if (re-search-forward ":END:" nil t)
            (setq end (point))
          (error "No :END: found for drawer"))
        (org-fc-hide-region start end)))))

;;;; Hiding Headings

(defun org-fc-hide-subheadings-if (test)
  "Hide subheadings matching the predicate TEST.
TEST is a function taking no arguments and will be called for
each of the immediate subheadings of the current headline, with
the point on the relevant subheading.  TEST should return nil if
the subheading is to be revealed, non-nil if it is to be hidden.
Returns a list containing the position of each immediate
subheading of the current topic."
  (let ((entry-level (org-current-level))
        (sections nil))
    (org-show-subtree)
    (save-excursion
      (org-map-entries
       (lambda ()
         (when (and (not (outline-invisible-p))
                    (> (org-current-level) entry-level))
           (when (or (/= (org-current-level) (1+ entry-level))
                     (funcall test))
             (outline-hide-subtree))
           (push (point) sections)))
       t 'tree))
    (reverse sections)))

(defun org-fc-hide-subheading (name)
  "Hide all subheadings matching NAME."
  (org-fc-hide-subheadings-if
   (lambda () (string= (org-get-heading t) name))))

(defun org-fc-hide-all-subheadings-except (heading-list)
  "Hide all subheadings except HEADING-LIST."
  (org-fc-hide-subheadings-if
   (lambda () (not (member (org-get-heading t) heading-list)))))

;;;; Hiding Headline Contents

(defun org-fc-hide-content (&optional text)
  "Hide the main text of a heading *before* the first subheading.
If TEXT is non-nil, the content is replaced with TEXT."
  (let (start end)
    (save-excursion
      (org-back-to-heading)
      (forward-line)
      (setq start (point)))
    (save-excursion
      (outline-next-heading)
      (setq end (point)))
    (org-fc-hide-region start end text)))

(defun org-fc-hide-heading (&optional text)
  "Hide the title of the headline at point.
If TEXT is non-nil, the heading is replaced with TEXT."
  ;; Case sensitive search
  (let ((case-fold-search nil))
    (save-excursion
      (beginning-of-line)
      (if (looking-at org-complex-heading-regexp)
          (org-fc-hide-region (match-beginning 4) (match-end 4) (or text "..."))
        (error "Point is not on a heading")))))

;;;; Narrowing Outline Trees

(defun org-fc-narrow-tree ()
  "Narrow the outline tree.
Only parent headings of the current heading remain visible."
  (interactive)
  (save-excursion
    (org-fc-goto-entry-heading)
    (let* ((end (org-fc-overlay--point-at-end-of-previous))
           (tags (org-get-tags nil 'local))
           (notitle (member "notitle" tags))
           (noheading (member "noheading" tags))
           (el (org-element-at-point))
           (current-end (org-element-property :contents-end el)))
      (if noheading
          (org-fc-hide-heading))
      (while (org-up-heading-safe)
        (let ((start (point-at-eol))
              (end_ (org-fc-overlay--point-at-end-of-previous)))
          (if (< start end)
              (org-fc-hide-region end start))
          (setq end end_)))
      (let ((at (org-fc-overlay--point-after-title))
            (eop (org-fc-overlay--point-at-end-of-previous)))
        ;; Don't hide anything if the heading is at the beginning of the buffer
        (if eop
            (if (and at (not notitle))
                (org-fc-hide-region at eop)
              (org-fc-hide-region (point-min) eop))))
      (org-fc-hide-region current-end (point-max)))))

;;;; Footer

(provide 'org-fc-overlay)

;;; org-fc-overlay.el ends here
