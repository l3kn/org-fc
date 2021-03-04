;;; org-fc-type-vocab.el --- Card type for learning vocabulary -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Leon Rische

;; Author: Leon Rische <emacs@leonrische.me>
;; Url: https://www.leonrische.me/pages/org_flashcards.html
;; Package-requires: ((emacs "26.3") (org "9.3"))
;; Version: 0.0.1

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
;; Cards of this type should have the (foreign language) word as their
;; heading with a definition on the back.
;;
;; During review, the user is prompted for the definition of the word
;; or asked to type in the word based on its definitions.
;;
;;; Code:

(require 'org-fc-audio)

(defcustom org-fc-type-vocab-slow-speed 0.7
  "Speed to use for slow playback."
  :type 'number
  :group 'org-fc)

(defcustom org-fc-type-vocab-audio-property "FC_VOCAB_AUDIO"
  "Property with path to audio file."
  :type 'string
  :group 'org-fc)

(defun org-fc-type-vocab-init ()
  "Mark headline as card of the vocab type."
  (interactive)
  (org-fc--init-card "vocab")
  (org-fc-review-data-update '("front" "back")))

(defun org-fc-type-vocab-setup (position)
  "Prepare POSITION of a vocab card for review."
  (pcase position
    ("front"
     (org-fc-audio-play org-fc-type-vocab-audio-property)
     (org-fc-type-normal-setup position))
    ("back"
     (org-fc-type-vocab-typing-setup)
     (org-fc-audio-play org-fc-type-vocab-audio-property)
     'rate)
    (_ (error "Invalid vocab position %s" position))))

(defun org-fc-type-vocab-flip ()
  "Flip a vocab card."
  (org-fc-type-normal-flip))

(defun org-fc-type-vocab-play ()
  "Play vocab audio file at normal speed."
  (interactive)
  (org-fc-audio-play org-fc-type-vocab-audio-property))

(defun org-fc-type-vocab-play-slow ()
  "Play vocab audio file at slow speed."
  (interactive)
  (org-fc-audio-play org-fc-type-vocab-audio-property org-fc-type-vocab-slow-speed))

(defun org-fc-vocab-content ()
  "Heading position & text as a (pos . string) pair."
  (save-excursion
    (org-fc-goto-entry-heading)
    (let ((case-fold-search nil))
      (if (looking-at org-complex-heading-regexp)
          (cons
           (match-beginning 4)
           (buffer-substring-no-properties (match-beginning 4) (match-end 4)))))))

(defun org-fc-type-vocab-typing-setup ()
  "Prepare a text-input vocab card for review."
  (interactive)
  (org-show-subtree)
  (let* ((pos-content (org-fc-vocab-content))
         (content (cdr pos-content))
         (start (car pos-content))
         (end (+ start (length content)))
         (ov (org-fc-hide-region start end "..."))
         (deemph (org-fc-deemphasize content))
         (diff (org-fc-diff (read-string "Answer: ") (cdr deemph))))
    (delete-overlay ov)
    ;; Overlay for user input
    (when (car deemph)
      (setq start (1+ start))
      (setq end (1- end)))
    (org-fc-hide-region start end (car diff))
    ;; Overlay for expected answer, using the newline after the answer
    (if (cdr diff)
        (org-fc-hide-region
         end (1+ end)
         (concat
          "\n! "
          (if (null (car deemph))
              (cdr diff)
            (org-fc-emphasize
             (concat (car deemph) (cdr diff) (car deemph))))
          "")))))

(org-fc-register-type
 'vocab
 'org-fc-type-vocab-setup
 'org-fc-type-vocab-flip
 'org-fc-noop)

;;; Footer

(provide 'org-fc-type-vocab)

;;; org-fc-type-vocab.el ends here
