;;; org-fc-transient.el --- Transient for interacting with the SRS -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Samuel W. Flint

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; Package-Requires ((emacs "28.1") (org "9.3") (org-fc "0.1.0"))
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; This file contains a transient interface to interact with the
;; org-fc spaced-repetition system.

(require 'transient)
(require 'org-fc)

;;; Code:

(transient-define-prefix org-fc-transient ()
  "A transient interface to org-fc."
  [:description "Org Flashcards"
                [("d" "Dashboard" org-fc-dashboard)]
                [("r" "Start Review" org-fc-review)]
                [("u" "Update Card" org-fc-update)]]
  [:description "Initialize as..."
                :if-mode org-mode
                [("in" "Normal" org-fc-type-normal-init)]
                [("id" "Double" org-fc-type-double-init)]
                [("it" "Text Input" org-fc-type-text-input-init)]
                [("ic" "Deletion" (lambda () (interactive) (funcall-interactively #'org-fc-type-cloze-init 'deletion)))]
                [("ie" "Enum" (lambda () (interactive) (funcall-interactively #'org-fc-type-cloze-init 'enumeration)))]
                [("is" "Single" (lambda () (interactive) (funcall-interactively #'org-fc-type-cloze-init 'single)))]
                [("iC" "Context" (lambda () (interactive) (funcall-interactively #'org-fc-type-cloze-init 'context)))]]
  [:description "Audio" :if (lambda () (and (eql major-mode 'org-mode)
                                            (featurep 'org-fc-audio)))
                [("ab" "Before Anything" org-fc-audio-set-before-setup)]
                [("as" "After Setup" org-fc-audio-set-after-setup)]
                [("af" "After Flip" org-fc-audio-set-after-flip)]
                [("ap" "Play Audio" org-fc-audio-play)]])

(provide 'org-fc-transient)

;;; org-fc-transient.el ends here
