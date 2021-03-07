;;; org-fc.el --- Spaced Repetition System for Emacs org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Leon Rische

;; Author: Leon Rische <emacs@leonrische.me>
;; Url: https://www.leonrische.me/pages/org_flashcards.html
;; Package-requires: ((emacs "26.3") (org "9.3"))
;; Version: 0.1.0

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
;; A Spaced repetition system for Emacs org-mode.
;;
;;; Code:

(require 'cl-lib)
(require 'org-id)
(require 'parse-time)
(require 'subr-x)

(require 'org-fc-compat)

(require 'org-fc-awk)
(require 'org-fc-cache)
(require 'org-fc-algo-sm2)

(require 'org-fc-dashboard)
(require 'org-fc-review)

(require 'org-fc-type-normal)
(require 'org-fc-type-double)
(require 'org-fc-type-text-input)
(require 'org-fc-type-cloze)

;;; Customization

(defgroup org-fc nil
  "Manage and review flashcards with Emacs."
  :group 'external
  :group 'text)

(defcustom org-fc-directories '("~/org/")
  "Directories to search for flashcards."
  :type 'string
  :group 'org-fc)

(defvar org-fc-source-path
  (file-name-directory
   (file-truename (or load-file-name (buffer-file-name))))
  "Location of the org-fc sources.
Used to generate absolute paths to the awk scripts.")

(defcustom org-fc-review-history-file (expand-file-name "org-fc-reviews.tsv" user-emacs-directory)
  "File to store review results in."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-shuffle-positions t
  "Shuffle positions before review."
  :type 'boolean
  :group 'org-fc)

(defcustom org-fc-append-failed-cards t
  "Add failed cards to the end of the review session."
  :type 'boolean
  :group 'org-fc)

(defcustom org-fc-index-function #'org-fc-awk-index
  "Function used to index cards in a list of paths."
  :type 'function
  :group 'org-fc)

;;;; Org Tags / Properties

(defcustom org-fc-type-property "FC_TYPE"
  "Property used to store the cards type."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-created-property "FC_CREATED"
  "Property used to store the cards creation time."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-type-cloze-max-hole-property "FC_CLOZE_MAX"
  "Name of the property to use for storing the max hole index."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-suspended-tag "suspended"
  "Tag for marking suspended cards."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-flashcard-tag "fc"
  "Tag for marking headlines as flashcards."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-demo-tag "fc-demo"
  "Tag for marking headlines as demo flashcards.
When demo flashcards are reviewed, their review data is not
updated.  This is used for the `org-fc-demo' and for testing card
types."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-review-data-drawer "REVIEW_DATA"
  "Name of the drawer used to store review data."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-drawer-whitelist '()
  "Drawers that are not hidden during review."
  :type 'list
  :group 'org-fc)

(defcustom org-fc-stats-review-min-box 0
  "Minimum box for reviews to include in the review stats."
  :type 'integer
  :group 'org-fc)

;;;; Spacing Parameters

(defcustom org-fc-algorithm 'sm2-v1
  "Algorithm for spacing reviews of cards."
  :type '(choice (const sm2-v1) (const sm2-v2))
  :group 'org-fc)

(defcustom org-fc-bury-siblings nil
  "If non-nil, show at most one position of a card per review.
Does not apply to cloze single and cloze enumeration cards."
  :type 'boolean
  :group 'org-fc)

;;; Footer

(provide 'org-fc)

;;; org-fc.el ends here
