;;; org-fc-hydra.el --- Hydra for interacting with the SRS -*- lexical-binding: t; -*-

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
;; This file contains an example for setting up a hydra to interact
;; with the flashcard system.
;;
;;; Code:

(require 'org-fc)

(defhydra org-fc-hydra ()
  ("m" org-fc-dashboard "Dashboard" :exit t)
  ("r" org-fc-review "Start Review")
  ("u" org-fc-update "Update Card")
  ("t" org-fc-hydra-type/body "Init Type" :exit t)
  ("q" nil "Quit" :exit t))

(defhydra org-fc-hydra-type ()
  ("n" org-fc-type-normal-init "Normal" :exit t)
  ("d" org-fc-type-double-init "Double" :exit t)
  ("t" org-fc-type-text-input-init "Text Input" :exit t)
  ("c" (org-fc-type-cloze-init 'deletion) "Deletion" :exit t)
  ("e" (org-fc-type-cloze-init 'enumeration) "Enum" :exit t)
  ("s" (org-fc-type-cloze-init 'single) "Single" :exit t)
  ("x" (org-fc-type-cloze-init 'context) "Context" :exit t)
  ("q" nil "Quit" :exit t))

;;;; Footer

(provide 'org-fc-hydra)

;;; org-fc-hydra.el ends here
