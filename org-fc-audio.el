;;; org-fc-audio.el --- Audio playback during review -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Leon Rische

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
;; Adds audio playback during review.
;;
;; Audio files can be played at different times:
;; - before the card is set up
;; - after the card is set up
;;
;; This distinction is relevant for text-input cards where the card
;; setup ends only after the user has entered their answer.
;;
;; The `mpv` media player needs to be installed for this to work.
;;
;;; Code:

(require 'org-fc)

(defcustom org-fc-audio-property-before "FC_AUDIO_BEFORE_SETUP"
  "Name of the property to use for storing before-setup audio files."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-audio-property-after "FC_AUDIO_AFTER_SETUP"
  "Name of the property to use for storing after-setup audio files."
  :type 'string
  :group 'org-fc)

(defun org-fc-audio-set-before (file)
  "Set the befor-setup audio property of the current card to FILE."
  (interactive "f")
  (if (org-fc-entry-p)
      (org-set-property org-fc-audio-property-before file)))

(defun org-fc-audio-set-after (file)
  "Set the after-setup audio of the current card to FILE."
  (interactive "f")
  (if (org-fc-entry-p)
      (org-set-property org-fc-audio-property-after file)))

(cl-defun org-fc-audio-play (property)
  "Play the audio of the current card.
Look up the file from PROPERTY."
  (if-let ((file (org-entry-get (point) property)))
      (org-fc-audio-play-file file)))

(defun org-fc-audio-play-file (file)
  "Play the audio file FILE."
  (start-process-shell-command
   "org-fc audio"
   nil
   (format "mpv %s" file)))

(add-hook
 'org-fc-before-setup-hook
 (lambda () (org-fc-audio-play org-fc-audio-property-before)))

(add-hook
 'org-fc-after-setup-hook
 (lambda () (org-fc-audio-play org-fc-audio-property-after)))

;;; Footer

(provide 'org-fc-audio)

;;; org-fc-audio.el ends here
