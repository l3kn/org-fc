;;; org-fc-audio.el --- Audio playback during review -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Leon Rische

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

(require 'org-fc-core)
(require 'org-fc-review)

(defcustom org-fc-audio-before-setup-property "FC_AUDIO_BEFORE_SETUP"
  "Name of the property to use for storing before-setup audio files."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-audio-after-setup-property "FC_AUDIO_AFTER_SETUP"
  "Name of the property to use for storing after-setup audio files."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-audio-after-flip-property "FC_AUDIO_AFTER_FLIP"
  "Name of the property to use for storing after-flip audio files."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-audio-before-setup-prefix "FC_AUDIO_BEFORE_SETUP"
  "Prefix of the property to use for storing before-setup audio files."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-audio-after-setup-prefix "FC_AUDIO_AFTER_SETUP"
  "Prefix of the property to use for storing after-setup audio files."
  :type 'string
  :group 'org-fc)

(defcustom org-fc-audio-after-flip-prefix "FC_AUDIO_AFTER_FLIP"
  "Prefix of the property to use for storing after-flip audio files."
  :type 'string
  :group 'org-fc)

(defvar org-fc-audio-last-file nil)

(defun org-fc-audio-set-before-setup (file)
  "Set the befor-setup audio property of the current card to FILE."
  (interactive "f")
  (when (org-fc-entry-p)
    (org-set-property org-fc-audio-before-setup-property file)))

(defun org-fc-audio-set-after-setup (file)
  "Set the after-setup audio of the current card to FILE."
  (interactive "f")
  (when (org-fc-entry-p)
    (org-set-property org-fc-audio-after-setup-property file)))

(defun org-fc-audio-set-after-flip (file)
  "Set the after-setup audio of the current card to FILE."
  (interactive "f")
  (when (org-fc-entry-p)
    (org-set-property org-fc-audio-after-flip-property file)))

(defun org-fc-audio-play (property &optional speed)
  "Play the audio of the current card.
Look up the file from PROPERTY. If SPEED is non-nil, play back
the file at the given speed."
  (interactive
   (list
    (completing-read
     "Type: "
     (let ((props (mapcar #'car (org-entry-properties))))
       (remove-if-not
        (lambda (prop)
          (or
           (string= org-fc-audio-before-setup-property prop)
           (string= org-fc-audio-after-setup-property prop)
           (string= org-fc-audio-after-flip-property prop)
           (string-prefix-p org-fc-audio-before-setup-prefix prop)
           (string-prefix-p org-fc-audio-after-setup-prefix prop)
           (string-prefix-p org-fc-audio-after-flip-prefix prop)))
        props)))))
  (if-let ((file (org-entry-get (point) property)))
      (org-fc-audio-play-file file (or speed 1.0))))

(defun org-fc-audio-play-file (file speed)
  "Play the audio FILE at SPEED."
  (setq org-fc-audio-last-file file)
  (start-process-shell-command
   "org-fc audio"
   nil
   (format "mpv %s --speed=%f" file speed)))

(defun org-fc-audio-play-position (prefix)
  "Play the audio file for PREFIX and the current position."
  (org-fc-review-with-current-item current-item
    (when current-item
      (let* ((pos (plist-get current-item :position))
             (property (format "%s_%s" prefix (upcase pos))))
        (org-fc-audio-play property)))))

(add-hook
 'org-fc-before-setup-hook
 (lambda ()
   (org-fc-audio-play org-fc-audio-before-setup-property)
   (org-fc-audio-play-position org-fc-audio-before-setup-prefix)))

(add-hook
 'org-fc-after-setup-hook
 (lambda ()
   (org-fc-audio-play org-fc-audio-after-setup-property)
   (org-fc-audio-play-position org-fc-audio-after-setup-prefix)))

(add-hook
 'org-fc-after-flip-hook
 (lambda ()
   (org-fc-audio-play org-fc-audio-after-flip-property)
   (org-fc-audio-play-position org-fc-audio-after-flip-prefix)))

(defun org-fc-audio-replay ()
  (interactive)
  (when org-fc-audio-last-file
    (org-fc-audio-play-file org-fc-audio-last-file 1.0)))

(defun org-fc-audio-replay-slow ()
  (interactive)
  (when org-fc-audio-last-file
    (org-fc-audio-play-file org-fc-audio-last-file 0.7)))

;;; Footer

(provide 'org-fc-audio)

;;; org-fc-audio.el ends here
