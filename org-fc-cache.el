;;; org-fc-cache.el --- Cache for org-fc -*- lexical-binding: t; -*-

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
;; Even with the AWK based indexer, indexing cards before each review
;; gets slow if there are a lot of files / cards.
;;
;; After running the indexer one time, file checksums are used to
;; determine which cache entries need to be updated, assuming only a
;; small subset of the flashcard files is changed between reviews,
;; this is much faster than building the full index each time.

;;; Code:

(require 'org-fc-core)

;;; Queue / Processing of Files

(defvar org-fc-cache
  (make-hash-table :test #'equal)
  "Cache mapping filenames to card lists.")

(defun org-fc-cache-update ()
  "Make sure the cache is up to date."
  (let* ((hashes (org-fc-cache-hashes org-fc-directories))
         (changed
          (cl-remove-if
           (lambda (file)
             (string=
              (plist-get (gethash file org-fc-cache) :hash)
              (gethash file hashes)))
           (hash-table-keys hashes))))
    ;; Update changed files
    (dolist (new (org-fc-awk-index-files changed))
      (let* ((path (plist-get new :path))
             (hash (gethash path hashes)))
        (puthash
         path
         (plist-put new :hash hash)
         org-fc-cache)))
    ;; Remove deleted files
    (dolist (file (hash-table-values org-fc-cache))
      (unless (gethash file hashes)
        (remhash file org-fc-cache)))))

;;; Filtering Entries

(defun org-fc-cache-index (paths &optional filter)
  "Find cards in PATHS matching an optional FILTER.
FILTER is assumed to be a predicate function taking a single card
as its input."
  (org-fc-cache-update)
  ;; Make sure paths are absolute & canonical
  ;; Keys of the hash table can be assumed to be absolute & canonical.
  (setq paths (mapcar #'expand-file-name paths))
  (let (res)
    (maphash
     (lambda (path file)
       (when (cl-some (lambda (p) (string-prefix-p p path)) paths)
         ;; Use push instead of `nconc' because `nconc' would break
         ;; the entries of the hash table.
         (if filter
             (dolist (card (cl-remove-if-not filter (plist-get file :cards)))
               (push (plist-put
                      (plist-put card :path path)
                      :filetitle
                      (plist-get file :title)) res))
           (dolist (card (plist-get file :cards))
             (push
              (plist-put
               (plist-put card :path path)
               :filetitle
               (plist-get file :title)) res)))))
     org-fc-cache)
    res))

;; TODO: Check for awk errors
;; TODO: This should go into the awk file
(defun org-fc-awk-index-files (files)
  "Generate a list of all cards and positions in FILES.
Unlike `org-fc-awk-index-paths', files are included directly in
the AWK command and directories are not supported."
  (mapcar
   (lambda (file)
     (plist-put file :cards
                (mapcar
                 (lambda (card)
                   (plist-put
                    card :tags
                    (org-fc-awk-combine-tags
                     (plist-get card :inherited-tags)
                     (plist-get card :local-tags))))
                 (plist-get file :cards))))
   (read
    (shell-command-to-string
     (org-fc-awk--command
      "awk/index.awk"
      :variables (org-fc-awk--indexer-variables)
      :input (mapconcat #'identity files " "))))))

;;; Cache Mode

(defun org-fc-cache--enable ()
  "Enable org-fc-cache.
Initializes the cache and adds hooks."
  (message "building org-fc cache...")
  (org-fc-cache-update)
  (add-hook 'org-fc-before-setup-hook #'org-fc-cache-coherence-check)
  (setq org-fc-index-function #'org-fc-cache-index)
  (message "org-fc cache enabled"))

(defun org-fc-cache--disable ()
  "Disable org-fc-cache.
Resets the cache and removes hooks."
  (setq org-fc-cache (make-hash-table :test #'equal))
  (remove-hook 'org-fc-before-setup-hook #'org-fc-cache-coherence-check)
  (setq org-fc-index-function #'org-fc-awk-index)
  (message "org-fc cache disabled"))

(define-minor-mode org-fc-cache-mode
  "Minor mode for caching org-fc card data.

This mode sets up several hooks to ensure the case updated when files change,
are renamed or deleted."
  :lighter "org-fc cache"
  :group 'org-fc
  :require 'org-fc
  :global t
  (if org-fc-cache-mode
      (org-fc-cache--enable)
      (org-fc-cache--disable)))

;;; Coherence Check

;; TODO: There already is a similar check in org-fc,
;; those should be combined.

;;;###autoload
(defun org-fc-cache-coherence-check ()
  "Check if the entry at point is coherent with its cache representation.
This is especially relevant w.r.t a card's due date / suspension state before review."
  (org-fc-review-with-current-item cur
    (if (org-fc-suspended-entry-p)
        (error "Trying to review a suspended card"))
    (let* ((position (plist-get cur :position))
           (review-data (org-fc-get-review-data))
           (row (assoc position review-data #'string=))
           (due (parse-iso8601-time-string (nth 4 row))))
      (unless (time-less-p due (current-time))
        (error "Trying to review a non-due card")))))

;;; Hashing

(defun org-fc-cache-hashes (directories)
  "Compute hashsums of all org files in DIRECTORIES."
  (let ((output (shell-command-to-string
                 (org-fc-awk--pipe
                  (org-fc-awk--find directories)
                  (org-fc-awk--xargs "sha1sum"))))
        (table (make-hash-table :test #'equal)))
    (dolist (line (split-string output "\n" t))
      (let ((parts (split-string line "  ")))
        (puthash (cadr parts) (car parts) table)))
    table))

;;; Footer

(provide 'org-fc-cache)

;;; org-fc-cache.el ends here
