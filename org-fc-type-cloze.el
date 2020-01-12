(defvar org-fc-type-cloze-max-hole-property "FC_CLOZE_MAX")
(defvar org-fc-type-cloze-type-property "FC_CLOZE_TYPE")

(defvar org-fc-type-cloze-types
  '(deletion enumeration context))

(defvar org-fc-type-cloze--overlays '())

(defvar org-fc-type-cloze-context 1
  "Number of surrounding cards to show for 'context' type cards")

(defface org-fc-type-cloze-hole-face
  '((t (:bold t)))
  "Face for org-fc cloze card holes."
  :group 'org-fc)

(defvar org-fc-type-cloze-hole-re
  (rx
   (seq
    "{{"
    (group-n 1 (* (or (seq "$" (+ (not (any "$"))) "$")
                      (not (any "}"))))) "}"
    (?  (seq "{" (group-n 2 (* (or
                                (seq "$" (not (any "$")) "$")
                                (not (any "}"))))) "}"))
    (?  "@" (group-n 3 (+ digit)))
    "}"))
  "Regexp for a cloze hole without an id.")

(defvar org-fc-type-cloze-id-hole-re
  (rx
   (seq
    "{{"
    (group-n 1 (* (or (seq "$" (+ (not (any "$"))) "$")
                      (not (any "}"))))) "}"
    (?  (seq "{" (group-n 2 (* (or
                                (seq "$" (not (any "$")) "$")
                                (not (any "}"))))) "}"))
    (seq "@" (group-n 3 (+ digit)))
    "}"))
  "Regexp for a cloze hole with an id.")

(defun org-fc-type-cloze-max-hole-id ()
  (let ((max-id (org-entry-get (point) org-fc-type-cloze-max-hole-property)))
    (if max-id
        (string-to-number max-id)
      -1)))

(defun org-fc-type-cloze-hole (deletion)
  "Generate the string used to mark the hole left by DELETION"
  (format "[%s...]" (or (plist-get deletion :hint) "")))

;; NOTE: The way parts of the hole are hidden / revealed is probably
;; unnecessarily complicated. I couldn't get latex / org text emphasis
;; to work otherwise.  If the hole has no hint, we can't use any
;; properties of match 2.
(defun org-fc-type-cloze--overlay-current ()
  "Generate a list of overlays to display the hole currently
  being reviewed."
  (if (match-beginning 2)
      (list
       :before-text
       (org-fc-hide-region hole-beg (match-beginning 1))
       :text
       (org-fc-hide-region (match-beginning 1) (match-end 1))
       :separator
       (org-fc-hide-region (match-end 1) (match-beginning 2) "[...")
       :hint
       (org-fc-overlay-region (match-beginning 2) (match-end 2))
       :after-hint
       (org-fc-hide-region (match-end 2) hole-end "]"))
    (list
     :before-text
     (org-fc-hide-region hole-beg (match-beginning 1))
     :text
     (org-fc-hide-region (match-beginning 1) (match-end 1))
     :hint
     (org-fc-hide-region (match-end 1) hole-end "[...]"))))

(defun org-fc-type-cloze-hide-holes (hole type)
  (save-excursion
    (org-fc-goto-entry-heading)
    (let* ((el (org-element-at-point))
           (end (org-element-property :contents-end el))
           (overlays nil))
      (while (re-search-forward org-fc-type-cloze-id-hole-re end t)
        (let ((text (match-string 1))
              (hint (match-string 2))
              (id (string-to-number (match-string 3)))
              (hole-beg (match-beginning 0))
              (hole-end (match-end 0)))
          (if (= hole id)
              (setq overlays (org-fc-type-cloze--overlay-current))
            (if (and (eq type 'enumeration) overlays)
                (org-fc-hide-region hole-beg hole-end "...")
              (progn
                (org-fc-hide-region hole-beg (match-beginning 1))
                (org-fc-hide-region (match-end 1) hole-end))))))
      overlays)))

(defun org-fc-type-cloze-flip ()
  (-when-let (overlays org-fc-type-cloze--overlays)
    (if (plist-member overlays :separator)
        (org-fc-hide-overlay (plist-get overlays :separator)))
    (if (plist-member overlays :after-hint)
        (org-fc-hide-overlay (plist-get overlays :after-hint)))
    (org-fc-hide-overlay (plist-get overlays :hint))
    ;; (delete-overlay (plist-get overlays :text))
    (org-fc-show-overlay
     (plist-get overlays :text)
     'org-fc-type-cloze-hole-face))
  (org-fc-review-rate-hydra/body))

(defun org-fc-type-cloze-setup (position)
  (let ((hole (string-to-number position))
        (cloze-type (intern (org-entry-get (point) org-fc-type-cloze-type-property))))
    (org-show-subtree)
    (setq
     org-fc-type-cloze--overlays
     (org-fc-type-cloze-hide-holes hole cloze-type)))
    (org-fc-review-flip-hydra/body))

(defun org-fc-type-cloze-read-type ()
  (intern
   (completing-read
    "Cloze Type: "
    org-fc-type-cloze-types)))

(defun org-fc-type-cloze-init (type)
  "Initialize the current heading for use as a cloze card of subtype TYPE.
Processes all holes in the card text."
  (interactive (list (org-fc-type-cloze-read-type)))
  (unless (member type org-fc-type-cloze-types)
    (error "Invalid cloze card type: %s" type))
  (org-fc--init-card "cloze")
  (org-fc-type-cloze-update)
  (org-set-property
   org-fc-type-cloze-type-property
   (format "%s" type)))

(defun org-fc-type-cloze-update ()
  "Update the review data & deletions of the current heading."
  (let* ((el (org-element-at-point))
         (end (org-element-property :contents-end el))
         (hole-id (1+ (org-fc-type-cloze-max-hole-id)))
         ids)
    (save-excursion
      (while (re-search-forward org-fc-type-cloze-hole-re end t)
        (let ((id (match-string 3))
              (hole-end (match-end 0)))
          (unless id
            (setq id hole-id)
            (incf hole-id 1)
            (let ((id-str (number-to-string id)))
              (incf end (+ 1 (length id-str)))
              (goto-char hole-end)
              (backward-char)
              (insert "@" id-str)))
          (push (format "%s" id) ids))))
    (org-set-property
     org-fc-type-cloze-max-hole-property
     (format "%s" (1- hole-id)))
    (org-fc-review-data-update (reverse ids))))

(org-fc-register-type
 'cloze
 'org-fc-type-cloze-setup
 'org-fc-type-cloze-flip
 'org-fc-type-cloze-update)

(provide 'org-fc-type-cloze)
