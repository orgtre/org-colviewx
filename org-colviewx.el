;;; org-colviewx.el --- Extensions for org-colview -*- lexical-binding: t; -*-

;; Copyright (C) 2023 orgtre

;; Author: orgtre
;; URL: https://github.com/orgtre/org-colviewx

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains some extensions and configuration for the column
;; view for Org (org-colview.el).

;;; Code:

;; * Setup

(require 'org-colview)

(defgroup org-colviewx nil
  "Extensions for org-colview.el."
  :group 'org-properties)

(defcustom org-colviewx-filter-presets nil
  "Presets for `org-colviewx-filter'."
  :type '(repeat string))

(defcustom org-columns-separator
  (propertize " " 'face
                      (list
                       ;;:inherit 'org-level-1
                       ;;:foreground "gray90"
                       :background "gray80"
                       ;; :background "#398eac"
                       ;; (face-attribute 'escape-glyph :foreground)
                       ;; :underline 'unspecified
                       ;;:inverse-video t
                       :family "Arial"
                       ;;:inherit 'variable-pitch
                       ))
  "Separator to use between columns.

Suggested values include \"|\", space (\" \"), thin space (\" \"),
and hair space (\" \") (default), with suitable text properties.

Thin/hair space can be inserted using `insert-char'. To appear thin they
need to have a variable-width face family (Arial makes them very thin).

Moreover, note that they are displayed using the hardcoded face
`nobreak-space' which specifies an underline and a foreground color
inherited from face `escape-glyph'. Overriding these properties does
not work (?), but one can either change these faces directly or set the
text properties of `org-columns-separator' to fit with their default
values."
  :group 'org-properties
  :type 'string)


(defface org-colviewx-link
  '((t :inherit org-link
       :underline nil))
  "Like org-link but without underline.")

;; #TODO this is not in effect after switch to dark mode
(add-hook 'org-colviewx-hook
          (lambda ()
            (set-face-attribute 'nobreak-space nil :underline 'unspecified)
            (set-face-attribute 'nobreak-space t :underline 'unspecified)))

;; (set-face-attribute 'escape-glyph nil :foreground "gray90")
;; (set-face-attribute 'escape-glyph t :foreground "gray90")

(set-face-attribute 'org-column nil :background 'unspecified)
(set-face-attribute 'org-column t :background 'unspecified)
(set-face-attribute 'org-column-title nil
                    :background 'unspecified
                    ;;:inherit 'org-level-1
                    :underline nil)
(set-face-attribute 'org-column-title t :background 'unspecified)


(defconst org-colviewx-offset 0
  "Number of columns to move right before placing overlay.")


;; * org-column hooks

(defvar org-colviewx-hook nil
  "Hook for functions attaching themselves to `org-columns'.")

(defvar org-colviewx-quit-hook nil
  "Hook for functions attaching themselves to `org-columns-quit'.")

(defun org-colviewx-org-columns-advice (&rest _r)
  "Run `org-colviewx-hook'."
  (run-hooks 'org-colviewx-hook))

(advice-add 'org-columns :after #'org-colviewx-org-columns-advice)

(defun org-colviewx-org-columns-quit-advice (&rest _r)
  "Run `org-colviewx-quit-hook'."
    (run-hooks 'org-colviewx-quit-hook))

(advice-add 'org-columns-quit :after #'org-colviewx-org-columns-quit-advice)


;; * Minor mode

(defvar org-colviewx-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Navigate
    (define-key map (kbd "C-c C-n")
                #'org-colviewx-next-item)
    (define-key map (kbd "C-c C-p")
                #'org-colviewx-previous-item)
    map)
  "Keymap for `org-colviewx-minor-mode'.")


(define-minor-mode org-colviewx-minor-mode
  "Minor mode for Org column view extensions."
  :keymap org-colviewx-minor-mode-map
  (if org-colviewx-minor-mode
      (org-colviewx-setup)
    (org-colviewx-teardown)))


(defun org-colviewx-setup ()
  "Setup org-colviewx."
  ;; disable speed commands as they conflict with org-columns:
  (org-colviewx-toggle-top 1)
  (setq-local org-use-speed-commands nil)
  ;; disable as this behaves strangely in column view:
  (setq-local org-special-ctrl-a/e (cons nil t))
  (org-fold-core-add-folding-spec 'org-colviewx-filter '((:isearch-ignore . t)))
  ;;(org-colviewx-reveal-all 'org-colviewx-filter)
  )


(defun org-colviewx-reveal-all (spec)
  "Reveal all text with folding specifcation SPEC."
  (org-fold-core-region (point-min)
                        (point-max)
                        nil spec))


(defun org-colviewx-teardown ()
  "Teardown org-colviewx."
  (org-colviewx-toggle-top -1)
  (kill-local-variable 'org-use-speed-commands)
  (kill-local-variable 'org-special-ctrl-a/e)
  (org-colviewx-reveal-all 'org-colviewx-filter)
  (let ((inhibit-read-only t))
    (remove-list-of-text-properties (point-min) (point-max) (list 'display))))


(add-hook 'org-colviewx-hook #'org-colviewx-minor-mode)
(add-hook 'org-colviewx-quit-hook (lambda () (org-colviewx-minor-mode -1)))


;; * Navigating


(defun org-colviewx-forward ()
  "Move point one cell right.
Ensure it stays in the column view table."
  (interactive)
  (when (> (+ (length org-columns-current-fmt-compiled) org-colviewx-offset)
           (1+ (org-current-text-column)))
    (forward-char)))


(defun org-colviewx-backward ()
  "Move point one cell left.
Ensure it stays in the column view table."
  (interactive)
  (unless (<= (current-column) org-colviewx-offset)
    (backward-char)))


(defvar org-colviewx-last-column nil
  "Stores the last column view column of point.")


(defun org-colviewx-next-item ()
  "Move point one item (row) down.
Ensure it stays in the column view table and in the same column."
  (interactive)
  (when (get-char-property (point) 'org-columns-key)
    (setq org-colviewx-last-column (org-current-text-column)))
  (let ((pos (point)))
    (call-interactively 'org-next-visible-heading)
    (if (and (bolp) (org-at-heading-p))
        (forward-char org-colviewx-last-column)
      (goto-char pos))))


(defun org-colviewx-previous-item ()
  "Move point one item (row) up.
Ensure it stays in the column view table and in the same column."
  (interactive)
  (when (get-char-property (point) 'org-columns-key)
    (setq org-colviewx-last-column (org-current-text-column)))
  (let ((pos (point)))
    (call-interactively 'org-previous-visible-heading)
    (if (and (bolp) (org-at-heading-p))
        (forward-char org-colviewx-last-column)
      (goto-char pos))))


(defun org-colviewx-beginning-of-contents (&optional end)
  "Go to first non-whitespace character of entry content.
Skips meta-data. With END go to last non-whitespace character instead."
  (interactive "P")
  (org-end-of-meta-data t)
  (if (org-at-heading-p)
      (forward-line -1)
    (when end
      (if (re-search-forward "[ \n]*\n\\*" nil t)
          (goto-char (match-beginning 0))
        (org-end-of-subtree)
        (re-search-backward "[^ \n]")
        (goto-char (match-end 0)))))
  (org-fold-show-context))


(defun org-colviewx-save-column (&rest _r)
  (setq org-colviewx-last-column (org-current-text-column)))


(defun org-colviewx-goto-last-column (&rest _r)
  (when org-colviewx-last-column
    (move-to-column org-colviewx-last-column)))


(defun org-colviewx-beginning-of-contents+ (&optional end)
  (interactive)
  (org-colviewx-save-column)
  (org-colviewx-beginning-of-contents end))


;; * Viewing

(defun org-colviewx-toggle-top (&optional arg)
  "Toggle visibility of the top of the buffer.

The top refers to the part before the first headline and it is
hidden using `narrow-to-region'. When revealing the top, scroll
the window down the number of lines revealed or as far as allowed
without moving point.

Unconditionally hide the top when ARG is larger than zero;
unconditionally reveal it when ARG is smaller than zero."
  (interactive)
  (if (and (> (point-min) 1)
           (or (not arg) (< arg 0)))
      (let ((revealed-lines (count-screen-lines 1 (point-min))))
        (widen)
        (save-excursion
          (scroll-down
           (min revealed-lines
                (- (window-text-height)
                   (count-screen-lines (point) (window-start))
                   scroll-margin 2)))))
    (when (and (= (point-min) 1)
               (or (not arg) (> arg 0)))
      (narrow-to-region (save-excursion
                          (goto-char 1)
                          ;; make sure we include invisible headings in fold
                          (let ((invisible-heading t))
                            (while invisible-heading
                              (re-search-forward org-outline-regexp-bol nil t)
                              (setq invisible-heading
                                    (org-fold-core-folded-p))))
                          (pos-bol))
                        (point-max)))))


(defvar org-colviewx-entry-folded-at-last-toggle-drawer t
  "Used by `org-colviewx-entry-toggle-drawer'.")


(defun org-colviewx-entry-toggle-drawer (&optional arg)
  "Toggle visibility of the property drawer of entry at point.

The entry itself is always unfolded, but the drawer is only
unfolded if that is required for toggling its visbility.

When this command is called twice in a row and the entry was
folded at the first of these calls, the entry is folded again
on the second call.

When ARG is `off' always reveal the drawer.
When ARG is any other non-nil value, hide it.
When called interactively one `\\[universal-argument]' prefix
sets ARG to t, while two set it to `off'."
  (interactive "P")
  (when (called-interactively-p 'any)
    (cond
     ((equal arg '(4)) (setq arg t))
     ((equal arg '(16)) (setq arg 'off))))
  (save-excursion
    (org-back-to-heading)
    (let ((h-folded-p (org-fold-folded-p (line-end-position)))
          (d-pos (car (org-get-property-block))))
      (when h-folded-p
        (org-fold-heading nil t))
      (when d-pos
        (goto-char d-pos)
        (left-char)
        (cond
         (h-folded-p
          (org-fold-hide-drawer-toggle (or arg 'off)))
         ((not h-folded-p)
          (if (and (equal last-command 'org-colviewx-entry-toggle-drawer)
                   org-colviewx-entry-folded-at-last-toggle-drawer)
              (org-fold-hide-entry)
            (org-fold-hide-drawer-toggle (or arg nil))))))
      (setq org-colviewx-entry-folded-at-last-toggle-drawer
            h-folded-p))))


;; ** Connected vertical divider lines

;;#TODO ideally this patch would be integrated into org-colview.el

;;#TODO set this in the proper place
;; (set-face-attribute 'nobreak-space nil :underline nil)
;; (set-face-attribute 'nobreak-space nil :inherit 'org-level-1)
;; hair/thin space displays with the hardcoded face nobreak-space
;; so we need to adapt it by removing the underline and changing the color


(defun org-columns--display-here-title ()
  "Overlay the newline before the current line with the table title."
  (interactive)
  (let ((title (make-string org-colviewx-offset ? ))
	(linum-offset (org-line-number-display-width 'columns))
        (space (propertize " " 'face 'org-column-title))
	(i 0))
    (dolist (column org-columns-current-fmt-compiled)
      (pcase column
	(`(,property ,name . ,_)
	 (let* ((width (aref org-columns-current-maxwidths i))
		(fmt (format "%%-%d.%ds " width width)))
	   (setq title (concat title
                               (org-add-props
                                   (format fmt (or name property))
                                   nil 'face 'org-column-title)
                               org-columns-separator space)))))
      (cl-incf i))
    (setq-local org-previous-header-line-format header-line-format)
    (setq org-columns-full-header-line-format
	  (concat
	   (org-add-props space nil
             'display `(space :align-to ,linum-offset))
           (substring title 0 -1)))
    (setq org-columns-previous-hscroll -1)
    (add-hook 'post-command-hook #'org-columns-hscroll-title nil 'local)))


(defun org-columns--display-here (columns &optional dateline)
  "Overlay the current line with column display.
COLUMNS is an alist (SPEC VALUE DISPLAYED).  Optional argument
DATELINE is non-nil when the face used should be
`org-agenda-column-dateline'."
  (when (and (ignore-errors (require 'face-remap))
             org-columns-header-line-remap)
    (setq org-columns-header-line-remap
	  (face-remap-add-relative 'header-line '(:inherit default))))
  (save-excursion
    (beginning-of-line)
    ;; (put-text-property (progn (goto-char (pos-bol))(point))
    ;;                    (progn (re-search-forward "\\*+ ")
    ;;                           (backward-char)(point))
    ;;                    'display "")
    ;;(forward-char org-colviewx-offset)
    (let* ((level-face (and (looking-at "\\(\\**\\)\\(\\* \\)")
			    (org-get-level-face 2)))
	   (ref-face (or level-face
			 (and (eq major-mode 'org-agenda-mode)
			      (org-get-at-bol 'face))
			 'default))
	   (color (list :foreground (face-attribute ref-face :foreground)))
	   (font (list :family (face-attribute 'default :family)))
	   (face (list color font 'org-column ref-face))
	   (face1 (list color font 'org-agenda-column-dateline ref-face)))
      ;; Each column is an overlay on top of a character.  So there has
      ;; to be at least as many characters available on the line as
      ;; columns to display.
      (let ((columns (length org-columns-current-fmt-compiled))
	    (chars (- (line-end-position)
                      (- (line-beginning-position) org-colviewx-offset))))
	(when (> columns chars)
	  (save-excursion
	    (end-of-line)
	    (let ((inhibit-read-only t))
	      (insert (make-string (- columns chars) ?\s))))))
      ;; Display columns.  Create and install the overlay for the
      ;; current column on the next character.
      (let ((i 0)
	    (last (1- (length columns))))
	(dolist (column columns)
	  (pcase column
	    (`(,spec ,original ,value)
	     (let* ((property (car spec))
		    (width (aref org-columns-current-maxwidths i))
		    (fmt (format (if (= i last) "%%-%d.%ds %s"
				   "%%-%d.%ds %s ")
				 width width org-columns-separator))
		    (ov (org-columns--new-overlay
			 (point) (1+ (point))
			 (org-columns--overlay-text
			  value fmt width property original)
			 (if dateline face1 face))))
	       (overlay-put ov 'keymap org-columns-map)
	       (overlay-put ov 'org-columns-key property)
	       (overlay-put ov 'org-columns-value original)
	       (overlay-put ov 'org-columns-value-modified value)
	       (overlay-put ov 'org-columns-format fmt)
	       (overlay-put ov 'line-prefix "")
	       (overlay-put ov 'wrap-prefix "")
	       (forward-char))))
	  (cl-incf i)))
      ;; Make the rest of the line disappear.
      (let ((ov (org-columns--new-overlay (point) (line-end-position))))
	(overlay-put ov 'invisible t)
	(overlay-put ov 'keymap org-columns-map)
	(overlay-put ov 'line-prefix "")
	(overlay-put ov 'wrap-prefix ""))
      (let ((ov (make-overlay (1- (line-end-position))
			      (line-beginning-position 2))))
	(overlay-put ov 'keymap org-columns-map)
	(push ov org-columns-overlays))
      (with-silent-modifications
	(let ((inhibit-read-only t))
	  (put-text-property
	   (line-end-position 0)
	   (line-beginning-position 2)
	   'read-only
	   (substitute-command-keys
	    (concat "Type \\<org-columns-map>`\\[org-columns-edit-value]'"
                    " to edit property"))))))))


;; * Transforming

(defun org-colviewx-transform-links (_column-title value)
  "Transforms VALUEs containing Org links."
  (when (and (string-prefix-p "[[" value)
             (string-suffix-p "]]" value))
    (string-match org-link-bracket-re value)
    (let ((target (match-string 1 value))
          (descr (match-string 2 value)))
      (setq value (or descr target))
      ;; put the suitable faces:
      (put-text-property 0 (length value) 'face 'org-link value)
      (put-text-property 0 (length value) 'mouse-face 'highlight value)
      ;; help echo:
      (put-text-property 0 (length value) 'help-echo
                         (format "LINK: %s" target) value)
      ;; put a local keymap:
      (put-text-property 0 (length value) 'keymap
                         '(keymap (mouse-1 . org-columns-open-link)) value)
      value)))


(defconst org-colviewx-string-number-regex
  (concat "^[+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\(?:e[+-]?[0-9]+\\)?"
          "\\|[.][0-9]+\\(?:e[+-]?[0-9]+\\)?\\)$")
  "Matches integers and floats with exponent.
This allows for leading and trailing decimal point, leading zeros in base,
leading zeros in exponent, and + signs.")


(defun org-colviewx-transform-numbers (column-title value)
  "Transforms all numbers to align right."
  ;; TODO use (format "% 6d" 10)
  (when (string-match-p org-colviewx-string-number-regex value)
    (put-text-property 0 (length value) 'face 'org-property-value value)
    (let ((pos
           (cl-position column-title org-columns-current-fmt-compiled
                        :test (lambda (x y) (equal x (car y))))))
      (when-let
          ((target-width
            (or (nth 2 (nth pos org-columns-current-fmt-compiled))
                (aref org-columns-current-maxwidths pos)))
           (vlength
            (length value)))
        (when (< vlength target-width)
          (concat
           (make-string (- target-width
                           vlength)
                        ? )
           value))))))


(defun org-colviewx-transform-content (column-title value)
  (when (equal column-title "CONTENT")
    ;;#TODO at some calls to org-columns this somehow
    ;; leads to the error that point is before the first heading
    ;;(org-back-to-heading-or-point-min)
    (org-back-to-heading)
    (org-end-of-meta-data)
    (unless (org-at-heading-p)
      (when (re-search-forward "[ \n]*\\([^ \n].*\\)"
                               (org-entry-end-position) t)
        (setq value (match-string-no-properties 1))
        ;; (put-text-property 0 (length value)
        ;;                    'face 'org-property-value value)
        ;; (put-text-property 0 (length value) 'keymap
        ;;                    '(keymap (?e . org-columns-open-link)) value)
        value))))


(defun org-colviewx-display-transformer (column-title value)
  "Modifies the value to display in column view."
  (or
   ;; (org-colviewx-transform-links column-title value)
   ;; (org-colviewx-transform-numbers column-title value)
   ;;(org-colviewx-transform-content column-title value)
   ))

(setq org-columns-modify-value-for-display-function
      #'org-colviewx-display-transformer)


;; * Sorting

(defun org-colviewx-sort (&optional arg)
  "Sort entries from column view by value in current column.

This only sorts the children of the column view top level,
which is the entry with the current column view specification.

When ARG is non-nil (interactively with prefix), sort in
reverse order."
  (interactive "P")
  (let ((colname (get-char-property (point) 'org-columns-key))
        (colnum (current-column))
        (inhibit-read-only t)
        (top-folded (> (point-min) 1)))
    (when top-folded
      (org-colviewx-toggle-top -1))
    (org-with-wide-buffer
     (org-columns-goto-top-level)
     ;;(org-sort-entries nil (if arg ?R ?r) nil nil colname)
     (org-colviewx-sort-entries nil (if arg ?F ?f)
                                (lambda () (org-entry-get nil colname))
                                #'org-colviewx-sort-function)
     ;;#TODO try sorting without removing overlays
     (org-columns)
     (outline-hide-sublevels
      (org-current-level)))
    (when top-folded
      (org-colviewx-toggle-top 1))
    (move-to-column colnum)))


(defun org-colviewx-sort-entries
    (&optional with-case sorting-type getkey-func compare-func property
	       interactive?)
  "Sort entries on a certain level of an outline tree.

This is `org-sort-entries' with the line:
(or (org-at-heading-p) (outline-next-heading))
changed to:
(or (org-at-heading-p) (outline-next-visible-heading 1))
Wihout this modification, `org-sort-entries' fails to sort if the first
heading is invisible with folding spec `org-colviewx-filter'.

See `org-sort-entries' for a description of the arguments
WITH-CASE, SORTING-TYPE, GETKEY-FUNC, COMPARE-FUNC, PROPERTY,
and INTERACTIVE?."
  (interactive (list current-prefix-arg nil nil nil nil t))
  (let ((case-func (if with-case 'identity 'downcase))
        start beg end stars re re2
        txt what tmp)
    ;; Find beginning and end of region to sort
    (cond
     ((org-region-active-p)
      ;; we will sort the region
      (setq end (region-end)
            what "region")
      (goto-char (region-beginning))
      (unless (org-at-heading-p) (outline-next-heading))
      (setq start (point)))
     ((or (org-at-heading-p)
          (ignore-errors (progn (org-back-to-heading) t)))
      ;; we will sort the children of the current headline
      (org-back-to-heading)
      (setq start (point)
	    end (progn (org-end-of-subtree t t)
		       (or (bolp) (insert "\n"))
		       (when (>= (org-back-over-empty-lines) 1)
			 (forward-line 1))
		       (point))
	    what "children")
      (goto-char start)
      (org-fold-show-subtree)
      (outline-next-heading))
     (t
      ;; we will sort the top-level entries in this file
      (goto-char (point-min))
      (or (org-at-heading-p) (outline-next-visible-heading 1))
      (setq start (point))
      (goto-char (point-max))
      (beginning-of-line 1)
      (when (looking-at ".*?\\S-")
	;; File ends in a non-white line
	(end-of-line 1)
	(insert "\n"))
      (setq end (point-max))
      (setq what "top-level")
      (goto-char start)
      (org-fold-show-all '(headings drawers blocks))))

    (setq beg (point))
    (when (>= beg end) (goto-char start) (user-error "Nothing to sort"))

    (looking-at "\\(\\*+\\)")
    (setq stars (match-string 1)
	  re (concat "^" (regexp-quote stars) " +")
	  re2 (concat "^" (regexp-quote (substring stars 0 -1)) "[ \t\n]")
	  txt (buffer-substring beg end))
    (unless (equal (substring txt -1) "\n") (setq txt (concat txt "\n")))
    (when (and (not (equal stars "*")) (string-match re2 txt))
      (user-error "Region to sort contains a level above the first entry"))

    (unless sorting-type
      (message
       "Sort %s: [a]lpha  [n]umeric  [p]riority  p[r]operty  todo[o]rder  [f]unc
               [t]ime [s]cheduled  [d]eadline  [c]reated  cloc[k]ing
               A/N/P/R/O/F/T/S/D/C/K means reversed:"
       what)
      (setq sorting-type (read-char-exclusive)))

    (unless getkey-func
      (and (= (downcase sorting-type) ?f)
	   (setq getkey-func
		 (or (and interactive?
			  (org-read-function
			   "Function for extracting keys: "))
		     (error "Missing key extractor")))))

    (and (= (downcase sorting-type) ?r)
	 (not property)
	 (setq property
	       (completing-read "Property: "
				(mapcar #'list (org-buffer-property-keys t))
				nil t)))

    (when (member sorting-type '(?k ?K)) (org-clock-sum))
    (message "Sorting entries...")

    (save-restriction
      (narrow-to-region start end)
      (let ((restore-clock?
	     ;; The clock marker is lost when using `sort-subr'; mark
	     ;; the clock with temporary `:org-clock-marker-backup'
	     ;; text property.
	     (when (and (eq (org-clocking-buffer) (current-buffer))
			(<= start (marker-position org-clock-marker))
			(>= end (marker-position org-clock-marker)))
	       (with-silent-modifications
		 (put-text-property (1- org-clock-marker) org-clock-marker
				    :org-clock-marker-backup t))
	       t))
	    (dcst (downcase sorting-type))
	    (case-fold-search nil)
	    (now (current-time)))
        (org-preserve-local-variables
	 (sort-subr
	  (/= dcst sorting-type)
	  ;; This function moves to the beginning character of the
	  ;; "record" to be sorted.
	  (lambda nil
	    (if (re-search-forward re nil t)
		(goto-char (match-beginning 0))
	      (goto-char (point-max))))
	  ;; This function moves to the last character of the "record" being
	  ;; sorted.
	  (lambda nil
	    (save-match-data
	      (condition-case nil
		  (outline-forward-same-level 1)
		(error
		 (goto-char (point-max))))))
	  ;; This function returns the value that gets sorted against.
	  (lambda ()
	    (cond
	     ((= dcst ?n)
	      (string-to-number
	       (org-sort-remove-invisible (org-get-heading t t t t))))
	     ((= dcst ?a)
	      (funcall case-func
		       (org-sort-remove-invisible (org-get-heading t t t t))))
	     ((= dcst ?k)
	      (or (get-text-property (point) :org-clock-minutes) 0))
	     ((= dcst ?t)
	      (let ((end (save-excursion (outline-next-heading) (point))))
		(if (or (re-search-forward org-ts-regexp end t)
			(re-search-forward org-ts-regexp-both end t))
		    (org-time-string-to-seconds (match-string 0))
		  (float-time now))))
	     ((= dcst ?c)
	      (let ((end (save-excursion (outline-next-heading) (point))))
		(if (re-search-forward
		     (concat "^[ \t]*\\[" org-ts-regexp1 "\\]")
		     end t)
		    (org-time-string-to-seconds (match-string 0))
		  (float-time now))))
	     ((= dcst ?s)
	      (let ((end (save-excursion (outline-next-heading) (point))))
		(if (re-search-forward org-scheduled-time-regexp end t)
		    (org-time-string-to-seconds (match-string 1))
		  (float-time now))))
	     ((= dcst ?d)
	      (let ((end (save-excursion (outline-next-heading) (point))))
		(if (re-search-forward org-deadline-time-regexp end t)
		    (org-time-string-to-seconds (match-string 1))
		  (float-time now))))
	     ((= dcst ?p)
              (if (re-search-forward org-priority-regexp (line-end-position) t)
		  (string-to-char (match-string 2))
		org-priority-default))
	     ((= dcst ?r)
	      (or (org-entry-get nil property) ""))
	     ((= dcst ?o)
	      (when (looking-at org-complex-heading-regexp)
		(let* ((m (match-string 2))
		       (s (if (member m org-done-keywords) '- '+)))
		  (- 99 (funcall s (length (member m org-todo-keywords-1)))))))
	     ((= dcst ?f)
	      (if getkey-func
		  (progn
		    (setq tmp (funcall getkey-func))
		    (when (stringp tmp) (setq tmp (funcall case-func tmp)))
		    tmp)
		(error "Invalid key function `%s'" getkey-func)))
	     (t (error "Invalid sorting type `%c'" sorting-type))))
	  nil
	  (cond
	   ((= dcst ?a) 'string-collate-lessp)
	   ((= dcst ?f)
	    (or compare-func
		(and interactive?
		     (org-read-function
		      (concat "Function for comparing keys "
			      "(empty for default `sort-subr' predicate): ")
		      'allow-empty))))
	   ((member dcst '(?p ?t ?s ?d ?c ?k)) '<))))
	(org-cycle-hide-drawers 'all)
	(when restore-clock?
	  (move-marker org-clock-marker
		       (1+ (next-single-property-change
			    start :org-clock-marker-backup)))
	  (remove-text-properties (1- org-clock-marker) org-clock-marker
				  '(:org-clock-marker-backup t)))))
    (run-hooks 'org-after-sorting-entries-or-items-hook)
    (message "Sorting entries...done")))


(defun org-colviewx-sort-function (x y)
  "Returns non-nil if X should sort before Y (is less than Y).

This function gives a nice sort order when used with `org-colviewx-sort'.

Both arguments can be either a cons, a non-number string, or a
number-string. Across types the sort order is as mentioned.
Within types, `string-lessp` is used for non-number strings,
and `<` is used for number-strings."
  (cond
   ((and (consp x)(consp y))
    nil)
   ((and (consp x)(not (consp y)))
    t)
   ((and (not (consp x))(consp y))
    nil)
   (t
    (let ((sxp (not (string-match-p org-colviewx-string-number-regex x)))
          (syp (not (string-match-p org-colviewx-string-number-regex y))))
      (cond
       ((and sxp syp)
        (string-lessp x y))
       ((and sxp (not syp))
        t)
       ((and (not sxp) syp)
        nil)
       (t
        (< (string-to-number x) (string-to-number y))))))))


(defun org-colviewx-sort-reverse ()
  "Calls `org-colviewx-sort' with non-nil argument."
  (interactive)
  (org-colviewx-sort t))


;; * Filtering

(defun org-colviewx-filter (&optional arg)
  "Filter rows by column values."
  (interactive "P")
  (save-excursion
    ;;(org-colviewx-reset-filter)
    (let* ((word (get-char-property (point) 'org-columns-value))
           (prop (get-char-property (point) 'org-columns-key))
           (match
            (cond
             ((equal arg '(4))
              (format "LEVEL=1+%s%s" prop
                      (read-string
                       (concat "Match part starting with "
                               "</=/>/<=/>=/<> (with \"\" "
                               "for string comparison):\n"))))
             ((equal arg '(16))
              (format "LEVEL=1+%s"
                      (completing-read "Match: "
                                       ;; (read-string "Match: ")
                                       org-colviewx-filter-presets)))
             (t
              (format "LEVEL=1+%s<>{.*\\b%s.*}" prop word))))
           (top-folded (> (point-min) 1))
           (count 0))
      (when top-folded
        (org-colviewx-toggle-top -1))
      (org-map-entries (lambda ()
                         (org-colviewx-fold-subtree)
                         (setq count (1+ count)))
                       match)
      (when top-folded
        (org-colviewx-toggle-top 1))
      (message "Folded %s entries." count))))


(defun org-colviewx-fold-subtree ()
  "Fold current subtree.
The subtree is folded using specification org-colviewx-filter.
It is folded such that its position when folded is at the end of the
line before the start of the subtree and not at the beginning of the
heading after the end of the subtree, as this causes fewer issues."
  (interactive)
  (org-fold-core-region (1- (org-entry-beginning-position))
                        (save-excursion (org-end-of-subtree t t) (1- (point)))
                        t 'org-colviewx-filter))


(defun org-colviewx-reset-filter ()
  "Remove all filters."
  (interactive)
  (let ((top-folded (> (point-min) 1)))
    (when top-folded
      (org-colviewx-toggle-top -1))
    (org-colviewx-reveal-all 'org-colviewx-filter)
    (when top-folded
      (org-colviewx-toggle-top 1)))
  (outline-hide-sublevels
      (org-current-level))
  (message "Resetting filter."))


;; * Editing

(defun org-colviewx-redo-row ()
  "Construct the column display of the current row again."
  (interactive)
  (let ((inhibit-read-only t))
    ;; need to explicitly delete, otherwise value not always updated
    (mapc #'delete-overlay (overlays-in (line-beginning-position)
                                        (line-end-position)))
    (org-columns--display-here
     (save-excursion (org-columns--collect-values)))))


(defun org-colviewx-edit-value (&optional key)
  "Edit property value, but update only current row."
  (interactive)
  (let* ((key (or key (get-char-property (point) 'org-columns-key)))
         (value (get-char-property (point) 'org-columns-value))
         (nval (string-trim (org-read-property-value key (point)))))
    (when (not (equal nval value))
      (org-entry-put (point) key nval)
      (org-colviewx-redo-row))))


(defun org-colviewx-copy-value ()
  "Copy value of current cell."
  (interactive)
  (kill-new (get-char-property (point) 'org-columns-value)))


(defun org-colviewx-paste-value ()
  "Paste value into current cell, replacing old contents."
  (interactive)
  (org-entry-put nil (get-char-property (point) 'org-columns-key)
                 (current-kill 0))
  (let ((inhibit-read-only t))
    (mapc #'delete-overlay (overlays-in (line-beginning-position)
                                        (line-end-position)))
    (org-columns--display-here
     (save-excursion (org-columns--collect-values)))))


(defun org-colviewx-copy-value-from-above ()
  "Copy value from cell above into current cell."
  (interactive)
  (save-excursion
    (org-colviewx-previous-item)
    (org-colviewx-copy-value))
  (org-colviewx-paste-value))


(defun org-colviewx-delete-value ()
  "Delete value of current cell."
  (interactive)
  (org-delete-property (get-char-property (point) 'org-columns-key))
  (let ((inhibit-read-only t))
    (mapc #'delete-overlay (overlays-in (line-beginning-position)
                                        (line-end-position)))
    (org-columns--display-here
     (save-excursion (org-columns--collect-values)))))


(defun org-colviewx-cut-subtree ()
  "Cut the current subtree into the clipboard."
  (interactive)
  (let ((inhibit-read-only t))
    (org-cut-subtree)))


;; * Keybindings

;; navigating
(org-defkey org-columns-map "f" #'org-colviewx-forward)
(org-defkey org-columns-map [right] #'org-colviewx-forward)
(org-defkey org-columns-map "b" #'org-colviewx-backward)
(org-defkey org-columns-map [left] #'org-colviewx-backward)

(org-defkey org-columns-map "n" #'org-colviewx-next-item)
(org-defkey org-columns-map [down] #'org-colviewx-next-item)
(org-defkey org-columns-map "p" #'org-colviewx-previous-item)
(org-defkey org-columns-map [up] #'org-colviewx-previous-item)

(org-defkey org-mode-map (kbd "C-c C-.") #'org-colviewx-beginning-of-contents)
(org-defkey org-columns-map "." #'org-colviewx-beginning-of-contents+)

(org-defkey org-columns-map "j" #'org-goto)


;; viewing
(org-defkey org-columns-map "c" #'org-cycle)
(org-defkey org-columns-map "C" #'org-shifttab)
(org-defkey org-columns-map "d" #'org-colviewx-entry-toggle-drawer)
(org-defkey org-columns-map "t" #'org-colviewx-toggle-top)


;; sorting
(org-defkey org-columns-map "^" #'org-colviewx-sort)
(org-defkey org-columns-map [(meta down)] #'org-colviewx-sort)
(org-defkey org-columns-map [(meta up)] #'org-colviewx-sort-reverse)


;; filtering

(org-defkey org-columns-map "\\" #'org-colviewx-filter)
(org-defkey org-columns-map "|" #'org-colviewx-reset-filter)


;; editing
(org-defkey org-columns-map "r" #'org-colviewx-redo-row)
(org-defkey org-columns-map "e" #'org-colviewx-edit-value)
(org-defkey org-columns-map (kbd "s-c") #'org-colviewx-copy-value)
(org-defkey org-columns-map (kbd "s-v") #'org-colviewx-paste-value)
(org-defkey org-columns-map (kbd "C-d") #'org-colviewx-copy-value-from-above)
(org-defkey org-columns-map (kbd "DEL") #'org-colviewx-delete-value)
(org-defkey org-columns-map (kbd "C-<backspace>") #'org-colviewx-cut-subtree)

(org-defkey org-columns-map "i"
            #'(lambda () (interactive) (org-insert-heading-respect-content)))

(org-defkey org-columns-map "=" #'org-columns-next-allowed-value)
(org-defkey org-columns-map "-" #'org-columns-previous-allowed-value)


(provide 'org-colviewx)

;;; org-colviewx.el ends here
