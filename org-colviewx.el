;; * Extensions for org-colview -*- lexical-binding: t; -*-

;; Copyright (C) 2023 orgtre

;; Author: orgtre
;; URL:

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

;; This file contains some extensions and configuration of the column
;; view for Org (org-colview.el).

;;; Code:

;; * Setup

(require 'org-colview)

(defface org-colviewx-link
  '((t :inherit org-link
       :underline nil))
  "Like org-link but without underline.")

;; #TODO this is not in effect after switch to dark mode
(set-face-attribute 'org-column nil :background 'unspecified)
(set-face-attribute 'org-column t :background 'unspecified)
(set-face-attribute 'org-column-title nil
                    :background 'unspecified
                    :inherit 'org-level-1 :underline nil)
(set-face-attribute 'org-column-title t :background 'unspecified)


;; * org-column hooks

(defvar org-colviewx-hook nil
  "Hook for functions attaching themselves to `org-columns'.")

(defvar org-colviewx-quit-hook nil
  "Hook for functions attaching themselves to `org-columns-quit'.")

(defun org-colviewx-org-columns-advice (&rest r)
  "Run `org-colviewx-hook'."
  (run-hooks 'org-colviewx-hook))

(advice-add 'org-columns :after #'org-colviewx-org-columns-advice)

(defun org-colviewx-org-columns-quit-advice (&rest r)
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
  ;; disable speed commands as they conflict with org-columns:
  (setq-local org-use-speed-commands nil)
  ;; disable as this behaves strangely in column view:
  (setq-local org-special-ctrl-a/e (cons nil t)))


(defun org-colviewx-teardown ()
  (kill-local-variable 'org-use-speed-commands)
  (kill-local-variable 'org-special-ctrl-a/e))


(add-hook 'org-colviewx-hook #'org-colviewx-minor-mode)
(add-hook 'org-colviewx-quit-hook (lambda () (org-colviewx-minor-mode -1)))


;; * Navigating

(defun org-colviewx-next-item ()
  (interactive)
  (when (get-char-property (point) 'org-columns-key)
    (setq org-colviewx-last-column (org-current-text-column)))
  (let ((pos (point)))
    (call-interactively 'org-next-visible-heading)
    (if (and (bolp) (org-at-heading-p))
        (forward-char org-colviewx-last-column)
      (goto-char pos))))


(defun org-colviewx-previous-item ()
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
      (previous-line)
    (when end
      (if (re-search-forward "[ \n]*\n\\*" nil t)
          (goto-char (match-beginning 0))
        (org-end-of-subtree)
        (re-search-backward "[^ \n]")
        (goto-char (match-end 0)))))
  (org-show-context))


(defvar org-colviewx-last-column nil)

(defun org-colviewx-save-column (&rest r)
  (setq org-colviewx-last-column (org-current-text-column)))

(defun org-colviewx-goto-last-column (&rest r)
  (when org-colviewx-last-column
    (move-to-column org-colviewx-last-column)))

(defun org-colviewx-beginning-of-contents+ (&optional end)
  (interactive)
  (org-colviewx-save-column)
  (org-colviewx-beginning-of-contents end))


;; * Viewing

(defun org-colviewx-entry-toggle-drawer (&optional arg)
  "Toggle visibility of the property drawer of entry at point.
The entry itself is always unfolded, but the drawer is only
unfolded if that is required for toggling its visbility.
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
          (org-fold-hide-drawer-toggle (or arg nil))))))))


;; ** Connected vertical divider lines

(set-face-attribute 'nobreak-space nil :underline nil)
(set-face-attribute 'nobreak-space nil :inherit 'org-level-1)

(defcustom org-columns-separator
  (propertize " " 'face
                      (list
                       ;;:inherit 'org-level-1
                       :inverse-video t
                       :family "Arial"
                       ))
  "Separator to use between columns."
  :type 'string)


(defun org-columns--display-here-title ()
  "Overlay the newline before the current line with the table title."
  (interactive)
  (let ((title "")
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
	    (chars (- (line-end-position) (line-beginning-position))))
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


;; * Editing

(defun org-colviewx-edit-value (&optional key)
  "Edit property value, but update only current row."
  (interactive)
  (let* ((col (current-column))
         (key (or key (get-char-property (point) 'org-columns-key)))
         (value (get-char-property (point) 'org-columns-value))
         (nval (string-trim (org-read-property-value key (point)))))
    (when (not (equal nval value))
      (org-entry-put (point) key nval)
      (let ((inhibit-read-only t))
        ;; need to explicitly delete, otherwise value not always updated
        (mapc #'delete-overlay (overlays-in (line-beginning-position)
                                            (line-end-position)))
        (org-columns--display-here
         (save-excursion (org-columns--collect-values)))))))


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


;; * Transforming

(defun org-colviewx-transform-links (column-title value)
  "Transforms values containing Org links."
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


(defconst org-colviewx-string-number-regex
  (concat "^[+-]?\\(?:[0-9]+\\(?:[.][0-9]*\\)?\\(?:e[+-]?[0-9]+\\)?"
          "\\|[.][0-9]+\\(?:e[+-]?[0-9]+\\)?\\)$")
  "Matches integers and floats with exponent.
This allows for leading and trailing decimal point, leading zeros in base,
leading zeros in exponent, and + signs.")


(defun org-colviewx-transform-content (column-title value)
  (when (equal column-title "CONTENT")
    (org-back-to-heading)
    (org-end-of-meta-data)
    (unless (org-at-heading-p)
      (when (re-search-forward "[ \n]*\\([^ \n].*\\)"
                               (org-entry-end-position) t)
        (setq value (match-string-no-properties 1))
        (put-text-property 0 (length value)
                           'face 'org-property-value value)
        (put-text-property 0 (length value) 'keymap
                           '(keymap (?e . org-columns-open-link)) value)
        value))))


(defun org-colviewx-display-transformer (column-title value)
  "Modifies the value to display in column view."
  (or
   ;; (org-colviewx-display-transform-links column-title value)
   ;; (org-colviewx-transform-numbers column-title value)
   ;; (org-colviewx-display-transform-content column-title value)   
   ))

(setq org-columns-modify-value-for-display-function
      #'org-colviewx-display-transformer)


;; * Sorting

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


(defun org-colviewx-sort (&optional arg)
  "Sort entries from column view by value in current column.

This only sorts the children of the column view top level,
which is the entry with the current column view specification.

When ARG is non-nil (interactively with prefix), sort in
reverse order."
  (interactive "P")
  (let ((colname (get-char-property (point) 'org-columns-key))
        (colnum (current-column))
        (inhibit-read-only t))
    (org-columns-goto-top-level)
    ;;(org-sort-entries nil (if arg ?R ?r) nil nil colname)
    (org-sort-entries nil (if arg ?F ?f)
                      (lambda () (org-entry-get nil colname))
                      #'org-colviewx-sort-function)
    (org-columns)
    (outline-hide-sublevels
     (1+ (org-current-level)))
    (move-to-column colnum)))

(defun org-colviewx-sort-reverse ()
  "Calls `org-colviewx-sort' with non-nil argument."
  (interactive)
  (org-colviewx-sort t))


;; * Keybindings

(org-defkey org-columns-map "^" #'org-colviewx-sort)
(org-defkey org-columns-map [(meta down)] #'org-colviewx-sort)
(org-defkey org-columns-map [(meta up)] #'org-colviewx-sort-reverse)

(org-defkey org-columns-map "j" #'org-goto)

;; viewing
(org-defkey org-columns-map "c" #'org-cycle)
(org-defkey org-columns-map "C" #'org-shifttab)
(org-defkey org-columns-map "d" #'org-colviewx-entry-toggle-drawer)


;; navigating
(org-defkey org-columns-map "f"
	    (lambda () (interactive) (goto-char (1+ (point)))))

(org-defkey org-columns-map "b" #'backward-char)

(org-defkey org-columns-map "n" #'org-colviewx-next-item)
(org-defkey org-columns-map "p" #'org-colviewx-previous-item)

(org-defkey org-mode-map (kbd "C-c C-.") #'org-colviewx-beginning-of-contents)
(org-defkey org-columns-map "." #'org-colviewx-beginning-of-contents+)


;; editing
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


(provide 'org-colview)

;;; org-colviewx.el ends here
