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
(set-face-attribute 'org-column nil :background nil)
(set-face-attribute 'org-column t :background nil)
(set-face-attribute 'org-column-title nil
                    :background nil :inherit 'org-level-1 :underline nil)
(set-face-attribute 'org-column-title t :background nil)


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
  (org-speed-move-safe 'org-next-visible-heading)
  (forward-char org-colviewx-last-column))
  

(defun org-colviewx-previous-item ()
  (interactive)
  (when (get-char-property (point) 'org-columns-key)
    (setq org-colviewx-last-column (org-current-text-column)))
  (org-speed-move-safe 'org-previous-visible-heading)
  (forward-char org-colviewx-last-column))


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
sets ARG to `t', while two set it to `off'."
  (interactive "P")
  (when (called-interactively-p)
    (cond
     ((equal arg '(4)) (setq arg t))
     ((equal arg '(16)) (setq arg 'off))))
  (save-excursion
    (org-back-to-heading)
    (let ((h-folded-p (org-fold-folded-p (line-end-position)))
          (d-pos (car (org-get-property-block)))
          d-folded-p)
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

