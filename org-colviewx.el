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
;; view of Org (org-colview.el).

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
                       ;; :inherit 'org-level-1
                       ;; :foreground "gray90"
                       :background "gray80"
                       ;; :background "#398eac"
                       ;; (face-attribute 'escape-glyph :foreground)
                       ;; :underline 'unspecified
                       ;; :inverse-video t
                       :family "Arial"
                       ;; :inherit 'variable-pitch
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

(defcustom org-colviewx-always-visible-properties nil
  "List of properties to always show in the properties drawer.
Properties in this list are not hidden even when using
`org-colviewx-toggle-column-properties-visibility'."
  :type '(repeat string))

(defcustom org-colviewx-fit-and-move-frame t
  "Whether to fit frame to the column view and move it into display."
  :type 'bool)

(defcustom org-colviewx-side-windows-primary-side 'left
  "Side on which to display side buffers.
Used by `org-colviewx-side-windows-setup'."
  :type '(choice (const left) (const top)
                 (const right) (const bottom)))

(defcustom org-colviewx-side-windows-secondary-side 'bottom
  "Secondary side on which to display side buffers.
This side is used when the side windows don't fit on
`org-colviewx-side-windows-primary-side'.
See `org-colviewx-side-windows-setup'."
  :type '(choice (const top) (const bottom)))

(defcustom org-colviewx-side-windows-height 18
  "Height of side buffers when displayed at top or bottom.
Used by `org-colviewx-show-entry-in-side-buffers'."
  :type 'integer)

(defcustom org-colviewx-side-windows-width 40
  "Width of the side buffer showing the current entry.
Used by `org-colviewx-show-entry-in-side-buffers'."
  :type 'integer)

(defcustom org-colviewx-side-windows-extra-width 1
  "Extra adjustment to the width required by side windows.
Value given in pixels. This can be used to correct errors made by
`org-colviewx-side-windows-setup'."
  :type 'integer)

(defcustom org-colviewx-side-windows-resize-frame t
  "Whether to resize frame to make side windows fit."
  :type 'bool)


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

(defvar-local org-colviewx-column-properties-hidden nil
  "List of column properties currently hidden.")

(defvar-local org-colviewx-side-windows-current-side nil
  "Used by `org-colviewx-side-windows-show-entry'.")


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
  (org-fold-core-add-folding-spec 'org-colviewx-filter
                                  '((:isearch-ignore . t)))
  ;;(org-colviewx-reveal-all 'org-colviewx-filter)
  (when org-colviewx-fit-and-move-frame
    (org-colviewx-fit-and-move-frame)))


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
  (when org-colviewx-column-properties-hidden
    (org-colviewx-toggle-column-properties-visibility))
  (when org-colviewx-side-windows-current-side
    (org-colviewx-side-windows-toggle)))


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


(defun org-colviewx-beginning-of-contents (&optional arg)
  "Go to first non-whitespace character of entry content.
Skips meta-data. When the entry content is only meta-data and
whitespace, go to the first subheading; but if there is no
subheading go to the end of of the last non-meta-data line of
entry, inserting it if necessary.

With a `\\[universal-argument]' prefix argument go to last \
non-whitespace
character instead. With a `\\[universal-argument] \
\\[universal-argument]' prefix argument go to
last non-whitespace character in subtree."
  (interactive "P")
  (let ((initial-level (org-current-level)))
    (org-end-of-meta-data t)
    (if (and (org-at-heading-p)
             (= initial-level (org-current-level)))
        (progn (backward-char)
               (when (save-excursion
                       (backward-char)
                       (looking-at-p "[^ \n\t]"))
                 (newline)))
      (cond
       ((equal arg '(4))
        (if (re-search-forward "[ \n\t]*\n\\*" nil t)
            (goto-char (match-beginning 0))
          (org-end-of-subtree)
          (re-search-backward "[^ \n\t]")
          (goto-char (match-end 0))))
       ((equal arg '(16))
        (org-end-of-subtree t)
        (re-search-backward "[^ \n\t]")
        (goto-char (match-end 0)))))
    (when (called-interactively-p 'interactive)
      (org-fold-show-context))))


(defun org-colviewx-save-column (&rest _r)
  (setq org-colviewx-last-column (org-current-text-column)))


(defun org-colviewx-goto-last-column (&rest _r)
  (when org-colviewx-last-column
    (move-to-column org-colviewx-last-column)))


(defun org-colviewx-beginning-of-contents+ (&optional arg)
  "Wrapper around `org-colviewx-beginning-of-contents'.
If there is a side buffer showing the contents, first select it,
else store the current column before moving point so that one can
return to it later. ARG is just passed on."
  (interactive "P")
  (let (window)
    (if (or (and (member org-colviewx-side-windows-current-side '(left right))
                 (setq window (get-buffer-window
                               (concat (buffer-name) "::colviewx-entry"))))
            (and (member org-colviewx-side-windows-current-side '(top bottom))
                 (setq window (get-buffer-window
                               (concat (buffer-name) "::colviewx-content")))))
        (progn
          (select-window window)
          (goto-char (point-min))
          (org-colviewx-beginning-of-contents arg))
      (org-colviewx-save-column)
      (funcall-interactively
       #'org-colviewx-beginning-of-contents arg))))


;; * Viewing

(defun org-colviewx-open-link ()
  "TODO"
  )


(defun org-colviewx-toggle-top (&optional arg)
  "Toggle visibility of the top of the buffer.

The top refers to the part before the first headline and it is
hidden using `narrow-to-region'. When revealing the top, scroll
the window down the number of lines revealed or as far as allowed
without moving point.

Unconditionally hide the top when ARG is larger than zero;
unconditionally reveal it when ARG is smaller than zero."
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) 0))
  (if (and (> (point-min) 1)
           (<= arg 0))
      (let ((revealed-lines (count-screen-lines 1 (point-min))))
        (widen)
        (save-excursion
          (scroll-down
           (min revealed-lines
                (- (window-text-height)
                   (count-screen-lines (point) (window-start))
                   scroll-margin 2)))))
    (when (and (= (point-min) 1)
               (>= arg 0))
      (narrow-to-region (save-excursion
                          (goto-char 1)
                          ;; make sure we include invisible headings in fold
                          (let ((invisible-heading t)
                                no-visible-heading)
                            (while (and invisible-heading
                                        (not no-visible-heading))
                              (unless (re-search-forward
                                       org-outline-regexp-bol
                                       nil t)
                                (setq no-visible-heading t))
                              (setq invisible-heading
                                    (org-fold-core-folded-p)))
                            (if no-visible-heading (point-max) (pos-bol))))
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


(defun org-colviewx-show-all-drawers (&optional arg)
  "Show all drawers; with ARG hide all."
  (interactive "P")
  (if arg
      (org-cycle-hide-drawers 'all)
    (org-fold-show-all '(drawers))))


(defun org-colviewx-toggle-column-properties-visibility ()
  "Toggle visibility of column properties.
This is done by adding/removing properties currently displayed in
columns to/from `org-custom-properties' and if applicable toggling
`org-toggle-custom-properties-visibility'. Hidden properties are
tracked in `org-colviewx-column-properties-hidden'. Properties in
`org-colviewx-always-visible-properties' are never hidden."
  (interactive)
  (if org-colviewx-column-properties-hidden
      (progn
        (while org-colviewx-column-properties-hidden
          (setq org-custom-properties
                (delete (pop org-colviewx-column-properties-hidden)
                        org-custom-properties)))
        (when org-custom-properties-overlays
          (org-toggle-custom-properties-visibility)))
    (mapc (lambda (x)
            (unless
                (or (member (car x) org-colviewx-always-visible-properties)
                    (member (car x) org-custom-properties))
              (setq org-colviewx-column-properties-hidden
                    (cons (car x) org-colviewx-column-properties-hidden))
              (setq org-custom-properties
                    (cons (car x) org-custom-properties))))
          org-columns-current-fmt-compiled)
    (when org-custom-properties-overlays
      (org-toggle-custom-properties-visibility))
    (org-toggle-custom-properties-visibility)))


(defun org-colviewx-switch-format ()
  "Switch column view format using completion.
The select format is activated using `org-columns'.

When column view is active, candidates are taken with inheritance
from the entry at the start of the columns region by successively
looking for properties named COLUMNS1, COLUMNS2, etc. until a nil
value. The current and default column formats are also included.

When column view is not active, only look for properties in a
drawer at the top of the file."
  (interactive)
  (let ((num 1) fmts choice)
    (org-with-wide-buffer
     (goto-char (or (marker-position org-columns-top-level-marker)
                    (point-min)))
     (setq fmts (list (or org-columns-current-fmt
                          (org-entry-get nil "COLUMNS" t))))
     (unless (equal org-columns-default-format (car fmts))
       (push org-columns-default-format fmts))
     (while-let ((fmt (org-entry-get nil (format "COLUMNS%d" num) t)))
       (push fmt fmts)
       (setq num (1+ num))))
    (setq choice (completing-read "Format: " fmts))
    (org-columns-quit)
    (org-columns nil choice)))


;; ** Resize frame


(defun org-colviewx-fit-and-move-frame ()
  "Fit the frame to the column view and move it into display.
The frame is moved so that its left edge is displayed and if possible
also its right edge. Resizing and moving only happens if necessary for
showing all columns."
  (interactive)
  (org-colviewx-resize-frame)
  (org-colviewx-move-frame-into-display-horizontally))


(defun org-colviewx-resize-frame ()
  "Resize frame to fit the column view, if necessary."
  (let* ((columns-pxw (string-pixel-width
                       (concat (or header-line-format
                                   org-columns-full-header-line-format)
                               (or org-ellipsis "..."))))
         (window-pxw (window-body-width nil t))
         (frame-text-pxw (frame-text-width))
         (new-frame-text-pxw (+ frame-text-pxw
                                (when (org-colviewx-reserved-char-p)
                                  (frame-char-width))
                                (- columns-pxw window-pxw)))
         (max-frame-text-pxw (- (display-pixel-width)
                                (- (frame-outer-width)
                                   frame-text-pxw)))
         (feasible-new-frame-text-pxw (min new-frame-text-pxw
                                           max-frame-text-pxw)))
    (when (> feasible-new-frame-text-pxw frame-text-pxw)
      (set-frame-width nil feasible-new-frame-text-pxw nil t))))


(defun org-colviewx-move-frame-into-display-horizontally ()
  "Move frame so that left/right edges are displayed.
If frame is too large to show both edges, move it so that its
left edge is displayed on the left display edge."
  (let* ((pos (frame-position))
         (width-outside (- (+ (car pos) (frame-outer-width))
                           (display-pixel-width))))
    (cond
     ((> width-outside 0)
      (set-frame-position nil (max 0 (- (car pos) width-outside)) (cdr pos)))
     ((> 0 (car pos))
      (set-frame-position nil 0 (cdr pos))))))


;; ** Side windows

(defvar-local org-colviewx-side-windows-old-text-width nil
  "Used to reset frame after toggling off side windows.")

(defvar-local org-colviewx-side-windows-old-xpos nil
  "Used to reset frame after toggling off side windows.")

(defun org-colviewx-side-windows-toggle (&optional arg)
  "Toggle side window(s) displaying current entry.

Display one or two indirect buffers narrowed to the property drawer and
content of the current entry in side windows of the current window.
How the side windows are displayed in determined by
`org-colviewx-side-windows-setup', unless called with a \
`\\[universal-argument]' prefix,
in which case the side windows are displayed on
`org-colviewx-side-windows-secondary-side'.

Add advice to `org-colviewx-next-item' and `org-colviewx-previous-item'
to update the side window(s) after they are called."
  (interactive "P")
  (if org-colviewx-side-windows-current-side
      (let ((entry-buffer (concat (buffer-name) "::colviewx-entry"))
            (content-buffer (concat (buffer-name) "::colviewx-content")))
        (advice-remove 'org-colviewx-next-item
                       #'org-colviewx-side-windows-show-entry)
        (advice-remove 'org-colviewx-previous-item
                       #'org-colviewx-side-windows-show-entry)
        (when (window-with-parameter 'window-side)
          (window-toggle-side-windows))
        (when (get-buffer entry-buffer)
          (kill-buffer entry-buffer))
        (when (get-buffer content-buffer)
          (kill-buffer content-buffer))
        (when org-colviewx-side-windows-old-xpos
          (set-frame-position nil org-colviewx-side-windows-old-xpos
                              (cdr (frame-position)))
          (setq org-colviewx-side-windows-old-xpos nil))
        (when org-colviewx-side-windows-old-text-width
          (set-frame-width nil org-colviewx-side-windows-old-text-width nil t)
          (setq org-colviewx-side-windows-old-text-width nil))
        (setq org-colviewx-side-windows-current-side nil))
    (advice-add 'org-colviewx-next-item :after
                #'org-colviewx-side-windows-show-entry)
    (advice-add 'org-colviewx-previous-item :after
                #'org-colviewx-side-windows-show-entry)
    (if (not arg)
        (org-colviewx-side-windows-setup)
      (setq org-colviewx-side-windows-current-side
            org-colviewx-side-windows-secondary-side)
      (org-colviewx-side-windows-show-entry))))


(defun org-colviewx-side-windows-setup ()
  "Transform the frame and show side windows.
Tries to be smart about how to show the side windows.

When the side window fits into the current frame without truncating or
wrapping the columns displayed, show it in
`org-colviewx-side-windows-primary-side'.

When the side window does't fit without truncating/wrapping the columns
displayed, `org-colviewx-side-windows-resize-frame' is non-nil,
and the resized frame would fit on screen, resize the frame and move it
to ensure the side window is fully displayed on screen. Else when the
side window doesn't fit, show it either on top or below according to
`org-colviewx-side-windows-secondary-side'. The width calculated for the
side window can be adjusted with `org-colviewx-side-windows-extra-width'.

When resizing the frame, the old frame x-position and width are stored
in buffer-local variables and are reset when side windows are toggled
off."
  (let ((primary-side org-colviewx-side-windows-primary-side))
    (if (member primary-side '(left right))
        (let* ((columns-pxw (string-pixel-width
                             (concat header-line-format
                                     (or org-ellipsis "..."))))
               (window-pxw (window-body-width nil t))
               (available-pxw (- window-pxw columns-pxw
                                 (when (org-colviewx-reserved-char-p)
                                   (frame-char-width))))
               (required-pxw (+ (* org-colviewx-side-windows-width
                                   (frame-char-width))
                                org-colviewx-side-windows-extra-width))
               (required-frame-text-pxw (+ (frame-text-width)
                                           (- required-pxw available-pxw))))
          (cond
           ((>= available-pxw required-pxw)
            (setq org-colviewx-side-windows-current-side primary-side))
           ((and org-colviewx-side-windows-resize-frame
                 (>= (display-pixel-width)
                     (+ required-frame-text-pxw
                        (- (frame-outer-width)
                           (frame-text-width)))))
            (setq org-colviewx-side-windows-old-text-width (frame-text-width))
            (when (equal primary-side 'left)
              (setq org-colviewx-side-windows-old-xpos
                    (car (frame-position)))
              (set-frame-position nil (max 0
                                           (- (car (frame-position))
                                              (- required-frame-text-pxw
                                                 (frame-text-width))))
                                  (cdr (frame-position))))
            (when (and (equal primary-side 'right)
                       (> (+ (car (frame-position))
                             (frame-outer-width)
                             (- required-frame-text-pxw
                                (frame-text-width)))
                          (display-pixel-width)))
              (setq org-colviewx-side-windows-old-xpos
                    (car (frame-position)))
              (set-frame-position nil (max 0
                                           (- (display-pixel-width)
                                              (+ required-frame-text-pxw
                                                 (- (frame-outer-width)
                                                    (frame-text-width)))))
                                  (cdr (frame-position))))
            (set-frame-width nil required-frame-text-pxw nil t)
            (setq org-colviewx-side-windows-current-side primary-side))
           (t
            (setq org-colviewx-side-windows-current-side
                  org-colviewx-side-windows-secondary-side))))
      (setq org-colviewx-side-windows-current-side primary-side))
    (org-colviewx-side-windows-show-entry)))


(defun org-colviewx-reserved-char-p ()
  "Checks whether a continuation glyph is reserved.
Applies to current window. Extracted from `window-max-chars-per-line'"
  (or (not (display-graphic-p))
      (not overflow-newline-into-fringe)
      (eq left-fringe-width 0)
      (and (null left-fringe-width)
           (= (frame-parameter nil 'left-fringe) 0))
      (eq right-fringe-width 0)
      (and (null right-fringe-width)
           (= (frame-parameter nil 'right-fringe) 0))))


(defun org-colviewx-side-windows-show-entry (&optional side)
  "Show drawer and content of current entry in side window(s).

SIDE should be one of the symbols left, top, right, or bottom;
defaults default the side is determined using the value of
`org-colviewx-side-windows-current-side' or
`org-colviewx-side-windows-primary-side', in that order. When SIDE is
top or bottom, two separate side windows are created for the entry
drawer and content, otherwise just one side window is used.

`org-colviewx-side-windows-height' and `org-colviewx-side-windows-width'
control the size of the side window(s)."
  (interactive)
  (unless (buffer-base-buffer)
    (unless side (setq side (or org-colviewx-side-windows-current-side
                                org-colviewx-side-windows-primary-side)))
    (let ((top-or-bottom (member side '(bottom top)))
          boe boc eos entry-buffer-name content-buffer-name
          entry-buffer content-buffer)
      (save-excursion
        (setq boe (progn (org-back-to-heading t) (point)))
        (setq eos (progn (org-end-of-subtree t) (point))))
      (when top-or-bottom
        (save-excursion
          (setq boc (progn (org-colviewx-beginning-of-contents) (point)))))
      (setq entry-buffer-name (concat (buffer-name) "::colviewx-entry"))
      (setq entry-buffer (or (get-buffer entry-buffer-name)
                             (make-indirect-buffer (current-buffer)
                                                   entry-buffer-name
                                                   t)))
      (display-buffer-in-side-window
       entry-buffer
       `((side . ,side)
         (slot . -1)
         (window-width . ,org-colviewx-side-windows-width)
         (window-height . ,org-colviewx-side-windows-height)
         (preserve-size . '(t . nil))))
      (with-current-buffer entry-buffer
        (narrow-to-region boe (if top-or-bottom boc eos))
        (org-fold-show-all)
        (org-map-entries (lambda ()
                           (mapc #'delete-overlay
                                 (overlays-in (pos-bol)(pos-eol)))))
        (setq header-line-format nil)
        (set-window-point (get-buffer-window) (point-min)))
      (when top-or-bottom
        (setq content-buffer-name (concat (buffer-name) "::colviewx-content"))
        (setq content-buffer (or (get-buffer content-buffer-name)
                                 (make-indirect-buffer (current-buffer)
                                                       content-buffer-name
                                                       t)))
        (display-buffer-in-side-window
         content-buffer
         `((side . ,side)
           (slot . 1)
           (window-width . ,(- (window-width)
                               org-colviewx-side-windows-width))))
        (with-current-buffer content-buffer
          (narrow-to-region boc eos)
          (org-fold-show-all)
          (org-map-entries (lambda ()
                             (mapc #'delete-overlay
                                   (overlays-in (pos-bol)(pos-eol)))))
          (setq header-line-format nil)
          (set-window-point (get-buffer-window) (point-min)))))))


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
       "Sort %s: [a]lpha  [n]umeric  [p]riority  p[r]operty  todo[o]rder
               [f]unc [t]ime [s]cheduled  [d]eadline  [c]reated  cloc[k]ing
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


(defun org-colviewx-move-subtree-down (&optional arg)
  "Calls `org-move-subtree-down' while inhibiting read-only."
  (interactive "p")
  (let ((inhibit-read-only t))
    (org-move-subtree-down arg)
    ;; movement destroys the overlay, so recreate it
    (org-columns--display-here
     (save-excursion (org-columns--collect-values)))))


(defun org-colviewx-move-subtree-up (&optional arg)
  "Calls `org-move-subtree-up' while inhibiting read-only."
  (interactive "p")
  (let ((inhibit-read-only t))
    (org-move-subtree-up arg)
    ;; movement destroys the overlay, so recreate it
    (org-columns--display-here
     (save-excursion (org-columns--collect-values)))))



;; * Filtering

(defconst org-colviewx-filter-prompt
  "Enter <, =, >, <=, >=, or <> plus NUMBER, \"STRING\", or {REGEX}.
Entries where %s satisfies this comparison will be hidden:\n")


(defun org-colviewx-filter (&optional arg)
  "Filter rows by column values.

The filtering is done by calling `org-colviewx-fold-subtree' through
`org-map-entries', where the latters match argument is by default
constructed to require that the property shown in the column at point
matches the first word of the property value at point, and that the
entry is at the same level as the original one.

With a numeric prefix ARG, match word number ARG of the value at point
(where 0 matches the full value and a negative integer means count from
the end of value).

With a `\\[universal-argument]' prefix argument, prompt for a comparison \
operator and value
which will be used as match argument together with the current column key
and the current entry level.

With a `\\[universal-argument] \\[universal-argument]' prefix argument, \
prompt for a full, arbitrary match argument;
see Info node `(org) Matching tags and properties'. Custom variable
`org-colviewx-filter-presets' can be used to provide a list of presets.

The search scope is always the current buffer respecting restrictions."
  (interactive "P")
  (save-excursion
    (let* ((value (get-char-property (point) 'org-columns-value))
           (prop (get-char-property (point) 'org-columns-key))
           (level (org-current-level))
           (match
            (cond
             ((numberp arg)
              (cond ((= 0 arg)
                     (format "LEVEL=%d+%s<>{.*\\b%s\\b.*}"
                             level prop value))
                    ((> 0 arg)
                     (format "LEVEL=%d+%s<>{.*\\b%s\\b.*}"
                             level prop (nth (1- (abs arg))
                                             (nreverse
                                              (split-string value "[, ]+")))))
                    (t
                     (format "LEVEL=%d+%s<>{.*\\b%s\\b.*}"
                             level prop (nth (1- arg)
                                             (split-string value "[, ]+"))))))
             ((equal arg '(4))
              (format "LEVEL=%d+%s%s"
                      level prop (read-string
                                  (format org-colviewx-filter-prompt prop))))
             ((equal arg '(16))
              (completing-read "Hide entries matching: "
                               org-colviewx-filter-presets))
             (t
              (format "LEVEL=%d+%s<>{.*\\b%s\\b.*}"
                      level prop (nth 0 (split-string value "[, ]+"))))))
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
      (message "Folded %s entries matching \"%s\"." count match))))


(defun org-colviewx-fold-subtree ()
  "Fold the current subtree.
The subtree is folded using folding spec org-colviewx-filter,
see `org-fold-core--specs'. It is folded such that its position when
folded is at the end of the line before the start of the subtree and not
at the beginning of the heading after the end of the subtree, as this
causes fewer issues."
  (interactive)
  (org-fold-core-region (1- (org-entry-beginning-position))
                        (save-excursion (org-end-of-subtree t t) (1- (point)))
                        t 'org-colviewx-filter))


(defun org-colviewx-reset-filter ()
  "Remove all filters."
  (interactive)
  (let ((top-folded (> (point-min) 1))
        (level (org-current-level)))
    (when top-folded
      (org-colviewx-toggle-top -1))
    (org-colviewx-reveal-all 'org-colviewx-filter)
    (when top-folded
      (org-colviewx-toggle-top 1))
    (when level
      (outline-hide-sublevels level))
    (message "Resetting filter.")))


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


(defun org-colviewx-replace-regexp (prop regexp to-string)
  "Replace REGEXP in property values with TO-STRING.
PROP gives the property within whose values REGEXP is matched.

Search happens across the current buffer, but respects the restriction
and ignores entries folded via `org-colviewx-filter'.

When called interactively, first try reading PROP from the char property
org-columns-key at point (which exists if point is on an org-columns
overlay), else prompt the user for it; with a `\\[universal-argument]' \
prefix argument
always prompt. REGEXP and TO-STRING are always prompted from the user
in interactive calls.

Note that when providing REGEXP interactively, special characters
have to be single quoted `\\', but when providing it directly,
they have to be double quoted `\\\\'. TO-STRING is passed as argument
REP to `replace-regexp-in-string' (which see).

This command can also be used when column view is not active. If it is
active, overlays are updated when necesary."
  (interactive
   (let* ((prop-at-point (get-char-property (point) 'org-columns-key))
          (prop (if (or current-prefix-arg
                        (not prop-at-point))
                    (completing-read "Property: "
                                     (org-buffer-property-keys))
                  prop-at-point))
          (regexp (query-replace-read-from
                   (format "In property %s, replace regexp" prop) t))
          (to-string (if (consp regexp)
                         (prog1 (cdr regexp) (setq regexp (car regexp)))
                       (query-replace-read-to
                        regexp
                        (format "In property %s, replace regexp" prop) t))))
     (list prop regexp to-string)))
  (save-excursion
    (let ((column-prop-p (and org-columns-overlays
                              (assoc (upcase prop)
                                     org-columns-current-fmt-compiled)))
          (count 0) match replacement)
      (goto-char (point-min))
      (while (re-search-forward (format "^:%s:[ \t]*\\(.*\\)$" prop) nil t)
        (unless (org-fold-folded-p (point) 'org-colviewx-filter)
          (setq match (match-string 1))
          (save-match-data
            (setq replacement (replace-regexp-in-string regexp to-string
                                                        match t)))
          (when (not (equal match replacement))
            (replace-match replacement t nil nil 1)
            (setq count (1+ count))
            (when column-prop-p
              (save-excursion
                (org-back-to-heading t)
                (org-colviewx-redo-row))))))
      (message "Replaced matches in %d values of property %s." count prop))))



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
(org-defkey org-columns-map "D" #'org-colviewx-show-all-drawers)
(org-defkey org-columns-map "t" #'org-colviewx-toggle-top)
(org-defkey org-columns-map "s" #'org-colviewx-side-windows-toggle)
(org-defkey org-columns-map "P"
            #'org-colviewx-toggle-column-properties-visibility)
(org-defkey org-columns-map "F" #'org-colviewx-fit-and-move-frame)
(org-defkey org-columns-map "S" #'org-colviewx-switch-format)


;; sorting
(org-defkey org-columns-map "^" #'org-colviewx-sort)
(org-defkey org-columns-map [(meta down)] #'org-colviewx-sort)
(org-defkey org-columns-map [(meta up)] #'org-colviewx-sort-reverse)
(org-defkey org-columns-map [(shift meta down)]
            #'org-colviewx-move-subtree-down)
(org-defkey org-columns-map [(shift meta up)]
            #'org-colviewx-move-subtree-up)


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
(org-defkey org-columns-map  "%" #'org-colviewx-replace-regexp)

(org-defkey org-columns-map "i" #'org-insert-heading-respect-content)

(org-defkey org-columns-map "A" #'org-columns-edit-attributes)
(org-defkey org-columns-map "=" #'org-columns-next-allowed-value)
(org-defkey org-columns-map "-" #'org-columns-previous-allowed-value)


(provide 'org-colviewx)

;;; org-colviewx.el ends here
