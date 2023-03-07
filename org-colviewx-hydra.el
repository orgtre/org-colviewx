;;; org-colviewx-hydra.el --- Hydra for org-colview -*- lexical-binding: t; -*-

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

;; Creates a hydra containing all the commands provided by org-colview.el
;; and org-colviewx.el.

;;; Code:

(require 'hydra)
(require 'org-colview)
(require 'org-colviewx)

(defcustom org-colviewx-hydra-wide-cutoff 119
  "Frame width from which to use the wide hydra."
  :group 'org-colviewx
  :type 'integer)

;;;###autoload
(defun org-colviewx-hydra ()
  "Hydra for org-colviewx.
`org-colviewx-hydra-wide-cutoff' controls whether a narrow or wide
version of the hydra is presented. The keybindings in the hydra are
the same as the keybindings active on the column view overlays.
Eeach u indicates that the command changes behavior with one universal
argument prefix, and n indicates that it takes numeric prefix arguments."
  (interactive)
  (if (>= (frame-parameter nil 'width)
          org-colviewx-hydra-wide-cutoff)
      (hydra-org-colviewx-wide/body)
    (hydra-org-colviewx/body)))

;; (org-defkey org-columns-map "x" #'org-colviewx-hydra)

(defhydra hydra-org-colviewx (:color pink :hint nil)
  (format
   "     %s                ^^^^^^^%s                     ^^^^%s
  _→_/_f_ forward               _e_ edit value             _v_ view value
  _←_/_b_ backward            _s-c_ copy value             _o_ open link
  _↓_/_n_ next item           _s-v_ paste value            _c_ cycle (uuu)
  _↑_/_p_ previous item       _C-d_ copy from above        _C_ global cycle (n)
     ^^_._ contents (u)         _⌫_ delete value           _D_ show drawers (n)
     ^^_j_ org-goto           _C-⌫_ cut subtree            ^%s
^^^^                             _i_ insert heading         _d_ drawer (uu)
     ^%s                  ^^^^^^_%%_ replace in column (u)  _t_ top of buffer (n)
     _r_ redo row              ^^^%s                 ^^^^^^^_s_ side windows (u)
     _g_ redo all              ^^_a_ edit                   _P_ column properties
     _F_ fit/move frame   _S-→_/_=_ next
     _S_ switch format    _S-←_/_-_ previous               ^%s
     ^%s               ^^^^^^^^^0-9 set               _\\^_/_M-↓_ sort (u)
     _A_ edit attributes                             ^^^^^^^_M-↑_ sort reverse
     _>_ widen (n)             ^^^%s              ^^^^^^^^_S-M-↓_ move tree down (n)
     _<_ narrow (n)            ^_\\_ filter (nuu)      ^^^_S-M-↑_ move tree up (n)
  _M-→_ move right            ^^_|_ reset filter
  _M-←_ move left                                      ^^^^^%s
_S-M-→_ new                                            ^^^_q_ quit colview
_S-M-←_ delete                                         ^^^_x_ exit hydra"
   (propertize "Navigate" 'face 'hydra-face-blue)
   (propertize "Edit" 'face 'hydra-face-blue)
   (propertize "View" 'face 'hydra-face-blue)
   (propertize "toggle:" 'face 'hydra-face-blue)
   (propertize "Layout" 'face 'hydra-face-blue)
   (propertize "allowed:" 'face 'hydra-face-blue)
   (propertize "Sort" 'face 'hydra-face-blue)
   (propertize "column:" 'face 'hydra-face-blue)
   (propertize "Filter" 'face 'hydra-face-blue)
   (propertize "Abort" 'face 'hydra-face-blue))
  ;; Navigate
  ("→" org-colviewx-forward)
  ("f" org-colviewx-forward)
  ("←" org-colviewx-backward)
  ("b" org-colviewx-backward)
  ("↓" org-colviewx-next-item)
  ("n" org-colviewx-next-item)
  ("↑" org-colviewx-previous-item)
  ("p" org-colviewx-previous-item)
  ("." org-colviewx-beginning-of-contents+)
  ("j" org-goto)
  ;; Edit
  ("e" org-colviewx-edit-value)
  ("s-c" org-colviewx-copy-value)
  ("s-v" org-colviewx-paste-value)
  ("C-d" org-colviewx-copy-value-from-above)
  ("⌫" org-colviewx-delete-value)
  ("C-⌫" org-colviewx-cut-subtree)
  ("i" org-insert-heading-respect-content)
  ("%" org-colviewx-replace-regexp)
  ;; allowed:
  ("a" org-columns-edit-allowed)
  ("=" org-columns-next-allowed-value)
  ("S-→" org-columns-next-allowed-value)
  ("-" org-columns-previous-allowed-value)
  ("S-←" org-columns-previous-allowed-value)
  ;; "View
  ("v" org-columns-show-value)
  ("o" org-colviewx-open-link)
  ("c" org-cycle)
  ("C" org-shifttab)
  ("D" org-colviewx-show-all-drawers)
  ;; toggle:
  ("d" org-colviewx-entry-toggle-drawer)
  ("t" org-colviewx-toggle-top)
  ("s" org-colviewx-side-windows-toggle)
  ("P" org-colviewx-toggle-column-properties-visibility)
  ;; Layout
  ("r" org-colviewx-redo-row)
  ("g" org-columns-redo)
  ("F" org-colviewx-fit-and-move-frame)
  ("S" org-colviewx-switch-format)
  ;; column:
  ("A" org-columns-edit-attributes)
  (">" org-columns-widen)
  ("<" org-columns-narrow)
  ("M-→"  org-columns-move-right)
  ("M-←" org-columns-move-left)
  ("S-M-→" org-columns-new)
  ("S-M-←" org-columns-delete)
  ;; Sort
  ("^" org-colviewx-sort)
  ("M-↓" org-colviewx-sort)
  ("M-↑" org-colviewx-sort-reverse)
  ("S-M-↓" org-colviewx-move-subtree-down)
  ("S-M-↑" org-colviewx-move-subtree-up)
  ;; Filter
  ("\\" org-colviewx-filter)
  ("|" org-colviewx-reset-filter)
  ;; Exit
  ("q" org-columns-quit :color blue) ;; also C-c C-c
  ("x" nil :color blue))

(defhydra hydra-org-colviewx-wide (:color pink :hint nil)
  (format
   "   %s            ^^^^^^^%s                     ^^^^%s                      ^^^%s                  ^%s
_→_/_f_ forward           _e_ edit value             _v_ view value              _r_ redo row         _\\^_/_M-↓_ sort (u)
_←_/_b_ backward        _s-c_ copy value             _o_ open link               _g_ redo all           _M-↑_ sort reverse
_↓_/_n_ next item       _s-v_ paste value            _c_ cycle (uuu)             _F_ fit/move frame   _S-M-↓_ move tree down (n)
_↑_/_p_ previous item   _C-d_ copy from above        _C_ global cycle (n)        _S_ switch format    _S-M-↑_ move tree up (n)
   ^^_._ contents (u)     _⌫_ delete value           _D_ show drawers (n)        ^%s
   ^^_j_ org-goto       _C-⌫_ cut subtree            ^%s                   ^^^^^^_A_ edit attributes       ^^%s
^^^^                       _i_ insert heading         _d_ drawer (uu)             _>_ widen (n)             ^_\\_ filter (nuu)
                       ^^^_%%_ replace in column (u)  _t_ top of buffer (n)       _<_ narrow (n)            ^^_|_ reset filter
                       ^^^^^%s                 ^^^^^^^_s_ side windows (u)     _M-→_ move right
                       ^^^^_a_ edit                   _P_ column properties    _M-←_ move left             ^^%s
                  ^^_S-→_/_=_ next                                        ^^_S-M-→_ new                   ^_q_ quit colview
                  ^^_S-←_/_-_ previous                                    ^^_S-M-←_ delete                ^_x_ exit hydra
                     ^^^^^0-9 set"
   (propertize "Navigate" 'face 'hydra-face-blue)
   (propertize "Edit" 'face 'hydra-face-blue)
   (propertize "View" 'face 'hydra-face-blue)
   (propertize "Layout" 'face 'hydra-face-blue)
   (propertize "Sort" 'face 'hydra-face-blue)
   (propertize "column:" 'face 'hydra-face-blue)
   (propertize "toggle:" 'face 'hydra-face-blue)
   (propertize "Filter" 'face 'hydra-face-blue)
   (propertize "allowed:" 'face 'hydra-face-blue)
   (propertize "Abort" 'face 'hydra-face-blue))
  ;; Navigate
  ("→" org-colviewx-forward)
  ("f" org-colviewx-forward)
  ("←" org-colviewx-backward)
  ("b" org-colviewx-backward)
  ("↓" org-colviewx-next-item)
  ("n" org-colviewx-next-item)
  ("↑" org-colviewx-previous-item)
  ("p" org-colviewx-previous-item)
  ("." org-colviewx-beginning-of-contents+)
  ("j" org-goto)
  ;; Edit
  ("e" org-colviewx-edit-value)
  ("s-c" org-colviewx-copy-value)
  ("s-v" org-colviewx-paste-value)
  ("C-d" org-colviewx-copy-value-from-above)
  ("⌫" org-colviewx-delete-value)
  ("C-⌫" org-colviewx-cut-subtree)
  ("i" org-insert-heading-respect-content)
  ("%" org-colviewx-replace-regexp)
  ;; allowed:
  ("a" org-columns-edit-allowed)
  ("=" org-columns-next-allowed-value)
  ("S-→" org-columns-next-allowed-value)
  ("-" org-columns-previous-allowed-value)
  ("S-←" org-columns-previous-allowed-value)
  ;; "View
  ("v" org-columns-show-value)
  ("o" org-colviewx-open-link)
  ("c" org-cycle)
  ("C" org-shifttab)
  ("D" org-colviewx-show-all-drawers)
  ;; toggle:
  ("d" org-colviewx-entry-toggle-drawer)
  ("t" org-colviewx-toggle-top)
  ("s" org-colviewx-side-windows-toggle)
  ("P" org-colviewx-toggle-column-properties-visibility)
  ;; Layout
  ("r" org-colviewx-redo-row)
  ("g" org-columns-redo)
  ("F" org-colviewx-fit-and-move-frame)
  ("S" org-colviewx-switch-format)
  ;; column:
  ("A" org-columns-edit-attributes)
  (">" org-columns-widen)
  ("<" org-columns-narrow)
  ("M-→"  org-columns-move-right)
  ("M-←" org-columns-move-left)
  ("S-M-→" org-columns-new)
  ("S-M-←" org-columns-delete)
  ;; Sort
  ("^" org-colviewx-sort)
  ("M-↓" org-colviewx-sort)
  ("M-↑" org-colviewx-sort-reverse)
  ("S-M-↓" org-colviewx-move-subtree-down)
  ("S-M-↑" org-colviewx-move-subtree-up)
  ;; Filter
  ("\\" org-colviewx-filter)
  ("|" org-colviewx-reset-filter)
  ;; Exit
  ("q" org-columns-quit :color blue) ;; also C-c C-c
  ("x" nil :color blue))

(provide 'org-colviewx-hydra)

;;; org-colviewx-hydra.el ends here
