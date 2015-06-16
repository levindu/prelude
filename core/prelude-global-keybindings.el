;;; prelude-global-keybindings.el --- Emacs Prelude: some useful keybindings.
;;
;; Copyright © 2011-2015 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Lots of useful keybindings.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; Smart tab
(global-set-key (kbd "TAB") 'prelude-smart-tab)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Font size
;; (global-set-key (kbd "C-+") 'text-scale-increase)
;; (global-set-key (kbd "C--") 'text-scale-decrease)

;; Window switching. (C-x o goes to the next window)
(global-set-key (kbd "C-x O") 'prelude-other-window-backward) ;; back one

;; Indentation help
(global-set-key (kbd "C-^") 'prelude-top-join-line)

;; Start proced in a similar manner to dired
(unless (eq system-type 'darwin)
    (global-set-key (kbd "C-x p") 'proced))

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x m") 'prelude-shell)

;; Start eshell, use C-u to create new
(global-set-key (kbd "C-x M") 'eshell)

;; If you want to be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'smex)

;; Check syntax/whitespace
(global-set-key (kbd "C-x y") 'prelude-check-mode)

;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" 'apropos)

;; A quick major mode help with discover-my-major
(define-key 'help-command (kbd "C-m") 'discover-my-major)

(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)

(define-key 'help-command (kbd "C-i") 'info-display-manual)

;; replace zap-to-char functionaity with the more powerful zop-to-char
(global-set-key (kbd "M-z") 'zop-up-to-char)
(global-set-key (kbd "M-Z") 'zop-to-char)

;; kill lines backward
(global-set-key (kbd "C-<backspace>") (lambda ()
                                        (interactive)
                                        (kill-line 0)
                                        (indent-according-to-mode)))

(global-set-key [remap kill-whole-line] 'prelude-kill-whole-line)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp
                 isearch-string
               (regexp-quote isearch-string))))))

;;flush or keep lines in occur-mode buffer
(define-key occur-mode-map "f"
  (lambda () (interactive)
    (let ((buffer-read-only))
      (save-excursion
        (beginning-of-buffer)
        (call-interactively 'flush-lines)))))

(define-key occur-mode-map "k"
  (lambda () (interactive)
    (let ((buffer-read-only))
      (save-excursion
        (beginning-of-buffer)
        (call-interactively 'keep-lines)))))

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

(unless (fboundp 'toggle-frame-fullscreen)
  (global-set-key (kbd "<f11>") 'prelude-fullscreen))

;; toggle menu-bar visibility
(global-set-key (kbd "<f12>") 'menu-bar-mode)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(use-package git-messenger
  :bind ("C-x v p" . git-messenger:popup-message))

(global-set-key (kbd "C-=") 'er/expand-region)

;; Use C-' to jump in isearch
(avy-setup-default)

(global-set-key (kbd "C-c j") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "s-.") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "s-w") 'ace-window)

(global-set-key (kbd "C-'") 'prelude-switch-to-previous-buffer)
(global-set-key (kbd "C-`") 'prelude-switch-to-same-mode-buffer)
(global-set-key (kbd "C-\"") 'goto-last-change)

;; M-g is the goto commander
(smartrep-define-key global-map "M-g"
  '(;; error navigation
    ("M-n" . next-error)
    ("n" . next-error)
    ("M-p" . previous-error)
    ("p" . previous-error)

    ;; window switch
    ("M-o" . other-window)
    ("o" . other-window)
    ("M-i" . prelude-other-window-backward)
    ("i" . prelude-other-window-backward)
    ("M-u" . winner-undo)
    ("u" . winner-redo)

    ;; buffer switch
    ("M-j" . previous-buffer)
    ("j" . previous-buffer)
    ("M-k" . next-buffer)
    ("k" . next-buffer)

    ;; misc
    ("M-m" . goto-last-change)
    ("m" . goto-last-change-reverse)
    ("M-f" . ahs-forward)
    ("M-b" . ahs-backward)
    ("M-F" . ahs-forward-definition)
    ("M-B" . ahs-backward-definition)
    ))

;; (global-set-key (kbd "M-g M-g") 'goto-line)
;; (global-set-key (kbd "M-g g") 'goto-line)
(global-set-key (kbd "M-g M-c") 'avy-goto-char)
(global-set-key (kbd "M-g c") 'avy-goto-char)
(global-set-key (kbd "M-g M-w") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "M-g w") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "M-g M-l") 'avy-goto-line)
(global-set-key (kbd "M-g l") 'avy-goto-line)
(global-set-key (kbd "M-g M-y") 'ace-window)
(global-set-key (kbd "M-g y") 'ace-window)

(global-set-key (kbd "M-g M-v") 'switch-to-buffer-other-window)
(global-set-key (kbd "M-g v") 'switch-to-buffer-other-window)
(global-set-key (kbd "M-g .") 'imenu)

(provide 'prelude-global-keybindings)

;;; prelude-global-keybindings.el ends here
