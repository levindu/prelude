;;; prelude-org-norang.el --- Emacs Prelude: org-mode norang style configuration.
;;
;; Copyright © 2015 Levin Du
;;
;; Author: Levin Du <zslevin@gmail.com
;; URL: https://github.com/zlevin/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is adapted http://orgmode.org/worg/org-tutorials/org-outside-org.html

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

;;; Usage
;;;; Poporg
;; poporg helps editing program strings and comments using Org mode.

;; 1. Put cursor in comment or string.
;; 2. Press `M-# "' to invoke poporg-dwim
;; 3. Edit text in new org-mode buffer.
;; 4. Press `C-x C-s' to save,
;;    or kill the buffer to abort.

;;; Code:
;; Customize the outline prefix key in "personal/preload/" directory
;; (setq outline-minor-mode-prefix "\M-#")

(use-package outline
  :commands outline-minor-mode
  :config
  (progn
    ;; outshine
    (use-package outshine
      :config
      (progn
        (setq outshine-use-speed-commands t)))

    ;; outorg
    (use-package outorg
      :commands outorg-edit-as-org) 

    ;; poporg
    (use-package poporg
      :commands poporg-dwim)
    (let ((map (lookup-key outline-minor-mode-map outline-minor-mode-prefix)))
      (define-key map (kbd "\"") 'poporg-dwim))

    ;; navi-mode
    (use-package navi-mode
      :commands (navi-search-and-switch navi-switch-to-twin-buffer))

    ;; hook
    (add-hook 'outline-minor-mode-hook 'outshine-hook-function)))

(add-hook 'prog-mode-hook 'outline-minor-mode)

(provide 'prelude-outline)
;;; prelude-outline.el ends here
