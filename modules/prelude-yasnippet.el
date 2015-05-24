;;; prelude-yasnippet.el --- Emacs Prelude: yasnippet configuration.
;;
;; Copyright © 2015 Levin Du
;;
;; Author: Levin Du <zslevin@gmail.com>
;; URL: http://batsov.com/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for yasnippet.

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

(prelude-require-packages '(yasnippet auto-yasnippet))

(use-package yasnippet
  :commands yas-global-mode
  :diminish yas-minor-mode
  :init (setq yas-prompt-functions '(yas-dropdown-prompt
                                     yas-ido-prompt
                                     yas-completing-prompt)))

;; M-x aya-create
;; M-x aya-expand

(provide 'prelude-yasnippet)
;;; prelude-yasnippet.el ends here
