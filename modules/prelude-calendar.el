;;; prelude-calendar.el --- Emacs Prelude: calendar configuration.
;;
;; Copyright © 2015 Levin Du
;;
;; Author: Levin Du <zslevin@gmail.com>
;; URL: https://github.com/zslevin/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for calendar.

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

(require 'calfw-cal)
;; (require 'calfw-ical)
(require 'calfw-org)

(defun ex-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")  ; orgmode source
    ;; (cfw:howm-create-source "Blue")  ; howm source
    ;; (cfw:cal-create-source "Orange") ; diary source
    ;; (cfw:ical-create-source "Moon" "~/moon.ics" "Gray")  ; ICS source1
    ;; (cfw:ical-create-source "gcal" "https://..../basic.ics" "IndianRed") ; google calendar ICS
    )))

(cond
 (nil ;; Default setting
  (setq cfw:fchar-junction ?+
        cfw:fchar-vertical-line ?|
        cfw:fchar-horizontal-line ?-
        cfw:fchar-left-junction ?+
        cfw:fchar-right-junction ?+
        cfw:fchar-top-junction ?+
        cfw:fchar-top-left-corner ?+
        cfw:fchar-top-right-corner ?+ ))
 (nil ;; Unicode characters
  (setq cfw:fchar-junction ?╋
        cfw:fchar-vertical-line ?┃
        cfw:fchar-horizontal-line ?━
        cfw:fchar-left-junction ?┣
        cfw:fchar-right-junction ?┫
        cfw:fchar-top-junction ?┯
        cfw:fchar-top-left-corner ?┏
        cfw:fchar-top-right-corner ?┓))
 (t ;; Another unicode chars
  (setq cfw:fchar-junction ?╬
        cfw:fchar-vertical-line ?║
        cfw:fchar-horizontal-line ?═
        cfw:fchar-left-junction ?╠
        cfw:fchar-right-junction ?╣
        cfw:fchar-top-junction ?╦
        cfw:fchar-top-left-corner ?╔
        cfw:fchar-top-right-corner ?╗)))

;; * Diary
;; (setq diary-file (USER_DATA_PATH "diary"))
(add-hook 'diary-hook 'appt-make-list)

(setq display-time-world-list
      '(("America/Los_Angeles" "Seattle")
        ("America/New_York" "New York")
        ("Europe/London" "London")
        ("Europe/Paris" "Paris")
        ("Asia/Shanghai" "Shanghai")
        ("Asia/Tokyo" "Tokyo")))
(setq display-time-world-time-format "%F %a %T %z")

(provide 'prelude-calendar)
;;; prelude-calendar.el ends here
