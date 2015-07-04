;;; prelude-chinese.el --- Emacs Prelude: Chinese configuration.
;;
;; Copyright © 2015 Levin Du
;;
;; Author: Levin Du <zslevin@gmail.com>
;; URL: https://github.com/zslevin/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for Chinese.

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

;; 设置 sentence-end 可以识别中文标点
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[
\t]\\)\\)[ \t\n]*")

;; `M-x pangu-spacing-mode' 自动在中英文间加空格
(use-package pangu-spacing
  :commands pangu-spacing-mode)

(defun pangu-spacing-cleanup()
  "Insert separator between English words and Chinese charactors in buffer."
  (interactive)
  (require 'pangu-spacing)
  (let ((pangu-spacing-real-insert-separtor t))
    (pangu-spacing-modify-buffer)))

(defun prelude-insert-chinese-date()
  "Insert current datetime in Chinese format."
  (interactive)
  (let* ((system-time-locale "zh_CN.UTF-8")
         (time-strs (mapcar (lambda (format)
                              (format-time-string format))
                            '("%Y年%-m月%-d日 %A %p%-I时%-M分"
                              "%c"
                              "%Y年%-m月%-d日"))))
    (when (fboundp 'format-cntime-string)
      (setq time-strs (append time-strs
                              (mapcar (lambda (format)
                                        (format-cntime-string format))
                                      '("%Y年%m%d%q%o")))))
    (insert (completing-read "Select: " time-strs))))


;; 世界时 M-x display-time-world


;; * 输入法
;; ** eim 输入法
(use-package eim
  :commands eim-use-package
  :init
  (progn
    ;; Tooltip 暂时还不好用
    (setq eim-use-tooltip nil)

    (register-input-method
     "eim-wb" "euc-cn" 'eim-use-package
     "五笔" "汉字五笔输入法" "wb.txt")

    (register-input-method
     "eim-py" "euc-cn" 'eim-use-package
     "拼音" "汉字拼音输入法" "py.txt")))
(setq default-input-method "eim-wb")
;; `M-x eim-punc-translate-toggle' 切换全半角标点 
;; `M-x eim-insert-ascii' 插入英文数字

;; ** 输入法切换
(use-package ime-switch
  :bind ("C-\\" . ime-switch-input-method)
  :ensure nil
  :init
  (setq ime-switch-list '("eim-wb" "eim-py" "japanese")))

;; * 时间
;; ** 状态栏时间
(setq display-time-string-forms
      '((format-time-string "%H:%M/%d") ;;"%m-%d %H:%M"
        ;; 显示农历
        (when (fboundp 'format-cntime-string)
          (format-cntime-string " %d%q%o")))) ;;"%a %m%d%q%o"

;; ** 世界时
(setq display-time-world-list '(("Asia/Shanghai" "上海")
                                ("America/New_York" "纽约")
                                ("Europe/London" "伦敦")
                                ("Europe/Paris" "巴黎")
                                ("Asia/Tokyo" "东京")))

;; ** Org-mode
(defun org/agenda-format-date-chinese (date)
  (let* ((time (encode-time
                0 0 0
                (calendar-extract-day date)
                (calendar-extract-month date)
                (calendar-extract-year date)))
         (iso-week (org-days-to-iso-week
		    (calendar-absolute-from-gregorian date)))
         (day-of-week (calendar-day-of-week date))
         (weekstring (if (= day-of-week 1)
                         (format " 第%02d周" iso-week)
                       "")))
    (format "%-9s %s %s%s"
            (calendar-day-name date)
            (format-time-string "%-m月%-d日" time)
            (if (fboundp 'format-cntime-string)
                (format-cntime-string "%m%d" time)
              "")
            weekstring)))
(setq org-agenda-format-date 'org/agenda-format-date-chinese)

(provide 'prelude-chinese)
;;; prelude-chinese.el ends here
