;;; calendar-lunar.el --- calendar for Chinese lunar

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Charles Wang  for wcy-chinese-calendar.el
;;         Milton Wu(wulei) for chinese-calendar.el (miltonwulei@163.com)
;;         Levin Du for the current version (zslevin@gmail.com)
;;
;; Keywords: calendar, i18n

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Installation:
;; Put the following line in your .emacs:
;;
;;    (add-to-path 'load-path "path/where/to/calendar-lunar"
;;    (require 'calendar-lunar)
;;
;;; Code:

;; * Requires
(eval-when-compile
  (require 'cl)
  (require 'names))
(require 's)
(require 'calendar)
(require 'cal-china)
(require 'cal-move)
(require 'holidays)
(require 'diary-lib)
(require 'solar)

(defvar displayed-month)
(defvar displayed-year)
(defvar date)
(defvar date-string)
(defvar diary-entries-list)
(defvar entry)
(defvar original-date)

;; * Core
;; ** Begin of calendar-lunar- namespace definition
(define-namespace calendar-lunar-

;; ** Variables
(defvar indent 0
  "Indent to display chinese calendar")

;;store the current year's jieqi list
(defvar current-year-jieqi-list nil)

(defvar celestial-stem
      ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])

(defvar terrestrial-branch
      ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])

(defvar shuxiang-names
  ["鼠" "牛" "虎" "兔" "龙" "蛇" "马" "羊" "猴" "鸡" "狗" "猪"])

(defvar n-hemi-seasons '( "春分" "夏至" "秋分"  "冬至"))
(defvar s-hemi-seasons '("秋分" "夏至" "春分" "冬至"))

(defvar chinese-number ["一" "二" "三" "四" "五" "六" "七" "八" "九" "十"])

(defvar jieqi-names
  ["小寒" "大寒" "立春" "雨水" "惊蛰" "春分"
   "清明" "谷雨" "立夏" "小满" "芒种" "夏至"
   "小暑" "大暑" "立秋" "处暑" "白露" "秋分"
   "寒露" "霜降" "立冬" "小雪" "大雪" "冬至"])

(defvar month-names
  [
   "正月" "二月" "三月" "四月" "五月" "六月"
   "七月" "八月" "九月" "十月" "冬月" "腊月"])

(defvar day-names
  [
   "初一" "初二" "初三" "初四" "初五" "初六" "初七" "初八" "初九" "初十"
   "十一" "十二" "十三" "十四" "十五" "十六" "十七" "十八" "十九" "二十"
   "廿一" "廿二" "廿三" "廿四" "廿五" "廿六" "廿七" "廿八" "廿九" "三十"
   ])

;;定义日记模式中识别每个条目开始的日期信息
(defvar diary-date-forms
  '(;; 美式格式
    (month "/" day "[^/0-9]")
    (month "/" day "/" year "[^0-9]")
    (monthname " *" day "[^,0-9]")
    (monthname " *" day ", *" year "[^0-9]")
    (dayname "\\W")

    ;; 英式格式,不要和美式格式混用，选择一个
    ;; (day "/" month "[^/0-9]")
    ;; (day "/" month "/" year "[^0-9]")
    ;; (backup day " *" monthname "\\W+\\<[^*0-9]")
    ;; (day " *" monthname " *" year "[^0-9]")
    ;; (dayname "\\W")

    ;; 中国格式
    (year "年[ ]*" month "月[ ]*" day "日[^/0-9]星期[一二
三四五六日]")
    ))

;; ** Functions
;; *** Datetime
;;将阳历日期换算成阴历,返回汉字日期名称
(defun year-name (date)
  "return chinese month name string ,the date form is (month day year)"
  (let* ((year (car (cdr (calendar-chinese-from-absolute
                          (calendar-absolute-from-gregorian date))))))
    (sexagesimal-name year)))

(defun month-name (date)
  "return chinese month name string ,the date form is (month day year)"
  (let* ((a-date (calendar-absolute-from-gregorian date));;绝对日期
         (c-date (calendar-chinese-from-absolute a-date));;阴历日期
         (c-month (nth 2 c-date)))
    (format "%s%s"
            (if (not (integerp c-month));; .5格式表示是闰月
                "闰" "")
            (aref month-names (1- (floor c-month))))))

(defun day-name(date)
  "Return this day's calendar-chinese name , the date is gregorian date "
  (let* ((c-day (nth 3
                     (calendar-chinese-from-absolute
                      (calendar-absolute-from-gregorian date)))))

    (format "%s" (aref day-names (1- c-day)))))

(defun week-name (date)
  (let ((day  (calendar-day-of-week date)))
    (concat "星期"
            (if (eq day 0)
                "日"
              (aref chinese-number (1- day))))))

(defun sexagesimal-name (n)
  "The N-th name of the Chinese sexagesimal cycle.
N congruent to 1 gives the first name, N congruent to 2 gives the second name,
..., N congruent to 60 gives the sixtieth name."
  (format "%s%s"
          (aref celestial-stem (% (1- n) 10))
          (aref terrestrial-branch (% (1- n) 12))))

(defun shuxiang-name (year)
  " The chinese shuxiang name of the year."
  (let ((n (mod (- year 4) 12) ))
    (aref shuxiang-names n)))

;;定义中国日期的显示格式
(defun date-display-form (date)
  (let* ((weekname (week-name date))
         (day   (calendar-extract-day date))
         (month (calendar-extract-month date))
         (year  (calendar-extract-year date)))
    (format "%4d年%d月%d日 %s" year month day weekname)))

;; *** Jieqi
;;定义节气的识别函数
;;24节气的计算方法是，
;;从冬至开始，地球围绕太阳每转动15度的那一刻就是一个节日
(defun next-jieqi-date (d)
  "Return the next Jieqi after or on the absolute date d,
the Return data form is absolute date"
  (solar-date-next-longitude d 15))

(defun create-jieqi-list (year)
  " Create the year's jieqi ,store it in the variable
calendar-lunar-current-year-jieqi-list.
The format is (year (month day) (month day) ... (month day))
"
  (if (not (equal current-year-jieqi-list nil))
      (setq current-year-jieqi-list nil))
  (let* ((start-date
          (calendar-astro-from-absolute
           (calendar-absolute-from-gregorian (list 1 1 year))))
         (i 0)
         jieqi-date temp-date)
    (setq current-year-jieqi-list (list year))
    (while (< i 24)
      (setq i (1+ i))
      (setq temp-date  (next-jieqi-date start-date))
      (setq jieqi-date
            (calendar-gregorian-from-absolute
             (floor (calendar-astro-to-absolute temp-date))))
      (setq current-year-jieqi-list
            (cons (list (car jieqi-date) (car (cdr jieqi-date)))
                  current-year-jieqi-list))
      (setq start-date (1+ temp-date)))
    (setq current-year-jieqi-list
          (nreverse current-year-jieqi-list))))

(defun jieqi-p (month day year)
  "If the date is jieqi return t,else return nil"
  ;;if the jieqi-list is empty or the jieqi-list is not for this year
  ;;then create the jieqi-list for the year
  (if (or (equal current-year-jieqi-list nil)
          (not
           (equal (car current-year-jieqi-list) year)))
      (create-jieqi-list year))
  (if (member (list month day) current-year-jieqi-list)
      t
    nil))

(defun jieqi-name (month day year)
  "Get the chinese jieqi name if that day is  a jieqi, else return nil"
  (if (jieqi-p month day year)
      (let ((name-index (+ (* 2 (1- month)) (/ day 15) )))
        (aref jieqi-names name-index))))

;; *** Calendar
(defun day-name-displayed(month day year)
  "If this day is a jieqi return it's jieqi name
else if this day is the first day of a month return that month's name
else return the  day name"
  (let* ((jieqi-name (jieqi-name month day year))
         (month-name )
         (c-day (nth 3
                     (calendar-chinese-from-absolute
                      (calendar-absolute-from-gregorian (list month day year))))))
    (if jieqi-name
        jieqi-name
      (if (= c-day 1)
          (let* ((month-name
                  (month-name (list month day year))))
            (if (= 2 (length month-name))
                month-name
              ;;avoid to display leap month name in 3 characters
              (concat
               (char-to-string (aref month-name 0))
               (char-to-string (aref month-name 1)))))
        (day-name (list month day year))))))

;;设置日历显示模式下的状态栏
(defun set-mode-line ()
  (let* ((date (calendar-cursor-to-date))
         (s1 (calendar-date-string date t))
         (s2 (format "农历%s年%s%s"
                     (year-name date)
                     (month-name date)
                     (day-name date)))
         (s3 (cursor-holidays))
         (x (list "" s1 s2 s3 ""))
         ;; (y (make-string (apply '+  (mapcar 'count-chinese-character x )) ? ))
         )
    (progn
      (setq calendar-mode-line-format
            ;; (append x (list y)))
            x)
      (calendar-update-mode-line)
      (force-mode-line-update))))

;;;在mode line 显示光标当前日期的节日名称，如果该日是节日的话
(defun cursor-holidays ()
  "set mode line , holidays for the date specified by the cursor in the calendar window.
  return today's hoilday name string if today is a holiday ,otherwise return 2 space "
  (let* ((date (calendar-cursor-to-date t))
         ;; (date-string (calendar-date-string date))
         (holiday-list (calendar-check-holidays date))
         (holiday-string (mapconcat 'identity holiday-list "; ")))
    (format "%s" holiday-string)))

;;计算汉字符数目的函数
(defun count-chinese-character (string)
  "count the numbers of chinese character "
  (length (remq nil
                (mapcar 'multibyte-string-p
                        (mapcar 'char-to-string string)))))

;; * Format / Conversion
;;test this function use this
;;(format-cntime-string "%Y年%m%d%q%o" (encode-time 0 0 0 4 2 1992))
;; the result is "壬申年正月 初一立春壬申年春节"
(defun format-cntime-string (string &optional time universal)
  "string format is '%y %m %d %j %h '
`%Y' This stands for the year with century
`%m' This stands for the month (01-12).
`%d' This stands for the day of month, zero-padded.
`%q' This stands for the jieqi of that day ,
     if the day is  not a jieqi ,output empty string
`%o' This stands for the holidays of that day,
     if the day has no holiday, output empty string
any other argument is totally same as the function format-time-string
"
  (if (not time)
      (setq time (current-time)))
  (let* ((calendar-list (decode-time time))
         (date (list (nth 4 calendar-list)
                     (nth 3 calendar-list)
                     (nth 5 calendar-list))))
    (format-time-string
     (s-replace-all
      (list
       (cons "%Y" (year-name date))
       (cons "%m" (month-name date))
       (cons "%d" (day-name date))
       (cons "%q" (or (jieqi-name
                       (car date)
                       (nth 1 date)
                       (nth 2 date))
                      ""))
       (cons "%s" (mapconcat 'identity
                             (calendar-check-holidays date)
                             "; ")))
      string)
     time universal)))

;; 新历转换为农历
(defun chinese-from-gregorian-date (month day &optional year)
  "Convert date from Gregorian form to Chinese lunar.

Call it like (chinese-from-gregorian-date 12 12 1977) and return
like (11 2 1977 \"shuxiang\" \"sexagesimal\")."

  (let* ((year (or year (calendar-extract-year
                         (calendar-current-date))))
         (absolute (calendar-absolute-from-gregorian
                    (list month day year)))
         (chinese-date (calendar-chinese-from-absolute absolute))
         (cyear (nth 1 chinese-date))
         (cmonth (nth 2 chinese-date))
         (cday (nth 3 chinese-date)))
    (if (> cmonth month) (decf year))
    (list cmonth
          cday
          year
          (shuxiang-name year)
          (concat (sexagesimal-name cyear)
                  "年"
                  (sexagesimal-name
                   (+ (* 12 cyear) (floor cmonth) 50))
                  "月"
                  (sexagesimal-name
                   (+ absolute 15))
                  "日"))))

;; 农历转换为新历
;; 闰7月用 7.5 表示，如 (chinese-to-gregorian-date 7.5 16 2006) => (9 8 2006)
(defun chinese-to-gregorian-date (cmonth cday &optional year)
  "Convert date from Chinese lunar to Gregorian form.

Call it like (chinese-to-gregorian-date 11 2 1977) and return like (12 12 1977)."

  (let* ((year (or year (calendar-extract-year
                         (calendar-current-date))))
         (current-chinese-date (calendar-chinese-from-absolute
                                (calendar-absolute-from-gregorian
                                 (list 6 1 year))))
         (cycle (car current-chinese-date))
         (cyear (cadr current-chinese-date))
         (birthday-chinese-full (list cycle cyear cmonth cday))
         (birthday-gregorian-full (calendar-gregorian-from-absolute
                                   (calendar-chinese-to-absolute
                                    birthday-chinese-full))))
    birthday-gregorian-full))

;; ** End of calendar-lunar- namespace
) ;; end of define-namespace calendar-lunar-

;; * Alias
(defalias 'format-cntime-string 'calendar-lunar-format-cntime-string)
(defalias 'chinese-from-gregorian-date 'calendar-lunar-chinese-from-gregorian-date)
(defalias 'chinese-to-gregorian-date 'calendar-lunar-chinese-to-gregorian-date)

;; * Integration
;; ** Setting
(setq calendar-day-abbrev-array
      ["Su日" "Mo一" "Tu二" "We三" "Th四" "Fr五" "Sa六" ])
;; (setq calendar-day-name-array
;;       ["星期日" "星期一" "星期二" "星期三" "星期四" "星期五"  "星期六" ])
(setq calendar-day-name-array calendar-day-abbrev-array)

;;按中国习惯，周一为每周的第一天
(setq calendar-week-start-day 1)
;;始终让当前的月份居中
(setq calendar-offset 0)

(setq calendar-date-display-form '((calendar-lunar-date-display-form date)))

;; 天干地支
(setq calendar-chinese-celestial-stem calendar-lunar-celestial-stem)
(setq calendar-chinese-terrestrial-branch calendar-lunar-terrestrial-branch)
(defalias 'calendar-chinese-sexagesimal-name 'calendar-lunar-sexagesimal-name)

;; 春分夏至秋分冬至
(setq solar-n-hemi-seasons calendar-lunar-n-hemi-seasons)
(setq solar-s-hemi-seasons calendar-lunar-s-hemi-seasons)

(defun solar-equinoxes-solstices ()
  "*local* date and time of equinoxes and solstices, if visible in the calendar window.
Requires floating point."
  (let ((m displayed-month)
        (y displayed-year))
    (let* ((calendar-standard-time-zone-name
            (if calendar-time-zone calendar-standard-time-zone-name "UTC"))
           (calendar-daylight-savings-starts
            (if calendar-time-zone calendar-daylight-savings-starts))
           (calendar-daylight-savings-ends
            (if calendar-time-zone calendar-daylight-savings-ends))
           (calendar-time-zone (if calendar-time-zone calendar-time-zone 0))
           (k (1- (/ m 3)))
           (d0 (solar-equinoxes/solstices k y))
           (d1 (list (car d0) (floor (car (cdr d0))) (car (cdr (cdr d0)))))
           (h0 (* 24 (- (car (cdr d0)) (floor (car (cdr d0))))))
           (adj (dst-adjust-time d1 h0))
           (d (list (car d1) (+ (car (cdr d1))
                                (/ (car (cdr adj)) 24.0))
                    (car (cdr (cdr d1)))))
           (abs-day (calendar-absolute-from-gregorian d)))
      (list
       (list (calendar-gregorian-from-absolute (floor abs-day))
             (format "%s %s"
                     (nth k (if (and calendar-latitude
                                     (< (calendar-latitude) 0))
                                solar-s-hemi-seasons
                              solar-n-hemi-seasons))
                     (solar-time-string
                      (* 24 (- abs-day (floor abs-day)))
                      (if (dst-in-effect abs-day)
                          calendar-daylight-time-zone-name
                        calendar-standard-time-zone-name))))))))


;; ** Holiday
;;;定义中国的节日
(setq holiday-general-holidays
      '((holiday-fixed 1 1 "元旦")
        (holiday-chinese-new-year )
        (holiday-fixed 3 8 "妇女节")
        (holiday-fixed 3 12 "植树节")
        (holiday-fixed 5 1 "劳动节")
        (holiday-fixed 5 4 "青年节")
        (holiday-fixed 6 1 "儿童节")
        (holiday-fixed 9 10 "教师节")
        (holiday-fixed 10 1 "国庆节")))

(setq holiday-local-holidays
      '((lunar-holiday-fixed 1 15 "元宵节")
        ;; (chinese-lunar-day 1 1 "我的生日")
        (lunar-holiday-fixed 5 5 "端午节")
        (lunar-holiday-fixed 8 15 "中秋节")
        (lunar-holiday-fixed 9 9 "重阳节")))

(setq holiday-christian-holidays nil)
(setq holiday-hebrew-holidays  nil)
(setq holiday-islamic-holidays nil)

;;;定义阴历节日,添加至other-holiday列表中
(setq holiday-other-holidays
      '((holiday-fixed 2 14 "情人节")
        (holiday-fixed 4 1 "愚人节")
        (holiday-float 5 0 2 "母亲节")
        (holiday-float 6 0 3 "父亲节")
        (holiday-float 11 4 4 "感恩节")
        (holiday-fixed 12 25 "圣诞节")))

(setq calendar-holidays
      (append holiday-general-holidays
              holiday-local-holidays
              holiday-other-holidays))

;;;重定义得到当前日历节日的函数，本来是一屏显示3个月，
;;;所以必须改变，否则一次将3个月的日期重复标记

;;;重定义每个节日是否在当前显示的函数
(defun holiday-fixed (month day string)
  "Holiday on MONTH, DAY (Gregorian) called STRING.
If MONTH, DAY is visible, the value returned is the list (((MONTH DAY year)
STRING)).  Returns nil if it is not visible in the current calendar window."
  (let ((m displayed-month)
        (y displayed-year))
    (if (= m month)
        (list (list (list month day y) string)))))

(defun holiday-float (month dayname n string &optional day)
  "Holiday on MONTH, DAYNAME (Nth occurrence) called STRING.
If the Nth DAYNAME in MONTH is visible, the value returned is the list
\(((MONTH DAY year) STRING)).

If N<0, count backward from the end of MONTH.

An optional parameter DAY means the Nth DAYNAME on or after/before MONTH DAY.

Returns nil if it is not visible in the current calendar window."
  (let ((m displayed-month)
        (y displayed-year))
    (if (= m month)
        (list (list (calendar-nth-named-day n dayname month y day) string)))))

(defun lunar-holiday-fixed(cmonth cday cname)
  "Chinese calendar holiday, month and day in Chinese calendar (CMONTH, CDAY).

If corresponding MONTH and DAY in gregorian calendar is visible,
the value returned is the list \(((MONTH DAY year) STRING)).
Returns nil if it is not visible in the current calendar window."
  (let* ((m displayed-month)
         (y displayed-year)
         (gdate (calendar-gregorian-from-absolute
                 (+ (cadr (assoc cmonth (calendar-chinese-year y))) (1- cday))))
         (gm (car gdate))
         (gd (cadr gdate))
         (gy (caddr gdate)))
    (if (= m gm)
        (list (list (list gm gd gy) cname)))))

(defun holiday-chinese-new-year ()
  "Date of Chinese New Year."
  (let ((m displayed-month)
        (y displayed-year))
    (calendar-increment-month m y 1)
    (if (< m 5)
        (let ((chinese-new-year
               (calendar-gregorian-from-absolute
                (car (cdr (assoc 1 (calendar-chinese-year y)))))))
          (if (calendar-date-is-visible-p chinese-new-year)
              (list
               (list chinese-new-year
                     (format "%s年春节"
                             (calendar-lunar-sexagesimal-name (+ y 57))))))))))


(defun calendar-list-holidays ()
  "Create a buffer containing the holidays for the current calendar window.
The holidays are those in the list calendar-notable-days.  Returns t if any
holidays are found, nil if not."
  (interactive)
  (message "Looking up holidays...")
  (let ((holiday-list (calendar-holiday-list))
        (m1 displayed-month)
        (y1 displayed-year)
        (m2 displayed-month)
        (y2 displayed-year))
    (if (not holiday-list)
        (progn
          (message "Looking up holidays...none found")
          nil)
      (set-buffer (get-buffer-create holiday-buffer))
      (setq buffer-read-only nil)
      (calendar-set-mode-line
       (if (= y1 y2)
           (format "Notable Dates from %s to %s, %d%%-"
                   (calendar-month-name m1) (calendar-month-name m2) y2)
         (format "Notable Dates from %s, %d to %s, %d%%-"
                 (calendar-month-name m1) y1 (calendar-month-name m2) y2)))
      (erase-buffer)
      (insert
       (mapconcat
        #'(lambda (x) (concat (calendar-date-string (car x))
                             ": " (car (cdr x))))
        holiday-list "\n"))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (display-buffer holiday-buffer)
      (message "Looking up holidays...done")
      t)))

;;只显示当前这个月的节日
(defun list-holidays (y1 y2 &optional l label)
  "Display holidays for years Y1 to Y2 (inclusive).

The optional list of holidays L defaults to `calendar-holidays'.  See the
documentation for that variable for a description of holiday lists.

The optional LABEL is used to label the buffer created."
  (interactive
   (let* ((start-year (calendar-read
                       "Starting year of holidays (>0): "
                       #'(lambda (x) (> x 0))
                       (int-to-string (calendar-extract-year
                                       (calendar-current-date)))))
          (end-year (calendar-read
                     (format "Ending year (inclusive) of holidays (>=%s): "
                             start-year)
                     #'(lambda (x) (>= x start-year))
                     (int-to-string start-year)))
          (completion-ignore-case t)
          (lists
           (list
            (cons "All" calendar-holidays)
            (if (fboundp 'atan)
                (cons "Equinoxes/Solstices"
                      (list (list 'solar-equinoxes-solstices))))
            (if holiday-general-holidays (cons "General" holiday-general-holidays))
            (if holiday-local-holidays (cons "Local" holiday-local-holidays))
            (if holiday-other-holidays (cons "Other" holiday-other-holidays))
            (if holiday-christian-holidays (cons "Christian" holiday-christian-holidays))
            (if holiday-hebrew-holidays (cons "Hebrew" holiday-hebrew-holidays))
            (if holiday-islamic-holidays (cons "Islamic" holiday-islamic-holidays))
            (if holiday-oriental-holidays (cons "Oriental" holiday-oriental-holidays))
            (if holiday-solar-holidays (cons "Solar" holiday-solar-holidays))
            (cons "Ask" nil)))
          (choice (capitalize
                   (completing-read "List (TAB for choices): " lists nil t)))
          (which (if (string-equal choice "Ask")
                     (eval (read-variable "Enter list name: "))
                   (cdr (assoc choice lists))))
          (name (if (string-equal choice "Equinoxes/Solstices")
                    choice
                  (if (member choice '("Ask" ""))
                      "Holidays"
                    (format "%s Holidays" choice)))))
     (list start-year end-year which name)))
  (message "Computing holidays...")
  (let* ((holiday-buffer "*Holidays*")
         (calendar-holidays (if l l calendar-holidays))
         (title (or label "Holidays"))
         (holiday-list nil)
         (s (calendar-absolute-from-gregorian (list 1 1 y1)));;从第一个月开始
         (e (calendar-absolute-from-gregorian (list 12 1 y2)));;到最后一个月结束
         (d s);;算法中的月份
         (never t)
         (displayed-month 1)
         (displayed-year y1))
    (while (or never (<= d e))
      (setq holiday-list (append holiday-list (calendar-holiday-list)))
      (setq never nil)
      (calendar-increment-month displayed-month displayed-year 1);;一次只搜索一个月
      (setq d (calendar-absolute-from-gregorian
               (list displayed-month 1 displayed-year))))
    (save-excursion
      (set-buffer (get-buffer-create holiday-buffer))
      (setq buffer-read-only nil)
      (calendar-set-mode-line
       (if (= y1 y2)
           (format "%s for %s" title y1)
         (format "%s for %s-%s" title y1 y2)))
      (erase-buffer)
      (goto-char (point-min))
      (insert
       (mapconcat
        #'(lambda (x) (concat (calendar-date-string (car x))
                             ": " (car (cdr x))))
        holiday-list "\n"))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (display-buffer holiday-buffer)
      (message "Computing holidays...done"))))

;; ** Calendar
;;重新绑定一些键值
(define-key calendar-mode-map [prior] 'calendar-scroll-right);;一次只移动一个月
(define-key calendar-mode-map (kbd "M-v")   'calendar-scroll-right)
(define-key calendar-mode-map [next]  'calendar-scroll-left)
(define-key calendar-mode-map (kbd "C-v")  'calendar-scroll-left)

(add-hook 'calendar-move-hook 'calendar-lunar-set-mode-line)
(add-hook 'calendar-initial-window-hook 'calendar-lunar-set-mode-line)

;; *** Create
;;重新定义显示日历的窗口，一次只显示一个月的日历
(defun calendar-generate (month year)
  "Generate a one-month Gregorian calendar centered around MONTH, YEAR."
                                        ; A negative YEAR is interpreted as BC; -1 being 1 BC, and so on.
                                        ; Note that while calendars for years BC could be displayed as it
                                        ; stands, almost all other calendar functions (eg holidays) would
                                        ; at best have unpredictable results for such dates.
  (if (< (+ month (* 12 (1- year))) 1)
      (error "Months before January, 1 AD are not available"))
  (setq displayed-month month
        displayed-year year)
  (erase-buffer)
  ;;只产生一个月的月历
  (setq calendar-lunar-indent (max 0 (/ (- (window-width) (* 7 8)) 2)))
  (calendar-generate-month month year calendar-lunar-indent))

(defun calendar-generate-month (month year indent)
  "Produce a calendar for MONTH, YEAR on the Gregorian calendar.
The calendar is inserted in the buffer starting at the line on which point
is currently located, but indented INDENT spaces.  The indentation is done
from the first character on the line and does not disturb the first INDENT
characters on the line."
  (let* ((blank-days;; at start of month
          (mod (- (calendar-day-of-week (list month 1 year))
                  calendar-week-start-day)
               7))
         (last (calendar-last-day-of-month month year)))
    (goto-char (point-min))
    (calendar-move-to-column indent)
    (insert
     (calendar-string-spread
      (list (format "  %d年%d月    农历%s年%s    属相:%s"
                    year month
                    (calendar-lunar-year-name (list month 1 year))
                    (calendar-lunar-month-name (list month 1 year))
                    (calendar-lunar-shuxiang-name year))
            ) ?\s 42))
    (calendar-ensure-newline)
    (calendar-move-to-column indent)
    ;;插入星期标头，
    (dotimes (i 7)
      (insert (calendar-day-name (mod (+ calendar-week-start-day i) 7)
                                 () t))
      (insert "  　"))
    (calendar-ensure-newline)
    (calendar-move-to-column indent)
    ;; Add blank days before the first of the month
    (dotimes (i blank-days) (insert "    　　"));;4个半角空格两个全角空格
    ;; Put in the days of the month
    (loop for i from 1 to last do
          (insert (propertize
                   (format "%2d%s" i
                           (calendar-lunar-day-name-displayed  month i year))
                   'mouse-face 'highlight
                   'help-echo "mouse-2: menu of operations for this date"
                   'date t)
                  "  ")
          (and (zerop (mod (+ i blank-days) 7))
               (/= i last)
               (progn
                 (calendar-ensure-newline)
                 (calendar-move-to-column indent))))))

;; *** Cursor
;;将光标所在的日期转换成date对象
(defun calendar-cursor-to-date (&optional error event)
  "Return a list (month day year) of current cursor position.
If cursor is not on a specific date, signals an error if optional parameter
ERROR is t, otherwise just returns nil.
If EVENT is non-nil, it's an event indicating the buffer position to
use instead of point."
  (with-current-buffer
      (if event (window-buffer (posn-window (event-start event)))
        (current-buffer))
    (save-excursion
      (and event (setq event (event-start event))
           (goto-char (posn-point event)))
      (let* ((month  displayed-month)
             (year   displayed-year))
        ;; Call with point on either of the two digits in a 2-digit date,
        ;; or on or before the digit of a 1-digit date.
        (if (not (and (looking-at "[ 0-9]?[0-9][^0-9]")
                      (get-text-property (point) 'date)))
            (if error (user-error "Not on a date!"))
          ;; Go back to before the first date digit.
          (or (looking-at " ")
              (re-search-backward "[^0-9]"))
          (list month
                (string-to-number
                 (buffer-substring (1+ (point))
                                   (+ 1 calendar-day-digit-width (point))))
                displayed-year
                ))))))

;;重定义在日期之间移动的基本函数
(defun calendar-cursor-to-visible-date (date)
  "Move the cursor to DATE that is on the screen."
  (let* ((month (calendar-extract-month date))
         (day (calendar-extract-day date))
         (year (calendar-extract-year date))
         (first-of-month-weekday (calendar-day-of-week (list month 1 year))))
    (goto-line (+ 3 ;;日期从第三行开始
                  (/ (+ day  -1
                        (mod
                         (- (calendar-day-of-week (list month 1 year))
                            calendar-week-start-day)
                         7))
                     7)))
    (move-to-column (+ calendar-lunar-indent 1
                       (* 8 (mod
                             (- (calendar-day-of-week date)
                                calendar-week-start-day)
                             7)))))
  ;; (calendar-cursor-to-nearest-date)
  )

;;重定义让光标在日期之间准确定位的函数
(defun calendar-cursor-to-nearest-date ()
  "Move the cursor to the closest date.
The position of the cursor is unchanged if it is already on a date.
Returns the list (month day year) giving the cursor position."
  (let ((date (calendar-cursor-to-date))
        (column (current-column)))
    (if date
        date
      (if (> 3 (count-lines (point-min) (point)));;日期从第三行开始
          (progn
            (goto-line 3)
            (move-to-column column)))
      (if (not (looking-at "[0-9]"))
          (if (and (not (looking-at " *$"));;非行尾
                   (< column 21))
              (progn
                (re-search-forward "[0-9]" nil t)
                (backward-char 1)
                )
            (re-search-backward "[0-9]" nil t)))
      (calendar-cursor-to-date))))


;;重新定义 calendar-date-is-visible-p函数，
;;该函数在calendar的很多函数中作为判断该日期是否显示在
;;窗口中,以便在需要的时候重绘窗口
(defun calendar-date-is-visible-p (date)
  "Return t if DATE is legal and is visible in the calendar window."
  (let ((gap (calendar-interval
              displayed-month displayed-year
              (calendar-extract-month date) (calendar-extract-year date))))
    (and (calendar-date-is-valid-p date) (> 1 gap) (< -1 gap))))

(defun calendar-forward-day (arg)
  "Move the cursor forward ARG days.
Moves backward if ARG is negative."
  (interactive "p")
  (unless (zerop arg)
    (let* ((cursor-date (or (calendar-cursor-to-date)
                            (progn
                              (if (> arg 0) (setq arg (1- arg)))
                              (calendar-cursor-to-nearest-date))))
           (new-cursor-date
            (calendar-gregorian-from-absolute
             (+ (calendar-absolute-from-gregorian cursor-date) arg)))
           (new-display-month (calendar-extract-month new-cursor-date))
           (new-display-year (calendar-extract-year new-cursor-date)))
      ;; Put the new month on the screen, if needed.
      (unless (calendar-date-is-visible-p new-cursor-date)
        ;; The next line gives smoother scrolling IMO (one month at a
        ;; time rather than two).
        ;; (calendar-increment-month new-display-month new-display-year
        ;;                          (if (< arg 0) 1 -1))
        (calendar-other-month new-display-month new-display-year))
      ;; Go to the new date.
      (calendar-cursor-to-visible-date new-cursor-date)))
  (run-hooks 'calendar-move-hook))

;; *** Mark
(defun mark-days-named (dayname &optional color)
  "Mark all dates in the calendar window that are day DAYNAME of the week.
0 means all Sundays, 1 means all Mondays, and so on."
  (save-excursion
    (set-buffer calendar-buffer)
    (let ((prev-month displayed-month)
          (prev-year displayed-year)
          (succ-month displayed-month)
          (succ-year displayed-year)
          (last-day)
          (day))
      (setq day (calendar-absolute-from-gregorian
                 (calendar-nth-named-day 1 dayname prev-month prev-year)))
      (setq last-day (calendar-absolute-from-gregorian
                      (calendar-nth-named-day -1 dayname succ-month succ-year)))
      (while (<= day last-day)
        (calendar-mark-visible-date (calendar-gregorian-from-absolute day))
        (setq day (+ day 7))))))

(defun mark-date-pattern (month day year &optional color)
  "Mark all dates in the calendar window that conform to MONTH/DAY/YEAR.
A value of 0 in any position is a wildcard."
  (save-excursion
    (set-buffer calendar-buffer)
    (let ((m displayed-month)
          (y displayed-year))
      (calendar-mark-month m y month day year color))))

;;重定义标记节日的函数
(defun calendar-mark-visible-date (date &optional mark)
  "Mark DATE in the calendar window with MARK.
MARK is a single-character string, a list of face attributes/values, or a face.
MARK defaults to `diary-entry-marker'."
  (if (calendar-date-is-valid-p date)
      (with-current-buffer calendar-buffer
        (save-excursion
          (calendar-cursor-to-visible-date date)
          (setq mark
                (or (and (stringp mark) (= (length mark) 1) mark) ; single-char
                    ;; The next two use to also check font-lock-mode.
                    ;; See comments above diary-entry-marker for why
                    ;; this was dropped.
;;;                    (and font-lock-mode
;;;                         (or
                          (and (listp mark) (> (length mark) 0) mark) ; attrs
                          (and (facep mark) mark) ; )) face-name
                          diary-entry-marker))
          (cond
           ;; Face or an attr-list that contained a face.
           ((facep mark)
            (overlay-put
             (make-overlay (1- (point)) (+ 3 (point))) 'face mark))
           ;; Single-character mark, goes after the date.
           ((and (stringp mark) (= (length mark) 1))
            (overlay-put
             (make-overlay (1+ (point)) (+ 2 (point))) 'display mark))
           (t                           ; attr list
            (overlay-put
             (make-overlay (1- (point)) (+ 3 (point))) 'face
             (calendar-make-temp-face mark))))))))

;; ** Diary
(setq diary-date-forms calendar-lunar-diary-date-forms)

;; *** Entry
(define-key calendar-mode-map "iD"  'diary-insert-lunar-entry)
(define-key calendar-mode-map "iA"  'diary-insert-lunar-anniversary-entry)

(defun diary-insert-lunar-entry (arg)
  "Insert an lunar anniversary diary entry for the date given by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive "P")
  (diary-insert-lunar-common-entry "diary-lunar-date" arg))

(defun diary-insert-lunar-anniversary-entry (arg)
  "Insert an lunar anniversary diary entry for the date given by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive "P")
  (diary-insert-lunar-common-entry "diary-lunar-anniversary" arg))

(defun diary-insert-lunar-common-entry (sexp-name arg)
  (let* ((date (calendar-cursor-to-date t))
         (lunar-date (chinese-from-gregorian-date
                      (calendar-extract-month date)
                      (calendar-extract-day date)
                      (calendar-extract-year date)))
         (m (nth 0 lunar-date))
         (d (nth 1 lunar-date))
         (y (nth 2 lunar-date)))
    (diary-make-entry
     (format "%s(%s %d %d %d)"
             diary-sexp-entry-symbol
             sexp-name
             m d y)
     arg)))

;;重定义日记标记的函数
(defun diary-mark-sexp-entries ()
  "Mark days in the calendar window that have sexp diary entries.
Each entry in the diary file (or included files) visible in the calendar window
is marked.  See the documentation for the function `diary-list-sexp-entries'."
  (let* ((sexp-mark (regexp-quote diary-sexp-entry-symbol))
         (s-entry (format "^\\(%s(\\)\\|\\(%s%s(diary-remind\\)" sexp-mark
                          (regexp-quote diary-nonmarking-symbol)
                          sexp-mark))
         (file-glob-attrs (nth 1 (diary-pull-attrs nil '())))
         m y first-date last-date date mark file-glob-attrs
         sexp-start sexp entry entry-start)
    (with-current-buffer calendar-buffer
      (setq m displayed-month
            y displayed-year))
    ;; (calendar-increment-month m y -1)
    (setq first-date (calendar-absolute-from-gregorian (list m 1 y)))
    ;; (calendar-increment-month m y 2)
    (setq last-date
          (calendar-absolute-from-gregorian
           (list m (calendar-last-day-of-month m y) y)))
    (goto-char (point-min))
    (while (re-search-forward s-entry nil t)
      (setq diary-marking-entry-flag (char-equal (preceding-char) ?\())
      (re-search-backward "(")
      (setq sexp-start (point))
      (forward-sexp)
      (setq sexp (buffer-substring-no-properties sexp-start (point)))
      (forward-char 1)
      (if (and (bolp) (not (looking-at "[ \t]")))
          ;; Diary entry consists only of the sexp.
          (progn
            (backward-char 1)
            (setq entry ""))
        (setq entry-start (point))
        ;; Find end of entry.
        (forward-line 1)
        (while (looking-at "[ \t]")
          (forward-line 1))
        (if (bolp) (backward-char 1))
        (setq entry (buffer-substring-no-properties entry-start (point))))
      (setq date (1- first-date))
      ;; FIXME this loops over all visible dates.
      ;; Could be optimized in many cases. Depends on whether t or * present.
      (while (<= (setq date (1+ date)) last-date)
        (when (setq mark (diary-sexp-entry
                          sexp entry
                          (calendar-gregorian-from-absolute date)))
          (calendar-mark-visible-date
           (calendar-gregorian-from-absolute date)
           (or (cadr (diary-pull-attrs entry file-glob-attrs))
               (if (consp mark) (car mark)))))))))

;; *** Sexp
(defun diary-lunar-date (cmonth cday year &optional mark)
  "Specific lunar date(s) diary entry.

Entry applies if date is MONTH, DAY, YEAR.  Each parameter can be a
list of integers, `t' (meaning all values), or an integer.  The order
of the input parameters changes according to `calendar-date-style'
\(e.g. to DAY MONTH YEAR in the European style).

An optional parameter MARK specifies a face or single-character string to
use when highlighting the day in the calendar."
  (let* ((ddate (diary-make-date cmonth cday year))
         (dd (calendar-extract-day ddate))
         (mm (calendar-extract-month ddate))
         (yy (calendar-extract-year ddate))
         (lunar-date (chinese-from-gregorian-date
                      (calendar-extract-month date)
                      (calendar-extract-day date)
                      (calendar-extract-year date)))
         (m (nth 0 lunar-date))
         (d (nth 1 lunar-date))
         (y (nth 2 lunar-date)))
    (and
     (or (and (listp dd) (memq d dd))
         (equal d dd)
         (eq dd t))
     (or (and (listp mm) (memq m mm))
         (equal m mm)
         (eq mm t))
     (or (and (listp yy) (memq y yy))
         (equal y yy)
         (eq yy t))
     (cons mark entry))))

;; rewrite to allow diff=0
(defun diary-anniversary (month day &optional year mark)
  "Anniversary diary entry.
Entry applies if date is the anniversary of MONTH, DAY, YEAR.
The order of the input parameters changes according to
`calendar-date-style' (e.g. to DAY MONTH YEAR in the European style).

The diary entry can contain `%d' or `%d%s'; the %d will be replaced
by the number of years since the MONTH, DAY, YEAR, and the %s will
be replaced by the ordinal ending of that number (that is, `st',
`nd', `rd' or `th', as appropriate).  The anniversary of February 29
is considered to be March 1 in non-leap years.

An optional parameter MARK specifies a face or single-character
string to use when highlighting the day in the calendar."
  (let* ((ddate (diary-make-date month day year))
         (dd (calendar-extract-day ddate))
         (mm (calendar-extract-month ddate))
         (yy (calendar-extract-year ddate))
         (y (calendar-extract-year date))
         (diff (if yy (- y yy) 100)))
    (if (and (= mm 2) (= dd 29) (not (calendar-leap-year-p y)))
        (setq mm 3
              dd 1))
    (and (>= diff 0) (calendar-date-equal (list mm dd y) date)
         (cons mark (format entry diff (diary-ordinal-suffix diff))))))

(defun diary-lunar-anniversary (cmonth cday &optional year mark)
  "Chinese lunar anniversary diary entry.

An optional parameter MARK specifies a face or single-character string to
use when highlighting the day in the calendar."
  (let* ((lunar-date (chinese-from-gregorian-date
                      (calendar-extract-month date)
                      (calendar-extract-day date)
                      (calendar-extract-year date)))
         (m (nth 0 lunar-date))
         (d (nth 1 lunar-date))
         (y (nth 2 lunar-date))
         (diff (if year (- y year) 100))
         )
    (if (and (equal m cmonth) (equal d cday) (>= diff 0))
        (cons mark (format entry diff (diary-ordinal-suffix diff))))))

(defun diary-all-anniversary (month day &optional year is-lunar mark)
  "All anniversary diary entry including Chinese lunar and gregorian.

An optional parameter MARK specifies a face or single-character string to
use when highlighting the day in the calendar."
  (let* ((ddate (apply (if is-lunar
                           'chinese-to-gregorian-date
                         'chinese-from-gregorian-date)
                       (list month day
                             (or year
                                 (calendar-extract-year date)))))
         (gre-args (list month day year mark))
         (lunar-args (list (nth 0 ddate)
                           (nth 1 ddate)
                           (nth 2 ddate)
                           mark))
         tmp gre-result lunar-result result)
    ;; swap if needed
    (if is-lunar
        (setq tmp gre-args
              gre-args lunar-args
              lunar-args tmp))
    ;; call diary-*-anniversary
    (setq gre-result (apply 'diary-anniversary gre-args)
          lunar-result (apply 'diary-lunar-anniversary lunar-args)
          result (or gre-result lunar-result)
          tmp (if year
                  (if gre-result
                      (if lunar-result "(公/农历)" "(公历)")
                    "(农历)")
                ;; year missing
                (if is-lunar "(农历)" "(公历)")))
    ;; append result
    (when (consp result)
      (setcdr result (concat (cdr result) tmp)))
    result))

;; * Provide
(provide 'calendar-lunar)
;;; calendar-lunar.el ends here

