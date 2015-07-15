;;; prelude-org-norang.el --- Emacs Prelude: org-mode norang style configuration.
;;
;; Copyright © 2015 Levin Du
;;
;; Author: Levin Du <zslevin@gmail.com>
;; URL: https://github.com/zslevin/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is adapted from http://doc.norang.ca/org-mode.html
;; Many thanks to Bernt Hansen <bernt@norang.ca>

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

;;; Code

;; * Organization
;; ** Work flow
;; - Clock in (F12 i), day start.
;; - Open agenda (F12 a ), mark today's import task.
;; - Read mail & news, creating notes and tasks (C-C c).
;; - Start task by clock in (I in agenda).
;; - Mark task done (C-c C-t).
;; - For interruption, use (C-C c j), write title, finish with (C-c C-c)
;; - Lunch time, pinch out (F12 o). pinch in (F12 i) when back.
;; - Do more work.
;; - Archive task.
;;   - (m) to mark tasks with same target, (B r) to refile them in batch.
;;   - (C-c C-w) to archive single task.
;; - Mark habit task as done (C-c C-t).
;; - Browse agenda of coming week (F12 a w).
;; - Pinch out (F12 o) to end today's work.
;;
;; ** Task priority
;; - tasks in today's agenda
;;   - due today (not too late)
;;   - overdue (already delayed)
;;   - scheduled today (planed)
;;   - scheduled before but not finished
;;   - deadline in near future
;; - tasks with NEXT state
;; - tasks in current project
;; - Unfinished tasks in clock history (F12 F12)
;;
;; ** Org files:
;; - todo.org: personal tasks
;; - refile.org: quick capture tasks
;; - journal.org: journal
;; - mem.org: anniversary
;; - emacs.org: emacs related
;; - <company>.org: company related
;; - <company>-<project>.org: special project
;;
;; Use first head as main category, and second head as tasks and projects.
;;
;; The following variables must be set to match your need:
;; (setq org-directory "/PATH/TO/YOUR/ORG")
;; (setq org-default-notes-file
;;       (expand-file-name "refile.org" org-directory))
;; (setq org-default-journal-file
;;       (expand-file-name "journal.org" org-directory))
;; (setq org-agenda-files (list org-directory))

;; * Customization
(defvar prelude-org-auto-exclude-tag-list
  '("hold" "personal")
  "Tag list of automatic task exclusion in the agenda with =/ RET=")

(defconst prelude-org-organization-task-id
  "ffffffff-ffff-ffff-ffff-ffffffffffff"
  "Organization task id")

(defvar prelude-org-prefix-key [f12]
  "Prefix key to invoke org-mode.")

(defvar org-default-notes-file
  (expand-file-name "refile.org" org-directory))

(defvar org-default-journal-file
  (expand-file-name "journal.org" org-directory))

(defvar org-agenda-files (list org-directory))

(define-prefix-command 'prelude-org-prefix-keymap)
(global-set-key prelude-org-prefix-key 'prelude-org-prefix-keymap)

;; * Tasks and States
(setq org-use-fast-todo-selection t
      org-treat-S-cursor-todo-selection-as-state-change nil
      org-enforce-todo-dependencies t   ; task state can be marked DONE only if all subtasks are done.
      )

;; Keep project state from NEXT
(add-hook 'org-after-todo-state-change-hook 'prelude-org-mark-next-parent-tasks-todo 'append)
(add-hook 'org-clock-in-hook 'prelude-org-mark-next-parent-tasks-todo 'append)

;; ** todo keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("NEXT" :foreground "blue" :weight bold)
        ("DONE" :foreground "forest green" :weight bold)
        ("WAITING" :foreground "orange" :weight bold)
        ("HOLD" :foreground "magenta" :weight bold)
        ("CANCELLED" :foreground "forest green" :weight bold)
        ("PHONE" :foreground "forest green" :weight bold)
        ("MEETING" :foreground "forest green" :weight bold)))

;; ** todo state triggers
;; add/remote TAG according to TODO state
(setq org-todo-state-tags-triggers
      '(("CANCELLED" ("CANCELLED" . t))
        ("WAITING" ("WAITING" . t))
        ("HOLD" ("WAITING") ("HOLD" . t))
        (done ("WAITING") ("HOLD"))
        ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
        ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
        ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

;; * Tags
(setq org-tag-alist
      '((:startgroup  . nil)
        ("@errand"    . ?e)
        ("@office"    . ?o)
        ("@home"      . ?m)
        ("@lunchtime" . ?l)
        ("@outdoor"   . ?d)
        (:endgroup    . nil)
        ("WAITING"    . ?w)
        ("HOLD"       . ?h)
        ("PERSONAL"   . ?P)
        ("WORK"       . ?W)
        ("NOTE"       . ?n)
        ("CANCELLED"  . ?c)
        ("FLAGGED"    . ??)
        ))
;; You can also add
;;   #+FILETAGS: WORK @office
;; to the org files to to apply one or more tags to all of the
;; headings.
(setq
 ;; Allow setting single tags without the menu
 org-fast-tag-selection-single-key 'expert
 ;; For tag searches ignore tasks with scheduled and deadline dates
 org-agenda-tags-todo-honor-ignore-options t)

;; * Agenda
(define-key prelude-org-prefix-keymap " " (lambda (arg) (interactive "P") (org-agenda arg " ")))
(define-key prelude-org-prefix-keymap "a" 'org-agenda)

(setq
 org-agenda-custom-commands
 (let ((include-scheduled-and-waiting-tasks
        '(if prelude-org-hide-scheduled-and-waiting-next-tasks
             ""
           " (including SCHEDULED and WAITING tasks)"))
       (todo-ignored-scheduled-deadline-with-date
        '((org-agenda-todo-ignore-scheduled prelude-org-hide-scheduled-and-waiting-next-tasks)
          (org-agenda-todo-ignore-deadlines prelude-org-hide-scheduled-and-waiting-next-tasks)
          (org-agenda-todo-ignore-with-date prelude-org-hide-scheduled-and-waiting-next-tasks)))
       (todo-ignored-scheduled-deadline
        '((org-agenda-todo-ignore-scheduled prelude-org-hide-scheduled-and-waiting-next-tasks)
          (org-agenda-todo-ignore-deadlines prelude-org-hide-scheduled-and-waiting-next-tasks))))

   `(("N" "Notes" tags "NOTE"
      ((org-agenda-overriding-header "Notes")
       (org-tags-match-list-sublevels t)))
     ("h" "Habits" tags-todo "STYLE=\"habit\""
      ((org-agenda-overriding-header "Habits")
       (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
     (" " "Agenda"
      ((agenda "" nil)
       ;; task to refile
       (tags "REFILE"
             ((org-agenda-overriding-header "Tasks to Refile")
              (org-tags-match-list-sublevels nil)))
       ;; stuck projects
       (tags-todo "-CANCELLED/!"
                  ((org-agenda-overriding-header "Stuck Projects")
                   (org-agenda-skip-function 'prelude-org-skip-non-stuck-projects)
                   (org-agenda-sorting-strategy '(category-keep))))
       ;; projects
       (tags-todo "-HOLD-CANCELLED/!"
                  ((org-agenda-overriding-header "Projects")
                   (org-agenda-skip-function 'prelude-org-skip-non-projects)
                   (org-tags-match-list-sublevels 'indented)
                   (org-agenda-sorting-strategy '(category-keep))))
       ;; project next tasks
       (tags-todo "-CANCELLED/!NEXT"
                  ((org-agenda-overriding-header
                    (concat "Project Next Tasks" ,include-scheduled-and-waiting-tasks))
                   (org-agenda-skip-function 'prelude-org-skip-projects-and-habits-and-single-tasks)
                   ,@todo-ignored-scheduled-deadline-with-date
                   (org-tags-match-list-sublevels t)
                   (org-agenda-sorting-strategy
                    '(todo-state-down effort-up category-keep))))
       ;; project subtasks
       (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                  ((org-agenda-overriding-header
                    (concat "Project Subtasks" ,include-scheduled-and-waiting-tasks))
                   (org-agenda-skip-function 'prelude-org-skip-non-project-tasks)
                   ,@todo-ignored-scheduled-deadline-with-date
                   (org-agenda-sorting-strategy '(category-keep))))
       ;; standalone tasks
       (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                  ((org-agenda-overriding-header (concat "Standalone Tasks" ,include-scheduled-and-waiting-tasks))
                   (org-agenda-skip-function 'prelude-org-skip-project-tasks)
                   ,@todo-ignored-scheduled-deadline-with-date
                   (org-agenda-sorting-strategy '(category-keep))))
       ;; waiting and postponed tasks
       (tags-todo "-CANCELLED+WAITING|HOLD/!"
                  ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks" ,include-scheduled-and-waiting-tasks))
                   (org-agenda-skip-function 'prelude-org-skip-non-tasks)
                   (org-tags-match-list-sublevels nil)
                   ,@todo-ignored-scheduled-deadline))
       ;; tasks to archive
       (tags "-REFILE/"
             ((org-agenda-overriding-header "Tasks to Archive")
              (org-agenda-skip-function 'prelude-org-skip-non-archivable-tasks)
              (org-tags-match-list-sublevels nil))))
      nil))))

(setq org-agenda-start-on-weekday 1   ; nil: current day, 0: Sunday
      org-deadline-warning-days 14
      org-agenda-span 'day              ; day/week/N in overview display
      org-agenda-include-diary t)

;; todo list
(setq org-agenda-todo-ignore-scheduled 'all
      org-agenda-todo-ignore-deadlines 'far
      org-agenda-todo-ignore-timestamp 'all
      org-agenda-todo-ignore-with-date nil)

;; schedule list
(setq org-agenda-skip-deadline-if-done nil
      org-agenda-skip-scheduled-if-done nil
      org-agenda-skip-timestamp-if-done nil
      org-agenda-skip-additional-timestamps-same-entry t)

;; agenda view
(setq org-agenda-time-grid '((daily today remove-match)
                             #("----------------" 0 16 (org-heading t))
                             (830 1000 1200 1400 1600 1800))
      org-agenda-dim-blocked-tasks nil
      org-agenda-compact-blocks t
      org-agenda-tags-column -102
      org-agenda-window-setup 'current-window
      org-agenda-log-mode-items '(closed state))

;; use sticky view
(setq org-agenda-sticky t)

;; agenda sort & filter
(setq org-agenda-cmp-user-defined 'prelude-org-agenda-sort
      org-agenda-persistent-filter t)

;; misc
;; Search including archives (=C-c a s=)
(setq org-agenda-text-search-extra-files '(agenda-archives))
;; Task exclusion in the agenda with =/ RET=, Use =/ TAB p RET= to
;; display
(setq org-agenda-auto-exclude-function 'prelude-org-auto-exclude-function)

;; * Clock / Timestamp
;; | C-c C-x C-i | org-clock-in      |
;; | C-c C-x C-o | org-clock-out     |
;; | C-c C-x C-x | org-clock-cancel  |
;; | C-c C-x C-d | org-clock-display |
;; | C-c C-x C-r | org-clock-report  |
;; | C-c C-x C-u | org-dblock-update |

;;  - C-u C-c C-x C-i :: clock in from list of recently clocked tasks
;;  - C-u C-u C-c C-x C-i :: clock into current task and mark it as
;;       default clocking selection
;;  - (agenda) v c :: show clock conflict
;;  - (agenda) R :: org-clock-report
;;  - C-c C-x i RET :: org-insert-colums-dblock

(define-key prelude-org-prefix-keymap "i" 'prelude-org-punch-in)
(define-key prelude-org-prefix-keymap "o" 'prelude-org-punch-out)

(define-key prelude-org-prefix-keymap "g" 'org-clock-goto)
(define-key prelude-org-prefix-keymap [f12] 'org-clock-goto)
(define-key prelude-org-prefix-keymap "h" 'prelude-org-clock-in-history-list)
(define-key prelude-org-prefix-keymap "l" 'prelude-org-clock-in-last-task)

(define-key prelude-org-prefix-keymap "t" 'prelude-org-insert-inactive-timestamp)

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
(setq org-clock-history-length 24
      org-clock-in-resume t
      org-clock-into-drawer t
      ;; Change tasks to NEXT when clocking in
      org-clock-in-switch-to-state 'prelude-org-clock-in-to-next
      org-clock-out-remove-zero-time-clocks t
      ;; Clock out when moving task to a done state
      org-clock-out-when-done t
      ;; Save the running clock and all clock history when exiting
      ;; Emacs, load it on startup
      org-clock-persist t
      ;; Do not prompt to resume an active clock
      org-clock-persist-query-resume nil
      ;; Enable auto clock resolution for finding open clocks
      org-clock-auto-clock-resolution 'when-no-clock-is-running
      ;; Include current clocking task in clock reports
      org-clock-report-include-clocking-task t
      ;; org-clock-sound "/path/to/sound" FIXME
      org-time-stamp-rounding-minutes '(1 1)
      )

;; clock report
(setq org-agenda-clockreport-parameter-plist
      '(:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80))

(setq org-agenda-clock-consistency-checks
      '(:max-duration "4:00"
        :min-duration 0
        :max-gap 0
        :gap-ok-around ("4:00")))

(add-hook 'org-clock-out-hook 'prelude-org-clock-out-maybe 'append)
(add-hook 'org-clock-out-hook 'prelude-org-remove-empty-drawer-on-clock-out 'append)

;; ** Task estimates and column view
;; setup default column view: Task Effort Clock_Summary
(setq org-columns-default-format
      "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
(setq org-global-properties
      '(("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
        ("STYLE_ALL" . "habit")))

;; * Capture - Refile - Archive
;; ** Capture
;; Capture is used to take quick note.

;; Invoke =C-c c= anywhere, select template:
;;  - A phone call (p)
;;  - A meeting (m)
;;  - An email I need to respond to (r)
;;  - A new task (t)
;;  - A new note (n)
;;  - An interruption (j)
;;  - A new habit (h)

;; and finish with:
;;  - =C-c C-c= to exit and save note to org-default-notes-file.
;;  - =C-c C-w= to refile to other place.
;;  - =C-c C-k= to cancel.

;; In agenda, use =k c= to invoke org-capture, along with time stamp
;; under current cursor.

;; To find lately saved note, use:
;;  - =C-u C-c c= Visit the target location of a capture template. You
;;    get to select the template in the usual way.
;;  - =C-u C-u C-c c= Visit the last stored capture item in its buffer.

(global-set-key (kbd "C-C c") 'org-capture)

(setq org-capture-templates
      '(("t" "todo" entry (file "")
         "* TODO %?\n%U\n" :clock-in t :clock-resume t)
        ("r" "respond" entry (file "")
         "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
        ("n" "note" entry (file "")
         "* %? :NOTE:\n%U\n" :clock-in t :clock-resume t)
        ("c" "note from clipboard" entry (file "")
         "* %? :NOTE:\n%U\n%x\n" :clock-in t :clock-resume t)
        ("j" "journal" entry (file+datetree org-default-journal-file)
         "* %?\n%U\n" :clock-in t :clock-resume t)
        ("w" "org-protocol" entry (file "")
         "* TODO Review %c\n%U\n" :immediate-finish t)
        ("m" "Meeting" entry (file "")
         "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
        ("p" "Phone call" entry (file "")
         "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
        ("h" "Habit" entry (file "~/git/org/refile.org")
         "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
        ("T" "todo with annotation" entry (file "")
         "* TODO %?\n%U\n%i\n%a\n" :clock-in t :clock-resume t)
        ("N" "note with annotation" entry (file "")
         "* %? :NOTE:\n%U\n%i\n%a\n" :clock-in t :clock-resume t)))

;; ** Refile
;; In org-mode:
;;  - =C-c C-w= to refile task under cursor
;;  - =C-2 C-c C-w= to refile task as subtasks of the current clocking
;;    task.

;; In agenda-mode, scroll down to the =Tasks to Refile= block, mark the
;; tasks with =m= and then =B r= to refile all of them to a new location.

(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))
(setq org-refile-target-verify-function 'prelude-org-refile-verify-target)
(setq org-refile-use-outline-path t
      org-outline-path-complete-in-steps nil
      org-completion-use-ido t)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; ** Archive
;; Archiving is trivial.  Just mark all of the entries in the block agenda
;; using the =m= key and then archive them all to the appropriate place
;; with =B $=.

(setq org-archive-mark-done nil
      org-archive-location "%s_archive::* Archived Tasks")

;; * Edit
;; - =C-c b=    org-iswitchb
;; - =C-c l=    org-store-link
;; - =C-c C-l=  org-insert-link
;; - org-open-at-point-global
(global-set-key "\C-cL" 'org-insert-link-global)

;; * Redefine keys
;; ** org mode
(defun prelude-org-redefine-org-mode-map ()
  ;; Undefine C-c [ and C-c ] since this breaks my
  ;; org-agenda files when directories are include It
  ;; expands the files in the directories individually
  (org-defkey org-mode-map "\C-c["    'undefined)
  (org-defkey org-mode-map "\C-c]"    'undefined)
  (org-defkey org-mode-map "\C-c;"    'undefined)
  (org-defkey org-mode-map "\C-cs"    'show-all)
  (org-defkey org-mode-map "\C-xnp"   'prelude-org-narrow-to-project)
  (org-defkey org-mode-map "\C-xnu"   'prelude-org-narrow-up-one-level)
  (org-defkey org-mode-map "\C-ch"    'prelude-org-hide-other)

  ;; table
  (local-set-key "\M-\C-w" 'org-table-copy-region)
  (local-set-key "\M-\C-y" 'org-table-paste-rectangle)
  (local-set-key "\M-\C-l" 'org-table-sort-lines)

  (local-set-key (kbd "C-c M-o") 'prelude-org-mail-subtree)
  ;; display images
  (local-set-key "\M-I" 'org-toggle-iimage-in-org))
(add-hook 'org-mode-hook 'prelude-org-redefine-org-mode-map)

;; ** agenda mode
(defun prelude-org-redfine-agenda-mode-map ()
  ;; agenda
  (org-defkey org-agenda-mode-map "N" 'prelude-org-narrow-to-subtree)
  (org-defkey org-agenda-mode-map "U" 'prelude-org-narrow-up-one-level)
  (org-defkey org-agenda-mode-map "P" 'prelude-org-narrow-to-project)
  (org-defkey org-agenda-mode-map "V" 'prelude-org-view-next-project)
  (org-defkey org-agenda-mode-map "F" 'prelude-org-restrict-to-file-or-follow)
  (org-defkey org-agenda-mode-map "W" (lambda () (interactive) (setq bh/hide-scheduled-and-waiting-next-tasks t) (prelude-org-widen)))
  (org-defkey org-agenda-mode-map "q" 'bury-buffer))
(add-hook 'org-agenda-mode-hook 'prelude-org-redfine-agenda-mode-map)

;; ** speed keys
(setq org-use-speed-commands t)
(setq org-speed-commands-user
      '(("h" . prelude-org-hide-other)
        ("s" . show-all)
        ("F" . prelude-org-restrict-to-file-or-follow)
        ("N" . prelude-org-narrow-to-subtree)
        ("T" . prelude-org-org-todo)
        ("P" . prelude-org-narrow-to-project)
        ("U" . prelude-org-narrow-up-one-level)
        ("W" . prelude-org-widen)))



;; ** toggle
(define-key prelude-org-prefix-keymap "/" 'prelude-org-toggle)

;; * General setting
(setq
 org-log-done 'time                     ; Add a time stamp to the task when done
 org-log-state-notes-insert-after-drawers t
 org-log-into-drawer nil                ; Do not put log into drawer
 org-cycle-include-plain-lists nil      ; Do not cycle plain list, keep them visible
 org-return-follows-link t
 org-startup-truncated nil
 org-startup-indented t
 org-hide-leading-stars t
 org-ellipsis 'link
 org-reverse-note-order t
 org-stuck-projects '("" nil nil "") ; disable default stuck project setting
 org-cycle-separator-lines 0
 org-cycle-emulate-tab 'whitestart
 org-goto-auto-isearch nil
 org-blank-before-new-entry '((heading)
                              (plain-list-item . auto))
 org-insert-heading-respect-content nil
 org-show-following-heading t
 org-show-hierarchy-above t
 org-show-siblings '((default))
 org-yank-adjusted-subtrees t
 org-remove-highlights-with-change nil
 org-link-mailto-program '(compose-mail "%a" "%s")
 org-startup-folded 'content
 org-catch-invisible-edits 'error)

;; auto insert timestamp
;; (add-hook 'org-insert-heading-hook 'prelude-org-insert-heading-inactive-timestamp 'append)

(setq org-emphasis-alist
      (remove-if (lambda (x) (string-match-p "[+]" (car x)))
                 org-emphasis-alist))
(setq org-use-sub-superscripts nil
      org-export-with-sub-superscripts nil)


;; list
(setq org-list-indent-offset 2
      org-alphabetical-lists t)

;; keys
(setq org-special-ctrl-a/e 'reverse
      org-special-ctrl-k t)
(setq org-support-shift-select t)

;; * Integration/Extension
;; ** aapt
;; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'prelude-org-agenda-to-appt)

;; Activate appointments so we get notifications
(appt-activate t)

;; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'prelude-org-agenda-to-appt)

;; Erase all reminders and rebuilt reminders for today from the agenda
(defun prelude-org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

;; ** message
;; TODO: move to message mode configure file
(add-hook 'message-mode-hook 'message-mode-hook-org)
(defun message-mode-hook-org ()
  (orgstruct++-mode)
  (setq fill-column 72)
  (flyspell-mode 1)
  (turn-on-auto-fill)
  ;; (bbdb-define-all-aliases)
  (local-set-key (kbd "C-c M-o") 'org-mime-htmlize))

;; ** bbdb
(define-key prelude-org-prefix-keymap "w" 'prelude-org-insert-who)

;; Phone capture template handling with BBDB lookup
;; Adapted from code by Gregory J. Grubbs
(defun prelude-org-insert-who ()
  "Return name and company info for caller from bbdb lookup"
  (interactive)
  (require 'bbdb)
  (require 'bbdb-com)

  (let* (name rec caller)
    (setq name (completing-read "Who? "
                                bbdb-hashtable
                                'bbdb-completion-predicate
                                'confirm))
    (when (> (length name) 0)
      ; Something was supplied - look it up in bbdb
      (setq rec
            (or (first
                 (or (bbdb-search (bbdb-records) name nil nil)
                     (bbdb-search (bbdb-records) nil name nil)
                     (bbdb-search (bbdb-records) nil nil name)))
                name)))

    ; Build the bbdb link if we have a bbdb record, otherwise just return the name
    (setq caller (cond ((and rec (vectorp rec))
                        (let ((name (bbdb-record-name rec))
                              (organ (bbdb-record-organization rec)))
                          (concat "[[bbdb:"
                                  name "]["
                                  name "]]"
                                  (when organ
                                    (concat " - " organ)))))
                       (rec)
                       (t "NameOfCaller")))
    (insert caller)))

;; ** babel code block
;; *** languages
;; This activates a number of widely used languages, you are encouraged
;; to activate more languages using the customize interface for the
;; `=org-babel-load-languages=' variable, or with an elisp form like the
;; one below.  The customize interface of `=org-babel-load-languages='
;; contains an up to date list of the currently supported languages.
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)
   (ditaa . t)
   (dot . t)
   (mscgen . t)
   (python . t)
   (R . t)
   (plantuml . t)
   (java . t)
   ))

(setq org-babel-sh-command "bash")

;; *** execution / result
(setq org-confirm-babel-evaluate nil)

;; Default to show execution, no save
(setq org-babel-default-header-args
      '((:results . "silent")
        (:session . "none")
        (:exports . "code")
        (:cache . "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "yes")
        (:padnewline . "yes")))
;; (setq org-babel-default-header-args
;;       (cons '(:results . "silent")
;;             (assq-delete-all :results org-babel-default-header-args)))

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

;; *** fontification
;; The following displays the contents of code blocks in Org-mode files
;; using the major-mode of the code.  It also changes the behavior of
;; =TAB= to as if it were used in the appropriate major mode.  This means
;; that reading and editing code form inside of your Org-mode files is
;; much more like reading and editing of code using its major mode.
(setq org-src-preserve-indentation nil
      org-edit-src-content-indentation 0
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-window-setup 'current-window)

;; ** export
(setq org-export-with-timestamps t)

(require 'ox-html)
(require 'ox-latex)
(require 'ox-ascii)

;; ** org-habit
(require 'org-habit)
(setq org-habit-show-habits-only-for-today t
      org-habit-graph-column 50)
;; ** org-crypt
(require 'org-crypt)
                                        ; Encrypt all entries before saving
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
                                        ; GPG key to use for encryption
;; (setq org-crypt-key "F0B66B40")
(setq org-crypt-disable-auto-save nil)

;; ** ox-mediawiki
;; =M-x org-mw-export-as-mediawiki=
;; (require 'ox-mediawiki)
;; * Implementation
;; ** Todo
(defun prelude-org-org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
        (prelude-org-narrow-to-org-subtree)
        (org-show-todo-tree nil))
    (prelude-org-narrow-to-org-subtree)
    (org-show-todo-tree nil)))

(defun prelude-org-mark-next-parent-tasks-todo ()
  "Visit each parent task and change NEXT states to TODO"
  (let ((mystate (or (and (fboundp 'org-state)
                          state)
                     (nth 2 (org-heading-components)))))
    (when (equal mystate "NEXT")
      (save-excursion
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) (list "NEXT"))
            (org-todo "TODO")))))))

;; ** Agenda
(defun prelude-org-agenda ()
  (interactive)
  (if (not (buffer-live-p org-agenda-buffer))
      (call-interactively 'org-agenda)
    (switch-to-buffer org-agenda-buffer)))

(defun prelude-org-widen ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-agenda-remove-restriction-lock)
        (when org-agenda-sticky
          (org-agenda-redo)))
    (widen)))

(defun prelude-org-restrict-to-file-or-follow (arg)
  "Set agenda restriction to 'file or with argument invoke follow mode.
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both"
  (interactive "p")
  (if (equal arg 4)
      (org-agenda-follow-mode)
    (widen)
    (prelude-org-set-agenda-restriction-lock 4)
    (org-agenda-redo)
    (beginning-of-buffer)))

(defun prelude-org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (member tag prelude-org-auto-exclude-tag-list)
       (concat "-" tag)))               ; use / TAB p RET to display

(defun prelude-org-agenda-sort (a b)
  "Sorting strategy for agenda items.
Late deadlines first, then scheduled, then non-late deadlines"
  (let (result num-a num-b)
    (cond
     ; time specific items are already sorted first by org-agenda-sorting-strategy

     ; non-deadline and non-scheduled items next
     ((prelude-org-agenda-sort-test 'prelude-org-is-not-scheduled-or-deadline a b))

     ; deadlines for today next
     ((prelude-org-agenda-sort-test 'prelude-org-is-due-deadline a b))

     ; late deadlines next
     ((prelude-org-agenda-sort-test-num 'prelude-org-is-late-deadline '< a b))

     ; scheduled items for today next
     ((prelude-org-agenda-sort-test 'prelude-org-is-scheduled-today a b))

     ; late scheduled items next
     ((prelude-org-agenda-sort-test-num 'prelude-org-is-scheduled-late '> a b))

     ; pending deadlines last
     ((prelude-org-agenda-sort-test-num 'prelude-org-is-pending-deadline '< a b))

     ; finally default to unsorted
     (t (setq result nil)))
    result))

(defmacro prelude-org-agenda-sort-test (fn a b)
  "Test for agenda sort"
  `(cond
    ; if both match leave them unsorted
    ((and (apply ,fn (list ,a))
          (apply ,fn (list ,b)))
     (setq result nil))
    ; if a matches put a first
    ((apply ,fn (list ,a))
     (setq result -1))
    ; otherwise if b matches put b first
    ((apply ,fn (list ,b))
     (setq result 1))
    ; if none match leave them unsorted
    (t nil)))

(defmacro prelude-org-agenda-sort-test-num (fn compfn a b)
  `(cond
    ((apply ,fn (list ,a))
     (setq num-a (string-to-number (match-string 1 ,a)))
     (if (apply ,fn (list ,b))
         (progn
           (setq num-b (string-to-number (match-string 1 ,b)))
           (setq result (if (apply ,compfn (list num-a num-b))
                            -1
                          1)))
       (setq result -1)))
    ((apply ,fn (list ,b))
     (setq result 1))
    (t nil)))

(defun prelude-org-is-not-scheduled-or-deadline (date-str)
  (and (not (prelude-org-is-deadline date-str))
       (not (prelude-org-is-scheduled date-str))))

(defun prelude-org-is-due-deadline (date-str)
  (string-match "Deadline:" date-str))

(defun prelude-org-is-late-deadline (date-str)
  (string-match "In *\\(-.*\\)d\.:" date-str))

(defun prelude-org-is-pending-deadline (date-str)
  (string-match "In \\([^-]*\\)d\.:" date-str))

(defun prelude-org-is-deadline (date-str)
  (or (prelude-org-is-due-deadline date-str)
      (prelude-org-is-late-deadline date-str)
      (prelude-org-is-pending-deadline date-str)))

(defun prelude-org-is-scheduled (date-str)
  (or (prelude-org-is-scheduled-today date-str)
      (prelude-org-is-scheduled-late date-str)))

(defun prelude-org-is-scheduled-today (date-str)
  (string-match "Scheduled:" date-str))

(defun prelude-org-is-scheduled-late (date-str)
  (string-match "Sched\.\\(.*\\)x:" date-str))

;; ** Outline
(defun prelude-org-hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defun prelude-org-narrow-to-subtree ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (org-get-at-bol 'org-hd-marker)
          (prelude-org-narrow-to-org-subtree))
        (when org-agenda-sticky
          (org-agenda-redo)))
    (prelude-org-narrow-to-org-subtree)))

(defun prelude-org-narrow-up-one-level ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (prelude-org-get-pom-from-agenda-restriction-or-point)
          (prelude-org-narrow-up-one-org-level))
        (org-agenda-redo))
    (prelude-org-narrow-up-one-org-level)))

(defun prelude-org-narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree)
  (save-restriction
    (org-agenda-set-restriction-lock)))

(defun prelude-org-narrow-up-one-org-level ()
  (widen)
  (save-excursion
    (outline-up-heading 1 'invisible-ok)
    (prelude-org-narrow-to-org-subtree)))

;; ** Project / Task
(defvar prelude-org-project-list nil)

(defun prelude-org-narrow-to-project ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (prelude-org-get-pom-from-agenda-restriction-or-point)
          (prelude-org-narrow-to-org-project)
          (save-excursion
            (prelude-org-find-project-task)
            (org-agenda-set-restriction-lock)))
        (org-agenda-redo)
        (beginning-of-buffer))
    (prelude-org-narrow-to-org-project)
    (save-restriction
      (org-agenda-set-restriction-lock))))

(defun prelude-org-view-next-project ()
  (interactive)
  (let (num-project-left current-project)
    (unless (marker-position org-agenda-restrict-begin)
      (goto-char (point-min))
      ; Clear all of the existing markers on the list
      (while prelude-org-project-list
        (set-marker (pop prelude-org-project-list) nil))
      (re-search-forward "Tasks to Refile")
      (forward-visible-line 1))

    ; Build a new project marker list
    (unless prelude-org-project-list
      (while (< (point) (point-max))
        (while (and (< (point) (point-max))
                    (or (not (org-get-at-bol 'org-hd-marker))
                        (org-with-point-at (org-get-at-bol 'org-hd-marker)
                          (or (not (prelude-org-is-project-p))
                              (prelude-org-is-project-subtree-p)))))
          (forward-visible-line 1))
        (when (< (point) (point-max))
          (add-to-list 'prelude-org-project-list (copy-marker (org-get-at-bol 'org-hd-marker)) 'append))
        (forward-visible-line 1)))

    ; Pop off the first marker on the list and display
    (setq current-project (pop prelude-org-project-list))
    (when current-project
      (org-with-point-at current-project
        (setq prelude-org-hide-scheduled-and-waiting-next-tasks nil)
        (prelude-org-narrow-to-project))
      ; Remove the marker
      (setq current-project nil)
      (org-agenda-redo)
      (beginning-of-buffer)
      (setq num-projects-left (length prelude-org-project-list))
      (if (> num-projects-left 0)
          (message "%s projects left to view" num-projects-left)
        (beginning-of-buffer)
        (setq prelude-org-hide-scheduled-and-waiting-next-tasks t)
        (error "All projects viewed.")))))

(defun prelude-org-narrow-to-org-project ()
  (widen)
  (save-excursion
    (prelude-org-find-project-task)
    (prelude-org-narrow-to-org-subtree)))

(defun prelude-org-find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun prelude-org-map-subtree (fun)
  "Call FUN for every sub heading underneath the current one."
  (org-back-to-heading)
  (let ((level (funcall outline-level)))
    (save-excursion
      (while (and (progn
		    (outline-next-heading)
		    (> (funcall outline-level) level))
		  (not (eobp)))
	(funcall fun)))))

(defun prelude-org-check-subtree-any (any-fun)
  "Call ANY-FUN for every sub heading underneath the current one.
Return once the result is t."
  (catch 'prelude-org-map-subtree-any-found
    (prelude-org-map-subtree
     (lambda ()
       (when (funcall any-fun)
         (throw 'prelude-org-map-subtree-any-found t))))
    nil))

(defun prelude-org-is-project-p ()
  "Any task with a todo keyword subtask"
  (and (member (org-get-todo-state) org-todo-keywords-1)
       (org-with-wide-buffer
        (catch 'found
          (prelude-org-map-subtree
           (lambda ()
             (when (member (org-get-todo-state) org-todo-keywords-1)
               (throw 'found t))))
          nil))))

(defun prelude-org-is-project-has-task-p (todo-keywords)
  "Project has any leaf task with todo state in TODO-KEYWORDS list."
  (and (member (org-get-todo-state) org-todo-keywords-1)
       (org-with-wide-buffer
        (catch 'found
          (prelude-org-map-subtree
           (lambda ()
             (when (and (member (org-get-todo-state) todo-keywords)
                        (not (prelude-org-is-project-p)))
               (throw 'found t))))
          nil))))

(defun prelude-org-is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (prelude-org-find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun prelude-org-is-task-p ()
  "Any task with a todo keyword and no subtask"
  (and (member (org-get-todo-state) org-todo-keywords-1)
       (not (prelude-org-is-project-p))))

(defun prelude-org-list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun prelude-org-list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun prelude-org-skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (prelude-org-list-sublevels-for-projects-indented)
  (org-with-wide-buffer
   (let ((next-headline (save-excursion (outline-next-heading))))
     (if (prelude-org-is-project-p)
         (if (prelude-org-check-subtree-any
              #'(lambda ()
                  (and (string= (org-get-todo-state) "NEXT")
                       (not (member "WAITING" (org-get-tags-at))))))
             next-headline
           nil)
       next-headline))))

(defun prelude-org-skip-non-projects ()
  "Skip trees that are not projects"
  ;; (prelude-org-list-sublevels-for-projects-indented)
  (if (save-excursion (prelude-org-skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((prelude-org-is-project-p)
            nil)
           ((and (prelude-org-is-project-p)
                 (not (prelude-org-is-project-subtree-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun prelude-org-skip-project-trees-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((prelude-org-is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun prelude-org-skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits. No
skipping single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and prelude-org-hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((prelude-org-is-project-p)
        next-headline)
       ((and (prelude-org-is-task-p) (not (prelude-org-is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun prelude-org-skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((prelude-org-is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((prelude-org-is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun prelude-org-skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((prelude-org-is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (prelude-org-is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (prelude-org-is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun prelude-org-skip-all-but-next-tasks-and-projects ()
  "Show next tasks, and its maybe projects."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((member (org-get-todo-state) (list "NEXT"))
        nil)
       ((prelude-org-is-project-has-task-p (list "NEXT"))
        nil)
       (t
        subtree-end)))))

(defun prelude-org-skip-all-but-todo-tasks-and-projects ()
  "Show todo tasks and its maybe projects."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((org-is-habit-p)
        subtree-end)
       ((member (org-get-todo-state) (list "NEXT"))
        subtree-end)
       ((prelude-org-is-project-p)
        (if (prelude-org-is-project-has-task-p (list "TODO"))
           nil subtree-end))
       (t
        nil)))))

(defun prelude-org-skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((prelude-org-is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (prelude-org-is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (prelude-org-is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun prelude-org-skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((prelude-org-is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun prelude-org-skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-int (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))

;; ** Clock
(defvar prelude-org-keep-clock-running nil)

(defun prelude-org-punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq prelude-org-keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (prelude-org-clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (prelude-org-clock-in-organization-task-as-default)))))

(defun prelude-org-punch-out ()
  (interactive)
  (setq prelude-org-keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun prelude-org-clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find prelude-org-organization-task-id 'marker)
    (org-clock-in '(16))))

(defun prelude-org-clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

(defun prelude-org-clock-in-history-list ()
  (interactive)
  (org-clock-in '(4)))

(defun prelude-org-resume-clock ()
  (interactive)
  (if (marker-buffer org-clock-interrupted-task)
      (org-with-point-at org-clock-interrupted-task
        (org-clock-in))
    (org-clock-out)))

(defun prelude-org-clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun prelude-org-clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when prelude-org-keep-clock-running
            (prelude-org-clock-in-default-task)))))))

(defun prelude-org-clock-out-maybe ()
  (when (and prelude-org-keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (prelude-org-clock-in-parent-task)))

(defun prelude-org-clock-in-task-by-id (id)
  "Clock in a task by id"
  (require 'org-id)
  (org-with-point-at (org-id-find id 'marker)
    (org-clock-in nil)))

(defun prelude-org-clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (prelude-org-is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (prelude-org-is-project-p))
      "TODO"))))

(defun prelude-org-remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (let ((m (point-marker)))
      (mapc (lambda (d)
              (org-remove-empty-drawer-at d m)) org-drawers))))

;; ** Time
(defun prelude-org-insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun prelude-org-insert-heading-inactive-timestamp ()
  (save-excursion
    (org-return)
    (org-cycle)
    (prelude-org-insert-inactive-timestamp)))

;; ** Misc
(defvar prelude-org-hide-scheduled-and-waiting-next-tasks t)

(defun prelude-org-toggle (x)
  (interactive "cToggle with (t)runcate-lines")
  (cond ((equal x ?t)
         (prelude-org-toggle-truncate-lines)
         )))

(defun prelude-org-toggle-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

(defun prelude-org-toggle-next-task-display ()
  (interactive)
  (setq prelude-org-hide-scheduled-and-waiting-next-tasks (not prelude-org-hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if prelude-org-hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun prelude-org-get-pom-from-agenda-restriction-or-point ()
  (or (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
      (org-get-at-bol 'org-hd-marker)
      (and (equal major-mode 'org-mode) (point))
      org-clock-marker))

(defun prelude-org-refile-verify-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(defun prelude-org-mail-subtree ()
  (interactive)
  (org-mark-subtree)
  (org-mime-subtree))

(defun prelude-org-prepare-meeting-notes ()
  "Prepare meeting notes for email
   Take selected region and convert tabs to spaces, mark TODOs with leading >>>, and copy to kill ring for pasting"
  (interactive)
  (let (prefix)
    (save-excursion
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (untabify (point-min) (point-max))
        (goto-char (point-min))
        (while (re-search-forward "^\\( *-\\\) \\(TODO\\|DONE\\): " (point-max) t)
          (replace-match (concat (make-string (length (match-string 1)) ?>) " " (match-string 2) ": ")))
        (goto-char (point-min))
        (kill-ring-save (point-min) (point-max))))))

;; * Templates
;; ** todo template
(defun prelude-org-insert-todo-template ()
  (interactive)
  (insert "\
#+FILETAGS: PERSONAL
* Finances
  :PROPERTIES:
  :CATEGORY: Finance
  :END:

* Health and Recreation
  :PROPERTIES:
  :CATEGORY: Health
  :END:

* House Maintenance
  :PROPERTIES:
  :CATEGORY: House
  :END:

* Vehicle Maintenance
  :PROPERTIES:
  :CATEGORY: Vehicle
  :END:

* Tasks
** Organization
   :PROPERTIES:
   :CLOCK_MODELINE_TOTAL: today
   :ID:       ffffffff-ffff-ffff-ffff-ffffffffffff
   :END:

* Notes

* Passwords
"))

;; ** mem template
(defun prelude-org-insert-mem-template ()
  (interactive)
  (insert "\
#+FILETAGS: PERSONAL
* Special Dates
  :PROPERTIES:
  :CATEGORY: Date
  :END:
** Birthdays
** Anniversaries
** Holidays
"))

;; ** refile template
(defun prelude-org-insert-refile-template ()
  (interactive)
  (insert "\
#+FILETAGS: REFILE
"))

(provide 'prelude-org-norang)
;;; prelude-org-norang.el ends here
