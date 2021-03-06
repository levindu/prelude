;;; prelude-core.el --- Emacs Prelude: Core Prelude functions.
;;
;; Copyright © 2011-2015 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Here are the definitions of most of the functions added by Prelude.

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

(require 'thingatpt)
(require 'dash)
(require 'ov)

(defun prelude-open-with (arg)
  "Open visited file in default external program.
When in dired mode, open file under the cursor.

With a prefix ARG always prompt for command to use."
  (interactive "P")
  (let* ((current-file-name
          (if (eq major-mode 'dired-mode)
              (dired-get-file-for-visit)
            buffer-file-name))
         (open (pcase system-type
                 (`darwin "open")
                 ((or `gnu `gnu/linux `gnu/kfreebsd) "xdg-open")))
         (program (if (or arg (not open))
                      (read-shell-command "Open current file with: ")
                    open)))
    (start-process "prelude-open-with-process" nil program current-file-name)))

(defun prelude-buffer-mode (buffer-or-name)
  "Retrieve the `major-mode' of BUFFER-OR-NAME."
  (with-current-buffer buffer-or-name
    major-mode))

(defvar prelude-term-buffer-name "ansi"
  "The default `ansi-term' name used by `prelude-visit-term-buffer'.
This variable can be set via .dir-locals.el to provide multi-term support.")

(defun prelude-visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (prelude-start-or-switch-to (lambda ()
                                (ansi-term (getenv "SHELL") (concat prelude-term-buffer-name "-term")))
                              (format "*%s-term*" prelude-term-buffer-name)))

(defun prelude-search (query-url prompt)
  "Open the search url constructed with the QUERY-URL.
PROMPT sets the `read-string prompt."
  (browse-url
   (concat query-url
           (url-hexify-string
            (if mark-active
                (buffer-substring (region-beginning) (region-end))
              (read-string prompt))))))

(defmacro prelude-install-search-engine (search-engine-name search-engine-url search-engine-prompt)
  "Given some information regarding a search engine, install the interactive command to search through them"
  `(defun ,(intern (format "prelude-%s" search-engine-name)) ()
       ,(format "Search %s with a query or region if any." search-engine-name)
       (interactive)
       (prelude-search ,search-engine-url ,search-engine-prompt)))

(prelude-install-search-engine "google"     "http://www.google.com/search?q="              "Google: ")
(prelude-install-search-engine "youtube"    "http://www.youtube.com/results?search_query=" "Search YouTube: ")
(prelude-install-search-engine "github"     "https://github.com/search?q="                 "Search GitHub: ")
(prelude-install-search-engine "duckduckgo" "https://duckduckgo.com/?t=lm&q="              "Search DuckDuckGo: ")

(defun prelude-indent-rigidly-and-copy-to-clipboard (begin end arg)
  "Indent region between BEGIN and END by ARG columns and copy to clipboard."
  (interactive "r\nP")
  (let ((arg (or arg 4))
        (buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring-no-properties buffer begin end)
      (indent-rigidly (point-min) (point-max) arg)
      (clipboard-kill-ring-save (point-min) (point-max)))))

(defun prelude-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun prelude-smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.

With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (prelude-smart-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))

(defun prelude-top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

(defun prelude-kill-whole-line (&optional arg)
  "A simple wrapper around command `kill-whole-line' that respects indentation.
Passes ARG to command `kill-whole-line' when provided."
  (interactive "p")
  (kill-whole-line arg)
  (back-to-indentation))

(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun prelude-move-end-of-line (arg)
  "Move point back to non-whitespace of end of line.

Move point to the last non-whitespace character on this line.
If point is already there, move to the end of the line.
Effectively toggle between the first non-whitespace character and
the end of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (move-end-of-line (1- arg))))

  (let ((orig-point (point)))
    (move-end-of-line 1)
    (skip-syntax-backward " " (line-beginning-position))
    (when (= orig-point (point))
      (move-end-of-line 1))))

(global-set-key [remap move-beginning-of-line]
                'prelude-move-beginning-of-line)
(global-set-key [remap move-end-of-line]
                'prelude-move-end-of-line)

(defun prelude-indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

(defun prelude-todo-ov-evaporate (_ov _after _beg _end &optional _length)
  (let ((inhibit-modification-hooks t))
    (if _after (ov-reset _ov))))

(defun prelude-annotate-todo ()
  "Put fringe marker on TODO: lines in the curent buffer."
  (interactive)
  (ov-set (format "[[:space:]]*%s+[[:space:]]*TODO:" comment-start)
          'before-string
          (propertize (format "A")
                      'display '(left-fringe right-triangle))
          'modification-hooks '(prelude-todo-ov-evaporate)))

(defun prelude-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line
or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

(defun prelude-duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (prelude-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (-dotimes arg
      (lambda (n)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point))))
    (goto-char (+ origin (* (length region) arg) arg))))

(defun prelude-duplicate-and-comment-current-line-or-region (arg)
  "Duplicates and comments the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (prelude-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (comment-or-uncomment-region beg end)
    (setq end (line-end-position))
    (-dotimes arg
      (lambda (n)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point))))
    (goto-char (+ origin (* (length region) arg) arg))))

(defun prelude-rename-buffer-and-file ()
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun prelude-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun prelude-view-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    (goto-char (point-min))
    (re-search-forward "^$")
    (delete-region (point-min) (point))
    (delete-blank-lines)
    (set-auto-mode)))

(defun prelude-cleanup-buffer-or-region ()
  "Cleanup a region if selected, otherwise the whole buffer."
  (interactive)
  (call-interactively 'untabify)
  (unless (member major-mode prelude-indent-sensitive-modes)
    (call-interactively 'indent-region))
  (whitespace-cleanup))

(defun prelude-save-buffer-without-hooks ()
  "Save buffer without any `write-file-functions' or `after-save-hook'."
  (interactive)
  (let (write-file-functions after-save-hook)
    (save-buffer)))

(defun prelude-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (backward-kill-sexp)
    (insert (format "%s" value))))

(defun prelude-recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory prelude-dir 0))

(defun prelude-file-owner-uid (filename)
  "Return the UID of the FILENAME as an integer.

See `file-attributes' for more info."
  (nth 2 (file-attributes filename 'integer)))

(defun prelude-file-owned-by-user-p (filename)
  "Return t if file FILENAME is owned by the currently logged in user."
  (equal (prelude-file-owner-uid filename)
         (user-uid)))

(defun prelude-find-alternate-file-as-root (filename)
  "Wraps `find-alternate-file' with opening a file as root."
  (find-alternate-file (concat "/sudo:root@localhost:" filename)))

(require 'ido)
(defun prelude-sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (prelude-find-alternate-file-as-root buffer-file-name)))

(defun prelude-clone-file (dest-file)
  (interactive "GClone to file:")
  (unless (buffer-file-name)
    (error "Not file."))
  (when (file-directory-p dest-file)
    (setq dest-file (expand-file-name
                     (file-name-nondirectory (buffer-file-name))
                     dest-file)))
  (copy-file (buffer-file-name) dest-file)
  (find-file dest-file))

(defun prelude-reopen-as-root ()
  "Find file as root if necessary."
  (unless (or (tramp-tramp-file-p buffer-file-name)
              (equal major-mode 'dired-mode)
              (not (file-exists-p (file-name-directory buffer-file-name)))
              (file-writable-p buffer-file-name)
              (prelude-file-owned-by-user-p buffer-file-name))
    (prelude-find-alternate-file-as-root buffer-file-name)))
(add-hook 'find-file-hook 'prelude-reopen-as-root)

(defun prelude-start-or-switch-to (function buffer-name)
  "Invoke FUNCTION if there is no buffer with BUFFER-NAME.
Otherwise switch to the buffer named BUFFER-NAME.  Don't clobber
the current buffer."
  (if (not (get-buffer buffer-name))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (funcall function))
    (switch-to-buffer-other-window buffer-name)))

(defun prelude-insert-date ()
  "Insert a timestamp according to locale's date and time format."
  (interactive)
  (let ((time-strs (mapcar (lambda (format)
                             (format-time-string format))
                           '("%c"
                             "%Y-%m-%d %T"
                             "%Y-%m-%d %T%z"
                             "%Y-%m-%d"
                             "%FT%T%z"))))
  (insert (completing-read "Select: " time-strs))))

(defun prelude-insert-file-name ()
  "Insert path interactively"
  (interactive)
  (let ((path (read-file-name "Insert: ")))
    (insert path)))

(defun prelude-insert-directory-name ()
  "Insert path interactively"
  (interactive)
  (let ((path (read-directory-name "Insert: ")))
    (insert path)))

(defun prelude-kill-buffer-info (choice)
  "Copy the buffer-file-name to the kill-ring"
  (interactive "cCopy Buffer name (f)ile, (d)irectory, (l)ineno, (j)ava (w)ich-func ?")
  ;(message "your choice %c" choice)
  (let* ((path (if (eq major-mode 'dired-mode)
                   (dired-get-filename)
                 (or (buffer-file-name) "")))
         (name (file-name-nondirectory path))
         (new-kill-string
          (cond ((eq choice ?f) path)
                ((eq choice ?F) name)
                ((eq choice ?d) (file-name-directory path))
                ((eq choice ?l) (format "%s:%s" path
                                        (line-number-at-pos)))
                ((eq choice ?L) (format "%s:%s" name
                                        (line-number-at-pos)))
                ((eq choice ?j) (prelude-java-get-class:lineno))
                ((eq choice ?w) (which-function))
                ((eq choice ?W) (format "%s(%s)" (which-function) name)))))
    (when new-kill-string
      (message "\"%s\" copied" new-kill-string)
      (kill-new new-kill-string))))

(defun prelude-java-get-class:lineno ()
  (let ((lineno (number-to-string (line-number-at-pos)))
        (package
         (save-excursion
           (goto-char (point-min))
           (unless (search-forward-regexp
                    "^\\s-*package\\s-+\\(.+?\\)\\s-*;\\s-*$" nil t)
             (error "Java package name not found"))
           (match-string-no-properties 1))))
    (concat package "." (file-name-base) ":" lineno)))

(defun prelude-dos-line-end ()
  "Convert buffer to DOS line end."
  (interactive)
  (set-buffer-file-coding-system 'dos))

(defun prelude-unix-line-end ()
  "Convert buffer to UNIX line end."
  (interactive)
  (set-buffer-file-coding-system 'unix))

(defun prelude-recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: "
                                   (-map 'abbreviate-file-name recentf-list)
                                   nil t)))
    (when file
      (find-file file))))

(defun prelude-swap-windows (arg)
  "Swap with the following or previous (ARG < 0) window."
  (interactive "p")
  (if (= (count-windows) 1)
      (message "Only one windYou need exactly 2 windows to do this.")
    (let* ((w1 (selected-window))
           (w2 (funcall (if (> arg 0) 'next-window 'previous-window)))
           (b1 (window-buffer w1)) (b2 (window-buffer w2))
           (s1 (window-start w1))  (s2 (window-start w2))
           (p1 (window-point w1))  (p2 (window-point w2)))
      (set-window-buffer-start-and-point w1 b2 s2 p2)
      (set-window-buffer-start-and-point w2 b1 s1 p1)
      (other-window (if (> arg 0) 1 -1)))))

(defun prelude-other-window-backward (arg)
  (interactive "p")
  (other-window (- arg)))

(defun prelude-split-or-delete-window()
  "Split or delete window."
  (interactive)
  (if (not (one-window-p))
      (delete-other-windows)
    (split-window-sensibly)
    (other-window 1)))

(defun prelude-rotate-split-window ()
  "Rotate the split windows."
  (interactive)
  (let ((root (car (window-tree))))
    (if (listp root)
	(let* ((w1 (nth 2 root))
	       (w2 (nth 3 root))
               (b1 (window-buffer w1)) (b2 (window-buffer w2))
               (s1 (window-start w1))  (s2 (window-start w2))
               (p1 (window-point w1))  (p2 (window-point w2)))
	  (cond ((car root)		; currently vertically split
		 (delete-window w2)
		 (set-window-buffer-start-and-point
                  (split-window-horizontally) b2 s2 p2))
		(t		       ; currently horizontally split
		 (delete-window w2)
		 (set-window-buffer-start-and-point
                  (split-window-vertically)
                  b2 s2 p2))))
      (message "Root window not split"))))

(defun prelude-rotate-split-window--find (tree cur-win)
  "Find the window-tree leaf containing cur-win."
  (cl-loop for win in (nthcdr 2 tree)
           for result = (cond ((listp win)
                               (prelude-rotate-split-window--find win cur-win))
                              ((eq win cur-win) tree)) 
           when result return result))

(defun prelude-rotate-split-window ()
  "Rotate the split windows."
  (interactive)
  (let ((root (car (window-tree)))
        (cur-win (selected-window))
        (cur-win-found 0)
        peer-win
        tree)
    (assert (listp root) nil "Root window not split")
    (setq tree (prelude-rotate-split-window--find root cur-win))
    ;; find peer win
    (cl-loop for win in (nthcdr 2 tree)
             do (cond ((listp win) nil)
                      ((eq win cur-win)
                       (setq cur-win-found 1))
                      ((<= cur-win-found 1)
                       (setq peer-win win)
                       (when (= cur-win-found 1)
                         (setq cur-win-found 2)))))
    (assert peer-win nil "No peer window")
    (let* ((win-cons (if (= cur-win-found 1)
                          (cons peer-win cur-win)
                        (cons cur-win peer-win)))
           (w1 (car win-cons))
           (w2 (cdr win-cons))
           (b2 (window-buffer w2))
           (s2 (window-start w2))
           (p2 (window-point w2)))
      (delete-window w2)
      (select-window w1)
      (setq w2 (if (car tree)           ; currently vertically split
                   (split-window-horizontally)
                 (split-window-vertically)))
      (set-window-buffer-start-and-point w2 b2 s2 p2)
      (select-window (if (= cur-win-found 1) w2 w1)))))

(defun prelude-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun prelude-kill-other-buffers ()
  "Kill all buffers but the current one.
Doesn't mess with special buffers."
  (interactive)
  (when (y-or-n-p "Are you sure you want to kill all buffers but the current one? ")
    (-each
        (->> (buffer-list)
             (-filter #'buffer-file-name)
             (--remove (eql (current-buffer) it)))
      #'kill-buffer)))

(defun prelude-create-scratch-buffer ()
  "Create a new scratch buffer."
  (interactive)
  (let ((buf (generate-new-buffer "*scratch*")))
    (switch-to-buffer buf)
    (funcall initial-major-mode)))

(defvar prelude-tips
  '("Press <C-c o> to open a file with external program."
    "Press <C-c p f> to navigate a project's files with ido."
    "Press <s-r> to open a recently visited file."
    "Press <C-c p s g> to run grep on a project."
    "Press <C-c p p> to switch between projects."
    "Press <C-=> to expand the selected region."
    "Press <C-c g> to search in Google."
    "Press <C-c G> to search in GitHub."
    "Press <C-c y> to search in YouTube."
    "Press <C-c U> to search in DuckDuckGo."
    "Press <C-c r> to rename the current buffer and the file it's visiting if any."
    "Press <C-c t> to open a terminal in Emacs."
    "Press <C-c k> to kill all the buffers, but the active one."
    "Press <C-x g> to run magit-status."
    "Press <C-c D> to delete the current file and buffer."
    "Press <C-c s> to swap two windows."
    "Press <S-RET> or <M-o> to open a line beneath the current one."
    "Press <s-o> to open a line above the current one."
    "Press <C-c C-z> in a Elisp buffer to launch an interactive Elisp shell."
    "Press <C-Backspace> to kill a line backwards."
    "Press <C-S-Backspace> or <s-k> to kill the whole line."
    "Press <s-j> or <C-^> to join lines."
    "Press <s-.> or <C-c j> to jump to the start of a word in any visible window."
    "Press <f11> to toggle fullscreen mode."
    "Press <f12> to toggle the menu bar."
    "Explore the Tools->Prelude menu to find out about some of Prelude extensions to Emacs."
    "Access the official Emacs manual by pressing <C-h r>."
    "Visit the EmacsWiki at http://emacswiki.org to find out even more about Emacs."))

(defun prelude-tip-of-the-day ()
  "Display a random entry from `prelude-tips'."
  (interactive)
  (when (and prelude-tips (not (window-minibuffer-p)))
    ;; pick a new random seed
    (random t)
    (message
     (concat "Prelude tip: " (nth (random (length prelude-tips)) prelude-tips)))))

(defun prelude-eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.

    If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

(require 'epl)

(defun prelude-update ()
  "Update Prelude to its latest version."
  (interactive)
  (when (y-or-n-p "Do you want to update Prelude? ")
    (message "Updating installed packages...")
    (epl-upgrade)
    (message "Updating Prelude...")
    (cd prelude-dir)
    (shell-command "git pull")
    (prelude-recompile-init)
    (message "Update finished. Restart Emacs to complete the process.")))

(defun prelude-update-packages (&optional arg)
  "Update Prelude's packages.
This includes package installed via `prelude-require-package'.

With a prefix ARG updates all installed packages."
  (interactive "P")
  (when (y-or-n-p "Do you want to update Prelude's packages? ")
    (if arg
        (epl-upgrade)
      (epl-upgrade (-filter (lambda (p) (memq (epl-package-name p) prelude-packages))
                            (epl-installed-packages))))
    (message "Update finished. Restart Emacs to complete the process.")))

;;; Emacs in OSX already has fullscreen support
;;; Emacs has a similar built-in command in 24.4
(defun prelude-fullscreen ()
  "Make Emacs window fullscreen.

This follows freedesktop standards, should work in X servers."
  (interactive)
  (if (eq window-system 'x)
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                             '(2 "_NET_WM_STATE_FULLSCREEN" 0))
    (error "Only X server is supported")))

(defun prelude-find-user-init-file (&optional arg)
  "Edit the `prelude-user-init-file', in another window.
With a prefix argument ARG, find the `user-init-file' instead."
  (interactive "P")
  (if arg (find-file-other-window user-init-file)
    (find-file-other-window prelude-user-init-file)))

(defun prelude-find-shell-init-file ()
  "Edit the shell init file in another window."
  (interactive)
  (let* ((shell (car (reverse (s-split "/" (getenv "SHELL")))))
         (shell-init-file (cond
                           ((s-equals? "zsh" shell) ".zshrc")
                           ((s-equals? "bash" shell) ".bashrc")
                           (t (error "Unknown shell")))))
    (find-file-other-window (expand-file-name shell-init-file (getenv "HOME")))))

(defun prelude-wrap-with (s)
  "Create a wrapper function for smartparens using S."
  `(lambda (&optional arg)
     (interactive "P")
     (sp-wrap-with-pair ,s)))

;; needed for prelude-goto-symbol
(require 'imenu)

(defun prelude-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (cond
   ((not symbol-list)
    (let (name-and-pos symbol-names position)
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (prelude-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (completing-read "Symbol? " (reverse symbol-names)))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))
      (recenter)))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (prelude-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names (substring-no-properties name))
          (add-to-list 'name-and-pos (cons (substring-no-properties name) position))))))))

(defun prelude-switch-to-same-mode-buffer ()
  (interactive)
  (let ((current-mode major-mode)
        (next-mode nil))
    (while (not (eq next-mode current-mode))
      (next-buffer)
      (setq next-mode major-mode))))

(defvar prelude-dired-is-omit-mode-on t)

(defun prelude-dired-toggle-omit-mode ()
  (interactive)
  (setq prelude-dired-is-omit-mode-on
        (not prelude-dired-is-omit-mode-on))
  (prelude-dired-set-omit-mode))

(defun prelude-dired-set-omit-mode ()
  (dired-omit-mode (if prelude-dired-is-omit-mode-on 1 0)))

(define-minor-mode prelude-check-mode
  "Minor mode for checking syntax and whitespace."
  :lighter ""
  (if prelude-check-mode
      (progn
        (flycheck-mode +1)
        (let ((whitespace-style '(face trailing tabs tab-mark newline-mark)) ; tab-mark
              (whitespace-display-mappings
               ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
               '((newline -mark 10 [?\u21A9 ?\n] [?\u00B6 ?\n]) ; 10 LINE FEED
                 (tab-mark 9 [?\u00BB ?\t] [?\\ ?\t]))))
          (whitespace-mode +1)))
    (flycheck-mode -1)
    (whitespace-mode -1)))

(defun prelude-complete ()
  (interactive)
  (cond
   ((and (fboundp 'company-complete)
         company-mode)
    (call-interactively 'company-complete))
   (t (completion-at-point))))

(defun prelude-smart-tab (arg)
  (interactive "P")
  (cond
   ;; The region is active, indent it.
   ((use-region-p)
    (cond (arg ; prefix argument
           (indent-rigidly (region-beginning) (region-end) arg))
          (t
           (indent-region (region-beginning) (region-end)))))

   ((or ;; indent-to-left-margin is only meant for indenting,
        ;; so we force it to always insert a tab here.
        (eq indent-line-function 'indent-to-left-margin)
        (and (not tab-always-indent)
             (or (> (current-column) (current-indentation))
                 (eq this-command last-command))))
    (insert-tab arg))

   (t
    (let ((old-tick (buffer-chars-modified-tick))
          (old-point (point))
          (old-indent (current-indentation)))

      ;; Indent the line.
      (funcall indent-line-function)

      (cond
       ;; If a prefix argument was given, rigidly indent the following
       ;; sexp to match the change in the current line's indentation.
       (arg
        (let ((end-marker
               (save-excursion
                 (forward-line 0) (forward-sexp) (point-marker)))
              (indentation-change (- (current-indentation) old-indent)))
          (save-excursion
            (forward-line 1)
            (when (and (not (zerop indentation-change))
                       (< (point) end-marker))
              (indent-rigidly (point) end-marker indentation-change)))))

       ((looking-at "\\s\(")
        (save-excursion
          (indent-region (point)
                         (save-excursion (forward-sexp) (point)))))
       ((and (eq old-point (point))
             (eq old-tick (buffer-chars-modified-tick))
             (and (looking-back "[ \t]+$")))
        (delete-horizontal-space)
        (forward-line 1)
        (back-to-indentation))

       ;; If the text was already indented right, try completion.
       ((and (eq tab-always-indent 'complete)
             (eq old-point (point))
             (eq old-tick (buffer-chars-modified-tick)))
        (prelude-complete)))))))

(define-minor-mode prelude-subword-mode
  "Subword minor mode for editing sub word and CamelCase."
  :lighter " Sw"
  (if prelude-subword-mode
      ;; ON
      (let ((constituent-list '(?_ ?-)))
        ;; save syntax
        (unless (local-variable-p 'prelude-subword-saved-syntax-entries)
          (set (make-local-variable 'prelude-subword-saved-syntax-entries)
               (mapcar (lambda (c)(cons c (char-syntax c)))
                       constituent-list)))
        ;; modify syntax from w to _
        (mapc (lambda (x)
                (when (= (cdr x) ?w)
                  (modify-syntax-entry (car x) "_")))
              prelude-subword-saved-syntax-entries)
        (subword-mode +1))
    ;; OFF
    (subword-mode 0)
    ;; restore syntax
    (dolist (entry prelude-subword-saved-syntax-entries)
      (modify-syntax-entry (car entry) (char-to-string (cdr entry))))))

(defvar prelude-shell:binded-shell nil "Shell buffer binded to current buffer")
(make-variable-buffer-local 'prelude-shell:binded-shell)
(defvar prelude-shell:last-buffer nil "Last buffer invoking `prelude-shell'")
(defvar prelude-shell:last-shell nil "Last shell binded")

(defun prelude-shell(arg)
  "Open shell in new window."
  (interactive "P")
  (let ((saved-buf (current-buffer))
        (saved-mode major-mode))
    (cond
     ;; Use prefix arg to open new shell unconditionally
     (arg
      (call-interactively 'shell arg))
     ;; cycle if already in shell buffer
     ((eq saved-mode 'shell-mode)
      (prelude-switch-to-same-mode-buffer))
     ;; switch to binded shell
     (t
      (let ((shell-buf (or prelude-shell:binded-shell prelude-shell:last-shell)))
        (if (and shell-buf (buffer-live-p shell-buf))
            (switch-to-buffer-other-window shell-buf)
          (call-interactively 'shell)))))
    (unless (eq major-mode 'shell-mode)
      (error "Not shell-mode buffer"))
    (unless (eq saved-mode 'shell-mode)
      (setq prelude-shell:last-buffer saved-buf))
    (setq prelude-shell:last-shell (current-buffer))
    (when (buffer-live-p prelude-shell:last-buffer)
      (with-current-buffer prelude-shell:last-buffer
        (setq prelude-shell:binded-shell prelude-shell:last-shell)))))

(defconst prelude-compile-default-buffer-name "*compilation*")
(defvar prelude-compile-saved-winner-data nil)

(defun prelude-compile (&optional prompt)
  "Save buffers and start compile."
  (interactive "P")
  (if prompt
      (call-interactively 'compile)
    (let ((compilation-ask-about-save nil))
      (if (prelude-compile-is-running)
          (let ((win (get-buffer-window prelude-compile-default-buffer-name)))
            (if (window-live-p win)
                (winner-set prelude-compile-saved-winner-data)
              (setq prelude-compile-saved-winner-data (winner-conf))
              (display-buffer prelude-compile-default-buffer-name)))
        (save-window-excursion
          (recompile)
          (message (concat "Compiling: " compile-command)))))))

(defun prelude-compile-is-running ()
  (let ((comp-proc (get-buffer-process (get-buffer prelude-compile-default-buffer-name))))
    (and comp-proc (eq (process-status comp-proc) 'run))))

(defun prelude-compile-on-finish (buffer msg)
  "Notify that the compilation is finished."
  (with-current-buffer buffer
    (when (eq major-mode 'compilation-mode)
      (message "(compile result) $ %s\n\n%s" compile-command msg)
      (unless (string-match "^finished" msg)
        (display-buffer buffer)))))

(defun prelude-tag-preview (&optional jump-to-tag)
  "Show tag in other window with no prompt in minibuf."
  (interactive "P")
  (let ((default (funcall (or find-tag-default-function
			      (get major-mode 'find-tag-default-function)
			      'find-tag-default)))
        (win (get-buffer-window)))
    (cond ((equal jump-to-tag '(4)) (call-interactively 'find-tag))
          ((equal jump-to-tag '(16)) (call-interactively 'imenu))
          (t
           (find-tag-other-window default (equal last-tag default))
           (shrink-window (- (window-height) 16))
           (recenter 1)
           (select-window win)))))

(defun prelude-customize-face-at-point ()
  "Customize face at current point."
  (interactive)
  (customize-face (get-char-property (point) 'face)))

(provide 'prelude-core)
;;; prelude-core.el ends here
