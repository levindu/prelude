(defvar ime-switch-list
  '("eim-wb" "eim-py" "japanese")
  "Default input method list to switch between.")

;;;###autoload
(defun ime-switch-input-method (do-switch)
  "If not arg is specified, toggle input method on or off;
  Otherwise switch among `ime-switch-list'"
  (interactive "P")
  (if (null do-switch)
      (toggle-input-method)
    (let ((last-saved-keypress last-command-event)
          (methods ime-switch-list)
          (current-method (or current-input-method
                              (car input-method-history)
                              default-input-method))
          event)
      ;; locate input method
      (while (and methods
                  (not (eq current-method
                           (car methods))))
        (setq methods (cdr methods)))
      ;; if currently active, use next one
      (if current-input-method
          (setq methods (cdr methods)))
      ;; if nil, use list
      (or methods
          (setq methods ime-switch-list))
      ;; do switch
      (while (progn
               (setq current-method (car methods))
               ;; update mode-line display
               (let ((current-input-method "")
                     (current-input-method-title
                      (nth 3 (assoc current-method input-method-alist))))
                 (force-mode-line-update)
                 (message "Press '%s' to continue switching ime..." (single-key-description last-saved-keypress))
                 ;; read event
                 (eq last-saved-keypress
                     (setq event (read-event nil nil 0.8)))))
        (setq methods (or (cdr methods)
                          ime-switch-list)))
      (set-input-method current-method)
      (message "Switch ime complete.")
      (if event
          (setq unread-command-events (list last-input-event))))))

(provide 'ime-switch)

