(require 'htmlize)

;;;###autoload
(defun bbcodeize-region (beg end)
  "Convert the region to bbcode, preserving colors and decorations."
  (interactive "r")
  ;; Don't let zmacs region highlighting end up.
  (when (fboundp 'zmacs-deactivate-region)
    (zmacs-deactivate-region))
  (let ((bbcodebuf (save-restriction
		   (narrow-to-region beg end)
		   (bbcodeize-buffer-1))))
    (when (interactive-p)
      (switch-to-buffer bbcodebuf))
    bbcodebuf))

(defun bbcodeize-make-file-name (file)
  "Make an bbcode file name from FILE."
  (concat file ".bbcode.txt"))

(defun bbcodeize-buffer-1 ()
  ;; Internal function; don't call it from outside this file.  Bbcodeize
  ;; current buffer, writing the resulting bbcode to a new buffer, and
  ;; return it.
  (save-excursion
    ;; Protect against the hook changing the current buffer.
    (save-excursion
      (run-hooks 'bbcodeize-before-hook))
    ;; Convince font-lock support modes to fontify the entire buffer
    ;; in advance.
    (htmlize-ensure-fontified)
    (clrhash htmlize-extended-character-cache)
    (clrhash htmlize-memoization-table)
    ;; It's important that the new buffer inherits default-directory
    ;; from the current buffer.
    (let ((htmlbuf (generate-new-buffer (if (buffer-file-name)
                                            (bbcodeize-make-file-name
                                             (file-name-nondirectory
                                              (buffer-file-name)))
                                          "*bbcode*")))
          (completed nil))
      (unwind-protect
          (let* ((buffer-faces (htmlize-faces-in-buffer))
                 (face-map (htmlize-make-face-map (adjoin 'default buffer-faces)))
                 (title (if (buffer-file-name)
                            (file-name-nondirectory (buffer-file-name))
                          (buffer-name))))
            (with-current-buffer htmlbuf
              (buffer-disable-undo))
            (let ((text-markup
                   ;; Get the inserter method, so we can funcall it inside
                   ;; the loop.  Not calling `htmlize-method' in the loop
                   ;; body yields a measurable speed increase.
                   (indirect-function 'bbcodeize-font-text-markup))
                  ;; Declare variables used in loop body outside the loop
                  ;; because it's faster to establish `let' bindings only
                  ;; once.
                  next-change text face-list trailing-ellipsis
                  fstruct-list last-fstruct-list
                  (close-markup (lambda ())))
              ;; This loop traverses and reads the source buffer, appending
              ;; the resulting HTML to HTMLBUF.  This method is fast
              ;; because: 1) it doesn't require examining the text
              ;; properties char by char (htmlize-next-face-change is used
              ;; to move between runs with the same face), and 2) it doesn't
              ;; require frequent buffer switches, which are slow because
              ;; they rebind all buffer-local vars.
              (goto-char (point-min))
              (while (not (eobp))
                (setq next-change (htmlize-next-face-change (point)))
                ;; Get faces in use between (point) and NEXT-CHANGE, and
                ;; convert them to fstructs.
                (setq face-list (htmlize-faces-at-point)
                      fstruct-list (delq nil (mapcar (lambda (f)
                                                       (gethash f face-map))
                                                     face-list)))
                (multiple-value-setq (text trailing-ellipsis)
                  (htmlize-extract-text (point) next-change trailing-ellipsis))
                ;; Don't bother writing anything if there's no text (this
                ;; happens in invisible regions).
                (when (> (length text) 0)
                  ;; Open the new markup if necessary and insert the text.
                  (when (not (equalp fstruct-list last-fstruct-list))
                    (funcall close-markup)
                    (setq last-fstruct-list fstruct-list
                          close-markup (funcall text-markup fstruct-list htmlbuf)))
                  (princ text htmlbuf))
                (goto-char next-change))

              ;; We've gone through the buffer; close the markup from
              ;; the last run, if any.
              (funcall close-markup))

            ;; Insert the epilog and post-process the buffer.
            (with-current-buffer htmlbuf
              (buffer-enable-undo))
            (setq completed t)
            htmlbuf)

        (when (not completed)
          (kill-buffer htmlbuf))
        (htmlize-delete-tmp-overlays)))))

(defun bbcodeize-font-text-markup (fstruct-list buffer)
  (let* ((merged (htmlize-merge-faces fstruct-list))
	 (markup (htmlize-memoize
		  merged
		  (cons (concat
			 (and (htmlize-fstruct-foreground merged)
			      (format "[color=%s]" (htmlize-fstruct-foreground merged)))
			 (and (htmlize-fstruct-boldp merged)      "[b]")
			 (and (htmlize-fstruct-italicp merged)    "[i]")
			 (and (htmlize-fstruct-underlinep merged) "[u]")
			 (and (htmlize-fstruct-strikep merged)    "[s]"))
			(concat
			 (and (htmlize-fstruct-strikep merged)    "[/s]")
			 (and (htmlize-fstruct-underlinep merged) "[/u]")
			 (and (htmlize-fstruct-italicp merged)    "[/i]")
			 (and (htmlize-fstruct-boldp merged)      "[/b]")
			 (and (htmlize-fstruct-foreground merged) "[/color]"))))))
    (princ (car markup) buffer)
    (htmlize-lexlet ((markup markup) (buffer buffer))
      (lambda ()
        (princ (cdr markup) buffer)))))


(defun bbcodeize-region-for-paste (beg end)
  "Bbcodeize the region and return just the bbcode as a string."
  (let ((htmlbuf (bbcodeize-region beg end)))
    (unwind-protect
	(with-current-buffer htmlbuf
	  (buffer-substring (point-min) (point-max)))
      (kill-buffer htmlbuf))))

(provide 'bbcodeize)
