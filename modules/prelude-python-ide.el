(setq pymacs-python-command "python3")
(setq python-shell-interpreter "python3")
(load-file (concat starter-kit-src-dir "python/epy-init.el"))
(custom-set-faces
 '(py-pseudo-keyword-face ((t (:foreground "purple1"))) t))

;;; python
(require 'pymacs (concat epy-install-dir "pymacs.el"))

(defun setup-ropemacs ()
  "Setup the ropemacs harness"
  (message "****************************")
  (if (and (getenv "PYTHONPATH") (not (string= (getenv "PYTHONPATH") "")))
      (message "true")
    (message "false"))
  (message "****************************")
  ;; If PYTHONPATH is set and not an empty string
  (if (and (getenv "PYTHONPATH") (not (string= (getenv "PYTHONPATH") "")))
      ;; append at the end with separator
      (setenv "PYTHONPATH"
              (concat
               (getenv "PYTHONPATH") path-separator
               (concat epy-install-dir "python-libs/")))
    ;; else set it without separator
    (setenv "PYTHONPATH"
            (concat epy-install-dir "python-libs/"))
    )

  (pymacs-load "ropemacs" "rope-")

  ;; Stops from erroring if there's a syntax err
  (setq ropemacs-codeassist-maxfixes 3)

  ;; Configurations
  (setq ropemacs-guess-project t)
  (setq ropemacs-enable-autoimport t)


  (setq ropemacs-autoimport-modules '("os" "shutil" "sys" "logging"
                                      "django.*"))



  ;; Adding hook to automatically open a rope project if there is one
  ;; in the current or in the upper level directory
   (add-hook 'python-mode-hook
            (lambda ()
              (cond ((file-exists-p ".ropeproject")
                     (rope-open-project default-directory))
                    ((file-exists-p "../.ropeproject")
                     (rope-open-project (concat default-directory "..")))
                    )))
  )

;; Ipython integration with fgallina/python.el
(defun epy-setup-ipython ()
  "Setup ipython integration with python-mode"
  (interactive)

  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args ""
   python-shell-prompt-regexp "In \[[0-9]+\]: "
   python-shell-prompt-output-regexp "Out\[[0-9]+\]: "
   python-shell-completion-setup-code ""
   python-shell-completion-string-code "';'.join(get_ipython().complete('''%s''')[1])\n")
  )

;; Python or python mode?
(eval-after-load 'python
  '(progn
     ;;==================================================
     ;; Ropemacs Configuration
     ;;==================================================
     (setup-ropemacs)

     ;;==================================================
     ;; Virtualenv Commands
     ;;==================================================
     (autoload 'virtualenv-activate "virtualenv"
       "Activate a Virtual Environment specified by PATH" t)
     (autoload 'virtualenv-workon "virtualenv"
       "Activate a Virtual Environment present using virtualenvwrapper" t)


     ;; Not on all modes, please
     ;; Be careful of mumamo, buffer file name nil
     (add-hook 'python-mode-hook
               (lambda () (if (buffer-file-name)
                              (flymake-python-pyflakes-load))))

     ;; when we swich on the command line, switch in Emacs
     ;;(desktop-save-mode 1)
     (defun workon-postactivate (virtualenv)
       (require 'virtualenv)
       (virtualenv-activate virtualenv)
       (desktop-change-dir virtualenv))


     )
  )
;; Cython Mode
(autoload 'cython-mode "cython-mode" "Mode for editing Cython source files")

(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxi\\'" . cython-mode))

;; Py3 files
(add-to-list 'auto-mode-alist '("\\.py3\\'" . python-mode))

(add-hook 'python-mode-hook '(lambda ()
     (define-key python-mode-map "\C-m" 'newline-and-indent)))
(add-hook 'ein:notebook-python-mode-hook
          (lambda ()
            (define-key python-mode-map "\C-m" 'newline)))

;;; completion

;;; nose
(require 'nose)

;; Nose bindings
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key "\C-ca" 'nosetests-all)
            (local-set-key "\C-cM" 'nosetests-module)  ;; C-c m conflicts w/ pylint
            (local-set-key "\C-c." 'nosetests-one)
            (local-set-key "\C-cx" 'nosetests-stop)
            (local-set-key "\C-cpa" 'nosetests-pdb-all)
            (local-set-key "\C-cpm" 'nosetests-pdb-module)
            (local-set-key "\C-cp." 'nosetests-pdb-one))
          )

;;; project
;; This is an utility
(defun epy-proj-find-root-dir (&optional startdir)
  "Find the root dir of this file, it's a previous directory that
  has a setup.py file in it, if it doesn't find nothing return
  the startdir"

  (unless startdir
    (setq startdir default-directory))

  (if (member "setup.py" (directory-files startdir))
      (expand-file-name startdir)
    (if (equal startdir "/")
        startdir
      (epy-proj-find-root-dir  (expand-file-name (concat startdir "../")))
      )
    )
  )

;; Tests are stored with a certain plist
;; :name   the dotted name of the test
;; :module the dotted name of the module
;; :root   the directory whose module dotted name is relative
(pymacs-load "epy-unittest" "epy-unittest-")

(defun epy-proj-build-test-menu ()
  "Build the sub-menu related to test discovery"
  (interactive)
  (let ((newmap (make-sparse-keymap))
        (rootdir (epy-proj-find-root-dir))
        tests
        testname
        test
        module)

    ;; rootdir is needed to define tests
    (when rootdir
      (setq tests (epy-unittest-discover rootdir)))

    ;; There should be tests otherwise don't make the menu
    (when tests
      ;; I'm doing this instead of simply giving (current-local-map)to
      ;; have the menu for only this buffer, don't know why though.
      (set-keymap-parent newmap (current-local-map))
      (define-key newmap [menu-bar pytests]
        (cons "PyTests" (make-sparse-keymap "PyTests")))


      ;; Add each test to the menu
      (dolist (testentry tests)

        (setq module (first testentry))

        (define-key newmap (vector 'menu-bar 'pytests (make-symbol module))
          (cons module (make-sparse-keymap module)))

        (define-key newmap (vector 'menu-bar 'pytests (make-symbol module)
                                   'runall)
          (cons "Run All" `(lambda ()
                             (interactive)
                             (epy-proj-run-testmodule ,module ,rootdir)))
          )

        (dolist (test (car (last testentry)))
          (setq testname (plist-get test ':name))
          (define-key newmap (vector 'menu-bar 'pytests (make-symbol module) (make-symbol testname))
            ;; It took me all night to write this hackish closure!!!
            (cons testname `(lambda () (interactive) (epy-proj-run-test ',test)))) ;
          )
        )
      (use-local-map newmap))
    )
  )

(defun epy-proj-run-test (test)
  "Take a TEST plist that represents a test and run it using the
unittest (Python 2.7) utility"
  (let (default-directory)
    (cd (plist-get test ':root))
    (compile (concat "python -munittest "
                     (plist-get test ':module)
                     "."
                     (plist-get test ':name)))
    )
  )

(defun epy-proj-run-testmodule (module rootdir)
  "Take the MODULE as a string and run all the tests defined in it"
  (let (default-directory)
    (cd rootdir)
    (compile (concat "python -munittest " module))
    )
  )

;;; ipython
(when (executable-find "ipython")
  (require 'ipython)
  (setq org-babel-python-mode 'python-mode))

;;; functions
(add-hook 'python-mode-hook 'ex-python-mode-hook)
(defun ex-python-mode-hook()
  (modify-syntax-entry ?_ "w")      ; 下划线作为词的一部分
  (setq indent-tabs-mode nil)       ; 不插入 tab
  (setq comment-style 'extra-line)  ; 注释风格
  (set (make-local-variable 'parens-require-spaces) nil)

  ;; outline 设置
  (set (make-local-variable 'outline-regexp)
       "#!/\\|###+\\|[ \t]*\\(def\\|class\\) ")
  (set (make-local-variable 'outline-heading-end-regexp)
       ":?\\s-*\n")
  (setq outline-level 'ex-py-outline-level)
  (outline-minor-mode)

  (setq hippie-expand-try-functions-list ; python-try-complete
        (remove 'python-try-complete hippie-expand-try-functions-list))
  )

(defun ex-py-end-statement()
  "Add : to python statement if neccessary, or delete empty line."
  (interactive)
  (back-to-indentation)
  (if (looking-at "[ \t]*$")
      (delete-blank-lines)		; kill if in empty line
    (or (not (looking-at "\\<\\(if\\|else\\|elif\\|for\\|while\\|class\\|def\\)")) ; not in forbidden `;' statement
        (end-of-line)
        (looking-back "[:]")
        (insert ?\:))
    (end-of-line)
    (newline-and-indent)))

(defun ex-py-outline-level ()
  (let (buffer-invisibility-spec)
    (save-excursion
      (skip-chars-forward "\t ")
      (1+ (current-column)))))
