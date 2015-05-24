;;; twin.el --- Easy operation between twin directory

;; Copyright (C) 2015 Levin Du

;; Author: Levin Du <zslevin@gmail.com>
;; Maintainer: Levin Du <zslevin@gmail.com>
;; Created: 20 May 2015
;; Version: 1.0
;; Package-Version: 20150520.1314
;; Package-Requires: ()
;; Keywords: dotemacs startup speed config package
;; URL: https://github.com/zslevin/twin

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package aims to make switching or diffing files easy between
;; two directories.
;;

;;; Code:

(defvar twin-dirs nil)

(defun twin-create (dir-a dir-b)
  (interactive
   (let* ((dir default-directory)
          (my (read-directory-name "Project A directory: "
                                   dir))
          (other (read-directory-name "Project B directory: "
                                      (save-window-excursion
                                        (other-window 1)
                                        (if (string= dir default-directory)
                                            my
                                          default-directory)))))
     (list (expand-file-name my) (expand-file-name other))))
  (if (string= dir-a dir-b)
      (error "Same directories.")
    (if (string< dir-a dir-b)
        (setq twin-dirs (cons dir-b dir-a))
      (setq twin-dirs (cons dir-a dir-b)))))

(defun twin-get-peer-file (fn)
  (or (and (car twin-dirs)
           (cdr twin-dirs))
      (error "twin directories not set. Call `twin-create' first."))
  (let (my other (fn (expand-file-name fn)))
    (cond
     ((string-prefix-p (car twin-dirs) fn)
      (setq my (car twin-dirs)
            other (cdr twin-dirs)))
     ((string-prefix-p (cdr twin-dirs) fn)
      (setq my (cdr twin-dirs)
            other (car twin-dirs)))
     (t (error "No matched twin file.")))
    (concat other (substring fn (length my)))))

(defun twin-find-other-file ()
  (interactive)
  (find-file-other-window
   (twin-get-peer-file (buffer-file-name))))

(defun twin-ediff-other-file ()
  (interactive)
  (ediff-files (buffer-file-name)
               (twin-get-peer-file (buffer-file-name))))

(defun twin-ediff-other-directory ()
  (interactive)
  (let* ((file (buffer-file-name))
         (dir (if file (file-name-directory file)
                default-directory)))
    (ediff-directories dir (twin-get-peer-file dir) nil)))

(provide 'twin)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; twin.el ends here
