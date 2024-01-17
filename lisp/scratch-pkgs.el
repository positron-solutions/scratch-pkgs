;;; scratch-pkgs.el --- Scratch Packages -*- lexical-binding: t; -*-

;; Author:  Psionic K <contact@positron.solutions>
;; Copyright (C) 2023 Positron Solutions <contact@positron.solutions>
;; Homepage: http://github.com/positron-solutions/scratch-pkgs
;; Keywords: convenience
;; Package-Requires: ((emacs "28.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.2.0

;; This file is not part of Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Write small packages.  They are easier to debug, reload, and maintain than
;; use-package expressions.  You will have better control over dependencies, and
;; your dependencies will be properly declared.
;;
;; Add your scratch-pkgs to your Git repos so people can see what kinds of
;; nearly-packaged code you came up with.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defgroup scratch-pkgs nil "Scratch Packages." :prefix 'scratch-pkgs :group 'convenience)

;; TODO smarter package header maintenance, depend on ERK
(defcustom scratch-pkgs-template "\
;;; %1$s.el --- %2$s -*- lexical-binding: t; -*-

;; Install Elisp Repo Kit for automating header generation.

;;; Code:

(defgroup %1$ nil \"%2$s\" :prefix '%1$ :group 'convenience)

(defun %1$-hello ()
  \"Greet wisely.\"
  (interactive)
    (message \"Only the future is certain.\"))

(provide '%1$s)
;;; %1$s.el ends here.\n"
  "Template for file backed scratch."
  :group 'scratch-pkgs
  :type 'string)

(defun scratch-pkgs-default-init (buffer)
  "Create a package skeleton in BUFFER."
  (switch-to-buffer buffer)
  (let* ((feature-name (file-name-base (buffer-file-name buffer)))
         (feature-title (string-join (mapcar
                                      #'capitalize
                                      (string-split feature-name "-"))
                                     " ")))
    (insert (format scratch-pkgs-template
                    feature-name
                    feature-title))))

(defcustom scratch-pkgs-init #'scratch-pkgs-default-init
  "Function to run on each new scratch buffer."
  :group 'scratch-pkgs
  :type 'function)

(defcustom scratch-pkgs-mode 'local
  "One of `local' `elpaca' or `straight'."
  :group 'scratch-pkgs
  :options '(local straight elpaca)
  :type 'symbol)

(declare-function no-littering-expand-etc-file-name "no-littering")
(defun scratch-pkgs-local-repos-dir ()
  "Directory where repositories will live.."
  (if (featurep 'no-littering)
      (no-littering-expand-etc-file-name "scratch-pkgs/")
    (expand-file-name "etc/scratch-pkgs/" user-emacs-directory)))

(defun scratch-pkgs-packages-install-dir ()
  "Where packages are intended to be worked upon.
When `scratch-pkgs-mode' is `local', this value is equal to
`scratch-pkgs-local-repos-dir' because the packages are just added to the load
path and used ."
  (pcase scratch-pkgs-mode
    ('local (scratch-pkgs-local-repos-dir))
    ('straight (expand-file-name "straight/repos/" user-emacs-directory))
    ('elpaca (expand-file-name "elpaca/repos/" user-emacs-directory))))

(defun scratch-pkgs--repo-paths ()
  "Return alist of repo names and paths.
We can't list all packages in the packages dir when using `straight' and
`elpaca' mode because there will be a lot of non-scratch packages.  We decide
which ones are scratch by looking at the local repos."
  (let* ((files (directory-files (scratch-pkgs-local-repos-dir) nil "^[^.]"))
         (repo-paths (mapcar
                      (lambda (f) (when-let*
                                 ((path (expand-file-name
                                         f (scratch-pkgs-local-repos-dir)))
                                  (repo (and (file-directory-p path)
                                             (file-exists-p
                                              (expand-file-name ".git/" path)))))
                               (cons f path)))
                      files)))
    (seq-filter #'identity repo-paths)))

(defun scratch-pkgs--read ()
  "Return (REPO-NAME . PATH)."
  (let* ((repo-paths (scratch-pkgs--repo-paths)))
    (assoc-string
     (completing-read "Choose existing scratch package: "
                      repo-paths nil t)
     repo-paths)))

(defun scratch-pkgs--read-package ()
  "Return PATH to package root."
  (let ((repo-path (scratch-pkgs--read)))
    (expand-file-name (concat (car repo-path) ".el") (cdr repo-path))))

;;;###autoload
(defun scratch-pkgs (&optional file-path)
  "Open an old scratch FILE-PATH."
  (interactive (list (scratch-pkgs--read-package)))
  (if file-path
      (let* ((buffer (find-file-noselect file-path)))
        (switch-to-buffer buffer))
    (scratch-pkgs-new "scratch")))

(defun scratch-pkgs--init ()
  "Initialize a git repo unless one exists already."
  (let* ((dir (file-name-directory (buffer-file-name)))
         (git-bin (executable-find "git"))
         (output (get-buffer-create " *scratch-pkgs*"))
         (default-directory dir))
    (unless (file-exists-p dir)
      (make-directory dir t))
    (unless (file-exists-p (expand-file-name ".git" default-directory))
      (when (eq 'scratch-pkgs-mode 'local)
        (add-to-load-path dir))
      (unless (eq 0 (call-process git-bin nil output nil "init"))
        (pop-to-buffer output)
        (error "Could not init repository for new package"))
      (unless (eq 0 (call-process git-bin nil output nil "add" (buffer-file-name)))
        (pop-to-buffer output)
        (error "Could not add new package: %s" (buffer-file-name))))))

;;;###autoload
(defun scratch-pkgs-new (name)
  "Create new scratch package for feature NAME."
  (interactive "sNew feature symbol: ")
  (let* ((file-name (format "%s.el" name))
         (dir-name  (expand-file-name
                     (file-name-as-directory name)
                     (scratch-pkgs-local-repos-dir)))
         (file-path (expand-file-name file-name dir-name))
         (buffer (or (get-buffer file-name)
                     (find-file-noselect file-path))))
    (funcall scratch-pkgs-init buffer)
    (switch-to-buffer buffer)
    (emacs-lisp-mode)
    (add-hook 'before-save-hook #'scratch-pkgs--init nil t)))

(defun scratch-pkgs--straight-integration ()
  "TODO  Straight users can submit a PR."
  (display-warning
   '(scratch-pkgs straight)
   "`scratch-pkgs-straight-integration' is just a stub."))

(defun scratch-pkgs--load-path-integration ()
  "Set up load path to find scratch packages."
  (mapcar
   (lambda (d) (add-to-list 'load-path d))
   (mapcar #'cdr (scratch-pkgs--repo-paths))))

(declare-function elpaca-update-menus "elpaca")
;;;###autoload
(defun stratch-pkgs-elpaca-integration ()
  "Set up Elpaca to be able to use scratch packages.
This is intended for running this in your init.el if you use
Elpaca.  It will ensure that Elpaca can handle your dependencies
and building the package.  Keep in mind, you will need to run
`elpaca-merge' when you make updates.  The scratch package is
stored locally in `scratch-pkgs-dir' but Elpaca will be
configured to treat this like a remote repository."

  (unless (require 'elpaca nil t)
    (user-error "You need to bootstrap Elpaca if you want to use it with your \
scratch packages"))
  (defun scratch-pkgs--menu (_)
    "Return menu items for repos under `scratch-pkgs-dir'."
    (cl-loop
     for dir in (directory-files scratch-pkgs-dir nil "[^.]")
     for path = (file-name-as-directory (expand-file-name dir scratch-pkgs-dir))
     for pre-build =
     `(let ((default-directory ,path))
        (elpaca-with-process-call ("git" "config" "receive.denyCurrentBranch" "updateInstead")
          (or success (error "%s" stderr))))
     collect (cons (intern dir) (list :source "Scratch Packages"
                                      :description (format "Locally hosted @ %s" path)
                                      :recipe `(:package ,dir :repo ,path :pre-build ,pre-build)))))

  (add-to-list 'elpaca-menu-functions #'scratch-pkgs--menu)
  (elpaca-update-menus #'scratch-pkgs--menu))

(provide 'scratch-pkgs)
;;; scratch-pkgs.el ends here.
;; TODO make these local words generic across spelling pkgs
;; Local Variables:
;; ispell-buffer-session-localwords: ("pkgs")
;; jinx-local-words: "pkgs"
;; End:
