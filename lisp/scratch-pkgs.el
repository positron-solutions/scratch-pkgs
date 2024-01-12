;;; scratch-pkgs.el --- Scratch Packages -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2023 Positron Solutions <contact@positron.solutions>
;; Author:  Psionic K <contact@positron.solutions>
;; Keywords: convenience
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Homepage: http://github.com/positron-solutions/scratch-pkgs

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

(declare-function no-littering-expand-etc-file-name "no-littering")
(defcustom scratch-pkgs-dir
  (file-name-as-directory (if (featurep 'no-littering)
                              (no-littering-expand-etc-file-name "scratch-pkgs")
                            (expand-file-name "etc/scratch-pkgs" user-emacs-directory)))
  "Where scratches are saved."
  :group 'scratch-pkgs
  :type 'directory)

(defun scratch-pkgs--configure-load-path (symbol value)
  "Add scratch packages to the `load-path'.
SYMBOL and VALUE is from the setter in
`scratch-pkgs-add-to-load-path'."
  (set-default-toplevel-value symbol value)
  (unless (file-directory-p scratch-pkgs-dir)
    (make-directory scratch-pkgs-dir t))
  (when scratch-pkgs-dir (push scratch-pkgs-dir load-path)))

(defcustom scratch-pkgs-add-to-load-path t
  "When non-nil, adds your temporary packages to the load path."
  :set #'scratch-pkgs--configure-load-path
  :group 'scratch-pkgs
  :type 'boolean)

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

(defun scratch-pkgs--only-elisp-files (dir)
  "Return just normal elisp files in DIR."
  (let ((match-elisp (rx line-start
                         (+ not-newline)
                         (literal ".el")
                         line-end))
        (match-flycheck (rx line-start
                            (literal "flycheck_")
                            (+ not-newline)
                            (literal ".el"))))
    (save-match-data
      (seq-filter
       (lambda (f) (not (string-match-p match-flycheck f)))
       (directory-files dir nil match-elisp)))))

(defun scratch-pkgs--read ()
  "Read a file in the scratch dir if there are files."
  (when (file-directory-p scratch-pkgs-dir)
    (let* ((files (scratch-pkgs--only-elisp-files scratch-pkgs-dir)))
      (completing-read "Choose existing scratch package: " files nil t))))

;;;###autoload
(defun scratch-pkgs (&optional file-name)
  "Open an old scratch FILE-NAME."
  (interactive (list (scratch-pkgs--read)))
  (if file-name
      (let* ((file-path (expand-file-name file-name scratch-pkgs-dir))
             (buffer (find-file-noselect file-path)))
        (switch-to-buffer buffer))
    (scratch-pkgs-new "scratch")))

;;;###autoload
(defun scratch-pkgs-new (name)
  "Create new scratch package for feature NAME."
  (interactive "sFeature symbol: ")
  (let* ((file-name (format "%s.el" name))
         (file-path (expand-file-name file-name scratch-pkgs-dir))
         (buffer (or (get-buffer file-name)
                     (find-file-noselect file-path))))
    (funcall scratch-pkgs-init buffer)
    (switch-to-buffer buffer)
    (emacs-lisp-mode)))

(provide 'scratch-pkgs)
;;; scratch-pkgs.el ends here.
;; TODO make these local words generic across spelling pkgs
;; Local Variables:
;; ispell-buffer-session-localwords: ("pkgs")
;; jinx-local-words: "pkgs"
;; End:
