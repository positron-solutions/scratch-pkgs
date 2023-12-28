;;; scratch-pkgs-test.el --- test your freaking package!  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Psionic K

;; Author:  Psionic K <contact@positron.solutions>

;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;; this software and associated documentation files (the "Software"), to deal in
;; the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; Run the batch tests from root directory:
;; nix shell .github#emacsGit --quick --script .github/run-shim.el -- test
;; Test dependencies can be provided to the Emacsen declared inside the root
;; flake.nix.

;; For local development, dependencies should be installed by the user.  Tests
;; can be run from one of the project files using the `erk-ert-project'
;; command.

;;; Code:

(require 'ert)
(require 'scratch-pkgs)

;; (ert-deftest scratch-pkgs-hello-test ()
;;   (should (string-match-p (scratch-pkgs-hello)
;;                           "hello")))

(provide 'scratch-pkgs-test)
;;; scratch-pkgs-test.el ends here.
