<!-- !!!THIS FILE HAS BEEN GENERATED!!! Edit README.org -->

<!-- a href="https://melpa.org/#/erk"><img src="https://melpa.org/packages/erk-badge.svg" alt="melpa package"></a> <a href="https://stable.melpa.org/#/erk"><img src="https://stable.melpa.org/packages/erk-badge.svg" alt="melpa stable package"></a> -->
<a href="https://github.com/positron-solutions/scratch-pkgs/actions/?workflow=CI"><img src="https://github.com/positron-solutions/scratch-pkgs/actions/workflows/ci.yml/badge.svg" alt="CI workflow status"></a>
<a href="https://github.com/positron-solutions/scratch-pkgs/actions/?workflow=Developer+Certificate+of+Origin"><img src="https://github.com/positron-solutions/scratch-pkgs/actions/workflows/dco.yml/badge.svg" alt="DCO Check"></a>


# Scratch Packages

The repository hosting this package was created with [elisp-repo-kit](https://github.com/positron-solutions/elisp-repo-kit).  You can
clone and rename it using `erk-new` to quickly get a package set up.


# Install Scratch Packages

    ;; update this after you publish your new package!
    ;; (use-package scratch-pkgs) ; vanilla, assuming you have MELPA configured
    
    ;; package-vc
    (package-vc-install
     '(scratch-pkgs :url "https://github.com/positron-solutions/scratch-pkgs.git"
           :lisp-dir "lisp"
           :doc "doc/scratch-pkgs.texi"))
    
    ;; using elpaca's with explicit recipe
    (use-package scratch-pkgs
      :elpaca (scratch-pkgs :host github :repo "positron-solutions/scratch-pkgs"))
    
    ;; straight with explicit recipe
    (use-package scratch-pkgs
      :straight (scratch-pkgs :type git :host github :repo "positron-solutions/scratch-pkgs"))
    
    ;; or use manual load-path & require, you brave yak shaver


# Table of Contents

-   [Contributing](#org2e31983)


# Contributing

Because this repository is created from [elisp-repo-kit](https://github.com/positron-solutions/elisp-repo-kit), please file any issues
or make relevant pull requests there unless the changes are specific to this
repo.

See the [CONTRIBUTING](./CONTRIBUTING.md) guide for help making changes to this project.

For turn-key contribution to the software ecosystem that keeps you moving, see
the [funding links](https://github.com/sponsors/positron-solutions).

