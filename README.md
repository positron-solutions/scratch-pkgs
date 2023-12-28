<!-- !!!THIS FILE HAS BEEN GENERATED!!! Edit README.org -->

<!-- a href="https://melpa.org/#/erk"><img src="https://melpa.org/packages/erk-badge.svg" alt="melpa package"></a> <a href="https://stable.melpa.org/#/erk"><img src="https://stable.melpa.org/packages/erk-badge.svg" alt="melpa stable package"></a> -->
<a href="https://github.com/positron-solutions/scratch-pkgs/actions/?workflow=CI"><img src="https://github.com/positron-solutions/scratch-pkgs/actions/workflows/ci.yml/badge.svg" alt="CI workflow status"></a>
<a href="https://github.com/positron-solutions/scratch-pkgs/actions/?workflow=Developer+Certificate+of+Origin"><img src="https://github.com/positron-solutions/scratch-pkgs/actions/workflows/dco.yml/badge.svg" alt="DCO Check"></a>


# Scratch Packages

Create quick packages for personal use in your configuration.  A more
light-weight solution than [Elisp Repo Kit](https://github.com/positron-solutions/elisp-repo-kit) for quickly developing an idea within
a properly formed package.


## Why You Want This

Compared to using `ielm` and `lisp-interaction-mode` in regular scratch buffers,
packages support:

-   `byte-compile-and-load` to quickly reload (and let the compiler detect problems)
-   flycheck will work better
-   `edebug` and `debug`
-   proper loading, unloading, reloading
-   dependency declaration so that `use-package` etc can arrange the loading order
-   autoloads

I don't recommend `lisp-interaction` mode for scratch since packages have a lot of
advantages, org mode is better for persistent literate exploration, and ielm
(especially with history enabled) is better for truly ephemeral expression
building.


# Install

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

-   [Usage](#org81c61db)
    -   [Using Your Package in Your Config](#org35bd568)
    -   [Load Path](#orgec33624)
-   [Contributing](#org20ae8d3)


# Usage

-   Create a new package skeleton with `scratch-pkgs-new`
-   See existing scratch packages with `scratch-pkgs`


## Using Your Package in Your Config

Scratch packages works by:

-   Creates and stores packages inside `~/.emacs.d/var/scratch-pkgs/`
-   Adds that path to the loading path (see options below ⚠️)

You can then declare with use-package normally:

    (use-package my-scratch-package)


## Load Path

You want at least **one of these** solutions in place to make sure your scratch
packages can be used in your configuration normally.

1.  Require scratch packages very early in your configuration, something like this:
    
        (use-package scratch-pkgs)
        (require 'scratch-pkgs) ; this blocks, ensuring your scratch pkg will be in the load path
2.  Add an implicit dependency in your `use-package` expressions:
    
        (use-package my-scratch-package
          :after scratch-pkgs)
3.  Configure the load path manually instead:
    
        (push load-path "~/.emacs.d/var/scratch-pkgs/")
4.  For each package, use the explicit load path option of `use-package`
    (less convenient, so not recommended):
    
        (use-package my-scratch-package
          :load-path "~/.emacs.d/var/scratch-pkgs/")

See the [Lisp Libraries](emacs#Lisp Libraries) section of the Emacs manual and [Loading](elisp#Loading) in the Elisp
manual for more detailed treatment.


# Contributing

See the [CONTRIBUTING](./CONTRIBUTING.md) guide for help making changes to this project.  For
turn-key contribution to the software ecosystem that keeps you moving, see the
[funding links](https://github.com/sponsors/positron-solutions).

