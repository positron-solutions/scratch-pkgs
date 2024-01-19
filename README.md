<!-- !!!THIS FILE HAS BEEN GENERATED!!! Edit README.org -->

<!-- a href="https://melpa.org/#/erk"><img src="https://melpa.org/packages/erk-badge.svg" alt="melpa package"></a> <a href="https://stable.melpa.org/#/erk"><img src="https://stable.melpa.org/packages/erk-badge.svg" alt="melpa stable package"></a> -->
<a href="https://github.com/positron-solutions/scratch-pkgs/actions/?workflow=CI"><img src="https://github.com/positron-solutions/scratch-pkgs/actions/workflows/ci.yml/badge.svg" alt="CI workflow status"></a>
<a href="https://github.com/positron-solutions/scratch-pkgs/actions/?workflow=Developer+Certificate+of+Origin"><img src="https://github.com/positron-solutions/scratch-pkgs/actions/workflows/dco.yml/badge.svg" alt="DCO Check"></a>


# Scratch Packages

Create quick packages for personal use in your configuration.  A more
light-weight solution than [Elisp Repo Kit](https://github.com/positron-solutions/elisp-repo-kit) for quickly developing an idea within
a properly formed package.

⚠️ **This package will likely be rolled into Elisp Repo Kit** ⚠️
⚠️ \*The functionality is pre-release.  File bugs. ⚠️


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

In addition, many packages for Editing Elisp such as Lispy contains shortcuts for evaluating expressions before the point or evaluating them in the context of the other window.  This last advantage really undermines the functionality provided in `lisp-interaction-mode`


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

-   [Usage](#org6d2b36b)
    -   [Customization](#org7ef8536)
    -   [Using Your Package in Your Config](#org9c4b3fc)
-   [Contributing](#org64380e8)


# Usage

-   Create a new package skeleton with `scratch-pkgs-new`
-   See existing scratch packages with `scratch-pkgs`


## Customization

-   `scratch-pkgs-mode` controls the manner of integration with your Elisp package management strategy.  See the next section on using your package in your config.
-   `scratch-pkgs-template` customize initial buffer contents ⚠️ unstable ⚠️
-   `scratch-pkgs-init` function passed each new buffer.  Use to perform full customization of your new buffer behavior.
-   `scratch-pkgs-after-new-hook` runs after creating a buffer.  `scratch-pkgs-init` can do the same things, but this hook requires less care.
-   `scratch-pkgs-after-package-init-hook` runs only when you save your scratch buffer.  Because integrating the package with your Elisp package management may require setting up a git repository, much work is done only after the first save of a brand new package.  This hook runs after initialization.  You could run `magit-status` for instance to remind yourself of what was created after the first save.


## Using Your Package in Your Config

Scratch packages works by:

-   Creates and stores packages inside `~/.emacs.d/etc/scratch-pkgs/`
-   Integrates with your usual package loading (see options below ⚠️)  You can then declare with use-package normally:

    (use-package my-scratch-package)


### Correct Loading & Initialization Order

Scratch packages can integrate with Elisp package managers to streamline using your packages in your configuration.  It must to be loaded and perform initialization *before* your personal packages are loaded.

-   Load Path Initialization

    **Keep in mind, setting the load path will not handle your custom package dependencies**   One of the following solutions will make your packages available for simple `(require 'my-package)` expressions.
    
    1.  Require scratch packages very early in your configuration, something like this:
        
            (use-package scratch-pkgs)
            (require 'scratch-pkgs)
            (setopt scratch-pkgs-mode 'local)
            (scratch-pkgs-integrate)
    
    2.  Configure the load path manually instead:
        
            (push load-path "~/.emacs.d/var/scratch-pkgs/")
    3.  For each package, use the explicit load path option of `use-package`
        (less convenient, so not recommended):
        
            (use-package my-scratch-package
              :load-path "~/.emacs.d/var/scratch-pkgs/")
    
    See the [Lisp Libraries](emacs#Lisp Libraries) section of the Emacs manual and [Loading](elisp#Loading) in the Elisp
    manual for more detailed treatment.

-   Elpaca Initialization

    Usually just after loading other load-order sensitive packages like `no-littering`, you can load `scratch-pkgs` and perform integration:
    
        (elpaca (scratch-pkgs
                 :host github :repo "positron-solutions/scratch-pkgs")
          (require 'scratch-pkgs)
          (setopt scratch-pkgs-mode 'elpaca)
          (scratch-pkgs-integrate))
    
    Afterwards, `(use-package my-package)` will just work **and** Elpaca will handle the dependencies.

-   Straight

    **TODO**.  Other Elisp package management integrations to streamline usage are welcome.

-   Package

    **TODO**.  Likely we should use some `package-vc-install` style solution.


### Package Management Integration

-   Load Path

    If you followed the initialization instructions above, your package is already on the load path.  However, manual load-path packages don't do any automated dependency handling.  You can declare this manually:
    
        (use-package my-package
          :requires foo)
        
        (use-package my-package
          :requires (foo bar baz))

-   Elpaca

    Having Elpaca fetch dependencies and build your package is easy:
    
        (use-package my-package)
    
    ⚠️ Updates are **not** automatic in this workflow.  You need to commit changes you want to install permanently and then run `elpaca-fetch` and `elpaca-merge` to install your modifications into the `repos/` of your `elpaca-directory`

-   Straight

    **TODO**.  Other Elisp package management integrations to streamline usage are welcome.

-   Package

    **TODO**.  Likely we should use some `package-vc-install` style solution.


# Contributing

See the [CONTRIBUTING](./CONTRIBUTING.md) guide for help making changes to this project.  For
turn-key contribution to the software ecosystem that keeps you moving, see the
[funding links](https://github.com/sponsors/positron-solutions).

