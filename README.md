# quick-fasd.el
![License](https://img.shields.io/github/license/jamescherti/quick-fasd.el)
![](https://raw.githubusercontent.com/jamescherti/quick-fasd.el/main/.images/made-for-gnu-emacs.svg)

The **quick-fasd** Emacs package integrates the Fasd tool within the Emacs environment. Fasd, a command-line utility, enhances the productivity of users by providing fast access to frequently used files and directories. Inspired by tools such as autojump, z, and v, Fasd functions by maintaining a dynamic index of files and directories you access, allowing you to reference them quickly from the command line.

After installing quick-fasd in Emacs, you can easily navigate your file system directly within Emacs by using Fasd’s fast-access capabilities. For example, you can open recently accessed files or quickly jump to frequently used directories without leaving the Emacs environment.

Here’s how the package works:
- `quick-fasd-mode` hooks into `find-file-hook` to automatically add all visited files and directories to Fasd's database.
- The user can invoke the `quick-fasd-find-file` function, which prompts for input and display available candidates from the Fasd index, enabling rapid and efficient file navigation.

## Installation

### Emacs: Install with straight (Emacs version < 30)

To install *quick-fasd* with `straight.el`:

1. It if hasn't already been done, [add the straight.el bootstrap code](https://github.com/radian-software/straight.el?tab=readme-ov-file#getting-started) to your init file.
2. Add the following code to the Emacs init file:
```emacs-lisp
(use-package quick-fasd
  :ensure t
  :straight (quick-fasd
             :type git
             :host github
             :repo "jamescherti/quick-fasd.el"))
```

### Emacs: Installing with use-package and :vc (Built-in feature in Emacs version >= 30)

To install *quick-fasd* with `use-package` and `:vc` (Emacs >= 30):

``` emacs-lisp
(use-package quick-fasd
  :ensure t
  :vc (:url "https://github.com/jamescherti/quick-fasd.el"
       :rev :newest))
```

### Doom Emacs

Here is how to install *quick-fasd* on Doom Emacs:

1. Add to the `~/.doom.d/packages.el` file:
```elisp
(package! quick-fasd
 :recipe
 (:host github :repo "jamescherti/quick-fasd.el"))
```

2. Add to `~/.doom.d/config.el`:
```elisp
;; TODO: Load the mode here
(after! quick-fasd
 ;; TODO: setq options
 )
```

3. Run the `doom sync` command:
```
doom sync
```

## Usage

Add a shortcut key for `quick-fasd-find-file`{.verbatim} and enable the
`quick-fasd-mode`{.verbatim} which makes sure that opening files or
directories in Emacs updates the fasd database.

``` {.commonlisp org-language="emacs-lisp"}
(global-set-key (kbd "C-h C-/") 'quick-fasd-find-file)
(quick-fasd-mode 1)
```

Calling `quick-fasd-find-file`{.verbatim} with a prefix argument of

-   `C-u`{.verbatim} lists only directories
-   `M--`{.verbatim} lists only files

With no prefix it shows files and directories.

1.  Options

    Use the customize interface:

    `M-x customize-group RET fasd RET`{.verbatim}

    1.  Initial Prompt

        Usually `fasd`{.verbatim} will prompt for a initial query. To
        turn that off and get all results directly, set
        `quick-fasd-enable-initial-prompt`{.verbatim} to
        `nil`{.verbatim}.

    2.  Completion Function

        By default the standard `completing-read-function`{.verbatim}
        will be used, which could be using `helm`{.verbatim} or
        `ido`{.verbatim} depending on what you are using.

    3.  Standard search

        By default the `fasd`{.verbatim} search parameter is
        `-a`{.verbatim} which searches files and directories, you can
        customize this with the `quick-fasd-standard-search`{.verbatim}
        option.

## Author and License

The *quick-fasd* Emacs package has been written by steckerhalter and [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2024-2025 James Cherti
Copyright (C) 2013 steckerhalter

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.

## Links

- [quick-fasd.el @GitHub](https://github.com/jamescherti/quick-fasd.el)
