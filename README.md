# Zephir Mode for GNU Emacs

[![Licence](https://img.shields.io/badge/license-GPL_3-green.svg)](https://www.gnu.org/licenses/gpl-3.0.txt)
[![Travis CI](https://travis-ci.org/sergeyklay/zephir-mode.svg?branch=master)](https://travis-ci.org/sergeyklay/zephir-mode)
[![Coverage Status](https://coveralls.io/repos/github/sergeyklay/zephir-mode/badge.svg)](https://coveralls.io/github/sergeyklay/zephir-mode)
[![MELPA](https://melpa.org/packages/zephir-mode-badge.svg)](https://melpa.org/#/zephir-mode)
[![MELPA Stable](https://stable.melpa.org/packages/zephir-mode-badge.svg)](https://stable.melpa.org/#/zephir-mode)

A [GNU Emacs][emacs] major mode for editing [Zephir][zephir] code.
Provides font-locking, indentation, alignment and navigation support.

Zephir -- is a high level language that eases the creation and
maintainability of extensions for PHP.  Zephir extensions are
exported to C code that can be compiled and optimized by major C
compilers such as gcc/clang/vc++.  Functionality is exposed to the
PHP language. For more information see https://zephir-lang.com .

- [Features](#features)
- [Installation](#installation)
  - [Using MELPA](#using-melpa)
  - [Manual Install](#manual-install)
     - [Spacemacs Users](#spacemacs-users)
- [Usage](#usage)
  - [Interactive Commands](#interactive-commands)
  - [Keymapping](#keymapping)
- [Support](#support)
- [Changes](#changes)
- [External Links](#external-links)
- [License](#license)

## Features

1. Syntax highlighting
2. Indentation and alignment of expressions and statements
3. Tag navigation (aka `imenu`)
4. [Speedbar][speedbar] support

## Installation

Known to work with GNU Emacs 24.3 and later. Zephir Mode may work with older
versions of Emacs, or with other flavors of Emacs (e.g. XEmacs) but this is
_not_ guaranteed. Bug reports for problems related to using Zephir Mode with
older versions of Emacs will most like _not_ be addressed.

The master of all the material is the Git repository at
https://github.com/sergeyklay/zephir-mode .

**NOTE**: The `master` branch will always contain the latest **unstable**
version. If you wish to check older versions or formal, tagged release, please
switch to the relevant [tag][tags].

### Using MELPA

The best way of installing this major mode, at least for GNU Emacs 24, is to
use the packaging system. Add MELPA or MELPA Stable to the list of repositories
to access this mode. For those who want only formal, tagged releases use
MELPA Stable:

```elisp
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
```

For those who want rolling releases as they happen use MELPA:

```elisp
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
```

and then use <kbd>M-x</kbd> `package-list-packages` to get to the
package listing and install from there. MELPA tracks this Git repository and
updates relatively soon after each commit or formal release. For more detail on
setting up see [MELPA Getting Started][getting-started].

### Manual Install

1. Download `zephir-mode.el`

2. Put the file in your Elisp common folder like `$HOME/.emacs.d/lisp/`

3. Then you can include like this:
   ```elisp
   (add-to-list 'load-path
                (expand-file-name "lisp" user-emacs-directory))
   ```
4. Add **either** of the two following lines to your initialization file.
   The first only loads Zephir Mode when necessary, the 2nd always during
   startup of GNU Emacs.
   ```elisp
   (autoload 'zephir-mode "zephir-mode" nil t)
   ;; OR
   (require 'zephir-mode)
   ```
5. Optionally byte compile `zephir-mode.el` for faster startup:
   <kbd>M-x</kbd> `byte-compile`

#### Spacemacs Users

1. Download `zephir-mode.el`

2. Put the file in Spacemacs private directory:
   `$HOME/.emacs.d/private/local/zephir-mode/zephir-mode.el`

3. Then you can enable `zephir-mode` in your `$HOME/.spacemacs` file as follows:
   ```elisp
   ;; ...
   dotspacemacs-additional-packages
   '(
     (zephir-mode :location (recip :fetcher file
                                   :path "~/.emacs.d/private/local/zephir-mode/zephir-mode.el")))
   ```
## Usage

### Interactive Commands

| Command (For the <kbd>M-x</kbd> prompt.) | Description                                              |
| ---                                      | ---                                                      |
| `zephir-mode`                            | Switches to Zephir Mode.                                 |
| `zephir-mode-version`                    | Print version info for Zephir Mode.                      |
| `zephir-open-mode-github`                | Go to the Zephir Mode GitHub Page.                       |
| `zephir-open-zephir-github`              | Go to the Zephir GitHub Page.                            |
| `zephir-open-website-home`               | Go to the Zephir Website.                                |
| `zephir-current-class`                   | Insert current class name if cursor in class context.    |
| `zephir-current-namespace`               | Insert current namespace if cursor in namespace context. |
| `zephir-beginning-of-defun`              | Move to the beginning of the current or next function.   |
| `zephir-end-of-defun`                    | Move to the end of the current or next function.         |

Any file that matches the glob `*.zep` is automatically opened in `zephir-mode`.

### Keymapping

Keymaps can be added to the `zephir-mode-map` variable.

## Support

Feel free to ask question or make suggestions in our [issue tracker][issues] .

## Changes

To see what has changed in recent versions of Zephir Mode,
see the [CHANGELOG.md][changelog] .

## External Links

* [Zephir Forum][forum]
* [Zephir Language][language]

## License

Zephir Mode is open source software licensed under the
[GNU General Public Licence version 3][license] .

[speedbar]: "https://www.gnu.org/software/emacs/manual/html_node/emacs/Speedbar.html"
[tags]: https://github.com/sergeyklay/zephir-mode/tags
[getting-started]: https://melpa.org/#/getting-started
[emacs]: https://www.gnu.org/software/emacs/
[zephir]: https://zephir-lang.com
[derived-mode]: http://cc-mode.sourceforge.net/derived-mode-ex.el
[changelog]: ./CHANGELOG.md
[issues]: https://github.com/sergeyklay/zephir-mode/issues
[forum]: https://forum.zephir-lang.com
[language]: https://zephir-lang.com
