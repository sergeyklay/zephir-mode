# Zephir Mode for GNU Emacs

[![licence][license-badge]][license]
[![travis badge][travis-badge]][travis-link]

Provides font-locking, indentation and navigation support for the
[Zephir programming language][zephir] .

The mode heavily inspired by [`php-mode`][php-mode] and based on the
[`derived-mode-ex.el`][derived-mode] code supported with the `cc-mode`.

- [Installation](#installation)
  - [Using MELPA](#using-melpa)
  - [Manual Install](#manula-install)
- [Usage](#usage)
- [Changes](#changes)
- [External Links](#external-links)
- [License](#license)

## Installation

Known to work with GNU Emacs 24 and later. Zephir Mode may work with older
versions of Emacs but this is not guaranteed. Bug reports for problems related
to using Zephir Mode with older versions of Emacs will most like _not_ be
addressed.

The master of all the material is the Git repository at
https://github.com/sergeyklay/zephir-mode .

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

and then use <kbd>M</kbd> <kbd>x</kbd> ``package-list-packages`` to get to the
package listing and install from there. MELPA tracks this Git repository and
updates relatively soon after each commit or formal release. For more detail on
setting up see [MELPA Getting Started](https://melpa.org/#/getting-started).

### Manual Install

1. Download `zephir-mode.el`

2. Put the file in your Elisp common folder like `$HOME/.emacs.d/lisp/`

3. Then you can include like this:`
   ```elisp
   (add-to-list 'load-path
                (expand-file-name "lisp" user-emacs-directory))
   ```
4. Add **either** of the two following lines to your initialization file. The
   first only loads adoc mode when necessary, the 2nd always during startup
   of GNU Emacs.
   ```elisp
   (autoload 'zephir-mode "zephir-mode" nil t)
   ;; OR
   (require 'zephir-mode)
   ```
4. Optionally byte compile adoc-mode.el for faster startup:
   <kbd>M</kbd> <kbd>x</kbd> `byte-compile`

5. To use Zephir Mode, call `zephir-mode` after you opened an Zephir file:
    <kbd>M</kbd> <kbd>x</kbd> `zephir-mode`

## Usage

| Command (For the <kbd>M</kbd> <kbd>x</kbd> prompt.) | Description |
| --- | --- |
| `zephir-mode` | Switches to Zephir Mode. |
| `zephir-mode-version` | Print version info for Zephir Mode. |
| `zephir-mode-open-github` | Go to the Zephir Mode GitHub page. |
| `zephir-open-website-home` | Go to the Zephir website. |
| `zephir-current-class` | Insert current class name if cursor in class context. |
| `zephir-current-namespace` | Insert current namespace if cursor in namespace context. |

This mode inherit `c-beginning-of-defun` and `c-end-of-defun` from CC Mode but
it have two replacement functions specifically for Zephir.  Zephir Mode remap
the commands themselves and not their default key-bindings so that
zephir-specific versions will work even if the user has reconfigured their keys,
e.g. if they rebind `c-end-of-defun` to something other than
<kbd>C</kbd> <kbd>M</kbd> <kbd>e</kbd> .

| Command (For the <kbd>M</kbd> <kbd>x</kbd> prompt.) | Description |
| --- | --- |
| `zephir-beginning-of-defun` | Move to the beginning of the current or next function. |
| `zephir-end-of-defun` | Move to the end of the current or next function. |

## Changes

To see what has changed in recent versions of Zephir Mode,
see the [CHANGELOG.md][changelog] .

## External Links

* [Zephir Forum][forum]
* [Zephir Language][language]

## License

Zephir Mode is open source software licensed under the
[GNU General Public Licence version 3][license] .

[license-badge]: https://img.shields.io/badge/license-GPL_3-green.svg
[license]: https://www.gnu.org/licenses/gpl-3.0.txt
[zephir]: https://zephir-lang.com
[travis-badge]: https://api.travis-ci.org/sergeyklay/zephir-mode.svg
[travis-link]: https://travis-ci.org/sergeyklay/zephir-mode
[php-mode]: https://github.com/ejmr/php-mode
[derived-mode]: http://cc-mode.sourceforge.net/derived-mode-ex.el
[changelog]: ./CHANGELOG.md
[forum]: https://forum.zephir-lang.com
[language]: https://zephir-lang.com
