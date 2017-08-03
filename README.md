# Zephir Mode for GNU Emacs

[![licence][license-badge]][license]
[![travis badge][travis-badge]][travis-link]

Provides font-locking, indentation and navigation support for the
[Zephir][zephir] programming language for [GNU Emacs][emacs] .

The mode based on the [`derived-mode-ex.el`][derived-mode] code
supported with the `cc-mode`.

- [Features](#features)
- [Installation](#installation)
  - [Manual Install](#manual-install)
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

## Installation

Known to work with GNU Emacs 24 and later. Zephir Mode may work with older
versions of Emacs, or with other flavors of Emacs (e.g. XEmacs) but this is
_not_ guaranteed. Bug reports for problems related to using Zephir Mode with
older versions of Emacs will most like _not_ be addressed.

The master of all the material is the Git repository at
https://github.com/sergeyklay/zephir-mode .

### Manual Install

1. Download `zephir-mode.el`

2. Put the file in your Elisp common folder like `$HOME/.emacs.d/lisp/`

3. Then you can include like this:`
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
   <kbd>M</kbd> <kbd>x</kbd> `byte-compile`

## Usage

### Interactive Commands

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

Any file that matches the glob `*.zep` is automatically opened in `zephir-mode`.

### Keymapping

## Support

Feel free to ask question or make suggestions in our [issue tracker][issues].

Keymaps can be added to the `zephir-mode-map` variable.

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
[emacs]: https://www.gnu.org/software/emacs/
[zephir]: https://zephir-lang.com
[travis-badge]: https://api.travis-ci.org/sergeyklay/zephir-mode.svg
[travis-link]: https://travis-ci.org/sergeyklay/zephir-mode
[derived-mode]: http://cc-mode.sourceforge.net/derived-mode-ex.el
[changelog]: ./CHANGELOG.md
[issues]: https://github.com/sergeyklay/zephir-mode/issues
[forum]: https://forum.zephir-lang.com
[language]: https://zephir-lang.com
