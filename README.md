# Zephir Mode for GNU Emacs

[![licence][license-badge]][license]
[![travis badge][travis-badge]][travis-link]

A GNU Emacs major mode for editing Zephir code.

The mode heavily inspired by [`php-mode`][php-mode] and based on the
[`derived-mode-ex.el`][derived-mode] code supported with the `cc-mode`.

## Installation

Known to work with GNU Emacs 24 and later. Zephir Mode may work with older
versions of Emacs but this is not guaranteed. Bug reports for problems related
to using Zephir Mode with older versions of Emacs will most like _not_ be
addressed.

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

The master of all the material is the Git repository at
https://github.com/sergeyklay/zephir-mode .

## External Links

* [Zephir Forum][forum]
* [Zephir Language][language]

## License

Zephir Mode is open source software licensed under the
GNU General Public Licence version 3.

[license-badge]: https://img.shields.io/badge/license-GPL_3-green.svg
[license]: https://www.gnu.org/licenses/gpl-3.0.txt
[travis-badge]: https://api.travis-ci.org/sergeyklay/zephir-mode.svg
[travis-link]: https://travis-ci.org/sergeyklay/zephir-mode
[php-mode]: https://github.com/ejmr/php-mode
[derived-mode]: http://cc-mode.sourceforge.net/derived-mode-ex.el
[forum]: https://forum.zephir-lang.com
[language]: https://zephir-lang.com
