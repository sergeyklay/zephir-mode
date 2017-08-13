# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog][keep-cl] and this project adheres
to [Semantic Versioning][semver] .

## [Unreleased]
### Added
- Amended `c-primitive-type-kwds` (`uchar`, `ulong`, etc)
- Amended `c-protection-kwds` (`internal`)

## [0.3.3] - 2017-08-07
### Changed
- Use lexical binding for zephir-mode
  Refs: [EmacWiki][emacswiki-binding], [Yoo Box][yoobox-binding], [Emacs Stack Exchange][emacs-stack-exchange]
- Raised minimum Emacs support to 24.3

## [0.3.2] - 2017-08-06
### Removed
- Removed no longer used `zephir-mode-version-number` and `zephir-mode-modified`

### Changed
- Cleaned the syntax table
- The `zephir-mode-version` function now require the [`pkg-info`][pkg-info]

## [0.3.1] - 2017-08-03
### Fixed
- Fixed support of [Speedbar][speedbar] to observe Zephir files

### Changed
- Updated the documentation

## [0.3.0] - 2017-08-03
### Added
- Enabled Zephir Mode offset style

### Changed
- Updated Mode info

## [0.2.0] - 2017-07-31
### Added
- Added `zephir-mode-open-github` command to open the Zephir Mode GitHub page
- Added `zephir-open-website-home` command to open the Zephir web site

### Removed
- Removed unused `heredoc` functionality

## 0.1.0 - 2017-07-30
### Added
- Initial stable release

[Unreleased]: https://github.com/sergeyklay/zephir-mode/compare/0.3.3...HEAD
[0.3.3]: https://github.com/sergeyklay/zephir-mode/compare/0.3.2...0.3.3
[0.3.2]: https://github.com/sergeyklay/zephir-mode/compare/0.3.1...0.3.2
[0.3.1]: https://github.com/sergeyklay/zephir-mode/compare/0.3.0...0.3.1
[0.3.0]: https://github.com/sergeyklay/zephir-mode/compare/0.2.0...0.3.0
[0.2.0]: https://github.com/sergeyklay/zephir-mode/compare/0.1.0...0.2.0
[keep-cl]: http://keepachangelog.com
[semver]: http://semver.org
[speedbar]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Speedbar.html
[pkg-info]: https://github.com/lunaryorn/pkg-info.el
[emacswiki-binding]: https://www.emacswiki.org/emacs/DynamicBindingVsLexicalBinding
[yoobox-binding]: https://yoo2080.wordpress.com/2013/09/11/emacs-lisp-lexical-binding-gotchas-and-related-best-practices
[emacs-stack-exchange]: https://emacs.stackexchange.com/questions/2129/why-is-let-faster-with-lexical-scope
