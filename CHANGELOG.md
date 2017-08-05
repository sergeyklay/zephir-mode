# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog][keep-cl] and this project adheres
to [Semantic Versioning][semver] .

## [Unreleased]
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

[Unreleased]: https://github.com/sergeyklay/zephir-mode/compare/0.3.1...HEAD
[0.3.1]: https://github.com/sergeyklay/zephir-mode/compare/0.3.0...0.3.1
[0.3.0]: https://github.com/sergeyklay/zephir-mode/compare/0.2.0...0.3.0
[0.2.0]: https://github.com/sergeyklay/zephir-mode/compare/0.1.0...0.2.0
[keep-cl]: http://keepachangelog.com
[semver]: http://semver.org
[speedbar]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Speedbar.html
[pkg-info]: https://github.com/lunaryorn/pkg-info.el
