;;; zephir-mode.el --- Major mode for editing Zephir code -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Serghei Iakovlev

;; Author: Serghei Iakovlev (serghei@phalconphp.com)
;; Maintainer: Serghei Iakovlev
;; Version: 0.4.0
;; URL: https://github.com/sergeyklay/zephir-mode
;; Keywords: languages
;; Package-Requires: ((cl-lib "0.5") (pkg-info "0.4") (emacs "24.3"))

;; This file is not part of GNU Emacs.

;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;;   GNU Emacs major mode for editing Zephir code.  Provides font-locking,
;; indentation, alignment and navigation support.
;;
;;   Syntax checking: Flymake support is not provided.  See Flycheck at
;; http://www.flycheck.org for on-the-fly validation and liniting of Zephir
;; code.
;;
;;   Zephir -- is a high level language that eases the creation and
;; maintainability of extensions for PHP.  Zephir extensions are
;; exported to C code that can be compiled and optimized by major C
;; compilers such as gcc/clang/vc++.  Functionality is exposed to the
;; PHP language.  For more information see https://zephir-lang.com
;;
;;   Bug tracking is currently handled using the GitHub issue tracker at
;; https://github.com/sergeyklay/zephir-mode/issues
;;
;;   History is tracked in the Git repository rather than in this file.
;; See https://github.com/sergeyklay/zephir-mode/blob/master/CHANGELOG.md
;;
;; Usage:
;;
;;   Put this file in your Emacs Lisp path (eg. site-lisp) and add to
;; your .emacs file:
;;
;;   (require 'zephir-mode)
;;
;; To use abbrev-mode, add lines like this:
;;   (add-hook 'zephir-mode-hook
;;     '(lambda () (define-abbrev zephir-mode-abbrev-table "ex" "extends")))
;;
;; Many options available under Help:Customize
;; Options specific to zephir-mode are in
;;  Programming/Languages/Zephir

;;; Code:


;;; Compatibility

;; Work around emacs bug#18845, cc-mode expects cl to be loaded
;; while zephir-mode only uses cl-lib (without compatibility aliases)
(eval-and-compile
  (if (and (= emacs-major-version 24) (>= emacs-minor-version 4))
      (require 'cl)))


;;; Requirements

;; Tell the byte compiler about autoloaded functions from packages
(declare-function pkg-info-version-info "pkg-info" (package))

(eval-when-compile
  (require 'rx))

(require 'cl-lib)
(require 'pkg-info)


;;; Customization

;;;###autoload
(defgroup zephir nil
  "Major mode for editing Zephir code."
  :tag "Zephir"
  :prefix "zephir-"
  :group 'languages
  :link '(url-link :tag "GitHub" "https://github.com/sergeyklay/zephir-mode")
  :link '(url-link :tag "Zephir Forum" "https://forum.zephir-lang.com")
  :link '(url-link :tag "Official Site" "https://zephir-lang.com")
  :link '(emacs-commentary-link :tag "Commentary" "zephir-mode"))

(defvar zephir-website-url "https://zephir-lang.com"
  "Official website of Zephir programming language.")

(defvar zephir-mode-repo-url "https://github.com/sergeyklay/zephir-mode"
  "Zephir Mode GitHub page.")

(defvar zephir-mode-hook nil
  "List of functions to call when entering Zephir Mode.")

(defcustom zephir-indent-tabs-mode t
  "Indentation can insert tabs in Zephir Mode if this is non-nil."
  :type 'boolean
  :group 'zephir
  :safe 'booleanp)


;;; Version information

(defun zephir-mode-version (&optional show-version)
  "Display string describing the version of Zephir Mode.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list t))
  (let ((version (pkg-info-version-info 'zephir-mode)))
    (when show-version
      (message "Zephir Mode version: %s" version))
    version))


;;; Utilities

(defun zephir-syntax-context (&optional pos)
  "Determine the syntax context at POS, defaulting to point.

Return nil, if there is no special context at POS, or one of

`comment'
     POS is inside a comment

`single-quoted'
     POS is inside a single-quoted string

`double-quoted'
     POS is inside a double-quoted string"
  (let ((state (save-excursion (syntax-ppss pos))))
    (if (nth 4 state)
        'comment
      (pcase (nth 3 state)
        (`?\' 'single-quoted)
        (`?\" 'double-quoted)))))

(defun zephir-in-listlike (re-open-str)
  "If point is in a listlike, return the position of the opening char of it.
Otherwise return nil.  The RE-OPEN-STR is a regexp string
matching the opening character."
  (save-excursion
    (let ((opoint (nth 1 (syntax-ppss))))
      (when (and opoint
                 (progn
                   (goto-char opoint)
                   (looking-at-p re-open-str)))
        opoint))))


;;; Specialized rx

(eval-when-compile
  (defconst zephir-rx-constituents
    `(
      ;; Identifier.
      ;; The first character of an identifier may be a dollar sign.
      ;; After that, we expect a letter or an underscore.
      ;; The rest may contain any alphanumeric character + underscore.
      (identifier . ,(rx (optional "$")
                         (one-or-more (any alpha "_"))
                         (zero-or-more (any alnum "_"))))
      ;; Function declaraion.
      (fn-decl . ,(rx line-start
                      symbol-start
                      "function"
                      symbol-end)))
    "Additional special sexps for `zephir-rx'.")

  (defmacro zephir-rx (&rest sexps)
    "Zephir-specific replacement for `rx'.

See `rx' documentation for more information about REGEXPS param."
    (let ((rx-constituents (append zephir-rx-constituents rx-constituents)))
      (cond ((null sexps)
             (error "No regexp"))
            ((cdr sexps)
             (rx-to-string `(and ,@sexps) t))
            (t
             (rx-to-string (car sexps) t))))))


;;; Navigation


;;; Indentation


;;; Faces

;;;###autoload
(defgroup zephir-faces nil
  "Faces used in Zephir Mode"
  :tag "Zephir Faces"
  :group 'zephir
  :group 'faces)


;;; Font Locking

(defvar zephir-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Symbol constituents
    (modify-syntax-entry ?_   "_"      table)
    ;; Characters used to delimit string constants
    (modify-syntax-entry ?\"  "\""     table)
    (modify-syntax-entry ?\'  "\""     table)
    ;; Comment enders
    (modify-syntax-entry ?\n  "> b"    table)
    ;; Give CR the same syntax as newline
    (modify-syntax-entry ?\^m "> b"    table)
    ;; Set up block and line oriented comments
    (modify-syntax-entry ?/   ". 124b" table)
    (modify-syntax-entry ?*   ". 23"   table)
    ;; The dollar sign is an expression prefix for variables
    (modify-syntax-entry ?$   "'"      table)
    ;; The parenthesis, braces and brackets
    (modify-syntax-entry ?\(  "()"     table)
    (modify-syntax-entry ?\)  ")("     table)
    (modify-syntax-entry ?\{  "(}"     table)
    (modify-syntax-entry ?\}  "){"     table)
    (modify-syntax-entry ?\[  "(]"     table)
    (modify-syntax-entry ?\]  ")["     table)
    table)
  "Syntax table in use in `zephir-mode' buffers.

This includes setting ' and \" as string delimiters, and setting up
the comment syntax tokens handle both line style \"//\" and block style
\"/*\" \"*/\" comments.")

(defvar zephir-font-lock-keywords
  `(
    ;; Function names, i.e. `function foo()'.
    (,(zephir-rx (group fn-decl)
                 (one-or-more space)
                 (group identifier))
     2 font-lock-function-name-face))
  "Font lock keywords for Zephir Mode.")


;;; Alignment


;;; Dealing with strings


;;; Imenu


;;; Initialization

;;;###autoload
(define-derived-mode zephir-mode prog-mode "Zephir" ()
  "A major mode for editing Zephir code."
  :group 'zephir-mode
  ;; Comment setup
  (setq-local comment-use-syntax t)
  (setq-local comment-auto-fill-only-comments t)
  ;; Indentation
  (setq indent-tabs-mode zephir-indent-tabs-mode)
  ;; Zephir vars are case-sensitive
  (setq case-fold-search t)
  ;; Font locking
  (setq font-lock-defaults '((zephir-font-lock-keywords) nil nil)))

;;;###autoload
(defun zephir-mode-open-github ()
  "Go to the Zephir Mode GitHub page."
  (interactive)
  (browse-url zephir-mode-github-url))

;;;###autoload
(defun zephir-open-website-home ()
  "Go to the Zephir website."
  (interactive)
  (browse-url zephir-website-url))


;; Invoke zephir-mode when appropriate

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.zep\\'" . zephir-mode))

(provide 'zephir-mode)

;; Local Variables:
;; firestarter: ert-run-tests-interactively
;; End:

;;; zephir-mode.el ends here
