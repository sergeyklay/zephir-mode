;;; zephir-mode.el --- Major mode for editing Zephir code

;; Copyright (C) 2017 Serghei Iakovlev

;; Author:           Serghei Iakovlev (serghei@phalconphp.com)
;; Maintainer:       Serghei Iakovlev
;; Version:          0.1.0
;; URL:              https://github.com/sergeyklay/zephir-mode
;; Keywords:         zephir languages oop
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

(defconst zephir-mode-version-number "0.0.1"
  "Zephir Mode version number.")

(defconst zephir-mode-modified "2017-07-30"
  "Zephir Mode build date.")

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

;;; Usage

;; Put this file in your Emacs lisp path (eg. site-lisp) and add to
;; your .emacs file:
;;
;;   (require 'zephir-mode)

;; To use abbrev-mode, add lines like this:
;;   (add-hook 'zephir-mode-hook
;;     '(lambda () (define-abbrev zephir-mode-abbrev-table "ex" "extends")))

;; Many options available under Help:Customize
;; Options specific to zephir-mode are in
;;  Programming/Languages/Php
;; Since it inherits much functionality from c-mode, look there too
;;  Programming/Languages/C

;;; Commentary:

;; Zephir Mode is a major mode for editing Zephir source code.  It's an
;; extension of C mode; thus it inherits all C mode's navigation
;; functionality.  But it colors according to the Zephir grammar.

;; Note: The interface used in this file requires CC Mode 5.30 or
;; later.

;;; Code:

(require 'cc-mode)

;; These are only required at compile time to get the sources for the
;; language constants.  (The cc-fonts require and the font-lock
;; related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

(eval-and-compile
  ;; Make our mode known to the language constant system.  Use Java
  ;; mode as the fallback for the constants we don't change here.
  ;; This needs to be done also at compile time since the language
  ;; constants are evaluated then.
  (c-add-language 'zephir-mode 'java-mode))

(require 'font-lock)
(require 'add-log)
(require 'custom)
(require 'speedbar)

(require 'cl-lib)

(eval-when-compile
  (require 'regexp-opt)
  (defvar syntax-propertize-via-font-lock))

;; Work around emacs bug#18845, cc-mode expects cl to be loaded
;; while zephir-mode only uses cl-lib (without compatibility aliases)
(eval-and-compile
  (if (and (= emacs-major-version 24) (>= emacs-minor-version 4))
      (require 'cl)))

;; In emacs 24.4 and 24.5, lines after functions with a return type
;; are incorrectly analyzed as member-init-cont.
;;                                        ;
;; Before emacs 24.4, c member initializers are not supported this
;; way. Starting from emacs 25.1, cc-mode only detects member
;; initializers when the major mode is c++-mode.
(eval-and-compile
  (if (and (= emacs-major-version 24) (or (= emacs-minor-version 4)
                                          (= emacs-minor-version 5)))
      (defun c-back-over-member-initializers ()
        ;; Override of cc-engine.el, cc-mode in emacs 24.4 and 24.5 are too
        ;; optimistic in recognizing c member initializers. Since we don't
        ;; need it in zephir-mode, just return nil.
        nil)))

;; Local variables
;;;###autoload
(defgroup zephir nil
  "Major mode for editing Zephir code."
  :tag "Zephir"
  :prefix "zephir-"
  :group 'languages
  :link '(url-link :tag "Official Site" "https://github.com/sergeyklay/zephir-mode")
  :link '(url-link :tag "Zephir Forum" "https://forum.zephir-lang.com")
  :link '(url-link :tag "Zephir Language" "https://zephir-lang.com"))

(defsubst zephir-in-string-p ()
  (nth 3 (syntax-ppss)))

(defsubst zephir-in-comment-p ()
  (nth 4 (syntax-ppss)))

(defsubst zephir-in-string-or-comment-p ()
  (nth 8 (syntax-ppss)))

(defcustom zephir-lineup-cascaded-calls nil
  "Indent chained method calls to the previous line"
  :type 'boolean)

(defun zephir-create-regexp-for-method (visibility)
  "Make a regular expression for methods with the given `visiblity'.

`visibility' must be a string that names the visibility for a Zephir
method, e.g. 'public'.  The parameter `visibility' can itself also
be a regular expression.

The regular expression this function returns will check for other
keywords that can appear in method signatures, e.g. 'final' and
'static'.  The regular expression will have one capture group
which will be the name of the method."
  (concat
   ;; Initial space with possible 'abstract' or 'final' keywords
   "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?"
   ;; 'static' keyword may come either before or after visibility
   "\\(?:" visibility "\\(?:\\s-+static\\)?\\|\\(?:static\\s-+\\)?" visibility "\\)\\s-+"
   ;; Make sure 'function' comes next with some space after
   "function\\s-+"
   ;; Capture the name as the first group and the regexp and make sure
   ;; by the end we see the opening parenthesis for the parameters.
   "\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*("))

(defun zephir-create-regexp-for-classlike (type)
  "Accepts a `type' of a 'classlike' object as a string, such as
'class' or 'interface', and returns a regexp as a string which
can be used to match against definitions for that classlike."
  (concat
   ;; First see if 'abstract' or 'final' appear, although really these
   ;; are not valid for all values of `type' that the function
   ;; accepts.
   "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?"
   ;; The classlike type
   type
   ;; Its name, which is the first captured group in the regexp.  We
   ;; allow backslashes in the name to handle namespaces, but again
   ;; this is not necessarily correct for all values of `type'.
   "\\s-+\\(\\(?:\\sw\\|\\\\\\|\\s_\\)+\\)"))

(defvar zephir-imenu-generic-expression
  `(("Namespaces"
     ,(zephir-create-regexp-for-classlike "namespace") 1)
    ("Classes"
     ,(zephir-create-regexp-for-classlike "class") 1)
    ("Interfaces"
     ,(zephir-create-regexp-for-classlike "interface") 1)
    ("All Methods"
     ,(zephir-create-regexp-for-method "\\(?:\\sw\\|\\s_\\)+") 1)
    ("Internal Methods"
     ,(zephir-create-regexp-for-method "internal") 1)
    ("Private Methods"
     ,(zephir-create-regexp-for-method "private") 1)
    ("Protected Methods"
     ,(zephir-create-regexp-for-method "protected")  1)
    ("Public Methods"
     ,(zephir-create-regexp-for-method "public") 1)
    ("Anonymous Functions"
     "\\<\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*=\\s-*function\\s-*(" 1)
    ("Named Functions"
     "^\\s-*function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1))
  "Imenu generic expression for Zephir Mode. See `imenu-generic-expression'.")

(defun zephir-mode-version ()
  "Display string describing the version of Zephir Mode."
  (interactive)
  (message "Zephir Mode %s of %s"
           zephir-mode-version-number zephir-mode-modified))

(defvar zephir-mode-map
  ;; Add bindings which are only useful for Zephir Mode
  (let ((map (make-sparse-keymap)))
    ;; By default Zephir Mode binds C-M-h to c-mark-function, which it
    ;; inherits from cc-mode.  But there are situations where
    ;; c-mark-function fails to properly mark a function.  For
    ;; example, if we use c-mark-function within a method definition
    ;; then the region will expand beyond the method and into the
    ;; class definition itself.
    ;;
    ;; Changing the default to mark-defun provides behavior that users
    ;; are more likely to expect.
    (define-key map (kbd "C-M-h") 'mark-defun)

    ;; Many packages based on cc-mode provide the 'C-c C-w' binding
    ;; to toggle Subword Mode.  See the page
    ;;
    ;;     https://www.gnu.org/software/emacs/manual/html_node/ccmode/Subword-Movement.html
    ;;
    ;; for more information about Submode Word.
    (if (boundp 'subword-mode)
        (if subword-mode
            (subword-mode nil)
          (subword-mode t)))

    ;; We inherit c-beginning-of-defun and c-end-of-defun from CC Mode
    ;; but we have two replacement functions specifically for Zephir. We
    ;; remap the commands themselves and not their default
    ;; key-bindings so that our zephir-specific versions will work even
    ;; if the user has reconfigured their keys, e.g. if they rebind
    ;; c-end-of-defun to something other than C-M-e.
    (define-key map [remap c-beginning-of-defun] 'zephir-beginning-of-defun)
    (define-key map [remap c-end-of-defun] 'zephir-end-of-defun)

    ;; Use the Emacs standard indentation binding. This may upset c-mode
    ;; which does not follow this at the moment, but I see no better
    ;; choice.
    (define-key map [tab] 'indent-for-tab-command)
    map)
    "Keymap used in `zephir-mode' buffers.")

(c-lang-defconst c-identifier-ops
  zephir '(
           (left-assoc "\\" "::" "->")
           (prefix "\\" "::")))

;; Allow '\' when scanning from open brace back to defining
;; construct like class
(c-lang-defconst c-block-prefix-disallowed-chars
  zephir (cl-set-difference (c-lang-const c-block-prefix-disallowed-chars)
                            '(?\\)))

;; Allow $ so variables are recognized in cc-mode and remove @. This
;; makes cc-mode highlight variables and their type hints in arglists.
(c-lang-defconst c-symbol-start
  zephir (concat "[" c-alpha "_$]"))

(c-lang-defconst c-assignment-operators
  ;; falls back to java, so no need to specify the language
  zephir (append (remove ">>>=" (c-lang-const c-assignment-operators))
                 '(".=")))

(c-lang-defconst beginning-of-defun-function
  zephir 'zephir-beginning-of-defun)

(c-lang-defconst end-of-defun-function
  zephir 'zephir-end-of-defun)

(c-lang-defconst c-primitive-type-kwds
  "Primitive type keywords.  As opposed to the other keyword lists, the
keywords listed here are fontified with the type face instead of the
keyword face.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled.

Do not try to modify this list for end user customizations; the
`*-font-lock-extra-types' variable, where `*' is the mode prefix, is
the appropriate place for that."
  zephir '("uint" "int" "bool" "boolean" "float" "double" "long"
           "ulong" "char" "string" "resource" "void" "null"))

(c-lang-defconst c-class-decl-kwds
  "Keywords introducing declarations where the following block (if any)
contains another declaration level that should be considered a class.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled.

Note that presence on this list does not automatically treat the
following identifier as a type; the keyword must also be present on
`c-type-prefix-kwds' or `c-type-list-kwds' to accomplish that."
  zephir '("class" "interface"))

(c-lang-defconst c-brace-list-decl-kwds
  "Keywords introducing declarations where the following block (if any)
is a brace list.

Zephir does not have an \"enum\"-like keyword."
  zephir nil)

(c-lang-defconst c-typeless-decl-kwds
  zephir (append (c-lang-const c-class-decl-kwds) '("function")))

(c-lang-defconst c-modifier-kwds
  zephir '("abstract" "const" "final" "static"))

(c-lang-defconst c-protection-kwds
  "Access protection label keywords in classes."
  zephir '("private" "protected" "public"))

(c-lang-defconst c-postfix-decl-spec-kwds
  zephir '("implements" "extends"))

(c-lang-defconst c-type-list-kwds
  zephir '("new" "use" "implements" "extends" "namespace" "instanceof" "insteadof"))

(c-lang-defconst c-ref-list-kwds
  zephir nil)

(c-lang-defconst c-block-stmt-2-kwds
  zephir (append '("elseif" "foreach" "declare")
                 (remove "synchronized" (c-lang-const c-block-stmt-2-kwds))))

(c-lang-defconst c-simple-stmt-kwds
  zephir (append '("echo" "print" "die" "exit")
                 (c-lang-const c-simple-stmt-kwds)))

(c-lang-defconst c-constant-kwds
  zephir '("true"
           "false"
           "null"))

(c-lang-defconst c-lambda-kwds
  zephir '("function"
           "use"))

(c-lang-defconst c-other-block-decl-kwds
  zephir '("namespace"))

(c-lang-defconst c-other-kwds
  "Keywords not accounted for by any other `*-kwds' language constant."
  zephir '(
           "and"
           "array"
           "callable"
           "iterable"
           "as"
           "break"
           "catch"
           "clone"
           "empty"
           "eval"
           "fetch"
           "isset"
           "list"
           "or"
           "parent"
           "static"
           "unset"
           "var"
           "xor"
           "yield"
           "yield from"

           ;;; self for static references:
           "self"
           ))

(c-lang-defconst c-enums-contain-decls
  zephir nil)

(c-lang-defconst c-nonlabel-token-key
  "Regexp matching things that can't occur in generic colon labels.

This overrides cc-mode `c-nonlabel-token-key' to support switching on
double quoted strings and true/false/null.

Note: this regexp is also applied to goto-labels, a future improvement
might be to handle switch and goto labels differently."
  zephir (concat
          ;; All keywords except `c-label-kwds' and `c-constant-kwds'.
          (c-make-keywords-re t
            (cl-set-difference (c-lang-const c-keywords)
                               (append (c-lang-const c-label-kwds)
                                       (c-lang-const c-constant-kwds))
                               :test 'string-equal))))

(defconst zephir-beginning-of-defun-regexp
  "^\\s-*\\(?:\\(?:abstract\\|final\\|internal\\|private\\|protected\\|public\\|static\\)\\s-+\\)*function\\s-+&?\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*("
  "Regular expression for a Zephir function.")

(defun zephir-beginning-of-defun (&optional arg)
  "Move to the beginning of the ARGth Zephir function from point.
Implements Zephir version of `beginning-of-defun-function'."
  (interactive "p")
  (let ((arg (or arg 1)))
    (while (> arg 0)
      (re-search-backward zephir-beginning-of-defun-regexp
                          nil 'noerror)
      (setq arg (1- arg)))
    (while (< arg 0)
      (end-of-line 1)
      (let ((opoint (point)))
        (beginning-of-defun 1)
        (forward-list 2)
        (forward-line 1)
        (if (eq opoint (point))
            (re-search-forward zephir-beginning-of-defun-regexp
                               nil 'noerror))
        (setq arg (1+ arg))))))

(defun zephir-end-of-defun (&optional arg)
  "Move the end of the ARGth Zephir function from point.
Implements Zephir version of `end-of-defun-function'

See `zephir-beginning-of-defun'."
  (interactive "p")
  (zephir-beginning-of-defun (- (or arg 1))))

(defcustom zephir-mode-warn-if-mumamo-off t
  "Warn once per buffer if you try to indent a buffer without
mumamo-mode turned on."
  :type '(choice (const :tag "Warg" t) (const "Don't warn" nil)))

(defvar zephir-warned-bad-indent nil)

(defun zephir-cautious-indent-region (start end &optional quiet)
  (if (or (not zephir-mode-warn-if-mumamo-off)
          zephir-warned-bad-indent)
      (funcall 'c-indent-region start end quiet)))

(defun zephir-cautious-indent-line ()
  (if (or (not zephir-mode-warn-if-mumamo-off)
          zephir-warned-bad-indent)
      (let ((here (point))
            doit)
        (move-beginning-of-line nil)
        ;; Don't indent heredoc end mark
        (save-match-data
          (unless (and (looking-at "[a-zA-Z0-9_]+;\n")
                       (zephir-in-string-p))
            (setq doit t)))
        (goto-char here)
        (when doit
          (funcall 'c-indent-line)))))

(defun zephir-lineup-cascaded-calls (langelem)
  "Line up chained methods using `c-lineup-cascaded-calls',
but only if the setting is enabled"
  (if zephir-lineup-cascaded-calls
      (c-lineup-cascaded-calls langelem)))

(defun zephir-lineup-string-cont (langelem)
  "Line up string toward equal sign or dot
e.g.
$str = 'some'
     . 'string';
this ^ lineup"
  (save-excursion
    (goto-char (cdr langelem))
    (let (ret finish)
      (while (and (not finish) (re-search-forward "[=.]" (line-end-position) t))
        (unless (zephir-in-string-or-comment-p)
          (setq finish t
                ret (vector (1- (current-column))))))
      ret)))

(defun zephir-lineup-arglist-intro (langelem)
  (save-excursion
    (goto-char (cdr langelem))
    (vector (+ (current-column) c-basic-offset))))

(defun zephir-lineup-arglist-close (langelem)
  (save-excursion
    (goto-char (cdr langelem))
    (vector (current-column))))

(defun zephir-lineup-arglist (_langelem)
  (save-excursion
    (beginning-of-line)
    (if (looking-at-p "\\s-*->") '+ 0)))

(defun zephir-lineup-hanging-semicolon (_langelem)
  (save-excursion
    (beginning-of-line)
    (if (looking-at-p "\\s-*;\\s-*$") 0 '+)))

(c-add-style
 "zephir"
 '((c-basic-offset . 4)
   (c-offsets-alist . ((arglist-close . zephir-lineup-arglist-close)
                       (arglist-cont . (first zephir-lineup-cascaded-calls 0))
                       (arglist-cont-nonempty . (first zephir-lineup-cascaded-calls c-lineup-arglist))
                       (arglist-intro . zephir-lineup-arglist-intro)
                       (case-label . +)
                       (class-open . 0)
                       (comment-intro . 0)
                       (inlambda . 0)
                       (inline-open . 0)
                       (namespace-open . 0)
                       (lambda-intro-cont . +)
                       (label . +)
                       (statement-cont . (first zephir-lineup-cascaded-calls zephir-lineup-string-cont +))
                       (substatement-open . 0)
                       (topmost-intro-cont . (first zephir-lineup-cascaded-calls +))))))

(defconst zephir-heredoc-start-re
  "<<<\\(?:\\w+\\|'\\w+'\\)$"
  "Regular expression for the start of a Zephir heredoc.")

(defun zephir-heredoc-end-re (heredoc-start)
  "Build a regular expression for the end of a heredoc started by
the string `heredoc-start'."
  ;; Extract just the identifier without <<< and quotes.
  (string-match "\\w+" heredoc-start)
  (concat "^\\(" (match-string 0 heredoc-start) "\\)\\W"))

(defun zephir-heredoc-syntax ()
  "Mark the boundaries of searched heredoc."
  (goto-char (match-beginning 0))
  (c-put-char-property (point) 'syntax-table (string-to-syntax "|"))
  (if (re-search-forward (zephir-heredoc-end-re (match-string 0)) nil t)
      (goto-char (match-end 1))
    ;; Did not find the delimiter so go to the end of the buffer.
    (goto-char (point-max)))
  (c-put-char-property (1- (point)) 'syntax-table (string-to-syntax "|")))

(defun zephir-syntax-propertize-extend-region (start end)
  "Extend the propertize region if `start' or `end' falls inside a Zephir heredoc."
  (let ((new-start)
        (new-end))
    (goto-char start)
    (when (re-search-backward zephir-heredoc-start-re nil t)
      (let ((maybe (point)))
        (when (and (re-search-forward
                    (zephir-heredoc-end-re (match-string 0)) nil t)
                   (> (point) start))
          (setq new-start maybe))))
    (goto-char end)
    (when (re-search-backward zephir-heredoc-start-re nil t)
      (if (re-search-forward
           (zephir-heredoc-end-re (match-string 0)) nil t)
          (when (> (point) end)
            (setq new-end (point)))
        (setq new-end (point-max))))
    (when (or new-start new-end)
      (cons (or new-start start) (or new-end end)))))

(defun zephir-syntax-propertize-function (start end)
  "Apply propertize rules from `start' to `end'."
  (goto-char start)
  (while (and (< (point) end)
              (re-search-forward zephir-heredoc-start-re end t))
    (zephir-heredoc-syntax))
  (goto-char start)
  (while (re-search-forward "['\"]" end t)
    (when (zephir-in-comment-p)
      (c-put-char-property (match-beginning 0)
                           'syntax-table (string-to-syntax "_")))))

;; Faces

;;;###autoload
(defgroup zephir-faces nil
  "Faces used in Zephir Mode"
  :tag "Zephir Faces"
  :group 'zephir
  :group 'faces)

(defface zephir-string '((t (:inherit font-lock-string-face)))
  "Zephir Mode face used to highlight string literals."
  :group 'zephir-faces)

(defface zephir-keyword '((t (:inherit font-lock-keyword-face)))
  "Zephir Mode face used to highlight keywords."
  :group 'zephir-faces)

(defface zephir-builtin '((t (:inherit font-lock-builtin-face)))
  "Zephir Mode face used to highlight builtins."
  :group 'zephir-faces)

(defface zephir-function-name '((t (:inherit font-lock-function-name-face)))
  "Zephir Mode face used to highlight function names."
  :group 'zephir-faces)

(defface zephir-function-call '((t (:inherit default)))
  "Zephir Mode face used to highlight function names in calles."
  :group 'zephir-faces)

(defface zephir-method-call '((t (:inherit zephir-function-call)))
  "Zephir Mode face used to highlight method names in calles."
  :group 'zephir-faces)

(defface zephir-static-method-call '((t (:inherit zephir-method-call)))
  "Zephir Mode face used to highlight static method names in calles."
  :group 'zephir-faces)

(defface zephir-variable-name '((t (:inherit font-lock-variable-name-face)))
  "Zephir Mode face used to highlight variable names."
  :group 'zephir-faces)

(defface zephir-property-name '((t (:inherit zephir-variable-name)))
  "Zephir Mode face used to highlight property names."
  :group 'zephir-faces)

(defface zephir-variable-sigil '((t (:inherit default)))
  "Zephir Mode face used to highlight variable sigils ($)."
  :group 'zephir-faces)

(defface zephir-object-op '((t (:inherit default)))
  "Zephir Mode face used to object operators (->)."
  :group 'zephir-faces)

(defface zephir-paamayim-nekudotayim '((t (:inherit default)))
  "Zephir Mode face used to highlight \"Paamayim Nekudotayim\" scope resolution operators (::)."
  :group 'zephir-faces)

(defface zephir-type '((t (:inherit font-lock-type-face)))
  "Zephir Mode face used to highlight types."
  :group 'zephir-faces)

(defface zephir-constant '((t (:inherit font-lock-constant-face)))
  "Zephir Mode face used to highlight constants."
  :group 'zephir-faces)

(defface zephir-$this '((t (:inherit zephir-constant)))
  "Zephir Mode face used to highlight $this variables."
  :group 'zephir-faces)

(defface zephir-$this-sigil '((t (:inherit zephir-constant)))
  "Zephir Mode face used to highlight sigils($) of $this variable."
  :group 'zephir-faces)

(defface zephir-doc-annotation-tag '((t . (:inherit font-lock-constant-face)))
  "Zephir Mode face used to highlight annotation tags in doc-comment."
  :group 'zephir-faces)

(defface zephir-doc-variable-sigil '((t (:inherit font-lock-variable-name-face)))
  "Zephir Mode face used to highlight variable sigils($)."
  :group 'zephir-faces)

(defface zephir-doc-$this '((t (:inherit zephir-type)))
  "Zephir Mode face used to highlight $this variable in doc-comment."
  :group 'zephir-faces)

(defface zephir-doc-$this-sigil '((t (:inherit zephir-type)))
  "ZEPHIR Mode face used to highlight sigil of $this variable in doc-comment."
  :group 'zephir-faces)

(defface zephir-doc-class-name '((t (:inherit zephir-string)))
  "Face used to class names in doc-comment."
  :group 'zephir-faces)

;;;###autoload
(define-derived-mode zephir-mode c-mode "Zephir"
  "A major mode for editing Zephir code.

\\{zephir-mode-map}
"

  (c-initialize-cc-mode t)
  (c-init-language-vars zephir-mode)
  (c-common-init 'zephir-mode)

  (set (make-local-variable font-lock-string-face) 'zephir-string)
  (set (make-local-variable font-lock-keyword-face) 'zephir-keyword)
  (set (make-local-variable font-lock-builtin-face) 'zephir-builtin)
  (set (make-local-variable font-lock-function-name-face) 'zephir-function-name)
  (set (make-local-variable font-lock-variable-name-face) 'zephir-variable-name)
  (set (make-local-variable font-lock-constant-face) 'zephir-constant)

  ;; Modifying the Emacs Syntax Table.  See the page
  ;;     https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Class-Table.html
  (modify-syntax-entry ?_    "_" zephir-mode-syntax-table)
  (modify-syntax-entry ?`    "\"" zephir-mode-syntax-table)
  (modify-syntax-entry ?\"   "\"" zephir-mode-syntax-table)
  (modify-syntax-entry ?\n   "> b" zephir-mode-syntax-table)
  (modify-syntax-entry ?$    "'" zephir-mode-syntax-table)

  (set (make-local-variable 'syntax-propertize-via-font-lock)
       '(("\\(\"\\)\\(\\\\.\\|[^\"\n\\]\\)*\\(\"\\)" (1 "\"") (3 "\""))
         ("\\(\'\\)\\(\\\\.\\|[^\'\n\\]\\)*\\(\'\\)" (1 "\"") (3 "\""))))

  (add-to-list (make-local-variable 'syntax-propertize-extend-region-functions)
               #'zephir-syntax-propertize-extend-region)
  (set (make-local-variable 'syntax-propertize-function)
       #'zephir-syntax-propertize-function)

  (setq imenu-generic-expression zephir-imenu-generic-expression)

  ;; Zephir vars are case-sensitive
  (setq case-fold-search t)

  (setq indent-line-function 'zephir-cautious-indent-line)
  (setq indent-region-function 'zephir-cautious-indent-region)

  ;; syntax-begin-function is obsolete in Emacs 25.1
  (with-no-warnings
    (set (make-local-variable 'syntax-begin-function)
         'c-beginning-of-syntax))

  ;; We map the zephir-{beginning,end}-of-defun functions so that they
  ;; replace the similar commands that we inherit from CC Mode.
  ;; Because of our remapping we may not actually need to keep the
  ;; following two local variables, but we keep them for now until we
  ;; are completely sure their removal will not break any current
  ;; behavior or backwards compatibility.
  (set (make-local-variable 'beginning-of-defun-function)
       'zephir-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       'zephir-end-of-defun)

  (set (make-local-variable 'open-paren-in-column-0-is-defun-start)
       nil)
  (set (make-local-variable 'defun-prompt-regexp)
       "^\\s-*function\\s-+&?\\s-*\\(\\(\\sw\\|\\s_\\)+\\)\\s-*")
  (set (make-local-variable 'add-log-current-defun-header-regexp)
       zephir-beginning-of-defun-regexp)

  (when (>= emacs-major-version 25)
    (with-silent-modifications
      (save-excursion
        (zephir-syntax-propertize-function (point-min) (point-max))))))

;; Font Lock
(defconst zephir-phpdoc-type-keywords
  (list "string" "integer" "int" "boolean" "bool" "float"
        "double" "object" "mixed" "array" "resource"
        "void" "null" "false" "true" "self" "static"
        "callable" "iterable" "number"))

(defconst zephir-phpdoc-type-tags
  (list "param" "property" "property-read" "property-write" "return" "var"))

(defconst zephir-phpdoc-font-lock-doc-comments
  `(("{@[-[:alpha:]]+\\s-\\([^}]*\\)}" ; "{@foo ...}" markup.
     (0 'zephir-doc-annotation-tag prepend nil)
     (1 'zephir-string prepend nil))
    (,(rx (group "$") (group (in "A-Za-z_") (* (in "0-9A-Za-z_"))))
     (1 'zephir-doc-variable-sigil prepend nil)
     (2 'zephir-variable-name prepend nil))
    ("\\(\\$\\)\\(this\\)\\>" (1 'zephir-doc-$this-sigil prepend nil) (2 'zephir-doc-$this prepend nil))
    (,(concat "\\s-@" (regexp-opt zephir-phpdoc-type-tags) "\\s-+"
              "\\(" (rx (+ (? "?") (? "\\") (+ (in "0-9A-Z_a-z")) (? "[]") (? "|"))) "\\)+")
     1 'zephir-string prepend nil)
    (,(concat "\\(?:|\\|\\?\\|\\s-\\)\\("
              (regexp-opt zephir-phpdoc-type-keywords 'words)
              "\\)")
     1 font-lock-type-face prepend nil)
    ("https?://[^\n\t ]+"
     0 'link prepend nil)
    ("^\\(?:/\\*\\)?\\(?:\\s \\|\\*\\)*\\(@[[:alpha:]][-[:alpha:]\\]*\\)" ; "@foo ..." markup.
     1 'zephir-doc-annotation-tag prepend nil)))

(defvar zephir-phpdoc-font-lock-keywords
  `((,(lambda (limit)
        (c-font-lock-doc-comments "/\\*\\*" limit
          zephir-phpdoc-font-lock-doc-comments)))))

(defconst zephir-font-lock-keywords-1 (c-lang-const c-matchers-1 zephir)
  "Minimal highlighting for Zephir Mode.")

(defconst zephir-font-lock-keywords-2 (c-lang-const c-matchers-2 zephir)
  "Fast normal highlighting for Zephir Mode.")

(defconst zephir-font-lock-keywords-3
  (append
   zephir-phpdoc-font-lock-keywords
   ;; zephir-mode patterns *before* cc-mode:
   ;;  only add patterns here if you want to prevent cc-mode from applying
   ;;  a different face.
   '(
     ;; Highlight variables, e.g. 'var' in '$var' and '$obj->var', but
     ;; not in $obj->var()
     ("\\(->\\)\\(\\sw+\\)\\s-*(" (1 'zephir-object-op) (2 'zephir-method-call))

     ;; Highlight special variables
     ("\\(\\$\\)\\(this\\|that\\)\\_>" (1 'zephir-$this-sigil) (2 'zephir-$this))
     ("\\(\\$\\)\\([a-zA-Z0-9_]+\\)" (1 'zephir-variable-sigil) (2 'zephir-variable-name))
     ("\\(->\\)\\([a-zA-Z0-9_]+\\)" (1 'zephir-object-op) (2 'zephir-property-name))

     ;; Highlight function/method names
     ("\\<function\\s-+&?\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1 'zephir-function-name)

     ;; The dollar sign should not get a variable-name face, below
     ;; pattern resets the face to default in case cc-mode sets the
     ;; variable-name face (cc-mode does this for variables prefixed
     ;; with type, like in arglist)
     ("\\(\\$\\)\\(\\sw+\\)" 1 'zephir-variable-sigil)

     ;; Support the ::class constant in PHP5.6
     ("\\sw+\\(::\\)\\(class\\)\\b" (1 'zephir-paamayim-nekudotayim) (2 'zephir-constant)))

   ;; cc-mode patterns
   (c-lang-const c-matchers-3 zephir)

   ;; zephir-mode patterns *after* cc-mode:
   ;;   most patterns should go here, faces will only be applied if not
   ;;   already fontified by another pattern. Note that using OVERRIDE
   ;;   is usually overkill.
   `(
     ;; Highlight variables, e.g. 'var' in '$var' and '$obj->var', but
     ;; not in $obj->var()
     ("->\\(\\sw+\\)\\s-*(" 1 'zephir-method-call)

     ("\\(\\$\\|->\\)\\([a-zA-Z0-9_]+\\)" 2 'zephir-property-name)

     ;; Highlight all upper-cased symbols as constant
     ("\\<\\([A-Z_][A-Z0-9_]+\\)\\>" 1 'zephir-constant)

     ;; Highlight all statically accessed class names as constant,
     ;; another valid option would be using type-face, but using
     ;; constant-face because this is how it works in c++-mode.
     ("\\(\\sw+\\)\\(::\\)" (1 'zephir-constant) (2 'zephir-paamayim-nekudotayim))

     ;; Highlight class name after "use .. as"
     ("\\<as\\s-+\\(\\sw+\\)" 1 font-lock-type-face)

     ;; Highlight return types in functions and methods.
     ("function.+:\\s-*\\??\\(\\(?:\\sw\\|\\s_\\)+\\)" 1 font-lock-type-face)
     (")\\s-*:\\s-*\\??\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*\{" 1 font-lock-type-face)

     ;; Highlight class names used as nullable type hints
     ("\\?\\(\\(:?\\sw\\|\\s_\\)+\\)\\s-+\\$" 1 font-lock-type-face)))
  "Accurate normal highlighting for Zephir Mode.")

(defvar zephir-font-lock-keywords zephir-font-lock-keywords-3
  "Default expressions to highlight in Zephir Mode.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.zep\\'" . zephir-mode))

(provide 'zephir-mode)

;;; zephir-mode.el ends here

;; Local Variables:
;; firestarter: ert-run-tests-interactively
;; End:
