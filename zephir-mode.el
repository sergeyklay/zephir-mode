;;; zephir-mode.el --- Major mode for editing Zephir code

;; Copyright (C) 2017 Serghei Iakovlev

;; Author:     Serghei Iakovlev (serghei@phalconphp.com)
;; Maintainer: Serghei Iakovlev
;; Version:    0.0.1
;; URL:        https://github.com/sergeyklay/zephir-mode
;; Keywords:   zephir languages oop php

(defconst zephir-mode-version-number "0.0.1"
  "Zephir Mode version number.")

(defconst zephir-mode-modified "2017-07-26"
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

;;; Code:

(require 'cc-mode)
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

(eval-and-compile
  (c-add-language 'zephir-mode 'java-mode))

(defgroup zephir nil
  "Major mode for editing Zephir code."
  :tag "Zephir"
  :prefix "zephir-"
  :group 'languages
  :link '(url-link :tag "Official Site" "https://github.com/sergeyklay/zephir-mode")
  :link '(url-link :tag "Language Site" "https://zephir-lang.com"))

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

(defface zephir-zephir-tag '((t (:inherit font-lock-constant-face)))
  "Zephir Mode face used to highlight ZEPHIR tags."
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
  (set (make-local-variable font-lock-constant-face) 'zephir-constant))

;; Font Lock

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.zep\\'" . zephir-mode))

(provide 'zephir-mode)

;;; zephir-mode.el ends here

;; Local Variables:
;; firestarter: ert-run-tests-interactively
;; End:
