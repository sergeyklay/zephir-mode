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

;;;###autoload
(define-derived-mode zephir-mode c-mode "Zephir"
  "A major mode for editing Zephir code.

\\{zephir-mode-map}
"

  (c-initialize-cc-mode t)
  (c-init-language-vars zephir-mode)
  (c-common-init 'zephir-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.zep\\'" . zephir-mode))

(provide 'zephir-mode)

;;; zephir-mode.el ends here
