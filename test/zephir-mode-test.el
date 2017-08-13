;;; zephir-mode-test.el --- Zephir Mode: Unit test suite -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Serghei Iakovlev

;; Author: Serghei Iakovlev (serghei@phalconphp.com)
;; Maintainer: Serghei Iakovlev
;; Version: 0.3.4
;; URL: https://github.com/sergeyklay/zephir-mode

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

;;   Automate tests from the "test" directory using `ert', which comes bundled
;; with Emacs >= 24.1.

;;; Code:

(require 'zephir-mode)
(require 'ert)


;;;; Utilities

(defmacro zephir-test-with-temp-buffer (content &rest body)
  "Evaluate BODY in a temporary buffer with CONTENT."
  (declare (debug t)
           (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (zephir-mode)
     (font-lock-fontify-buffer)
     (goto-char (point-min))
     ,@body))

(defconst zephir-test-syntax-classes
  [whitespace punctuation word symbol open-paren close-paren expression-prefix
              string-quote paired-delim escape character-quote comment-start
              comment-end inherit generic-comment generic-string]
  "Readable symbols for syntax classes.

Each symbol in this vector corresponding to the syntax code of
its index.")

(defun zephir-test-face-at (pos &optional content)
  "Get the face at POS in CONTENT.

If CONTENT is not given, return the face at POS in the current buffer."
  (if content
      (zephir-test-with-temp-buffer content
                                    (get-text-property pos 'face))
    (get-text-property pos 'face)))

(defun zephir-test-syntax-at (pos)
  "Get the syntax at POS.

Get the syntax class symbol at POS, or nil if there is no syntax a
POS"
  (let ((code (syntax-class (syntax-after pos))))
    (aref puppet-test-syntax-classes code)))


;;;; Font locking

(ert-deftest zephir-mode-syntax-table/fontify-dq-string ()
  :tags '(fontification syntax-table)
  (should (eq (zephir-test-face-at 7 "foo = \"bar\"") 'font-lock-string-face)))

(ert-deftest zephir-mode-syntax-table/fontify-sq-string ()
  :tags '(fontification syntax-table)
  (should (eq (zephir-test-face-at 7 "foo = 'bar'") 'font-lock-string-face)))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; zephir-mode-test.el ends here
