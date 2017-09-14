;;; zephir-mode-moving-test.el --- Zephir Mode: Tests for moving cursor functions -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Serghei Iakovlev

;; Author: Serghei Iakovlev (serghei@phalconphp.com)
;; Maintainer: Serghei Iakovlev
;; Version: 0.4.0
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


;;;; Utilities

(defconst zephir-test-syntax-classes
  [whitespace punctuation word symbol open-paren close-paren expression-prefix
              string-quote paired-delim escape character-quote comment-start
              comment-end inherit generic-comment generic-string]
  "Readable symbols for syntax classes.

Each symbol in this vector corresponding to the syntax code of
its index.")

(defun zephir-test-syntax-at (pos)
  "Get the syntax at POS.

Get the syntax class symbol at POS, or nil if there is no syntax a
POS"
  (let ((code (syntax-class (syntax-after pos))))
    (aref zephir-test-syntax-classes code)))


;;;; Moving

(ert-deftest zephir-mode/beginning-of-defun/1 ()
  :tags '(moving)
  (zephir-test-with-temp-buffer
   "public function foo () {
    // body
}"
   (search-forward "body")
   (call-interactively 'beginning-of-defun)
   (should (= (point) (point-min)))))

(ert-deftest zephir-mode/beginning-of-defun/2 ()
  :tags '(moving)
  (zephir-test-with-temp-buffer
   "deprectaed internal static function $fetch() {
    // body
}"
   (search-forward "body")
   (call-interactively 'beginning-of-defun)
   (should (= (point) (point-min)))))

(ert-deftest zephir-mode/beginning-of-defun/3 ()
  :tags '(moving)
  (zephir-test-with-temp-buffer
   "public FuncTion CamelCased () {
    // body
}
"
   (search-forward "body")
   (call-interactively 'beginning-of-defun)
   (should (= (point) (point-min)))))

(ert-deftest zephir-mode/end-of-defun/1 ()
  :tags '(moving)
  (zephir-test-with-temp-buffer
   "public function foo () {
    // body
}
"
   (search-forward "body")
   (call-interactively 'end-of-defun)
   (should (= (point) (point-max)))))

(ert-deftest zephir-mode/end-of-defun/2 ()
  :tags '(moving)
  (zephir-test-with-temp-buffer
   "deprectaed internal static function $fetch () {
    // body
}
"
   (search-forward "body")
   (call-interactively 'end-of-defun)
   (should (= (point) (point-max)))))

(ert-deftest zephir-mode/end-of-defun/3 ()
  :tags '(moving)
  (zephir-test-with-temp-buffer
   "public FuncTion CamelCased ()
{
    // body
}
"
   (search-forward "body")
   (call-interactively 'end-of-defun)
   (should (= (point) (point-max)))))


;;;; Major mode definition

(ert-deftest zephir-mode/movement-setup ()
  :tags '(major-mode)
  (zephir-test-with-temp-buffer
   "public function foo"
   (should (local-variable-p 'beginning-of-defun-function))
   (should (local-variable-p 'end-of-defun-function))
   (should (equal beginning-of-defun-function #'zephir-beginning-of-defun))
   (should (equal end-of-defun-function #'zephir-end-of-defun))))

(provide 'zephir-mode-moving-test)

;;; zephir-mode-helper-test.el ends here
