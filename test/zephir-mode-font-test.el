;;; zephir-mode-font-test.el --- Zephir Mode: Font highlighting test suite -*- lexical-binding: t; -*-

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

(defun zephir-test-face-at (pos &optional content)
  "Get the face at POS in CONTENT.

If CONTENT is not given, return the face at POS in the current
buffer."
  (if content
      (zephir-test-with-temp-buffer content
                                    (get-text-property pos 'face))
    (get-text-property pos 'face)))


;;;; Font locking

(ert-deftest zephir-mode-syntax-table/fontify-dq-string ()
  :tags '(fontification syntax-table)
  (should (eq (zephir-test-face-at 7 "foo = \"bar\"") 'font-lock-string-face)))

(ert-deftest zephir-mode-syntax-table/fontify-sq-string ()
  :tags '(fontification syntax-table)
  (should (eq (zephir-test-face-at 7 "foo = 'bar'") 'font-lock-string-face)))

(ert-deftest zephir-mode-syntax-table/fontify-line-comment ()
  :tags '(fontification syntax-table)
  (zephir-test-with-temp-buffer "// class

public function foo () {}"
                                (should (eq (zephir-test-face-at 3) 'font-lock-comment-face))
                                (should (eq (zephir-test-face-at 7) 'font-lock-comment-face))
                                (should (eq (zephir-test-face-at 8) 'font-lock-comment-face))
                                (should-not (zephir-test-face-at 10))))

(ert-deftest zephir-mode-syntax-table/fontify-c-style-comment ()
  :tags '(fontification syntax-table)
  (zephir-test-with-temp-buffer "/*
class */  public function foo () {}"
                                (should (eq (zephir-test-face-at 1) 'font-lock-comment-face))
                                (should (eq (zephir-test-face-at 4) 'font-lock-comment-face))
                                (should (eq (zephir-test-face-at 8) 'font-lock-comment-face))
                                (should (eq (zephir-test-face-at 11) 'font-lock-comment-face))
                                (should-not (zephir-test-face-at 13))))

(ert-deftest zephir-mode-syntax-table/fontify-builtin-constants ()
  :tags '(fontification syntax-table)
  (zephir-test-with-temp-buffer
   "protected foo = null;
public bar NULL;
let baz = false;"
   (should (eq (zephir-test-face-at 17) 'font-lock-constant-face))
   (should (eq (zephir-test-face-at 34) 'font-lock-constant-face))
   (should (eq (zephir-test-face-at 50) 'font-lock-constant-face))
   (should-not (zephir-test-face-at 55))))

(ert-deftest zephir-mode-syntax-table/fontify-primitives ()
  :tags '(fontification syntax-table)
  (zephir-test-with-temp-buffer
   "function a(string b) {}
function b (string! a) -> int {}
function c (int a, var b) {}"
   (should (eq (zephir-test-face-at 12) 'font-lock-type-face))
   (should (eq (zephir-test-face-at 37) 'font-lock-type-face))
   (should-not (zephir-test-face-at 43))
   (should (eq (zephir-test-face-at 51) 'font-lock-type-face))
   (should (eq (zephir-test-face-at 70) 'font-lock-type-face))
   (should (eq (zephir-test-face-at 77) 'font-lock-type-face))
   (should-not (zephir-test-face-at 80))))

(ert-deftest zephir-mode-syntax-table/fontify-function-name/1 ()
  :tags '(fontification syntax-table)
  (zephir-test-with-temp-buffer "function foo()"
                                (should-not (zephir-test-face-at 9))
                                (should (eq (zephir-test-face-at 10) 'font-lock-function-name-face))
                                (should (eq (zephir-test-face-at 12) 'font-lock-function-name-face))))

(ert-deftest zephir-mode-syntax-table/fontify-visibility ()
  :tags '(fontification syntax-table)
  (zephir-test-with-temp-buffer
   "
public foo = 45;
protected static function abc ();
abstract public function xyz ();
internal function abc();
scoped function aaa();
inline class Re {}"
   (should-not (zephir-test-face-at 1))
   (should (eq (zephir-test-face-at 2) 'font-lock-keyword-face))
   (should-not (zephir-test-face-at 8))
   (should (eq (zephir-test-face-at 19) 'font-lock-keyword-face))
   (should-not (zephir-test-face-at 28))
   (should (eq (zephir-test-face-at 62) 'font-lock-keyword-face))
   (should (eq (zephir-test-face-at 86) 'font-lock-keyword-face))
   (should-not (zephir-test-face-at 94))
   (should (eq (zephir-test-face-at 111) 'font-lock-keyword-face))
   (should-not (zephir-test-face-at 117))
   (should (eq (zephir-test-face-at 134) 'font-lock-keyword-face))
   (should-not (zephir-test-face-at 140))))

(ert-deftest zephir-mode-syntax-table/fontify-this-call ()
  :tags '(fontification syntax-table)
  (zephir-test-with-temp-buffer
   "this; this->var; this->_method(); this->method();"
   (should (eq (zephir-test-face-at 1) 'font-lock-constant-face))
   (should-not (zephir-test-face-at 5))
   (should (eq (zephir-test-face-at 7) 'font-lock-constant-face))
   (should-not (zephir-test-face-at 11))
   (should (eq (zephir-test-face-at 18) 'font-lock-constant-face))
   (should-not (zephir-test-face-at 22))
   (should (eq (zephir-test-face-at 35) 'font-lock-constant-face))
   (should-not (zephir-test-face-at 39))))

(ert-deftest zephir-mode-syntax-table/fontify-constants ()
  :tags '(fontification syntax-table)
  (zephir-test-with-temp-buffer
   "const FOO = 4;
__FILE__
x__FOO__x
xFOOx
self::FOO
self::__CONST__
qwerty"
   (should-not (zephir-test-face-at 6))
   (should (eq (zephir-test-face-at 7) 'font-lock-constant-face))
   (should-not (zephir-test-face-at 10))
   (should (eq (zephir-test-face-at 16) 'font-lock-constant-face))
   (should-not (zephir-test-face-at 25))
   (should-not (zephir-test-face-at 28))
   (should-not (zephir-test-face-at 32))
   (should-not (zephir-test-face-at 35))
   (should (eq (zephir-test-face-at 47) 'font-lock-constant-face))
   (should (eq (zephir-test-face-at 57) 'font-lock-constant-face))
   (should (eq (zephir-test-face-at 65) 'font-lock-constant-face))
   (should-not (zephir-test-face-at 67))))

(ert-deftest zephir-mode-syntax-table/fontify-function-decl/1 ()
  :tags '(fontification syntax-table)
  (zephir-test-with-temp-buffer "internal function test() -> string"
                                (should (eq (zephir-test-face-at 1) 'font-lock-keyword-face))
                                (should (eq (zephir-test-face-at 8) 'font-lock-keyword-face))
                                (should-not (zephir-test-face-at 9))
                                (should (eq (zephir-test-face-at 10) 'font-lock-keyword-face))
                                (should (eq (zephir-test-face-at 17) 'font-lock-keyword-face))
                                (should-not (zephir-test-face-at 18))
                                (should (eq (zephir-test-face-at 19) 'font-lock-function-name-face))))

(ert-deftest zephir-mode-syntax-table/fontify-function-decl/2 ()
  :tags '(fontification syntax-table)
  (zephir-test-with-temp-buffer "scoped function $test() {};"
                                (should (eq (zephir-test-face-at 1) 'font-lock-keyword-face))
                                (should (eq (zephir-test-face-at 6) 'font-lock-keyword-face))
                                (should-not (zephir-test-face-at 7))
                                (should (eq (zephir-test-face-at 8) 'font-lock-keyword-face))
                                (should (eq (zephir-test-face-at 15) 'font-lock-keyword-face))
                                (should-not (zephir-test-face-at 16))
                                (should (eq (zephir-test-face-at 17) 'font-lock-function-name-face))))

(ert-deftest zephir-mode-syntax-table/fontify-function-decl/3 ()
  :tags '(fontification syntax-table)
  (zephir-test-with-temp-buffer "inline function $_() {};"
                                (should (eq (zephir-test-face-at 1) 'font-lock-keyword-face))
                                (should (eq (zephir-test-face-at 6) 'font-lock-keyword-face))
                                (should-not (zephir-test-face-at 7))
                                (should (eq (zephir-test-face-at 8) 'font-lock-keyword-face))
                                (should (eq (zephir-test-face-at 15) 'font-lock-keyword-face))
                                (should-not (zephir-test-face-at 16))
                                (should (eq (zephir-test-face-at 17) 'font-lock-function-name-face))))

(ert-deftest zephir-mode-syntax-table/fontify-function-decl/4 ()
  :tags '(fontification syntax-table)
  (zephir-test-with-temp-buffer "private function __() {};"
                                (should (eq (zephir-test-face-at 1) 'font-lock-keyword-face))
                                (should (eq (zephir-test-face-at 7) 'font-lock-keyword-face))
                                (should-not (zephir-test-face-at 8))
                                (should (eq (zephir-test-face-at 9) 'font-lock-keyword-face))
                                (should (eq (zephir-test-face-at 16) 'font-lock-keyword-face))
                                (should-not (zephir-test-face-at 17))
                                (should (eq (zephir-test-face-at 18) 'font-lock-function-name-face))))

(ert-deftest zephir-mode-syntax-table/fontify-function-decl/5 ()
  :tags '(fontification syntax-table)
  (zephir-test-with-temp-buffer "protected function $__() {};"
                                (should (eq (zephir-test-face-at 1) 'font-lock-keyword-face))
                                (should (eq (zephir-test-face-at 9) 'font-lock-keyword-face))
                                (should-not (zephir-test-face-at 10))
                                (should (eq (zephir-test-face-at 11) 'font-lock-keyword-face))
                                (should (eq (zephir-test-face-at 18) 'font-lock-keyword-face))
                                (should-not (zephir-test-face-at 19))
                                (should (eq (zephir-test-face-at 20) 'font-lock-function-name-face))))

(ert-deftest zephir-mode-syntax-table/fontify-import/1 ()
  :tags '(fontification syntax-table)
  (zephir-test-with-temp-buffer
   "use Super;"
   (should (eq (zephir-test-face-at 1) 'font-lock-keyword-face))
   (should (eq (zephir-test-face-at 3) 'font-lock-keyword-face))
   (should-not (zephir-test-face-at 4))
   (should (eq (zephir-test-face-at 5) 'font-lock-type-face))
   (should (eq (zephir-test-face-at 9) 'font-lock-type-face))
   (should-not (zephir-test-face-at 10))))

(ert-deftest zephir-mode-syntax-table/fontify-import/2 ()
  :tags '(fontification syntax-table)
  (zephir-test-with-temp-buffer
   "use $uper;"
   (should (eq (zephir-test-face-at 1) 'font-lock-keyword-face))
   (should (eq (zephir-test-face-at 3) 'font-lock-keyword-face))
   (should-not (zephir-test-face-at 4))
   (should (eq (zephir-test-face-at 5) 'font-lock-type-face))
   (should (eq (zephir-test-face-at 9) 'font-lock-type-face))
   (should-not (zephir-test-face-at 10))))

(ert-deftest zephir-mode-syntax-table/fontify-import/3 ()
  :tags '(fontification syntax-table)
  (zephir-test-with-temp-buffer
   "use \\Supper\\Base;"
   (should (eq (zephir-test-face-at 1) 'font-lock-keyword-face))
   (should (eq (zephir-test-face-at 3) 'font-lock-keyword-face))
   (should-not (zephir-test-face-at 4))
   (should (eq (zephir-test-face-at 5) 'font-lock-type-face))
   (should (eq (zephir-test-face-at 6) 'font-lock-type-face))
   (should (eq (zephir-test-face-at 12) 'font-lock-type-face))
   (should-not (zephir-test-face-at 17))))

(ert-deftest zephir-mode-syntax-table/fontify-import/4 ()
  :tags '(fontification syntax-table)
  (zephir-test-with-temp-buffer
   "use Supper\\Base as Super;"
   (should (eq (zephir-test-face-at 1) 'font-lock-keyword-face))
   (should (eq (zephir-test-face-at 3) 'font-lock-keyword-face))
   (should-not (zephir-test-face-at 4))
   (should (eq (zephir-test-face-at 5) 'font-lock-type-face))
   (should (eq (zephir-test-face-at 6) 'font-lock-type-face))
   (should (eq (zephir-test-face-at 12) 'font-lock-type-face))
   (should-not (zephir-test-face-at 16))
   (should (eq (zephir-test-face-at 21) 'font-lock-type-face))
   (should-not (zephir-test-face-at 26))))

(ert-deftest zephir-mode-syntax-table/fontify-extends/1 ()
  :tags '(fontification syntax-table)
  (zephir-test-with-temp-buffer
   "class A extends B {};"
   (should (eq (zephir-test-face-at 1) 'font-lock-keyword-face))
   (should-not (zephir-test-face-at 6))
   (should (eq (zephir-test-face-at 7) 'font-lock-type-face))
   (should-not (zephir-test-face-at 8))
   (should (eq (zephir-test-face-at 9) 'font-lock-keyword-face))
   (should-not (zephir-test-face-at 16))
   (should (eq (zephir-test-face-at 17) 'font-lock-type-face))
   (should-not (zephir-test-face-at 18))))

(ert-deftest zephir-mode-syntax-table/fontify-extends/2 ()
  :tags '(fontification syntax-table)
  (zephir-test-with-temp-buffer
   "class $Abc\\Cde extends Super implements Aaa {};"
   (should (eq (zephir-test-face-at 1) 'font-lock-keyword-face))
   (should-not (zephir-test-face-at 6))
   (should (eq (zephir-test-face-at 7) 'font-lock-type-face))
   (should-not (zephir-test-face-at 15))
   (should (eq (zephir-test-face-at 16) 'font-lock-keyword-face))
   (should-not (zephir-test-face-at 23))))

(ert-deftest zephir-mode-syntax-table/fontify-namespaces-and-classes ()
  :tags '(fontification syntax-table)
  (zephir-test-with-temp-buffer
   "namespace Foo;
interface Bar {}
abstract class Baz extends Buz implements A, B, C {}"
   (should (eq (zephir-test-face-at 1) 'font-lock-keyword-face))
   (should-not (zephir-test-face-at 10))
   (should (eq (zephir-test-face-at 11) 'font-lock-type-face))
   (should (eq (zephir-test-face-at 16) 'font-lock-keyword-face))
   (should-not (zephir-test-face-at 25))
   (should (eq (zephir-test-face-at 33) 'font-lock-keyword-face))
   (should-not (zephir-test-face-at 41))
   (should (eq (zephir-test-face-at 42) 'font-lock-keyword-face))
   (should-not (zephir-test-face-at 47))))

(provide 'zephir-mode-font-test)

;;; zephir-mode-font-test.el ends here
