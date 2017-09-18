;;; test-helper.el --- Zephir Mode: Non-interactive unit-test setup -*- lexical-binding: t; -*-

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

;; Non-interactive test suite setup for ERT Runner.

;;; Code:

(require 'ert-x)          ; `ert-with-test-buffer'
(require 'cl-lib)         ; `cl-defmacro'

;; Make sure the exact Emacs version can be found in the build output
(message "Running tests on Emacs %s" emacs-version)

;; The test fixtures assume an indentation width of 4, so we need to set that
;; up for the tests.
(setq-default default-tab-width 4
              indent-tabs-mode nil)

(when (require 'undercover nil t)
  (undercover "zephir-mode.el"))

(let* ((current-file (if load-in-progress load-file-name (buffer-file-name)))
       (source-directory (locate-dominating-file current-file "Cask"))
       ;; Don't load old byte-compiled versions
       (load-prefer-newer t))
  ;; Load the file under test
  (load (expand-file-name "zephir-mode" source-directory)))

;; Helpers

(cl-defmacro zephir-deftest (name args &body body)
  (declare (indent 2))
  `(ert-deftest ,(intern (format "zephir-ert-%s" name)) ()
     ""
     ,@args))

(cl-defmacro zephir-ert-with-test-buffer ((&rest args) initial-contents &body body)
  (declare (indent 2))
  `(ert-with-test-buffer (,@args)
     (zephir-mode)
     (insert ,initial-contents)
     ,@body))

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

(cl-defmacro zephir-def-indentation-test (name args initial-contents expected-output)
  (declare (indent 2))
  `(zephir-deftest ,name ,args
                   (zephir-ert-with-test-buffer (:name ,(format "(Expected)" name))
                                                ,initial-contents
                                                (let ((indented (ert-buffer-string-reindented)))
                                                  (delete-region (point-min) (point-max))
                                                  (insert ,expected-output)
                                                  (ert-with-test-buffer (:name ,(format "(Actual)" name))
                                                    (zephir-mode)
                                                    (insert indented)
                                                    (should (equal indented ,expected-output)))))))

(when (s-contains? "--win" (getenv "ERT_RUNNER_ARGS"))
  (defun ert-runner/run-tests-batch-and-exit (selector)
    (ert-run-tests-interactively selector)))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; test-helper.el ends here
