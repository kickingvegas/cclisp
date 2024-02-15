;;; test-cc-digital-logic.el --- Tests for cc-digital-logic  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'cc-digital-logic)

(ert-deftest test-digital-and ()
  (let ()
    (should (= 0 (digital-and 0 0 0)))
    (should (= 0 (digital-and 0 0 1)))
    (should (= 0 (digital-and 0 1 0)))
    (should (= 0 (digital-and 0 1 1)))
    (should (= 0 (digital-and 1 0 0)))
    (should (= 0 (digital-and 1 0 0)))
    (should (= 0 (digital-and 1 0 1)))
    (should (= 0 (digital-and 1 1 0)))
    (should (= 1 (digital-and 1 1 1)))))

(ert-deftest test-digital-nand ()
  (let ()
    (should (= 1 (digital-nand 0 0 0)))
    (should (= 1 (digital-nand 0 0 1)))
    (should (= 1 (digital-nand 0 1 0)))
    (should (= 1 (digital-nand 0 1 1)))
    (should (= 1 (digital-nand 1 0 0)))
    (should (= 1 (digital-nand 1 0 0)))
    (should (= 1 (digital-nand 1 0 1)))
    (should (= 1 (digital-nand 1 1 0)))
    (should (= 0 (digital-nand 1 1 1)))))

(ert-deftest test-digital-or ()
  (let ()
    (should (= 0 (digital-or 0 0 0)))
    (should (= 1 (digital-or 0 0 1)))
    (should (= 1 (digital-or 0 1 0)))
    (should (= 1 (digital-or 0 1 1)))
    (should (= 1 (digital-or 1 0 0)))
    (should (= 1 (digital-or 1 0 0)))
    (should (= 1 (digital-or 1 0 1)))
    (should (= 1 (digital-or 1 1 0)))
    (should (= 1 (digital-or 1 1 1)))))

(ert-deftest test-digital-nor ()
  (let ()
    (should (= 1 (digital-nor 0 0 0)))
    (should (= 0 (digital-nor 0 0 1)))
    (should (= 0 (digital-nor 0 1 0)))
    (should (= 0 (digital-nor 0 1 1)))
    (should (= 0 (digital-nor 1 0 0)))
    (should (= 0 (digital-nor 1 0 0)))
    (should (= 0 (digital-nor 1 0 1)))
    (should (= 0 (digital-nor 1 1 0)))
    (should (= 0 (digital-nor 1 1 1)))))

(ert-deftest test-digital-xor ()
  (let ()
    (should (= 0 (digital-xor 0 0)))
    (should (= 1 (digital-xor 0 1)))
    (should (= 1 (digital-xor 1 0)))
    (should (= 0 (digital-xor 1 1)))))

(ert-deftest test-digital-value-to-bool ()
  (let ()
    (should-not (digital-value-to-bool 0))
    (should (digital-value-to-bool 1))
    (should-error (digital-value-to-bool 2))
    (should-error (digital-value-to-bool "hey"))))

(ert-deftest test-digital-bool-to-value ()
  (let ()
    (should (= 1 (digital-bool-to-value t)))
    (should (= 0 (digital-bool-to-value nil)))
    (should (= 1 (digital-bool-to-value 1)))
    (should (= 0 (digital-bool-to-value 0)))
    (should-error (digital-bool-to-value 2))))

(provide 'test-cc-digital-logic)
;;; test-cc-digital-logic.el ends here
