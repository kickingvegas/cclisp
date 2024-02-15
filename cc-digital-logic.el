;;; cc-digital-logic.el --- Digital Boolean Operations  -*- lexical-binding: t; -*-

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

;; This is a library of functions for performing digital boolean logic where
;; the only legal input values are 0 and 1.

;; The following boolean logic operations are supported:
;; - multi-input and (`digital-and')
;; - multi-input or (`digital-or')
;; - multi-input nand (`digital-nand')
;; - multi-input nor (`digital-nor')
;; - 2-input xor (`digital-xor')

;;; Code:
(require 'shortdoc)

(defun digital-value-to-bool (a)
  "Convert digital value A to Elisp boolean type."
  (cond
   ((eq a 0) nil)
   ((eq a 1) t)
   (t (error "Invalid input: %S" a))))

(defun digital-bool-to-value (a)
  "Convert Elisp boolean A to digital value."
  (cond
   ((eq a t) 1)
   ((eq a nil) 0)
   ((eq a 1) 1)
   ((eq a 0) 0)
   (t (error "Invalid input: %S" a))))

(defun digital-and (a b &rest c)
  "Digital and operation on operands A, B and if available sequence C."
  (let* ((args (list a b))
         (args (append c args))
         (bool-args (mapcar 'digital-value-to-bool args)))
    (digital-bool-to-value (seq-reduce '(lambda (x y) (and x y)) bool-args t))))

(defun digital-nand (a b &rest c)
  "Digital nand operation on operands A, B and if available sequence C."
  (let* ((args (list a b))
         (args (append c args))
         (bool-args (mapcar 'digital-value-to-bool args)))
    (digital-bool-to-value
     (not (seq-reduce '(lambda (x y) (and x y)) bool-args t)))))

(defun digital-or (a b &rest c)
  "Digital or operation on operands A, B and if available sequence C."
  (let* ((args (list a b))
         (args (append c args))
         (bool-args (mapcar 'digital-value-to-bool args)))
    (digital-bool-to-value (seq-reduce '(lambda (x y) (or x y)) bool-args nil))))

(defun digital-nor (a b &rest c)
  "Digital nor operation on operands A, B and if available sequence C."
  (let* ((args (list a b))
         (args (append c args))
         (bool-args (mapcar 'digital-value-to-bool args)))
    (digital-bool-to-value
     (not (seq-reduce '(lambda (x y) (or x y)) bool-args nil)))))

(defun digital-xor (a b)
  "Digital xor operation on operands A and B."
  (let ((i (digital-value-to-bool a))
        (j (digital-value-to-bool b)))
    (digital-bool-to-value (xor i j))))

(define-short-documentation-group digital-logic
  "Digital Logic Operations"
  (digital-and
   :eval (digital-and 0 0 0 0 )
   :eval (digital-and 1 1 1 1)
   :no-manual)
  (digital-or
   :eval (digital-or 0 0 0 0 )
   :eval (digital-or 1 1 1 1)
   :no-manual)
  (digital-nand
   :eval (digital-nand 0 0 0 0 )
   :eval (digital-nand 1 1 1 1)
   :no-manual)
  (digital-nor
   :eval (digital-nor 0 0 0 0)
   :eval (digital-nor 1 1 1 1)
   :no-manual)
  (digital-xor
   :eval (digital-xor 0 0)
   :eval (digital-xor 0 1)
   :eval (digital-xor 1 0)
   :eval (digital-xor 1 1)
   :no-manual))

(provide 'cc-digital-logic)
;;; cc-digital-logic.el ends here
