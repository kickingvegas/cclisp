;;; cc-truth-table.el --- Truth Table                -*- lexical-binding: t; -*-

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

;; This package includes functions to support the insertion of a truth table
;; input into a buffer. Primarily intended for use in Org mode so that the
;; the input values can be operated on with boolean functions defined in
;; `cc-digital-logic'.
;;

;;; Code:

(defun tj/int-to-binary-string (i)
  "Convert integer I into its binary representation in string format.
This code written by Trey Jackson as SO example.
https://stackoverflow.com/questions/20568684/converting-number-to-base-2-binary-string-representation"
  (let ((res ""))
    (while (not (= i 0))
      (setq res (concat (if (= 1 (logand i 1)) "1" "0") res))
      (setq i (ash i -1)))
    (if (string= res "")
        (setq res "0"))
    res))

(defun cc-int-to-binary-list (i)
  "Generate list holding binary string representation of I."
  (mapcar (lambda (x) (string x)) (tj/int-to-binary-string i)))

(defun py-range (start &optional end step)
  "Python range emulation.
This function emulates the Python range function in Elisp.
START - start value of range.
END - end value of range.
STEP - step value for generated range.

Written by michaelJohn
https://stackoverflow.com/questions/15014996/range-data-type-or-generator-in-emacs-lisp-elisp"
  (unless end
    (setq end start
      start 0))
  (number-sequence start (1- end) step))

(defun cc/truth-table-input (bits)
  "Generate list of strings holding a truth table input with 2^BITS rows."
  (let* ((max (expt 2 bits))
         (truth-table-range (py-range max))
         (header-list-exponents
          (mapcar 'number-to-string (reverse (py-range bits))))
         (header-list (push "i" header-list-exponents))
         (header (format "|%s|" (string-join header-list "|")))
         (header-separator
          (format "|%s|"
                  (string-join (make-list (1+ bits) "-")
                               "+")))
         (result (list header-separator header)))

    (dolist (e truth-table-range)
      (setq-local binary-list (cc-int-to-binary-list e))
      (setq-local binary-list-offset (- bits (length binary-list)))

      (if (> binary-list-offset 0)
          (dolist (_ (py-range 0 binary-list-offset))
            (push "0" binary-list)))

      (push (format "|%s|%s|" e (string-join binary-list "|")) result))
    (reverse result)))

(defun cc/insert-truth-table-input (bits)
  "Insert truth table input with 2^BITS rows into current buffer.
If BITS is greater than 4, then a prompt is asked to avoid creation of
large tables."
  (interactive "nNumber of inputs (bits): ")

  (if (> bits 4)
      (if (y-or-n-p
           (format
            "This will generate a table with %s rows. Continue? "
            (expt 2 bits)))
          (let ((truth-table (cc/truth-table-input bits)))
            (insert (string-join truth-table "\n"))))
    (let ((truth-table (cc/truth-table-input bits)))
      (insert (string-join truth-table "\n")))))


(provide 'cc-truth-table)
;;; cc-truth-table.el ends here
