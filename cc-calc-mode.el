;;; cc-calc-mode.el --- Calc customization           -*- lexical-binding: t; -*-

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

(require 'calc)
(require 'transient)

(defun cc/confirm-before-calc-quit ()
  "Raise confirm prompt before invoking `calc-quit'."
  (interactive)
  (if (y-or-n-p "Really Quit? ")
      (calc-quit)
    (message "all good")))

(defun cc/calc-matrixp ()
  (math-matrixp (calc-top-n 1)))

(defun cc/calc-square-matrixp ()
  (math-square-matrixp (calc-top-n 1)))

(defun cc/calc-vectorp ()
  (math-vectorp (calc-top-n 1)))

(defun cc/calc-crossp ()
  (if (> (calc-stack-size) 1)
      (let ((arg1 (calc-top-n 1))
            (arg2 (calc-top-n 2)))
        (and (math-vectorp arg1)
             (math-vectorp arg2)
             (not (math-matrixp arg1))
             (not (math-matrixp arg2))
             (eq 3 (calcFunc-vlen arg1))
             (eq 3 (calcFunc-vlen arg2))))
    nil))


(defun cc/calc-matrixmultp ()
  (if (> (calc-stack-size) 1)
      (let ((arg1 (calc-top-n 1))
            (arg2 (calc-top-n 2)))
        (and (math-matrixp arg1)
             (math-matrixp arg2)))
    nil))

;;(cc/calc-crossp)
;;(cc/calc-vectorp)

;;(math-vectorp (calc-top-n 3))
;; (calcFunc-vlen (calc-top-n 17))
;; (not (math-matrixp (calc-top-n 17)))

(define-key calc-mode-map (kbd "q") 'cc/confirm-before-calc-quit)

(transient-define-prefix cc/calc-main-menu ()
  "Transient main menu for calc."
  [["Calc"
    ("&" "1/𝑥" calc-inv :transient nil)
    ("Q" "√" calc-sqrt :transient nil)
    ("n" "+∕− " calc-change-sign :transient nil)
    ("^" "𝑦^𝑥" calc-power :transient nil)]
   [""
    ("A" "|𝑥|" calc-abs :transient nil)
    ("!" "!" calc-factorial :transient nil)
    ("%" "٪" calc-percent :transient nil)
    ("d" "Δ%" calc-percent-change :transient nil)]
   ["Constants"
    ("p" "𝜋" calc-pi :transient nil)
    ("e" "𝑒" (lambda () (interactive) (calc-hyperbolic) (calc-pi)) :transient nil)]
   ["Modes"
    ("m" "Modes›" cc/calc-modes-menu :transient nil)]]

  [["Functions" ; test if anything is on the stack calc-stack-size 0
    ("t" "Trig›" cc/calc-trig-menu :transient nil)
    ("l" "Logarithmic›" cc/calc-logarithmic-menu :transient nil)
    ("b" "Binary›" cc/calc-binary-menu :transient nil)
    ("v" "Vector/Matrix›" cc/calc-vector-menu :transient nil)
    ("c" "Conversions›" cc/calc-conversions-menu :transient nil)
    ("u" "Unit Conversion›" cc/calc-units-menu :transient nil)]
   ["Stack"
    ;;("s" "Roll Up" calc-roll-up :transient nil)
    ("s" "Swap" calc-roll-down :transient nil)
    ("P" "Pack" calc-pack :transient nil)
    ("U" "Unpack" calc-unpack :transient nil)
    ("R" "Calc Reset" calc-reset :transient nil)]])

(transient-define-prefix cc/calc-conversions-menu ()
  ["Conversions"
   ("d" "To Degrees" calc-to-degrees :transient nil)
   ("r" "To Radians" calc-to-radians :transient nil)
   ("h" "To HMS" calc-to-hms :transient nil)
   ("p" "Toggle Polar/Rect" calc-polar :transient nil)])

(transient-define-prefix cc/calc-binary-menu ()
  [["Operators"
    ("a" "and" calc-and :transient nil)
    ("o" "or" calc-or :transient nil)
    ("x" "xor" calc-xor :transient nil)
    ("-" "diff" calc-diff :transient nil)
    ("!" "diff" calc-not :transient nil)]
   ["Shift"
    ("l" "binary left" calc-lshift-binary :transient nil)
    ("r" "binary right" calc-rshift-binary :transient nil)
    ("L" "arithmetic left" calc-lshift-arith :transient nil)
    ("R" "arithmetic right" calc-rshift-arith :transient nil)
    ("O" "binary" calc-rotate-binary :transient nil)]
   ["Utils"
    ("w" "Set Word Size" calc-word-size :transient nil)
    ("u" "Unpack Bits" calc-unpack-bits :transient nil)
    ("p" "Pack Bits" calc-pack-bits :transient nil)]])

(transient-define-prefix cc/calc-vector-menu ()
  "Vector Menu"
  [
   ["Manipulation"
    ("l" "Length" calc-vlength :transient nil)
    ("f" "Vector Find" calc-vector-find :transient nil) ; requires target N, not a vector
    ("a" "Vector Arrange" calc-arrange-vector :transient nil)
    ("s" "Sort" calc-sort :transient nil) ;; need to use inverse for rsort
    ;; ("h" "Histogram" calc-histogram :transient nil) ; requires prefix
    ("t" "Transpose" calc-transpose :transient nil)
    ("I" "Identity" calc-ident :transient nil)
    ("r" "Reverse" calc-reverse-vector :transient nil)
    ("|" "Concat" calc-concat :transient nil)]
   ["Arithmetic"
    ("T" "Conjugate Transpose" calc-conj-transpose :transient nil)
    ("A" "Frobenius Norm (abs)" calc-abs :transient nil)
    ("R" "Row Norm" calc-rnorm :transient nil)
    ("c" "Column Norm" calc-cnorm :transient nil)
    ("p" "RH Cross Product" calc-cross :if cc/calc-crossp :transient nil)
    ("L" "LU Decomposition" calc-mlud :if cc/calc-matrixp :transient nil)
    ("k" "Kronecker Product" calc-kron :if cc/calc-matrixmultp :transient nil)]
   ["Square Matrix"
    :if cc/calc-square-matrixp
    ("&" "Inverse" calc-inv :transient nil)
    ("d" "Determinant" calc-mdet :transient nil)
    ("t" "Trace" calc-mtrace :transient nil)]
   ["Set"
    ("D" "Deduplicate" calc-remove-duplicates :transient nil)
    ("u" "Union" calc-set-union :transient nil)
    ("i" "Intersect" calc-set-intersect :transient nil)
    ("-" "Difference" calc-set-difference :transient nil)
    ("x" "xor" calc-set-xor :transient nil)
    ("~" "Complement" calc-set-complement :transient nil)
    ("#" "Cardinality" calc-set-cardinality :transient nil)]]

  [;;"⦿ Single Variable Statistics"
   ["Mean and Error"
    ("C" "Vector Count" calc-vector-count :transient nil)
    ("S" "Sum" calc-vector-sum :transient nil)
    ("X" "Max" calc-vector-max :transient nil)
    ("m" "Mean" calc-vector-mean :transient nil)
    ("E" "Mean Error" calc-vector-mean-error :transient nil)
    ("M" "Median" calc-vector-median :transient nil)
    ("h" "Harmonic Mean" calc-vector-harmonic-mean :transient nil)
    ("g" "Geometric Mean" calc-vector-geometric-mean :transient nil)]
   ["Deviation and Variance"
    ("q" "Root Mean Square" calc-vector-rms :transient nil)
    ("1" "Standard Deviation" calc-vector-sdev :transient nil)
    ("2" "Population Standard Deviation" calc-vector-pop-sdev :transient nil)
    ("3" "Variance" calc-vector-variance :transient nil)
    ("4" "Population Variance" calc-vector-pop-variance :transient nil)]
   ["Paired-Sample Statistics" ; predicate for two vectors of the same size
    ("5" "Covariance" calc-vector-covariance :transient nil)
    ("6" "Population Covariance" calc-vector-pop-covariance :transient nil)
    ("7" "Correlation" calc-vector-correlation :transient nil)]])

(transient-define-prefix cc/calc-units-menu ()
  ["Unit Conversions"
   ("c" "Convert" calc-convert-units :transient nil)
   ("t" "Convert Temperature" calc-convert-temperature :transient nil)
   ("b" "Convert to Base Unit" calc-base-units :transient nil)
   ("a" "Autorange" calc-autorange-units :transient nil)
   ("r" "Remove Units" calc-remove-units :transient nil)
   ("x" "Extract Units" calc-extract-units :transient nil)])

(transient-define-prefix cc/calc-logarithmic-menu ()
  ["Logarithmic Functions"
    ("l" "𝑙𝑛" calc-ln :transient nil)
    ("e" "𝑒^𝑥" calc-exp :transient nil)
    ("L" "𝑙𝑜𝑔𝟣𝟢" calc-log10 :transient nil)
    ("M-l" "𝑙𝑜𝑔" calc-log :transient nil)
    ("M-e" "𝑒^𝑥 - 𝟣" calc-expm1 :transient nil)])

(defun cc/calc-cmplx-or-polar-label ()
  (if (eq calc-complex-mode 'polar)
      "Change to Complex Mode (now Polar)"
    "Change to Polar Mode (now Complex)"))

(defun cc/calc-symbolic-mode-label ()
  (if calc-symbolic-mode
      "Change to Numeric Mode (now Symbolic)"
    "Change to Symbolic Mode (now Numeric)"))

(defun cc/calc-prefer-frac-label ()
  (if calc-prefer-frac
      "Change to Floating Point Results (now Fractional)"
    "Change to Fractional Results (now Floating Point)"))

(defun cc/calc-number-radix-label ()
  (cond
   ((= calc-number-radix 10) "Decimal")
   ((= calc-number-radix 2) "Binary")
   ((= calc-number-radix 8) "Octal")
   ((= calc-number-radix 16) "Hexadecimal")
   (t (format "%d" calc-number-radix))))

(defun cc/calc-matrix-mode-label ()
  (cond
   ((eq calc-matrix-mode 'matrix) "Matrix")
   ((eq calc-matrix-mode 'sqmatrix) "Square Matrix")
   ((eq calc-matrix-mode 'scalar) "Scalar")
   ((eq calc-matrix-mode 'nil) "No assumptions")
   ((integerp calc-matrix-mode) (format "%dx%d"
                                        calc-matrix-mode
                                        calc-matrix-mode))))

(defun cc/calc-complex-format-label ()
  (cond
   ((eq calc-complex-format 'i) "x + yi")
   ((eq calc-complex-format 'j) "x + yj")
   ((not calc-complex-format) "(x, y)")))

(transient-define-prefix cc/calc-modes-menu ()
  [
   ["Toggles & Notations"
    ("z" "Leading Zeroes" calc-leading-zeros :transient nil)
    ("F" calc-frac-mode :description cc/calc-prefer-frac-label :transient nil)
    ("s" calc-symbolic-mode :description cc/calc-symbolic-mode-label :transient nil)
    ("p" calc-polar-mode :description cc/calc-cmplx-or-polar-label :transient nil)
    ;; ("m" calc-matrix-mode :description cc/calc-matrix-mode-label :transient nil) ; this is really about symbolic computation
    ("c" "Complex Number Format›" cc/calc-complex-format-menu
     :description (lambda ()
                    (format "Complex Number Format (now %s)›"
                            (cc/calc-complex-format-label)))
     :transient nil)
    ("H" "ℎ𝑚𝑠 notation" calc-hms-notation :transient nil)]
   ["Angular"
    ("d" "Degrees" calc-degrees-mode :transient nil)
    ("r" "Radians" calc-radians-mode :transient nil)
    ("h" "Degrees-Minutes-Seconds" calc-hms-mode :transient nil)]]

  [["Display"
    ("R" cc/calc-radix-menu
     :description (lambda ()
                    (format "Radix (now %s)›" (cc/calc-number-radix-label)))
     :transient nil)
    ("f" "Float Formats›" cc/calc-float-format-menu :transient nil)
    ("g" "Thousands Separators (group)" calc-group-digits :transient nil)
    ("," "Set Thousands Separator (group char)" calc-group-char :transient nil)
    ("P" "Decimal Separator" calc-point-char :transient nil)]])


(transient-define-prefix cc/calc-complex-format-menu ()
  ["Complex Number Display Format"
    ("c" calc-complex-notation
     :description "complex notation"
     :transient nil)

    ("i" calc-i-notation
     :description "𝑖 notation"
     :transient nil)

    ("j" calc-i-notation
     :description "𝑗 notation"
     :transient nil)])

(transient-define-prefix cc/calc-radix-menu ()
  [["Radix"
    ("0" "Decimal" calc-decimal-radix :transient nil)
    ("2" "Binary" calc-binary-radix :transient nil)
    ("8" "Octal" calc-octal-radix :transient nil)
    ("6" "Hexadecimal" calc-hex-radix :transient nil)
    ("n" "Other" calc-radix :transient nil)]])

(transient-define-prefix cc/calc-float-format-menu ()
  [["Float Format"
    ("n" "Normal" calc-normal-notation :transient nil)
    ("f" "Fixed Point" calc-fix-notation :transient nil)
    ("s" "Scientific" calc-sci-notation :transient nil)
    ("e" "Engineering" calc-eng-notation :transient nil)]])


(transient-define-prefix cc/calc-trig-menu ()
  ;; ["Arguments"
  ;;  ("i" "inverse" "--inverse")
  ;;  ("h" "hyperbolic" "--hyperbolic")]
  [["Trig"
    ("s" "sin" calc-sin :transient nil)
    ("c" "cos" calc-cos :transient nil)
    ("t" "tan" calc-tan :transient nil)]
   ["Inverse"
    ;;("S" "arcsin" calc-arcsin :transient nil)
    ;;(calc-fancy-prefix 'calc-inverse-flag msg n)

    ("S" "arcsin"
     (lambda ()
       (interactive)
       (calc-inverse)
       (calc-sin (calc-top-n 1)))
     :transient nil)

    ("C" "arccos" calc-arccos :transient nil)
    ("T" "arctan" calc-arctan :transient nil)]
   ["Hyperbolic"
    ("M-s" "sinh" calc-sinh :transient nil)
    ("M-c" "cosh" calc-cosh :transient nil)
    ("M-t" "tanh" calc-tanh :transient nil)]
   ["Inverse Hyperbolic"
    ("M-S" "arcsinh" calc-arcsinh :transient nil)
    ("M-C" "arccosh" calc-arccosh :transient nil)
    ("M-T" "arctanh" calc-arctanh :transient nil)]])

;; (defun cc/--trig-command (trig prefix-args)

;;   (cond
;;    ((eq trig :sin)
;;     (cond
;;      ((member "--inverse" prefix-args) (calc-arcsin))
;;      ((member "--hyperbolic" prefix-args) (calc-sinh))
;;      (t (calc-sin)))


;;     ))
;;     )

(define-key calc-mode-map (kbd "C-o") 'cc/calc-main-menu)


(provide 'cc-calc-mode)
;;; cc-calc-mode.el ends here
