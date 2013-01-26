;;; est.el --- german ESt. calculation

;; Copyright (C) 2013 Martin Balfanz <me@martinbalfanz.com>

;; Author: Martin Balfanz <me@martinbalfanz.com>
;; Version: 0.1.1
;; Keywords:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is an implementation of the german ESt.-formula provided by
;; §32a EStG for the years 2010-2012 of an unmarried person.
;;
;; source: https://www.bmf-steuerrechner.de/ekst/

;;; Code:

(defun est-message (tax)
  "Pretty print TAX value to minibuffer."
  (message (concat "You have to pay approximately "
                   (number-to-string (round tax))
                   " Euro.")))

(defun est-insert (tax)
  "Insert TAX value at point."
  (insert (number-to-string (round tax))))

(defun est-calc (zve)
  "Calculate taxes from ZVE."
  (cond
   ((< zve 8005.0) 0)
   ((< zve 13470.0) (let* ((y (/ (- zve 8004.0) 10000.0))
                           (est (* (+ (* 912.17 y) 1400.0) y)))
                      est))
   ((< zve 52882.0) (let* ((y (/ (- zve 13469.0) 10000.0))
                           (est (+ (* (+ (* 228.74 y) 2397.0) y) 1038.0)))
                      est))
   ((< zve 250731.0) (- (* 0.42 zve) 8172.0))
   (t (- (* 0.45 zve) 15694.0))))

;;;###autoload
(defun est (prefix zve)
  "Calculate tax from ZVE and print it to minibuffer.
If called with a PREFIX argument, the tax amout is inserted at point."
  (interactive "P\nnEnter yearly income: ")
  (if (not prefix)
      (est-message (est-calc zve))
    (est-insert (est-calc zve))))

(provide 'est)

;;; est.el ends here
