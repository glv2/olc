#|
Convert geographic coordinates between Latitude/Longitude and Open Location
Code.

Copyright 2020 Guillaume LE VAILLANT

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#

(defpackage :olc-tests
  (:use :common-lisp :fiveam :olc))

(in-package :olc-tests)


(def-suite olc-tests)

(in-suite olc-tests)

(defconstant +max-error-rate+ 0.0001)

(defun close-enough (x y)
  (<= (- 1 +max-error-rate+) (abs (/ x y)) (+ 1 +max-error-rate+)))

(defparameter *olc-data*
  '(((47.365562d0 8.524813d0) "8FVC9G8F+6W")
    ((20.370114d0 2.782235d0) "7FG49QCJ+2V")
    ((47 8) "8FVC2222+22")
    ((-41.2730625d0 174.7859375d0) "4VCPPQGP+Q9")
    ((-89.9999375d0 -179.9999375d0) "22222222+22")
    ((1.2 3.4) "6FH56C22+22")))

(test lat/lon->olc
  (mapc (lambda (data)
          (destructuring-bind (latitude longitude) (first data)
            (let* ((code (second data))
                   (olc (lat/lon->olc latitude longitude)))
              (is (string= code olc)))))
        *olc-data*))

(test olc->lat/lon
  (mapc (lambda (data)
          (destructuring-bind (latitude longitude) (first data)
            (let ((code (second data)))
              (destructuring-bind (lat lon) (olc->lat/lon code)
                (is (close-enough latitude lat))
                (is (close-enough longitude lon))))))
        *olc-data*))
