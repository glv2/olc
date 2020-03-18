#|
Convert geographic coordinates between Latitude/Longitude and Open Location
Code.

Copyright 2020 Guillaume Le Vaillant

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

(defpackage :olc
  (:use :common-lisp)
  (:export #:lat/lon->olc
           #:olc->lat/lon))

(in-package :olc)


(defun lat/lon->olc (latitude longitude &optional extra-precision)
  "Return the Open Location Code for the given LATITUDE and LONGITUDE."
  (check-type latitude (real -90 90))
  (check-type longitude (real -180 (180)))
  (let ((letters (load-time-value "23456789CFGHJMPQRVWX" t))
        (lat (+ latitude 90.0d0))
        (lon (+ longitude 180.0d0)))
    (multiple-value-bind (y0 ry) (floor lat 20)
      (multiple-value-bind (x0 rx) (floor lon 20)
        (multiple-value-bind (y1 ry) (floor ry)
          (multiple-value-bind (x1 rx) (floor rx)
            (multiple-value-bind (y2 ry) (floor (* 20 ry))
              (multiple-value-bind (x2 rx) (floor (* 20 rx))
                (multiple-value-bind (y3 ry) (floor (* 20 ry))
                  (multiple-value-bind (x3 rx) (floor (* 20 rx))
                    (multiple-value-bind (y4 ry) (floor (* 20 ry))
                      (multiple-value-bind (x4 rx) (floor (* 20 rx))
                        (let ((extra (char letters (+ (* 4 (floor (* 5 ry)))
                                                      (floor (* 4 rx))))))
                          (format nil "~c~c~c~c~c~c~c~c+~c~c~a"
                                  (char letters y0)
                                  (char letters x0)
                                  (char letters y1)
                                  (char letters x1)
                                  (char letters y2)
                                  (char letters x2)
                                  (char letters y3)
                                  (char letters x3)
                                  (char letters y4)
                                  (char letters x4)
                                  (if extra-precision extra "")))))))))))))))

(defun olc->lat/lon (code)
  "Return the latitude and longitude for the southwest corner of the given
Open Location Code square."
  (check-type code string)
  (let* ((letters (load-time-value "23456789CFGHJMPQRVWX" t))
         (code (case (length code)
                 ((12) code)
                 ((11) (concatenate 'string code "2"))
                 ((10) (concatenate 'string code "22"))
                 ((9) (concatenate 'string code "222"))
                 (t (error "Invalid code: ~a" code))))
         (code (substitute #\2 #\0 code :test #'char=))
         (y0 (position (char code 0) letters :test #'char-equal))
         (x0 (position (char code 1) letters :test #'char-equal))
         (y1 (position (char code 2) letters :test #'char-equal))
         (x1 (position (char code 3) letters :test #'char-equal))
         (y2 (position (char code 4) letters :test #'char-equal))
         (x2 (position (char code 5) letters :test #'char-equal))
         (y3 (position (char code 6) letters :test #'char-equal))
         (x3 (position (char code 7) letters :test #'char-equal))
         (y4 (position (char code 9) letters :test #'char-equal))
         (x4 (position (char code 10) letters :test #'char-equal))
         (extra (position (char code 11) letters :test #'char-equal)))
    (if (and (char= (char code 8) #\+)
             (every #'integerp (list x0 x1 x2 x3 x4 y0 y1 y2 y3 y4 extra)))
        (multiple-value-bind (y5 x5) (floor extra 4)
          (let* ((lon (+ (* 20 x0) x1 (/ x2 20) (/ x3 400)
                         (/ x4 8000) (/ x5 32000)))
                 (lat (+ (* 20 y0) y1 (/ y2 20) (/ y3 400)
                         (/ y4 8000) (/ y5 40000)))
                 (longitude (- lon 180.0d0))
                 (latitude (- lat 90.0d0)))
            (list latitude longitude)))
        (error "Invalid code: ~a" code))))
