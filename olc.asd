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

(asdf:defsystem "olc"
  :name "olc"
  :description "Convert coordinates between Latitude/Longitude and Open Location Code."
  :version "1.0"
  :author "Guillaume Le Vaillant"
  :license "GPL-3"
  :in-order-to ((test-op (test-op "olc/tests")))
  :components ((:file "olc")))

(asdf:defsystem "olc/tests"
  :name "olc/tests"
  :description "Unit tests for olc"
  :version "1.0"
  :author "Guillaume Le Vaillant"
  :license "GPL-3"
  :depends-on ("fiveam" "olc")
  :in-order-to ((test-op (load-op "olc/tests")))
  :perform (test-op (o s)
             (let ((tests (uiop:find-symbol* 'olc-tests :olc-tests)))
               (uiop:symbol-call :fiveam 'run! tests)))
  :components ((:file "olc-tests")))
