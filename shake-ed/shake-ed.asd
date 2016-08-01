;;;; Copyright (C) 2016 Teon Banek
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License along
;;;; with this program; if not, write to the Free Software Foundation, Inc.,
;;;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

(in-package #:asdf-user)

(defsystem "shake-ed"
  :description "shake-ed: A map editor for shake"
  :version "0.0.1"
  :author "Teon Banek <theongugl@gmail.com>"
  :licence "GPL2"
  :depends-on ("alexandria" "shiva" "shake-bspc"
               "qtools" "qtcore" "qtgui" "qtsvg" "qtopengl")
  :serial t
  :components ((:file "package")
               (:file "map-scene")
               (:file "shake-ed")))
