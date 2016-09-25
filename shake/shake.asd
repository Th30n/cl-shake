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

(defsystem "shake"
  :description "shake: A Doom like game"
  :version "0.0.1"
  :author "Teon Banek <theongugl@gmail.com>"
  :licence "GPL2"
  :depends-on ("sdl2" "alexandria" "shiva" "shake-bspc" "shake-gl" "shake-utils")
  :serial t
  :components ((:file "package")
               (:file "data")
               (:file "render-progs")
               (:file "model")
               (:file "world")
               (:file "image")
               (:file "render")
               (:file "shake"))
  :in-order-to ((test-op (test-op shake-test))))
