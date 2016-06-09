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

(in-package #:cl-user)

(defpackage #:shake-bspc
  (:nicknames #:shake-bsp #:sbsp)
  (:use #:cl #:shiva)
  (:export #:linedef
           #:make-linedef
           #:linedef-start
           #:linedef-end
           #:linedef-normal
           ;; line segment
           #:lineseg
           #:make-lineseg
           #:lineseg-orig-line
           #:lineseg-start
           #:lineseg-end
           #:lineseg-normal
           #:linedef->lineseg
           ;; bsp nodes
           #:node
           #:node-p
           #:node-line
           #:node-front
           #:node-back
           #:leaf
           #:leaf-p
           #:leaf-contents
           ;; build interface
           #:read-map
           #:build-bsp
           #:read-and-compile-map
           #:read-bsp
           #:compile-map-file
           ;; bsp queries
           #:determine-side
           #:back-to-front))

(defpackage #:shake-bspc.brush
  (:nicknames #:sbrush)
  (:use #:cl #:shiva #:sbsp)
  (:import-from #:alexandria
                #:unionf
                #:nconcf))
