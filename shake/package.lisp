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

(defpackage #:shake.data
  (:nicknames #:sdata)
  (:use #:cl #:alexandria #:shake-utils)
  (:export #:data-path
           #:with-data-dirs
           #:with-data-file
           ;; resource management
           #:with-resources
           #:add-res
           #:res
           #:res-let
           #:free-res
           #:free-resources))

(defpackage #:shake.model
  (:nicknames #:smdl)
  (:use #:cl #:alexandria #:shiva #:shake-utils #:shake.data)
  (:export #:load-model
           #:model
           #:model-nodes
           #:model-hull
           #:model-things
           #:surface
           #:surface-color
           #:surface-faces
           #:surface-texcoords)
  (:import-from #:sbsp
                #:lineseg-start
                #:lineseg-end
                #:lineseg-orig-line
                #:lineseg-t-start
                #:lineseg-t-end
                #:linedef-end))

(defpackage #:shake
  (:use #:cl #:alexandria #:shiva #:shake-gl #:shake-utils #:shake.data)
  (:export #:main
           #:recursive-hull-check
           #:hull-point-contents
           ;; mtrace
           #:mtrace
           #:mtrace-endpos
           #:mtrace-fraction
           #:mtrace-normal))
