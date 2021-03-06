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
  (:use #:cl #:shiva #:shake-utils)
  (:export #:define-disk-struct ;; this should move to serialization
           #:linedef
           #:make-linedef
           #:make-linedef-loop
           #:linedef-start
           #:linedef-end
           #:linedef-normal
           #:linedef-vec
           #:linedef=
           #:write-linedef
           #:read-linedef
           #:bounds-of-linedefs
           #:dist-line-point
           ;; sector definition
           #:sector
           #:sector-p
           #:make-sector
           #:copy-sector
           #:sector-ceiling-height
           #:sector-ceiling-texinfo
           #:sector-floor-height
           #:sector-floor-texinfo
           #:sector-contents
           ;; side definition
           #:sidedef
           #:sidedef-p
           #:make-sidedef
           #:copy-sidedef
           #:sidedef-color
           #:sidedef-front-sector
           #:sidedef-back-sector
           #:sidedef-lineseg
           #:sidedef-texinfo
           #:write-sidedef
           #:read-sidedef
           #:linedef->sidedef
           ;; texinfo
           #:texinfo
           #:texinfo-p
           #:make-texinfo
           #:copy-texinfo
           #:texinfo-offset
           #:texinfo-name
           #:texinfo-draw-mode
           ;; line segment
           #:lineseg
           #:make-lineseg
           #:lineseg-orig-line
           #:lineseg-start
           #:lineseg-end
           #:lineseg-normal
           #:linedef->lineseg
           #:split-lineseg
           ;; bsp nodes
           #:node
           #:node-p
           #:node-back
           #:node-bounds
           #:node-front
           #:node-line
           #:node-surface
           #:write-bsp
           #:read-bsp
           #:leaf
           #:leaf-p
           #:leaf-bounds
           #:leaf-contents
           #:leaf-surfaces
           ;; map editor interface
           #:map-thing
           #:make-map-thing
           #:map-thing-type
           #:map-thing-pos
           #:map-thing-angle
           #:map-thing-brushes
           #:map-file
           #:make-map-file
           #:map-file-brushes
           #:map-file-things
           #:write-map
           #:read-map
           #:invalid-map-file-error
           #:message
           #:compile-map-file
           #:*splitter-choice-strategy*
           #:build-bsp
           #:read-bsp
           ;; bsp file structure
           #:bspfile
           #:bspfile-nodes
           #:bspfile-clip-nodes
           #:bspfile-things
           #:write-bspfile
           #:read-bspfile
           ;; bsp queries
           #:partition-surfaces
           #:convex-hull-p
           #:point-in-hull-p
           #:triangulate
           #:determine-side
           #:bsp-trav
           #:bsp-rec
           #:back-to-front)
  (:import-from #:alexandria
                #:compose
                #:curry
                #:ensure-list
                #:make-keyword
                #:mappend
                #:maxf
                #:minf
                #:starts-with
                #:symbolicate))

(defpackage #:shake-bspc.brush
  (:nicknames #:sbrush)
  (:use #:cl #:shiva #:sbsp #:shake-utils)
  (:import-from #:alexandria
                #:appendf
                #:compose
                #:curry
                #:if-let
                #:map-product
                #:mappend
                #:unionf)
  (:export #:brush
           #:brush-p
           #:make-brush
           #:brush-lines
           #:brush-surfaces
           #:brush-contents
           #:copy-brush
           #:write-brush
           #:read-brush
           #:brush-translate
           #:brush-rotate
           #:non-convex-brush-error
           #:expand-brush
           #:prepare-brushes-for-bsp))
