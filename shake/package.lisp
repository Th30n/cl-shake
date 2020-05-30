;;;; Copyright (C) 2017 Teon Banek
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

(defpackage #:shake.debug
  (:use #:cl #:alexandria #:shake-utils)
  (:export #:timer
           #:timer-name
           #:timer-max
           #:timer-avg
           #:make-timer
           #:with-timer))

(defpackage #:shake.data
  (:nicknames #:sdata)
  (:use #:cl #:alexandria #:shake-utils)
  (:export #:data-path
           #:data-file-error
           #:exe-dir
           #:with-data-dirs
           #:with-data-file
           ;; resource management
           #:with-resources
           #:add-res
           #:res
           #:res-let
           #:free-res
           #:free-resources))

(defpackage #:shake.obj
  (:nicknames #:sobj)
  (:use #:cl #:shiva)
  (:export #:read-obj
           #:obj
           #:obj-geometric-vertices
           #:obj-normal-vertices
           #:obj-texture-vertices
           #:obj-element-data
           #:element-points
           #:vref-geometric
           #:vref-normal
           #:vref-texture
           #:vertex-val))

(defpackage #:shake.fbx
  (:nicknames #:sfbx)
  (:use #:cl #:shiva))

(defpackage #:shake.model
  (:nicknames #:smdl)
  (:use #:cl #:alexandria #:shiva #:shake-utils #:shake.data)
  (:export #:*world-model*
           #:bsp-model
           #:bsp-model-nodes
           #:bsp-model-hull
           #:bsp-model-things
           #:mleaf-floor-geometry
           #:mleaf-ceiling-geometry
           #:surface
           #:surface-geometry
           #:with-model-manager
           #:model-manager-default-model
           #:model-manager-reload-models
           #:get-model)
  (:import-from #:sbsp
                #:lineseg-start
                #:lineseg-end
                #:lineseg-orig-line
                #:lineseg-t-start
                #:lineseg-t-end
                #:linedef-end))

(defpackage #:shake.render
  (:nicknames #:srend)
  (:use #:cl #:alexandria #:shiva #:shake-utils #:shake.debug)
  (:export #:draw-gui-text
           #:draw-gui-quad
           #:draw-text
           #:load-map-images
           #:list-modes
           #:gl-config
           #:print-gl-info
           #:print-memory-usage
           #:render-surface
           #:render-system
           #:render-system-gl-config
           #:render-system-prog-manager
           #:render-system-rend-height
           #:render-system-rend-width
           #:with-render-system
           #:with-draw-frame
           ;; These are just command symbols
           #:set-fullscreen
           #:set-vid-mode
           #:set-win-size
           #:vid-restart))

(defpackage #:shake
  (:use #:cl #:alexandria #:shiva #:shake-gl #:shake-utils #:shake.data
        #:shake.debug)
  (:export #:main
           #:clip-hull
           #:hull-point-contents
           ;; mtrace
           #:mtrace
           #:mtrace-endpos
           #:mtrace-fraction
           #:mtrace-normal
           #:mtrace-start-solid-p
           ;; console and commands
           ;; TODO: These should be defined in another file, perhaps another
           ;; package
           #:add-command
           #:print-error
           #:print-warning
           #:printf))
