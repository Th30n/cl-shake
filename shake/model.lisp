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

(in-package #:shake.model)

(defstruct (surface (:include sbsp:sidedef))
  "Extended SIDEDEF which contains 3D faces for rendering."
  faces)

(defstruct model
  "A 3D model. The NODES slot contains bsp nodes for rendering. The HULL slot
  contains bsp nodes for collision detection."
  nodes
  hull)

(defun make-triangles (sidedef)
  (with-struct (lineseg- start end) (sbsp:sidedef-lineseg sidedef)
    (let ((start-3d (v (vx start) 1 (vy start)))
          (end-3d (v (vx end) 1 (vy end))))
      (list start-3d end-3d (v- start-3d (v 0 1 0))
            (v- start-3d (v 0 1 0)) end-3d (v- end-3d (v 0 1 0))))))

(defun sidedef->surface (sidedef)
  (make-surface :lineseg (sbsp:sidedef-lineseg sidedef)
                :color (sbsp:sidedef-color sidedef)
                :faces (make-triangles sidedef)))

(defun nadapt-nodes (node)
  (if (sbsp:leaf-p node)
      (zap (curry #'mapcar #'sidedef->surface) (sbsp:leaf-surfaces node))
      (progn
        (nadapt-nodes (sbsp:node-front node))
        (nadapt-nodes (sbsp:node-back node))))
  node)

(defun bspfile->model (bspfile)
  (make-model :hull (sbsp:bspfile-clip-nodes bspfile)
              :nodes (nadapt-nodes (sbsp:bspfile-nodes bspfile))))

(defun load-model (model-fname)
  (with-data-file (file model-fname)
    (bspfile->model (sbsp:read-bspfile file))))

