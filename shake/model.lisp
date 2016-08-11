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
  faces
  texcoords
  gl-arrays)

(defstruct model
  "A 3D model. The NODES slot contains bsp nodes for rendering. The HULL slot
  contains bsp nodes for collision detection. Remaining entities are stored in
  THINGS slot."
  nodes
  hull
  things)

(defun make-triangles (sidedef)
  (with-struct (lineseg- start end) (sbsp:sidedef-lineseg sidedef)
    (let ((start-3d (v (vx start) 1 (vy start)))
          (end-3d (v (vx end) 1 (vy end))))
      (list start-3d end-3d (v- start-3d (v 0 1 0))
            (v- start-3d (v 0 1 0)) end-3d (v- end-3d (v 0 1 0))))))

(defun make-texcoords (sidedef)
  (let ((texinfo (sbsp:sidedef-texinfo sidedef)))
    (unless (or (null texinfo) (eq :caulk texinfo))
      (let ((texcoord-bounds
             (ecase (sbsp:texinfo-draw-mode texinfo)
               (:tile
                (with-struct (lineseg- start end orig-line)
                    (sbsp:sidedef-lineseg sidedef)
                  ;; Winding is clockwise (right to left), so reverse the
                  ;; texture.
                  (cons (vdist start (linedef-end orig-line))
                        (vdist end (linedef-end orig-line)))))
               (:scale-to-fit
                (with-struct (lineseg- t-start t-end)
                    (sbsp:sidedef-lineseg sidedef)
                  ;; Reverse the coordinates, due to vertex winding.
                  (cons (- 1d0 t-start) (- 1d0 t-end)))))))
        (destructuring-bind (u-start . u-end) texcoord-bounds
          (mapcar (lambda (uv) (v+ (sbsp:texinfo-offset texinfo) uv))
                  (list (v u-start 0) (v u-end 0) (v u-start 1)
                        (v u-start 1) (v u-end 0) (v u-end 1))))))))


(defun load-gl-arrays (surface)
  (flet ((make-gl-array (data)
           (let ((arr (gl:alloc-gl-array :float (reduce #'+ (mapcar #'array-total-size data))))
                 (offset 0))
             (dolist (vec data)
               (dotimes (i (array-total-size vec))
                 (setf (gl:glaref arr offset) (coerce (row-major-aref vec i) 'single-float))
                 (incf offset)))
             (cons (gl:gl-array-byte-size arr) arr))))
  (with-struct (surface- faces texcoords color) surface
    (let ((position-array (make-gl-array faces))
          (color-array (make-gl-array (make-list 6 :initial-element color)))
          (normal-array (make-gl-array
                         (make-list 6 :initial-element
                                    (v2->v3 (sbsp:lineseg-normal
                                             (sbsp:sidedef-lineseg surface))))))
          (uv-array (when texcoords (make-gl-array texcoords))))
      (setf (surface-gl-arrays surface)
            (list position-array color-array normal-array uv-array)))))
  surface)

(defun sidedef->surface (sidedef)
  (load-gl-arrays
   (make-surface :lineseg (sbsp:sidedef-lineseg sidedef)
                 :color (sbsp:sidedef-color sidedef)
                 :texinfo (sbsp:sidedef-texinfo sidedef)
                 :faces (make-triangles sidedef)
                 :texcoords (make-texcoords sidedef))))

(defun nadapt-nodes (bsp)
  (sbsp:bsp-trav bsp (constantly nil)
                 (lambda (leaf)
                   (zap (curry #'mapcar #'sidedef->surface)
                        (sbsp:leaf-surfaces leaf))))
  bsp)

(defun bspfile->model (bspfile)
  (make-model :hull (sbsp:bspfile-clip-nodes bspfile)
              :nodes (nadapt-nodes (sbsp:bspfile-nodes bspfile))
              :things (sbsp:bspfile-things bspfile)))

(defun load-model (model-fname)
  (with-data-file (file model-fname)
    (bspfile->model (sbsp:read-bspfile file))))

(defun free-model (model)
  (with-struct (model- nodes) model
    (sbsp:bsp-trav nodes (constantly nil)
                   (lambda (leaf)
                     (dolist (surf (sbsp:leaf-surfaces leaf))
                       (dolist (array (surface-gl-arrays surf))
                         (when array (gl:free-gl-array (cdr array)))))))))
