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

(defstruct surf-triangles
  "Stores surface triangles ready for rendering.
  VERTS is a foreign array of vertex data, ready for sending to GPU.
  TEX-NAME is the name of texture image used, can be NIL."
  (num-verts 0 :type fixnum)
  (verts-byte-size 0 :type fixnum)
  verts
  (tex-name nil :type (or null string)))

(defun free-surf-triangles (surf-triangles)
  (gl:free-gl-array (surf-triangles-verts surf-triangles)))

(defstruct (surface (:include sbsp:sidedef))
  "Extended SIDEDEF which contains 3D faces for rendering."
  geometry)

(defstruct (mleaf (:include sbsp:leaf))
  "In memory, model leaf node of model BSP."
  floor-geometry)

(defstruct model
  "A 3D model. The NODES slot contains bsp nodes for rendering. The HULL slot
  contains bsp nodes for collision detection. Remaining entities are stored in
  THINGS slot."
  nodes
  hull
  things)

(defun make-triangles (sidedef)
  (with-struct (lineseg- start end) (sbsp:sidedef-lineseg sidedef)
    (let* ((floor-bottom (if-let ((front-sector
                                   (sbsp:sidedef-front-sector sidedef)))
                           (sbsp:sector-floor-height front-sector)
                           0))
           (floor-top (if-let ((back-sector
                                (sbsp:sidedef-back-sector sidedef)))
                        (max floor-bottom (sbsp:sector-floor-height back-sector))
                        1))
           (start-3d-bot (v2->v3 start floor-bottom))
           (end-3d-bot (v2->v3 end floor-bottom))
           (start-3d-top (v2->v3 start floor-top))
           (end-3d-top (v2->v3 end floor-top)))
      (list start-3d-top end-3d-top start-3d-bot
            start-3d-bot end-3d-top end-3d-bot))))

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

(defun make-gl-array (data)
  (let ((arr (gl:alloc-gl-array
              :float (reduce #'+ (mapcar #'array-total-size data))))
        (offset 0))
    (dolist (vec data)
      (dotimes (i (array-total-size vec))
        (setf (gl:glaref arr offset) (coerce (row-major-aref vec i) 'single-float))
        (incf offset)))
    (cons (coerce (gl:gl-array-byte-size arr) 'fixnum)
          arr)))

(defun sidedef->surf-triangles (sidedef)
  (let ((positions (make-triangles sidedef))
        (tex-name (when-let ((texinfo (sbsp:sidedef-texinfo sidedef)))
                    (string-downcase (sbsp:texinfo-name texinfo)))))
    (destructuring-bind (byte-size . verts)
        (make-gl-array
         (loop for pos in positions
            and color = (sbsp:sidedef-color sidedef)
            and normal = (v2->v3 (sbsp:lineseg-normal
                                  (sbsp:sidedef-lineseg sidedef)))
            and uv in (or (make-texcoords sidedef)
                          (make-list 6 :initial-element (v 0 0)))
            append (list pos color normal uv)))
      (make-surf-triangles :num-verts (list-length positions)
                           :verts-byte-size byte-size
                           :verts verts
                           :tex-name tex-name))))

(defun polygon->surf-triangles (polygon height)
  (let* ((triangles (sbsp:triangulate (mapcar #'sbsp:linedef->lineseg polygon)))
         (positions (mapcar #'sbsp:lineseg-start (apply #'append triangles))))
    (destructuring-bind (byte-size . verts)
        (make-gl-array
         (loop for pos in positions
            and color = (v 1 0 0)
            and normal = (v 0 1 0)
            and uv = (v 0 0)
            append (list (v2->v3 pos height) color normal uv)))
      (make-surf-triangles :num-verts (list-length positions)
                           :verts-byte-size byte-size
                           :verts verts))))

(defun sidedef->surface (sidedef)
  (make-surface :lineseg (sbsp:sidedef-lineseg sidedef)
                :front-sector (sbsp:sidedef-front-sector sidedef)
                :color (sbsp:sidedef-color sidedef)
                :texinfo (sbsp:sidedef-texinfo sidedef)
                :geometry (sidedef->surf-triangles sidedef)))

(defun leaf->mleaf (leaf)
  (let* ((floor-sector (first (mapcar (lambda (surf)
                                        (sbsp:sidedef-front-sector surf))
                                      (sbsp:leaf-surfaces leaf))))
         (floor-height (if floor-sector
                           (sbsp:sector-floor-height floor-sector)
                           0))
         (sector-points (remove-duplicates
                         (mappend (lambda (surf)
                                    (let ((line (sbsp:sidedef-lineseg surf)))
                                      (list (sbsp:lineseg-start line)
                                            (sbsp:lineseg-end line))))
                                  (sbsp:leaf-surfaces leaf))
                         :test #'v=))
         (ccw-points (when (length>= 3 sector-points)
                       (sbrush::construct-convex-hull sector-points)))
         (sector-poly (when (length>= 3 ccw-points)
                        (apply #'sbsp:make-linedef-loop ccw-points))))
    (make-mleaf :bounds (sbsp:leaf-bounds leaf)
                :surfaces (sbsp:leaf-surfaces leaf)
                :contents (sbsp:leaf-contents leaf)
                :floor-geometry (when (and floor-sector sector-poly)
                                  (polygon->surf-triangles sector-poly
                                                           floor-height)))))

(defun nadapt-nodes (bsp)
  (sbsp:bsp-rec bsp
                (lambda (node front back)
                  (setf (sbsp:node-front node) (funcall front)
                        (sbsp:node-back node) (funcall back))
                  node)
                (lambda (leaf)
                  (zap (curry #'mapcar #'sidedef->surface)
                       (sbsp:leaf-surfaces leaf))
                  (leaf->mleaf leaf))))

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
                     (with-struct (mleaf- surfaces floor-geometry) leaf
                       (when floor-geometry
                         (free-surf-triangles floor-geometry))
                       (dolist (surf surfaces)
                         (free-surf-triangles (surface-geometry surf))))))))
