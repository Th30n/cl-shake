;;;; Loading and managing various 3D models

(in-package #:shake.model)

(defvar *world-model* nil "Currently loaded 3D model of the world (map)")

(defstruct surf-triangles
  "Stores surface triangles ready for rendering.
  VERTS is a foreign array of `C-VERTEX-DATA', ready for sending to GPU.
  TEX-NAME is the name of texture image used, can be NIL."
  (num-verts 0 :type fixnum)
  (verts-byte-size 0 :type fixnum)
  verts
  (tex-name nil :type (or null string)))

(cffi:defcstruct (vertex-data :class c-vertex-data)
  (position :float :count 3)
  (color :float :count 3)
  (normal :float :count 3)
  (uv :float :count 2))

(defstruct l-vertex-data
  (position (v 0 0 0) :type (vec 3) :read-only t)
  (color (v 0 0 0) :type (vec 3) :read-only t)
  (normal (v 0 0 0) :type (vec 3) :read-only t)
  (uv (v 0 0) :type (vec 2) :read-only t))

(defmethod cffi:translate-from-foreign (ptr (type c-vertex-data))
  (cffi:with-foreign-slots ((position color normal uv) ptr (:struct vertex-data))
    (flet ((vec-from-foreign (ptr count)
             ;; NOTE: cffi:foreign-array-to-lisp cannot be used since we
             ;; convert C float array to Lisp double array.
             (apply #'v (loop for i from 0 below count collect
                             (cffi:mem-aref ptr :float i)))))
      (make-l-vertex-data :position (vec-from-foreign position 3)
                          :color (vec-from-foreign color 3)
                          :normal (vec-from-foreign normal 3)
                          :uv (vec-from-foreign uv 2)))))

(defmethod cffi:expand-from-foreign (ptr (type c-vertex-data))
  `(cffi:with-foreign-slots ((position color normal uv) ,ptr (:struct vertex-data))
     (flet ((vec-from-foreign (ptr count)
              (apply #'v (loop for i from 0 below count collect
                              (cffi:mem-aref ptr :float i)))))
       (make-l-vertex-data :position (vec-from-foreign position 3)
                           :color (vec-from-foreign color 3)
                           :normal (vec-from-foreign normal 3)
                           :uv (vec-from-foreign uv 2)))))

(defmethod cffi:translate-into-foreign-memory (vd (type c-vertex-data) ptr)
  (cffi:with-foreign-slots ((position color normal uv) ptr (:struct vertex-data))
    (flet ((vec-to-foreign (v ptr)
             ;; NOTE: cffi:lisp-array-to-foreign cannot be used since we
             ;; convert Lisp double array to C float array.
             (loop for i from 0 below (array-total-size v) do
                  (setf (cffi:mem-aref ptr :float i)
                        (coerce (aref v i) 'single-float)))))
      (vec-to-foreign (l-vertex-data-position vd) position)
      (vec-to-foreign (l-vertex-data-color vd) color)
      (vec-to-foreign (l-vertex-data-normal vd) normal)
      (vec-to-foreign (l-vertex-data-uv vd) uv))))

(defmethod cffi:expand-into-foreign-memory (vd (type c-vertex-data) ptr)
  `(cffi:with-foreign-slots ((position color normal uv) ,ptr (:struct vertex-data))
     (flet ((vec-to-foreign (v ptr)
              ;; NOTE: cffi:lisp-array-to-foreign cannot be used since we
              ;; convert Lisp double array to C float array.
              (loop for i from 0 below (array-total-size v) do
                   (setf (cffi:mem-aref ptr :float i)
                         (coerce (aref v i) 'single-float)))))
       (vec-to-foreign (l-vertex-data-position ,vd) position)
       (vec-to-foreign (l-vertex-data-color ,vd) color)
       (vec-to-foreign (l-vertex-data-normal ,vd) normal)
       (vec-to-foreign (l-vertex-data-uv ,vd) uv))))

(defun free-surf-triangles (surf-triangles)
  (cffi:foreign-free (surf-triangles-verts surf-triangles)))

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
    (let* ((floor-bottom (aif (sbsp:sidedef-front-sector sidedef)
                              (sbsp:sector-floor-height it)
                              0))
           (floor-top (aif (sbsp:sidedef-back-sector sidedef)
                           (max floor-bottom (sbsp:sector-floor-height it))
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

(defun sidedef->surf-triangles (sidedef)
  (let* ((positions (make-triangles sidedef))
         (tex-name (aif (sbsp:sidedef-texinfo sidedef)
                        (string-downcase (sbsp:texinfo-name it))))
         (num-verts (list-length positions))
         (verts (cffi:foreign-alloc '(:struct vertex-data) :count num-verts)))
    (loop for pos in positions
       and uv in (or (make-texcoords sidedef)
                     (make-list 6 :initial-element (v 0 0)))
       and i from 0 do
         (setf (cffi:mem-aref verts '(:struct vertex-data) i)
               (make-l-vertex-data
                :position pos
                :color (sbsp:sidedef-color sidedef)
                :normal (v2->v3 (sbsp:lineseg-normal
                                 (sbsp:sidedef-lineseg sidedef)))
                :uv uv)))
    (make-surf-triangles :num-verts num-verts
                         :verts-byte-size (* num-verts (cffi:foreign-type-size
                                                        '(:struct vertex-data)))
                         :verts verts
                         :tex-name tex-name)))

(defun polygon->surf-triangles (polygon height)
  (let* ((triangles (sbsp:triangulate (mapcar #'sbsp:linedef->lineseg polygon)))
         (positions (mapcar #'sbsp:lineseg-start (apply #'append triangles)))
         (num-verts (list-length positions))
         (verts (cffi:foreign-alloc '(:struct vertex-data) :count num-verts)))
    (loop for pos in positions and i from 0 do
         (setf (cffi:mem-aref verts '(:struct vertex-data) i)
               (make-l-vertex-data :position (v2->v3 pos height)
                                   :color (v 1 0 0)
                                   :normal (v 0 1 0)
                                   :uv (v 0 0))))
    (make-surf-triangles :num-verts num-verts
                         :verts-byte-size (* num-verts (cffi:foreign-type-size
                                                        '(:struct vertex-data)))
                         :verts verts)))

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
