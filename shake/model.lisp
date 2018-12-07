;;;; Loading and managing various 3D models

(in-package #:shake.model)

(defvar *world-model* nil "Currently loaded 3D model of the world (map)")

(cffi:defcstruct (vertex-data :class c-vertex-data)
  (position :float :count 3)
  (color :float :count 3)
  (normal :float :count 3)
  (uv :float :count 2))

(defstruct surf-triangles
  "Stores surface triangles ready for rendering.
  VERTS is a foreign array of `C-VERTEX-DATA', ready for sending to GPU.
  TEX-NAME is the name of texture image used, can be NIL."
  (num-verts 0 :type fixnum :read-only t)
  (verts-byte-size 0 :type fixnum :read-only t)
  (verts nil :read-only t)
  (tex-name nil :type (or null string) :read-only t))

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
  floor-geometry
  ceiling-geometry)

(declaim (inline make-bsp-model))
(defstruct bsp-model
  "A 3D model. The NODES slot contains bsp nodes for rendering. The HULL slot
  contains bsp nodes for collision detection. Remaining entities are stored in
  THINGS slot."
  (nodes nil :type (or mleaf sbsp:node) :read-only t)
  hull
  things)

(declaim (inline make-obj-model))
(defstruct obj-model
  "A 3D model obtained from Wavefront OBJ file."
  (verts nil :type surf-triangles :read-only t))

(defun make-triangles (sidedef)
  (with-struct (lineseg- start end) (sbsp:sidedef-lineseg sidedef)
    (let* ((front-sector (sbsp:sidedef-front-sector sidedef))
           (back-sector (sbsp:sidedef-back-sector sidedef))
           (floor-bottom (aif front-sector
                              (sbsp:sector-floor-height it)
                              0))
           (floor-top (aif back-sector
                           (sbsp:sector-floor-height it)
                           1))
           (ceiling-top (aif front-sector
                             (sbsp:sector-ceiling-height it)
                             1))
           (ceiling-bottom (aif back-sector
                                (sbsp:sector-ceiling-height it)
                                1)))
      (when (float< (coerce floor-top 'double-float)
                    (coerce floor-bottom 'double-float))
        (rotatef floor-bottom floor-top))
      (when (float< (coerce ceiling-top 'double-float)
                    (coerce ceiling-bottom 'double-float))
        (rotatef ceiling-bottom ceiling-top))
      (append
       (let ((start-3d-bot (v2->v3 start floor-bottom))
             (end-3d-bot (v2->v3 end floor-bottom))
             (start-3d-top (v2->v3 start floor-top))
             (end-3d-top (v2->v3 end floor-top)))
         (list start-3d-top end-3d-top start-3d-bot
               start-3d-bot end-3d-top end-3d-bot))
       (when (or front-sector back-sector)
         (let ((start-3d-bot (v2->v3 start ceiling-bottom))
               (end-3d-bot (v2->v3 end ceiling-bottom))
               (start-3d-top (v2->v3 start ceiling-top))
               (end-3d-top (v2->v3 end ceiling-top)))
           (list start-3d-top end-3d-top start-3d-bot
                 start-3d-bot end-3d-top end-3d-bot)))))))

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
         (verts (cffi:foreign-alloc '(:struct vertex-data) :count num-verts))
         (texcoords (or (make-texcoords sidedef)
                        (make-list 6 :initial-element (v 0 0)))))
    (when (or (sbsp:sidedef-front-sector sidedef)
              (sbsp:sidedef-back-sector sidedef))
      (setf texcoords (append texcoords (copy-list texcoords))))
    (loop for pos in positions
       and uv in texcoords
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

(defun polygon->surf-triangles (polygon height &key (color (v 1 0 0)))
  (let* ((triangles (sbsp:triangulate (mapcar #'sbsp:linedef->lineseg polygon)))
         (positions (mapcar #'sbsp:lineseg-start (apply #'append triangles)))
         (num-verts (list-length positions))
         (verts (cffi:foreign-alloc '(:struct vertex-data) :count num-verts)))
    (loop for pos in positions and i from 0 do
         (setf (cffi:mem-aref verts '(:struct vertex-data) i)
               (make-l-vertex-data :position (v2->v3 pos height)
                                   :color color
                                   :normal (v 0 1 0)
                                   :uv (v 0 0))))
    (make-surf-triangles :num-verts num-verts
                         :verts-byte-size (* num-verts (cffi:foreign-type-size
                                                        '(:struct vertex-data)))
                         :verts verts)))

(defun obj->model (obj)
  (declare (type sobj:obj obj))
  ;; This assumes that object only consists of triangles.
  (let* ((num-verts (* 3 (length (sobj:obj-element-data obj))))
         (verts (cffi:foreign-alloc '(:struct vertex-data) :count num-verts)))
    (loop for face across (sobj:obj-element-data obj) with i = 0 do
         (loop for point across (sobj:element-points face)
            for position = (sobj:vertex-val (aref (sobj:obj-geometric-vertices obj)
                                                  (sobj:vref-geometric point)))
            for normal = (sobj:vertex-val (aref (sobj:obj-normal-vertices obj)
                                                (sobj:vref-normal point)))
            for uv = (sobj:vertex-val (aref (sobj:obj-texture-vertices obj)
                                            (sobj:vref-texture point)))
            do (progn
                 (setf (cffi:mem-aref verts '(:struct vertex-data) i)
                       (make-l-vertex-data :position (vxyz position)
                                           :color (v 1 1 1)
                                           :normal (vxyz normal)
                                           :uv (vxy uv)))
                 (incf i))))
    (make-obj-model
     :verts
     (make-surf-triangles :num-verts num-verts
                          :verts-byte-size (* num-verts (cffi:foreign-type-size
                                                         '(:struct vertex-data)))
                          :verts verts
                          :tex-name "missing.bmp"))))

(defun sidedef->surface (sidedef)
  (make-surface :lineseg (sbsp:sidedef-lineseg sidedef)
                :front-sector (sbsp:sidedef-front-sector sidedef)
                :color (sbsp:sidedef-color sidedef)
                :texinfo (sbsp:sidedef-texinfo sidedef)
                :geometry (sidedef->surf-triangles sidedef)))

(defun dbg-sector-color ()
  (v (/ (+ 15 (random 240)) 256d0)
     (/ (+ 15 (random 240)) 256d0)
     (/ (+ 15 (random 240)) 256d0)))

(defun leaf->mleaf (leaf)
  (let* ((subsector (sbsp::leaf-subsector leaf))
         (sector (sbsp::subsector-orig-sector subsector))
         (floor-height (if sector
                           (sbsp:sector-floor-height sector)
                           0))
         (ceiling-height (if sector
                           (sbsp:sector-ceiling-height sector)
                           1)))
    (make-mleaf :bounds (sbsp:leaf-bounds leaf)
                :surfaces (sbsp:leaf-surfaces leaf)
                :contents (sbsp:leaf-contents leaf)
                :subsector (sbsp::leaf-subsector leaf)
                :floor-geometry
                (when-let ((lines (sbsp::subsector-lines subsector)))
                  (polygon->surf-triangles
                   lines
                   floor-height
                   :color (dbg-sector-color)))
                :ceiling-geometry
                (when-let ((lines (sbsp::subsector-lines subsector)))
                  (polygon->surf-triangles
                   lines
                   ceiling-height
                   :color (v 0.1 0.1 0.1))))))

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
  (make-bsp-model :hull (sbsp:bspfile-clip-nodes bspfile)
                  :nodes (nadapt-nodes (sbsp:bspfile-nodes bspfile))
                  :things (sbsp:bspfile-things bspfile)))

(defun load-model (model-fname)
  "Try to load a 3D model from MODEL-FNAME.  In case of error, print the error
and return the default model."
  (handler-case
      (with-data-file (file model-fname)
        (let ((ext (pathname-type model-fname)))
          (cond
            ((string= "obj" ext)
             (obj->model (sobj:read-obj file)))
            ((string= "bsp" ext)
             (bspfile->model (sbsp:read-bspfile file)))
            (t
             (format t "Unknown model format ~S" model-fname)
             (make-default-model)))))
    (sdata:data-file-error (c)
      (princ c)
      (make-default-model))))

(defun free-model (model)
  (declare (type (or obj-model bsp-model) model))
  (etypecase model
    (obj-model (free-surf-triangles (obj-model-verts model)))
    (bsp-model
     (with-struct (bsp-model- nodes) model
       (sbsp:bsp-trav nodes (constantly nil)
                      (lambda (leaf)
                        (with-struct (mleaf- surfaces floor-geometry ceiling-geometry) leaf
                          (when floor-geometry
                            (free-surf-triangles floor-geometry))
                          (when ceiling-geometry
                            (free-surf-triangles ceiling-geometry))
                          (dolist (surf surfaces)
                            (free-surf-triangles (surface-geometry surf))))))))))

(declaim (inline make-model-manager))
(defstruct model-manager
  "All model loading and unloading should go through this."
  (world-model nil :type (or null bsp-model))
  (models (make-hash-table :test #'equal) :type hash-table)
  ;; Default model is the cube
  (default-model nil :type obj-model :read-only t))

(defun make-default-model ()
  "Return the default cube model, size is 2 (-1, 1)."
  (with-input-from-string (s sobj::+cube+)
    (obj->model (sobj:read-obj s))))

(defun init-model-manager ()
  (make-model-manager
   :default-model (make-default-model)))

(defun shutdown-model-manager (model-manager)
  (declare (type model-manager model-manager))
  (maphash-values #'free-model (model-manager-models model-manager))
  (free-model (model-manager-default-model model-manager)))

(defmacro with-model-manager (model-manager &body body)
  `(bracket (,model-manager (init-model-manager) shutdown-model-manager)
     ,@body))

(defun add-model (model-manager model &key name)
  (declare (type model-manager model-manager)
           (type (or bsp-model obj-model))
           (type string name))
  (setf (gethash name (model-manager-models model-manager)) model))

(defun get-model (model-manager name)
  (declare (type model-manager model-manager)
           (type string name))
  (or (gethash name (model-manager-models model-manager))
      (add-model model-manager (load-model name) :name name)))
