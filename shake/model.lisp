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
  ;; TODO: Texture should be wired differently
  (tex-name nil :type (or null string)))

(declaim (inline make-l-vertex-data))
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
             (loop for i fixnum from 0 below (array-total-size v) do
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
              (loop for i fixnum from 0 below (array-total-size v) do
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
  (verts nil :type surf-triangles)
  (min-bounds (v 0 0 0) :type (vec 3))
  (max-bounds (v 0 0 0) :type (vec 3)))

(declaim (ftype (function (sbsp::sidedef) (values shiva-float shiva-float)) sidedef-uv))
(defun sidedef-uv (sidedef)
  ;; NOTE: This function doesn't cons, so keep it as such.
  (let ((texinfo (sbsp:sidedef-texinfo sidedef)))
    (if (or (null texinfo) (eq :caulk texinfo))
        (values #.(shiva-float 0) #.(shiva-float 1))
        (ecase (sbsp:texinfo-draw-mode texinfo)
          (:tile
           (with-struct (lineseg- start end orig-line)
               (sbsp:sidedef-lineseg sidedef)
             ;; Winding is clockwise (right to left), so reverse the
             ;; texture.
             (values (vdist start (linedef-end orig-line))
                     (vdist end (linedef-end orig-line)))))
          (:scale-to-fit
           (with-struct (lineseg- t-start t-end)
               (sbsp:sidedef-lineseg sidedef)
             ;; Reverse the coordinates, due to vertex winding.
             (values (- #.(shiva-float 1) t-start) (- #.(shiva-float 1) t-end))))))))

(defun sidedef-fill-vertex-data (sidedef ptr)
  ;; NOTE: This function conses very little. SB-PROFILE sometimes reports
  ;; consing, because it tracks GC page level allocation, so after multiple
  ;; invocations will this will actually be reported as consing.
  (with-struct (lineseg- start end) (sbsp:sidedef-lineseg sidedef)
    (let* ((front-sector (sbsp:sidedef-front-sector sidedef))
           (back-sector (sbsp:sidedef-back-sector sidedef))
           (floor-bottom (aif front-sector
                              (sbsp:sector-floor-height it)
                              #.(shiva-float 0)))
           (floor-top (aif back-sector
                           (sbsp:sector-floor-height it)
                           #.(shiva-float 1)))
           (ceiling-top (aif front-sector
                             (sbsp:sector-ceiling-height it)
                             #.(shiva-float 1)))
           (ceiling-bottom (aif back-sector
                                (sbsp:sector-ceiling-height it)
                                #.(shiva-float 1))))
      (when (float< (shiva-float floor-top) (shiva-float floor-bottom))
        (rotatef floor-bottom floor-top))
      (when (float< (shiva-float ceiling-top) (shiva-float ceiling-bottom))
        (rotatef ceiling-bottom ceiling-top))
      (multiple-value-bind (u-start u-end) (sidedef-uv sidedef)
        (flet ((set-data (index pos u v)
                 (let* ((uv (make-array 2 :element-type 'shiva-float))
                        (data
                         (make-l-vertex-data
                          :position pos
                          :normal (v2->v3 (sbsp:lineseg-normal (sbsp:sidedef-lineseg sidedef)))
                          :uv uv
                          :color (sbsp:sidedef-color sidedef))))
                   (declare (dynamic-extent uv data))
                   (vf uv u v)
                   ;; NOTE: This conses for some reason
                   (setf (cffi:mem-aref ptr '(:struct vertex-data) index) data))))
          (let ((start-3d-bot (make-array 3 :element-type 'shiva-float))
                (end-3d-bot (make-array 3 :element-type 'shiva-float))
                (start-3d-top (make-array 3 :element-type 'shiva-float))
                (end-3d-top (make-array 3 :element-type 'shiva-float)))
            (declare (dynamic-extent start-3d-bot end-3d-bot start-3d-top end-3d-top))
            (v2->v3f start-3d-bot start floor-bottom)
            (v2->v3f end-3d-bot end floor-bottom)
            (v2->v3f start-3d-top start floor-top)
            (v2->v3f end-3d-top end floor-top)
            (set-data 0 start-3d-top u-start 0)
            (set-data 1 end-3d-top u-end 0)
            (set-data 2 start-3d-bot u-start 1)
            (set-data 3 start-3d-bot u-start 1)
            (set-data 4 end-3d-top u-end 0)
            (set-data 5 end-3d-bot u-end 1))
          (when (or front-sector back-sector)
            (let ((start-3d-bot (make-array 3 :element-type 'shiva-float))
                  (end-3d-bot (make-array 3 :element-type 'shiva-float))
                  (start-3d-top (make-array 3 :element-type 'shiva-float))
                  (end-3d-top (make-array 3 :element-type 'shiva-float)))
              (declare (dynamic-extent start-3d-bot end-3d-bot start-3d-top end-3d-top))
              (v2->v3f start-3d-bot start ceiling-bottom)
              (v2->v3f end-3d-bot end ceiling-bottom)
              (v2->v3f start-3d-top start ceiling-top)
              (v2->v3f end-3d-top end ceiling-top)
              (set-data 6 start-3d-top u-start 0)
              (set-data 7 end-3d-top u-end 0)
              (set-data 8 start-3d-bot u-start 1)
              (set-data 9 start-3d-bot u-start 1)
              (set-data 10 end-3d-top u-end 0)
              (set-data 11 end-3d-bot u-end 1))))))))

(defun sidedef->surf-triangles (sidedef)
  (let* ((tex-name (aif (sbsp:sidedef-texinfo sidedef)
                        (string-downcase (sbsp:texinfo-name it))))
         (num-verts (if (or (sbsp:sidedef-front-sector sidedef)
                            (sbsp:sidedef-back-sector sidedef))
                        12
                        6))
         (verts-ptr (cffi:foreign-alloc '(:struct vertex-data) :count num-verts)))
    (sidedef-fill-vertex-data sidedef verts-ptr)
    (make-surf-triangles :num-verts num-verts
                         :verts-byte-size (* num-verts (cffi:foreign-type-size
                                                        '(:struct vertex-data)))
                         :verts verts-ptr
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
         (verts (cffi:foreign-alloc '(:struct vertex-data) :count num-verts))
         (min-bounds nil)
         (max-bounds nil))
    (loop for face across (sobj:obj-element-data obj) with i = 0 do
         (loop for point across (sobj:element-points face)
            for position = (sobj:vertex-val (aref (sobj:obj-geometric-vertices obj)
                                                  (sobj:vref-geometric point)))
            for normal = (sobj:vertex-val (aref (sobj:obj-normal-vertices obj)
                                                (sobj:vref-normal point)))
            for uv = (sobj:vertex-val (aref (sobj:obj-texture-vertices obj)
                                            (sobj:vref-texture point)))
            do (progn
                 (unless (and min-bounds max-bounds)
                   (setf min-bounds (vxyz position))
                   (setf max-bounds (vxyz position)))
                 (dotimes (i 3)
                   (when (< (aref position i) (aref min-bounds i))
                     (setf (aref min-bounds i) (aref position i)))
                   (when (> (aref position i) (aref max-bounds i))
                     (setf (aref max-bounds i) (aref position i))))
                 (setf (cffi:mem-aref verts '(:struct vertex-data) i)
                       (make-l-vertex-data :position (vxyz position)
                                           :color (v 1 1 1)
                                           :normal (vxyz normal)
                                           :uv (vxy uv)))
                 (incf i))))
    (assert (and min-bounds max-bounds))
    (make-obj-model
     :min-bounds min-bounds :max-bounds max-bounds
     :verts
     (make-surf-triangles :num-verts num-verts
                          :verts-byte-size (* num-verts (cffi:foreign-type-size
                                                         '(:struct vertex-data)))
                          :verts verts))))

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
             (shake:print-error "unknown model format ~S" model-fname)
             (make-default-model)))))
    (sdata:data-file-error (c)
      (shake:print-error "~A~%" c)
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

(defun reload-model (model model-fname)
  "Try to reload a MODEL from MODEL-FNAME.  In case of error, print the error
and return leave MODEL unchanged."
  (check-type model obj-model)
  (check-type model-fname string)
  (shake:printf "Reloading model '~A'~%" model-fname)
  (handler-case
      (with-data-file (file model-fname)
        (let ((ext (pathname-type model-fname)))
          (cond
            ((string= "obj" ext)
             (let ((new-model (obj->model (sobj:read-obj file))))
               (free-model model)
               (setf (surf-triangles-tex-name (obj-model-verts new-model))
                     (surf-triangles-tex-name (obj-model-verts model)))
               (setf (obj-model-verts model) (obj-model-verts new-model))
               (setf (obj-model-min-bounds model) (obj-model-min-bounds new-model))
               (setf (obj-model-max-bounds model) (obj-model-max-bounds new-model))))
            (t
             (shake:print-error "unknown model format ~S" model-fname))))
        (shake:printf "Reloaded model '~A'~%" model-fname))
    (sdata:data-file-error (c)
      (shake:print-error "~A~%" c))))

(declaim (inline make-model-manager))
(defstruct model-manager
  "All model loading and unloading should go through this."
  (world-model nil :type (or null bsp-model))
  (models (make-hash-table :test #'equal) :type hash-table)
  ;; Default model is the cube
  (default-model nil :type obj-model :read-only t))

(defun make-default-model ()
  "Return the default cube model, size is 2 (-1, 1)."
  (let ((model (with-input-from-string (s sobj::+cube+)
                 (obj->model (sobj:read-obj s)))))
    (setf (surf-triangles-tex-name (obj-model-verts model)) "missing.bmp")
    model))

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
  (check-type model-manager model-manager)
  (check-type name string)
  (or (gethash name (model-manager-models model-manager))
      (add-model model-manager (load-model name) :name name)))

(defun model-manager-reload-models (model-manager)
  (check-type model-manager model-manager)
  (maphash (lambda (model-fname model)
             (when (obj-model-p model) (reload-model model model-fname)))
           (model-manager-models model-manager)))
