;;;; Game definition and logic

(in-package #:shake)

(defstruct game
  (think-things nil :type list)
  (render-things nil :type list))

(defclass thing ()
  ())

(defgeneric thing-think (thing)
  (:documentation "Run thinking for this THING")
  (:method ((thing thing))
    "Default THING-THINK method is a no-op."
    nil))

(defgeneric game-add-thing (game thing)
  (:documentation "Register THING in GAME.")
  (:method (game (thing thing))
    "Add THING to list of all things in GAME."
    (check-type game game)
    (push thing (game-think-things game))))

(defclass weapon (thing)
  ((model :type smdl::obj-model
          :initarg :model :accessor weapon-model)
   (position :initform (v 0 0 0) :type (vec 3)
             :initarg :position :accessor weapon-position)
   (scale :initform #.(shiva-float 1.0) :type shiva-float
          :initarg :scale :accessor weapon-scale)
   (angle-y :initform #.(shiva-float 0.0) :type shiva-float
            :initarg :angle-y :accessor weapon-angle-y)
   (bounds-scale :initform (v 1 1 1) :type (vec 3)
                 :initarg :bounds-scale :accessor weapon-bounds-scale)))

(defmethod game-add-thing :after (game (weapon weapon))
  (push weapon (game-render-things game)))

(defmethod thing-think :after ((weapon weapon))
  (setf (weapon-angle-y weapon) (shiva-float (mod (get-time) 360))))

(defun make-shotgun (image-manager model-manager position &key (angle-y #.(shiva-float 0.0)))
  (check-type position (vec 3))
  (srend::load-image-from-file image-manager "shotgun.bmp")
  (let* ((shotgun-model (smdl:get-model model-manager "shotgun.obj"))
         (scale #.(shiva-float 0.125))
         (bounds-scale (vscale (* 0.5 scale)
                               (v- (smdl::obj-model-max-bounds shotgun-model)
                                   (smdl::obj-model-min-bounds shotgun-model)))))
    (assert (v= (vscale 0.5 (v+ (smdl::obj-model-min-bounds shotgun-model)
                                (smdl::obj-model-max-bounds shotgun-model)))
                (v 0 0 0)))
    (setf (smdl::surf-triangles-tex-name (smdl::obj-model-verts shotgun-model)) "shotgun.bmp")
    (make-instance 'weapon
                   :model shotgun-model
                   :position position
                   :angle-y angle-y
                   :scale scale
                   :bounds-scale bounds-scale)))

(defun weapon-view-transform (weapon)
  (check-type weapon weapon)
  (let ((pos (weapon-position weapon))
        (scale (weapon-scale weapon)))
    (m* (m* (translation :x (vx pos) :y (vy pos) :z (vz pos))
            (rotation (v 0 1 0) (* deg->rad (weapon-angle-y weapon))))
        (scale :x scale :y scale :z scale))))

(defun weapon-render (weapon camera model-manager)
  (check-type weapon weapon)
  (check-type camera camera)
  (check-type model-manager smdl::model-manager)
  (let* ((view-project (m* (camera-projection-matrix camera)
                           (camera-view-transform camera)))
         (mvp (m* view-project (weapon-view-transform weapon))))
    ;; Render the bounds cube
    (let ((pos (weapon-position weapon))
          (bounds-scale (v+ (weapon-bounds-scale weapon)
                            (v (* 0.5 sbsp::+clip-square+)
                               0.25
                               (* 0.5 sbsp::+clip-square+)))))
      (srend:render-surface
       (smdl::obj-model-verts (smdl:model-manager-default-model model-manager))
       (m* view-project
           (m* (translation :x (vx pos) :y (vy pos) :z (vz pos))
               (m* (rotation (v 0 1 0) (* deg->rad (weapon-angle-y weapon)))
                   (scale :x (vx bounds-scale) :y (vy bounds-scale) :z (vz bounds-scale)))))
       :wireframep t))
    ;; Render the weapon model
    (srend:render-surface (smdl::obj-model-verts (weapon-model weapon)) mvp)))

(defclass enemy (thing)
  ((model :type smdl::obj-model
          :initarg :model :accessor enemy-model)
   (center :initform (v 0 0 0) :type (vec 3)
           :initarg :center :accessor enemy-center)
   (position :initform (v 0 0 0) :type (vec 3)
             :initarg :position :accessor enemy-position)
   (scale :initform #.(shiva-float 1.0) :type shiva-float
          :initarg :scale :accessor enemy-scale)
   (angle-y :initform #.(shiva-float 0.0) :type shiva-float
            :initarg :angle-y :accessor enemy-angle-y)
   (bounds-scale :initform (v 1 1 1) :type (vec 3)
                 :initarg :bounds-scale :accessor enemy-bounds-scale)
   ;; AI part
   (last-attack-time-ms :initform 0 :type fixnum
                        :initarg :last-attack-time-ms
                        :accessor enemy-last-attack-time-ms)
   (target :initform nil :type (or null player thing) :initarg :target
           :accessor enemy-target)
   (last-visible-target-pos :initform nil :type (or null (vec 3))
                            :initarg :last-visible-target-pos
                            :accessor enemy-last-visible-target-pos)))

(defmethod game-add-thing :after (game (enemy enemy))
  (push enemy (game-render-things game)))

(defun player-visible-p (enemy player)
  (declare (special smdl:*world-model*))
  (check-type smdl:*world-model* smdl:bsp-model)
  (check-type enemy enemy)
  (check-type player player)
  (let* ((eye-origin (v+ (enemy-position enemy) (v 0 0.5 0)))
         (forward-dir
          (vnormalize
           (vxyz (vtransform (rotation (v 0 1 0) (* deg->rad (enemy-angle-y enemy))) (v 0 0 -1 0)))))
         (player-center (v+ (player-position player) (v 0 0.5 0)))
         (-dir-to-player (v- eye-origin player-center))
         (hull (smdl:bsp-model-hull smdl:*world-model*)))
    (when (<= (cos (* deg->rad 60)) (vdot forward-dir -dir-to-player))
      ;; TODO: Think about targeting additional points on player beside center.
      (multiple-value-bind (mtrace hitp) (clip-hull hull eye-origin player-center)
        (declare (ignore mtrace))
        (not hitp)))))

(defun enemy-shoot (enemy)
  (check-type enemy enemy)
  (assert (enemy-target enemy))
  (let ((attack-cooldown-ms 2000)
        (eye-origin (v+ (enemy-position enemy) (v 0 0.5 0)))
        (player-center (v+ (player-position (enemy-target enemy)) (v 0 0.5 0))))

    (when (<= (enemy-last-attack-time-ms enemy) (- (get-game-time) attack-cooldown-ms))
      (let ((dir-to-player (vnormalize (v- player-center eye-origin))))
        (let* ((-dir (v- dir-to-player)) ; Projectile's forward dir is -Z
               (x-dir (vnormalize (vcross (v 0 1 0) -dir)))
               (y-dir (vnormalize (vcross -dir x-dir)))
               (rotation (mat (list (vx x-dir) (vx y-dir) (vx -dir) 0)
                              (list (vy x-dir) (vy y-dir) (vy -dir) 0)
                              (list (vz x-dir) (vz y-dir) (vz -dir) 0)
                              (list 0 0 0 1)))
               (gun-position (v+ eye-origin dir-to-player)))
          (declare (special *game* *image-manager* *model-manager*))
          (game-add-thing
           *game*
           (make-projectile *image-manager* *model-manager* gun-position :rotation rotation)))
        ;; (printf "Attacking player at ~A, time ~Ams~%"
        ;;         (player-position (enemy-target enemy)) (get-game-time))
        (setf (enemy-last-attack-time-ms enemy) (get-game-time))))))

(defun enemy-chase (enemy)
  (check-type enemy enemy)
  (with-accessors ((target-pos enemy-last-visible-target-pos)
                   (origin enemy-position)) enemy
    (when target-pos
      (if (v= target-pos origin)
          (setf target-pos nil)
          (let* ((speed #.(shiva-float 0.025))
                 (velocity (vscale speed (vnormalize (v- target-pos origin)))))
            ;; TODO: This uses PLAYER-GROUND-MOVE, make a common movement function
            (setf origin (apply-gravity (player-ground-move origin velocity))))))))

(defmethod thing-think :after ((enemy enemy))
  (declare (special *player*))
  (check-type *player* player)
  (with-accessors ((target enemy-target)) enemy
    (if target
        (progn
          ;; Turn toward target
          ;; TODO: Add max turn rate
          (let ((dir (v- (enemy-last-visible-target-pos enemy) (enemy-position enemy))))
            (setf (enemy-angle-y enemy)
                  (* rad->deg (atan (vx dir) (vz dir)))))
          (if (player-visible-p enemy target)
              (progn
                (setf (enemy-last-visible-target-pos enemy) (player-position target))
                (enemy-shoot enemy))
              (enemy-chase enemy)))
        (when (player-visible-p enemy *player*)
          (if (not target)
              (setf target *player*)
              ;; Shoot or chase
              (enemy-shoot enemy))
          (setf (enemy-last-visible-target-pos enemy) (player-position target))))))

(defun make-enemy (image-manager model-manager position &key (angle-y #.(shiva-float 0.0)))
  (check-type position (vec 3))
  (srend::load-image-from-file image-manager "enemy.bmp")
  (let* ((model (smdl:get-model model-manager "enemy.obj"))
         (center (vscale 0.5 (v+ (smdl::obj-model-max-bounds model)
                                 (smdl::obj-model-min-bounds model))))
         (bounds-scale (vscale 0.5 (v- (smdl::obj-model-max-bounds model)
                                       (smdl::obj-model-min-bounds model)))))
    (setf (smdl::surf-triangles-tex-name (smdl::obj-model-verts model)) "enemy.bmp")
    (make-instance 'enemy
                   :model model
                   :center center
                   :position position
                   :angle-y angle-y
                   :bounds-scale bounds-scale)))

(defun enemy-view-transform (enemy)
  (check-type enemy enemy)
  (let ((pos (enemy-position enemy))
        (scale (enemy-scale enemy)))
    (m* (m* (translation :x (vx pos) :y (vy pos) :z (vz pos))
            (rotation (v 0 1 0) (* deg->rad (enemy-angle-y enemy))))
        (scale :x scale :y scale :z scale))))

(defun enemy-render (enemy camera model-manager)
  (check-type enemy enemy)
  (check-type camera camera)
  (check-type model-manager smdl::model-manager)
  (let* ((view-project (m* (camera-projection-matrix camera)
                           (camera-view-transform camera)))
         (mvp (m* view-project (enemy-view-transform enemy))))
    ;; Render the bounds cube
    (let ((pos (v+ (enemy-center enemy) (enemy-position enemy)))
          (bounds-scale (enemy-bounds-scale enemy)))
      (srend:render-surface
       (smdl::obj-model-verts (smdl:model-manager-default-model model-manager))
       (m* view-project
           (m* (translation :x (vx pos) :y (vy pos) :z (vz pos))
               (m* (rotation (v 0 1 0) (* deg->rad (enemy-angle-y enemy)))
                   (scale :x (vx bounds-scale) :y (vy bounds-scale) :z (vz bounds-scale)))))
       :wireframep t))
    ;; Render the enemy model
    (srend:render-surface (smdl::obj-model-verts (enemy-model enemy)) mvp)))

(defclass projectile (thing)
  ((model :type smdl::obj-model
          :initarg :model :accessor projectile-model)
   (center :initform (v 0 0 0) :type (vec 3)
           :initarg :center :accessor projectile-center)
   (position :initform (v 0 0 0) :type (vec 3)
             :initarg :position :accessor projectile-position)
   (scale :initform #.(shiva-float 1.0) :type shiva-float
          :initarg :scale :accessor projectile-scale)
   (rotation :initform (shiva::midentity4) :type (mat 4)
             :initarg :rotation :accessor projectile-rotation)
   (bounds-scale :initform (v 1 1 1) :type (vec 3)
                 :initarg :bounds-scale :accessor projectile-bounds-scale)
   (explodedp :initform nil :type boolean :accessor projectile-explodedp)))

(defmethod game-add-thing :after (game (projectile projectile))
  (push projectile (game-render-things game)))

(defmethod thing-think :after ((projectile projectile))
  (declare (special *game*))
  (if (projectile-explodedp projectile)
      (progn
        (zap (lambda (scale) (+ scale 0.05))
             (projectile-scale projectile))
        (when (> (projectile-scale projectile) 0.5)
          (setf (game-render-things *game*)
                (remove projectile (game-render-things *game*)))
          (setf (game-think-things *game*)
                (remove projectile (game-think-things *game*)))))
      (let* ((forward-dir
              (vnormalize (vxyz (vtransform (projectile-rotation projectile) (v 0 0 -1 0)))))
             (speed #.(shiva-float 0.05))
             (velocity (vscale speed forward-dir))
             (origin (projectile-position projectile))
             (hull (smdl:bsp-model-hull smdl:*world-model*)))
        (multiple-value-bind (mtrace hitp) (clip-hull hull origin (v+ origin velocity))
          (multiple-value-bind (hit-enemy dist) (hit-enemy-p origin (mtrace-endpos mtrace))
            (when (or hit-enemy hitp)
              (setf (projectile-explodedp projectile) t))
            (when hit-enemy
              ;; NOTE: Removal is horribly inefficient.
              (setf (game-think-things *game*)
                    (remove hit-enemy (game-think-things *game*)))
              (setf (game-render-things *game*)
                    (remove hit-enemy (game-render-things *game*))))
            (let ((dest (if hit-enemy
                            (v+ origin (vscale dist forward-dir))
                            (mtrace-endpos mtrace))))
              (setf (projectile-position projectile) dest)))))))

(defun make-projectile (image-manager model-manager position &key (rotation (shiva::midentity4)))
  (check-type position (vec 3))
  (srend::load-image-from-file image-manager "projectile.bmp")
  (let* ((model (smdl:get-model model-manager "projectile.obj"))
         (center (vscale 0.5 (v+ (smdl::obj-model-max-bounds model)
                                 (smdl::obj-model-min-bounds model))))
         (scale #.(shiva-float 0.125))
         (bounds-scale (vscale (* (+ scale 0.05) 0.5)
                               (v- (smdl::obj-model-max-bounds model)
                                   (smdl::obj-model-min-bounds model)))))
    (setf (smdl::surf-triangles-tex-name (smdl::obj-model-verts model)) "projectile.bmp")
    (make-instance 'projectile
                   :model model
                   :scale scale
                   :center center
                   :position position
                   :rotation rotation
                   :bounds-scale bounds-scale)))

(defun projectile-view-transform (projectile)
  (check-type projectile projectile)
  (let ((pos (projectile-position projectile))
        (scale (projectile-scale projectile))
        (rotation (projectile-rotation projectile)))
    (m* (m* (translation :x (vx pos) :y (vy pos) :z (vz pos))
            rotation)
        (scale :x scale :y scale :z scale))))

(defun projectile-render (projectile camera model-manager)
  (check-type projectile projectile)
  (check-type camera camera)
  (check-type model-manager smdl::model-manager)
  (let* ((view-project (m* (camera-projection-matrix camera)
                           (camera-view-transform camera)))
         (mvp (m* view-project (projectile-view-transform projectile))))
    ;; Render the bounds cube
    (let ((pos (v+ (projectile-center projectile) (projectile-position projectile)))
          (bounds-scale (projectile-bounds-scale projectile)))
      (srend:render-surface
       (smdl::obj-model-verts (smdl:model-manager-default-model model-manager))
       (m* view-project
           (m* (translation :x (vx pos) :y (vy pos) :z (vz pos))
               (m* (projectile-rotation projectile)
                   (scale :x (vx bounds-scale) :y (vy bounds-scale) :z (vz bounds-scale)))))
       :wireframep t))
    ;; Render the projectile model
    (srend:render-surface (smdl::obj-model-verts (projectile-model projectile)) mvp)))

(defclass door (thing)
  ((brush :type sbrush:brush
          :initarg :brush :accessor door-brush)
   (hull :type (or sbsp:node sbsp:leaf)
         :initarg :hull :accessor door-hull)
   (open-height :initform #.(shiva-float 0.95) :type shiva-float
                :initarg :open-height :accessor door-open-height
                :documentation "Height of open door relative to floor")
   (last-open-time-ms :initform nil :type (or null fixnum)
                      :initarg :last-open-time-ms :accessor door-last-open-time-ms)
   (ms-to-open :initform 2000 :type fixnum :initarg :ms-to-open :accessor door-ms-to-open)
   (ms-to-close :initform 2000 :type fixnum :initarg :ms-to-close :accessor door-ms-to-close)
   (keep-open-ms :initform 2000 :type fixnum
                 :initarg :keep-open-ms :accessor door-keep-open-ms)
   (surfs :initform nil :type list :initarg :surfs :accessor door-surfs)))

(defmethod game-add-thing :after (game (door door))
  (push door (game-render-things game)))

(defmethod thing-think :after ((door door))
  (declare (optimize (speed 3) (space 3)))
  (let* ((sector (sbsp:sidedef-back-sector
                  (first (sbrush:brush-surfaces (door-brush door)))))
         (floor-height (sbsp:sector-floor-height sector)))
    (flet ((set-height (frac)
             (let ((open-height (door-open-height door)))
               (declare (type shiva-float frac open-height))
               (setf (sbsp:sector-ceiling-height sector)
                     (+ floor-height (* frac open-height))))))
      (with-accessors ((last-open-time-ms door-last-open-time-ms)) door
        (if (not last-open-time-ms)
            (setf (sbsp:sector-ceiling-height sector) (sbsp:sector-floor-height sector))
            (let ((opened-ms (- (get-game-time) (the fixnum last-open-time-ms)))
                  (ms-to-open (door-ms-to-open door))
                  (ms-to-close (door-ms-to-close door))
                  (keep-open-ms (door-keep-open-ms door)))
              (declare (type fixnum ms-to-open ms-to-close keep-open-ms))
              (cond
                ((<= opened-ms (+ ms-to-open keep-open-ms))
                 (set-height (min (/ (shiva-float opened-ms) ms-to-open) #.(shiva-float 1.0))))
                ((<= (+ ms-to-open keep-open-ms) opened-ms (+ ms-to-open keep-open-ms ms-to-close))
                 (set-height
                  (- #.(shiva-float 1.0)
                     (min (/ (shiva-float (- opened-ms ms-to-open keep-open-ms)) ms-to-close)
                          #.(shiva-float 1.0)))))
                (t
                 (setf last-open-time-ms nil)))))))))

(defun make-door (brush)
  (check-type brush sbrush:brush)
  ;; Assert is just for testing the plain brush
  (unless (notany #'sbsp:sidedef-back-sector (sbrush:brush-surfaces brush))
    (print-warning "door already has back sector~%"))
  (let ((sector (sbsp:make-sector :contents :contents-solid
                                  :ceiling-height #.(shiva-float 0.0))))
    ;; Set the same instance on all sides, so we can mutate them all at once.
    (dolist (sidedef (sbrush:brush-surfaces brush))
      (setf (sbsp:sidedef-back-sector sidedef) sector))
    (let ((hull (sbsp:build-bsp
                 (sbrush:prepare-brushes-for-bsp
                  (list (sbrush:expand-brush brush :square sbsp::+clip-square+)))))
          (lines (mapcar (lambda (surf)
                           (sbsp:lineseg-orig-line (sbsp:sidedef-lineseg surf)))
                         (sbrush:brush-surfaces brush)))
          (ceiling-height (sbsp:sector-ceiling-height sector)))
      ;; Set the same instance to clip hull, so it's mutated as above.
      (sbsp:bsp-trav hull
                     (lambda (front back)
                       (declare (ignore front back)))
                     (lambda (leaf)
                       (when (eq (sbsp:leaf-contents leaf) :contents-solid)
                         (assert (equalp (sbsp::subsector-orig-sector (sbsp::leaf-subsector leaf)) sector))
                         (setf (sbsp::subsector-orig-sector (sbsp::leaf-subsector leaf))
                               sector))))
      (make-instance 'door
                     :brush brush
                     :hull hull
                     ;; TODO: Free these surfaces. We should probably create a
                     ;; single model and store it in model manager.
                     :surfs (cons
                             (smdl::polygon->surf-triangles lines ceiling-height)
                             (mapcar #'smdl::sidedef->surf-triangles
                                     (sbrush:brush-surfaces brush)))))))

(defun door-view-transform (door)
  (check-type door door)
  (let* ((sector (sbsp:sidedef-back-sector
                  (first (sbrush:brush-surfaces (door-brush door)))))
         (y-pos (- (sbsp:sector-ceiling-height sector)
                   (sbsp:sector-floor-height sector))))
    (translation :y y-pos)))

(defun door-render (door camera)
  (check-type door door)
  (check-type camera camera)
  (let* ((view-project (m* (camera-projection-matrix camera)
                           (camera-view-transform camera)))
         (mvp (m* view-project (door-view-transform door))))
    (dolist (surf (door-surfs door))
      (srend:render-surface surf mvp))))

(defun door-open (door)
  (check-type door door)
  (with-accessors ((last-open-time-ms door-last-open-time-ms)) door
    (unless last-open-time-ms
      (setf last-open-time-ms (get-game-time)))))

(defclass test-model-thing (thing)
  ((model :type smdl::obj-model
          :initarg :model :accessor test-model-thing-model)
   (center :initform (v 0 0 0) :type (vec 3)
           :initarg :center :accessor test-model-thing-center)
   (position :initform (v 0 0 0) :type (vec 3)
             :initarg :position :accessor test-model-thing-position)
   (scale :initform #.(shiva-float 1.0) :type shiva-float
          :initarg :scale :accessor test-model-thing-scale)
   (rotation :initform (shiva::midentity4) :type (mat 4)
             :initarg :rotation :accessor test-model-thing-rotation)
   (bounds-scale :initform (v 1 1 1) :type (vec 3)
                 :initarg :bounds-scale :accessor test-model-thing-bounds-scale)
   (image-file :initarg :image-file :type string
               :accessor test-model-thing-image-file)))

(defmethod game-add-thing :after (game (test-model test-model-thing))
  (push test-model (game-render-things game)))

(defun make-test-model-thing (image-manager model-manager position
                              &key (rotation (shiva::midentity4)) image-file model-file)
  (check-type position (vec 3))
  (check-type image-file string)
  (check-type model-file string)
  (srend::load-image-from-file image-manager image-file)
  (let* ((model (smdl:get-model model-manager model-file))
         (center (vscale 0.5 (v+ (smdl::obj-model-max-bounds model)
                                 (smdl::obj-model-min-bounds model))))
         (scale #.(shiva-float 1))
         (bounds-scale (vscale 0.5
                               (v- (smdl::obj-model-max-bounds model)
                                   (smdl::obj-model-min-bounds model)))))
    (make-instance 'test-model-thing
                   :image-file image-file
                   :model model
                   :scale scale
                   :center center
                   :position position
                   :rotation rotation
                   :bounds-scale bounds-scale)))

(defun test-model-thing-view-transform (test-model)
  (check-type test-model test-model-thing)
  (let ((pos (test-model-thing-position test-model))
        (scale (test-model-thing-scale test-model))
        (rotation (test-model-thing-rotation test-model)))
    (m* (m* (translation :x (vx pos) :y (vy pos) :z (vz pos))
            rotation)
        (scale :x scale :y scale :z scale))))

(defun test-model-thing-render (test-model camera)
  (check-type test-model test-model-thing)
  (check-type camera camera)
  (let* ((view-project (m* (camera-projection-matrix camera)
                           (camera-view-transform camera)))
         (mvp (m* view-project (test-model-thing-view-transform test-model))))
    ;; Render the test model
    (setf (smdl::surf-triangles-tex-name (smdl::obj-model-verts (test-model-thing-model test-model)))
          (test-model-thing-image-file test-model))
    (srend:render-surface (smdl::obj-model-verts (test-model-thing-model test-model)) mvp)))
