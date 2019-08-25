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
  ((model :initform nil :type smdl::obj-model
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
  ((model :initform nil :type smdl::obj-model
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
  ((model :initform nil :type smdl::obj-model
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
  ((brush :initform nil :type sbrush:brush
          :initarg :brush :accessor door-brush)
   (hull :initform nil :type (or sbsp:node sbsp:leaf)
         :initarg :hull :accessor door-hull)
   (last-open-time-ms :initform nil :type (or null fixnum)
                      :initarg :last-open-time-ms :accessor door-last-open-time-ms)))

(defmethod game-add-thing :after (game (door door))
  (push door (game-render-things game)))

(defmethod thing-think :after ((door door))
  (let ((sector (sbsp:sidedef-back-sector
                 (first (sbrush:brush-surfaces (door-brush door))))))
    (with-accessors ((last-open-time-ms door-last-open-time-ms)) door
      (if (not last-open-time-ms)
          (setf (sbsp:sector-ceiling-height sector) (sbsp:sector-floor-height sector))
          (let ((opened-ms (- (get-game-time) last-open-time-ms)))
            (cond
              ((<= opened-ms 4000)
               (setf (sbsp:sector-ceiling-height sector)
                     (shiva-float (min (/ opened-ms 2000.0) 1.0))))
              ((<= 4000 opened-ms 6000)
               (setf (sbsp:sector-ceiling-height sector)
                     (shiva-float (max (- 1.0 (/ (- opened-ms 4000) 2000.0))
                                       (sbsp:sector-floor-height sector)))))
              (t
               (setf last-open-time-ms nil))))))))

(defun make-door (brush)
  (check-type brush sbrush:brush)
  ;; Assert is just for testing the plain brush
  (unless (notany #'sbsp:sidedef-back-sector (sbrush:brush-surfaces brush))
    (print-warning "door already has back sector~%"))
  (let ((sector (sbsp:make-sector)))
    (dolist (sidedef (sbrush:brush-surfaces brush))
      (setf (sbsp:sidedef-back-sector sidedef) sector)))
  (make-instance 'door
                 :brush brush
                 :hull (sbsp:build-bsp
                        (sbrush:prepare-brushes-for-bsp
                         (list (sbrush:expand-brush brush :square sbsp::+clip-square+))))))

(defun door-render (door camera)
  (check-type door door)
  (check-type camera camera)
  (let ((mvp (m* (camera-projection-matrix camera)
                 (camera-view-transform camera))))
    (dolist (surf (sbrush:brush-surfaces (door-brush door)))
      (let ((geometry (smdl::sidedef->surf-triangles surf)))
        (srend:render-surface geometry mvp)
        (smdl::free-surf-triangles geometry)))
    (let* ((lines (mapcar (lambda (surf)
                            (sbsp:lineseg-orig-line (sbsp:sidedef-lineseg surf)))
                          (sbrush:brush-surfaces (door-brush door))))
           (sector (sbsp:sidedef-back-sector
                    (first (sbrush:brush-surfaces (door-brush door)))))
           (ceiling-height (sbsp:sector-ceiling-height sector))
           (ceiling-geometry (smdl::polygon->surf-triangles lines ceiling-height)))
      (srend:render-surface ceiling-geometry mvp)
      (smdl::free-surf-triangles ceiling-geometry))))

(defun door-open (door)
  (check-type door door)
  (with-accessors ((last-open-time-ms door-last-open-time-ms)) door
    (unless last-open-time-ms
      (setf last-open-time-ms (get-game-time)))))
