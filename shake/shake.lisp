;;;; Entry point for Shake game.

(in-package #:shake)

;; TODO: How does this work with executable image?
(defvar *base-dir*
  #.(uiop:pathname-directory-pathname (or *compile-file-truename* *load-truename*))
  "Directory pathname where this file is located.")

(defvar *win-width* nil "Current window width in pixels")
(defvar *win-height* nil "Current window height in pixels")
(defvar *rend-width* nil "Current rendering width in pixels")
(defvar *rend-height* nil "Current rendering height in pixels")

(declaim (inline make-plane))
(defstruct plane
  (normal nil :type (vec 3) :read-only t)
  (dist nil :type shiva-float :read-only t))

(defstruct projection
  (matrix nil :type (mat 4) :read-only t))

(defstruct (perspective (:include projection)
                        (:constructor make-perspective-priv))
  (fovy nil :type (shiva-float #.(shiva-float 0.0)) :read-only t)
  (aspect nil :type (shiva-float #.(shiva-float 0.0)) :read-only t)
  (near nil :type (shiva-float #.(shiva-float 0.0)) :read-only t)
  (far nil :type (shiva-float #.(shiva-float 0.0)) :read-only t))

(defun make-perspective (fovy aspect near far)
  (assert (float> (shiva-float far) (shiva-float near)))
  (make-perspective-priv :fovy (shiva-float fovy)
                         :aspect (shiva-float aspect)
                         :near (shiva-float near)
                         :far (shiva-float far)
                         :matrix (perspective fovy aspect near far)))

(defstruct camera
  "A camera structure, storing the projection matrix, position in world space
  and rotation as a quaternion."
  (projection nil :type projection)
  (position (v 0 0 0) :type (vec 3))
  (rotation (q 0 0 0 1) :type quat))

(defun camera-view-transform (camera)
  "Returns a matrix which transforms from world to camera space."
  (declare (type camera camera))
  (let* ((pos (camera-position camera))
         (translation (translation
                       :x (- (vx pos)) :y (- (vy pos)) :z (- (vz pos))))
         (q (qconj (camera-rotation camera))))
    (m* (q->mat q) translation)))

(defun camera-projection-matrix (camera)
  (declare (type camera camera))
  (projection-matrix (camera-projection camera)))

(defmacro define-extract-frustum-plane (plane-name vfun row-index)
  "Define a frustum plane extraction function. Taken from
  Real-Time Rendering 3rd edition, 16.14.1 Frustum Plane Extraction"
  `(defun ,(symbolicate plane-name '-frustum-plane) (camera)
     (declare (optimize (speed 3)))
     (with-struct (camera- projection-matrix view-transform) camera
       (let* ((m (m* projection-matrix view-transform))
              (plane (v- (,vfun (the (vec 4) (mat-row m 3))
                                (the (vec 4) (mat-row m ,row-index))))))
         (declare (type (mat 4) m) (type (vec 4) plane))
         (make-plane :normal (vnormalize (vxyz plane))
                     :dist (/ (vw plane) (vnorm (vxyz plane))))))))

(define-extract-frustum-plane left v+ 0)
(define-extract-frustum-plane right v- 0)
(define-extract-frustum-plane bottom v+ 1)
(define-extract-frustum-plane top v- 1)
(define-extract-frustum-plane near v+ 2)
(define-extract-frustum-plane far v- 2)

;; NOTE: This and `INTERSECT-FRUSTUM-2D' are on a hot path, they should be be
;; fast and perform 0 consing.  Tuned for SBCL.
(defun intersect-plane (plane mins maxs bound-y)
  "Intersect a PLANE with AABB defined MINS and MAXS.  BOUND-Y is used to lift
  the bounds to 3D.  Based on the algorithm from Real-Time Rendering 3rd
  edition, 16.10.1 AABB"
  (declare (optimize (speed 3)))
  (check-type plane plane)
  (check-type mins (vec 2))
  (check-type maxs (vec 2))
  (check-type bound-y shiva-float)
  (with-struct (plane- normal dist) plane
    (let ((center (make-array 3 :element-type 'shiva-float))
          (h (make-array 2 :element-type 'shiva-float))
          (bound-sum (make-array 2 :element-type 'shiva-float))
          (bound-diff (make-array 2 :element-type 'shiva-float)))
      (declare (dynamic-extent center h bound-sum bound-diff))
      ;; NOTE: Using LET* and DYNAMIC-EXTENT breaks for some reason
      (setf
       ;; (setf bound-sum (v+ mins maxs))
       (vx bound-sum) (+ (vx mins) (vx maxs))
       (vy bound-sum) (+ (vy mins) (vy maxs)))
      (setf
       ;; (setf bound-diff (v- maxs mins)))
       (vx bound-diff) (- (vx maxs) (vx mins))
       (vy bound-diff) (- (vy maxs) (vy mins)))
      (setf
       ;; (setf center (vscale 0.5 bound-sum))
       (vx center) (* 0.5 (vx bound-sum))
       (vy center) bound-y  ; v2->v3
       (vz center) (* 0.5 (vy bound-sum)))
      (setf
       ;; (setf h (vscale 0.5 bound-diff))
       (vx h) (* 0.5 (vx bound-diff))
       (vy h) (* 0.5 (vy bound-diff)))
      (let ((s (+ (v3dot center normal) dist))
            (e (+ (* (vx h) (abs (vx normal)))
                  (* bound-y (abs (vy normal)))
                  ;; Note 2D vy iz vz in 3D.
                  (* (vy h) (abs (vz normal))))))
        (cond
          ((float> (- s e) #.(shiva-float 0.0)) nil) ;; outside
          ((float> #.(shiva-float 0.0) (+ s e)) :inside)
          (t :intersect))))))

;; NOTE: This and `INTERSECT-PLANE' are on a hot path, they should be be fast
;; and perform 0 consing.  Tuned for SBCL.
(defun intersect-frustum-2d (frustum-planes bounds bound-y)
  "Intersect the frustum with axis aligned bounding rectangle. Returns NIL if
  bounds are outside, :INTERSECT if they intersect and :INSIDE if bounds are
  completely inside the frustum. BOUND-Y is used to lift the bounds from 2D
  into 3D."
  (declare (optimize (speed 3)))
  (check-type frustum-planes list)
  (check-type bounds (cons (vec 2) (vec 2)))
  (check-type bound-y shiva-float)
  (let ((mins (car bounds)) (maxs (cdr bounds))
        (intersect-type nil))
    (dolist (plane frustum-planes intersect-type)
      (if-let ((intersect (intersect-plane plane mins maxs bound-y)))
        (when (or (not intersect-type) (eq :intersect intersect))
          (setf intersect-type intersect))
        (return)))))

(defun nrotate-camera (xrel yrel camera)
  "Rotate the CAMERA for XREL degrees around the world Y axis and YREL degrees
  around the local X axis. The vertical angle is clamped."
  (let* ((xrot (q* (qrotation (v 0 1 0) (* deg->rad (- xrel)))
                   (camera-rotation camera)))
         (old-v-angle (* rad->deg (q->euler-x xrot)))
         (v-angle-diff yrel))
    (cond
      ((>= old-v-angle #.(shiva-float 90d0))
       (decf old-v-angle #.(shiva-float 180d0)))
      ((<= old-v-angle #.(shiva-float -90d0))
       (incf old-v-angle #.(shiva-float 180d0))))
    (let ((v-angle (clamp (+ old-v-angle v-angle-diff)
                          #.(shiva-float -89d0) #.(shiva-float 89d0))))
      (setf (camera-rotation camera)
            (q* xrot (qrotation (v 1 0 0) (* deg->rad (- v-angle old-v-angle)))))
      camera)))

(defun view-dir (dir-name camera)
  (let ((dir (ecase dir-name
               (:forward (v 0 0 -1))
               (:back (v 0 0 1))
               (:right (v 1 0 0))
               (:left (v -1 0 0)))))
    (vrotate (camera-rotation camera) dir)))

;; TODO: Move these to shake.render, or remove entirely.
(defun make-point-renderer ()
  "Creates a function which draws a single point. The function takes keywords
  :DRAW and :DELETE for drawing and deleting respectively."
  (let ((vao (gl:gen-vertex-array))
        deleted)
    (gl:bind-vertex-array vao)
    (gl:vertex-attrib 0 0 0 0)
    (gl:bind-vertex-array 0)
    (dlambda
     (:draw () (if deleted
                   (error "Trying to render with deleted point-renderer.")
                   (progn (gl:bind-vertex-array vao)
                          (gl:draw-arrays :points 0 1))))
     (:delete () (unless deleted
                   (gl:delete-vertex-arrays (list vao))
                   (setf deleted t))))))
(defun renderer-draw (renderer) (funcall renderer :draw))
(defun renderer-delete (renderer) (funcall renderer :delete))

(defun draw-timer-stats (timer &key (x 0) (y -20))
  (with-struct (timer- name max avg) timer
    (srend::draw-text (format nil "~A: ~,2Fms (avg) ~,2Fms (max)"
                              name (* 1s3 avg) (* 1s3 max))
                      :x x :y y)))

(declaim (type (unsigned-byte 32) +max-frame-skip+ +ticrate+
               *last-time* *base-time* *gametic*))
(defconstant +ticrate+ 60)
(defconstant +max-frame-skip+ 5)
(defvar *base-time* 0)
(defvar *last-time* 0)
(defvar *gametic* 0)

(defun get-time ()
  "Return time in 1/TICRATE second tics offset by *BASE-TIME*."
  (declare (optimize (speed 3)))
  (floor (* +ticrate+ (- (the (unsigned-byte 32) (sdl2:get-ticks)) *base-time*))
         1000))

(defun start-game-loop ()
  "Starts the game loop by reseting the game time."
  (setf *base-time* (sdl2:get-ticks)
        *last-time* *base-time*
        *gametic* 0))

(defun try-run-tics (build-ticcmd run-tic)
  (let ((new-tics (- (get-time) *last-time*)))
    (incf *last-time* new-tics)
    (repeat (min new-tics +max-frame-skip+)
      (funcall run-tic (funcall build-ticcmd))
      (incf *gametic*))))

(defvar *mouse* (cons 0 0))

(defun update-mouse-relative (xrel yrel)
  (incf (car *mouse*) xrel)
  (incf (cdr *mouse*) yrel))

(defvar *game-keys* (make-hash-table)
  "Mapping of currently pressed keys. Used for building a tick command.")

(defun reset-game-keys ()
  (clrhash *game-keys*))

(defun press-game-key (key)
  (setf (gethash key *game-keys*) t))

(defun release-game-key (key)
  (setf (gethash key *game-keys*) nil))

(defun game-key-down-p (key)
  (gethash key *game-keys*))

(defun press-mouse-button (button-id)
  (check-type button-id (unsigned-byte 8))
  (let ((button-key (make-keyword (format nil "~A~A" :mouse button-id))))
    (press-game-key button-key)))

(defun release-mouse-button (button-id)
  (check-type button-id (unsigned-byte 8))
  (let ((button-key (make-keyword (format nil "~A~A" :mouse button-id))))
    (release-game-key button-key)))

(defstruct ticcmd
  (forward-move #.(shiva-float 0.0) :type shiva-float)
  (side-move #.(shiva-float 0.0) :type shiva-float)
  (angle-turn (cons #.(shiva-float 0.0) #.(shiva-float 0.0))
              :type (cons shiva-float shiva-float)))

(defun build-ticcmd ()
  (let ((xrel (car *mouse*))
        ;; Invert the Y movement.
        (yrel (- (cdr *mouse*)))
        (sens #.(shiva-float 1.5d0))
        (move-speed (if (game-key-down-p :scancode-lshift)
                        #.(shiva-float 1.0)
                        #.(shiva-float 4.0)))
        (cmd (make-ticcmd)))
    (when (game-key-down-p :scancode-w)
      (incf (ticcmd-forward-move cmd) move-speed))
    (when (game-key-down-p :scancode-s)
      (decf (ticcmd-forward-move cmd) move-speed))
    (when (game-key-down-p :scancode-d)
      (incf (ticcmd-side-move cmd) move-speed))
    (when (game-key-down-p :scancode-a)
      (decf (ticcmd-side-move cmd) move-speed))
    (incf (car (ticcmd-angle-turn cmd)) (* xrel (/ sens #.(shiva-float 10d0))))
    (incf (cdr (ticcmd-angle-turn cmd)) (* yrel (/ sens #.(shiva-float 10d0))))
    (setf (car *mouse*) 0)
    (setf (cdr *mouse*) 0)
    cmd))

;; Player movement

(defun clip-velocity (velocity normal)
  "Clip the given VELOCITY by projecting it on NORMAL and return a parallel
  vector to the surface."
  (let ((change (vscale (vdot velocity normal) normal)))
    (v- velocity change)))

(defconstant +step-height+ #.(shiva-float 0.375d0) "Height of a stair step.")

(defun player-climb-move (origin velocity hull)
  (let ((step-origin (v+ origin (v 0.0 +step-height+ 0.0)))
        (height #.(shiva-float 0.5)))
    (if (/= 1.0 (mtrace-fraction (clip-hull hull origin step-origin :height height)))
        ;; Unable to move up to climb a stair.
        origin
        ;; Try climbing a stair with regular movement.
        ;; XXX: What if we need to climb again?
        (mtrace-endpos (clip-hull hull step-origin (v+ step-origin velocity)
                                  :height height)))))

;; Moving the player on the ground should work the following way.
;; The trivial case is when the full move can be made, so we just return the
;; final position after adding the velocity to the origin.
;; If the full move cannot be made:
;;   * try climbing a stair and continue moving;
;;   * try sliding along the obstacle.
;; The try which results in greater movement should be returned.
(defun player-ground-move (origin velocity)
  "Returns the destination by trying to move the ORIGIN point by the VELOCITY
  vector. Movement is clipped by detecting collisions with the
  SMDL:*WORLD-MODEL*."
  (declare (special smdl:*world-model*))
  (check-type origin (vec 3))
  (check-type velocity (vec 3))
  (let* ((hull (smdl:bsp-model-hull smdl:*world-model*))
         (height #.(shiva-float 0.5))
         (mtrace (clip-hull hull origin (v+ origin velocity) :height height)))
    (if (= (mtrace-fraction mtrace) 1.0)
        ;; Completed the whole move.
        (mtrace-endpos mtrace)
        ;; Partial move, try sliding or climbing.
        ;; XXX: Maybe use time-left to scale climb velocity?
        (let ((climb-endpos (player-climb-move origin velocity hull))
              (slide-endpos
               (let ((time-left (- 1.0 (mtrace-fraction mtrace)))
                     (new-origin (mtrace-endpos mtrace))
                     (new-vel (clip-velocity velocity
                                             (mtrace-normal mtrace))))
                 (mtrace-endpos
                  (clip-hull hull new-origin (v+ new-origin (vscale time-left new-vel))
                             :height height)))))
          (if (float> (vdistsq (v3->v2 origin) (v3->v2 climb-endpos))
                      (vdistsq (v3->v2 origin) (v3->v2 slide-endpos)))
              climb-endpos
              slide-endpos)))))

(defun groundedp (entity-position)
  (declare (special smdl:*world-model*))
  (check-type entity-position (vec 3))
  (let ((sector (hull-point-sector (smdl:bsp-model-hull smdl:*world-model*)
                                   (v3->v2 entity-position))))
    (or (not sector) ; We are off the map, treat it as grounded.
        (float= (sbsp:sector-floor-height sector) (vy entity-position)))))

(defun apply-gravity (entity-position)
  (declare (special *time-delta* smdl:*world-model*))
  (check-type entity-position (vec 3))
  (flet ((clamp-to-floor (position)
           (let ((floor (sbsp:sector-floor-height (hull-point-sector (smdl:bsp-model-hull smdl:*world-model*)
                                                                     (v3->v2 position)))))
             (v (vx position) (max (vy position) floor) (vz position)))))
    (if (groundedp entity-position)
        entity-position
        (let* ((gravity-speed #.(shiva-float 10d0))
               ;; TODO: Use remaining time fraction.
               (gravity-velocity (v 0.0 (- (* gravity-speed *time-delta*)) 0.0)))
          ;; TODO: Remove `clamp-to-floor' when `recursive-hull-check'
          ;; correctly clips vertical movement
          (clamp-to-floor
           (mtrace-endpos (clip-hull (smdl:bsp-model-hull smdl:*world-model*)
                                     entity-position
                                     (v+ entity-position gravity-velocity)
                                     :height #.(shiva-float 0.5))))))))

(defun move-player (player forward-move side-move &key (noclip nil))
  (let ((forward-dir (view-dir :forward player)))
    (unless noclip
      ;; Project forward-dir to plane of movement.
      (setf forward-dir (vnormalize (v (vx forward-dir) 0 (vz forward-dir)))))
    (let* ;; Intentionally make diagonal movement faster.
        ((velocity (v+ (vscale forward-move forward-dir)
                       (vscale side-move (view-dir :right player))))
         ;; TODO: Seperate player position from camera.
         (camera-offset (v 0.0 0.5 0.0))
         (origin (v- (camera-position player) camera-offset))
         (end-pos (v+ origin velocity)))
      (if noclip
          (setf (camera-position player) (v+ camera-offset end-pos))
          (let ((destination (apply-gravity (player-ground-move origin velocity))))
            (setf (camera-position player)
                  (v+ camera-offset destination)))))))

(defun run-tic (camera cmd)
  "Run a single tic/frame of game logic. CMD is used to read and act on player
  controls."
  (check-type camera camera)
  (check-type cmd ticcmd)
  (with-struct (ticcmd- forward-move side-move angle-turn) cmd
    (let ((*time-delta* (shiva-float (/ +ticrate+)))
          (noclip nil))
      (declare (special *time-delta*))
      (unless (and (zerop forward-move) (zerop side-move))
        (move-player camera (* forward-move *time-delta*) (* side-move *time-delta*) :noclip noclip))
      (destructuring-bind (x-turn . y-turn) angle-turn
        (unless (and (zerop x-turn) (zerop y-turn))
          (nrotate-camera x-turn y-turn camera)))
      (unless noclip
        (let ((camera-offset (v 0.0 0.5 0.0)))
          (setf (camera-position camera)
                (v+ camera-offset (apply-gravity (v- (camera-position camera) camera-offset)))))))))

(defun load-main-resources (render-system)
  (add-res "point-renderer" #'make-point-renderer #'renderer-delete)
  (let ((progs (srend:render-system-prog-manager render-system)))
    (srend::get-program progs "billboard" "text" "billboard")
    (srend::get-program progs "pass" "color"))
  (add-res "font"
           (lambda ()
             (srend::load-font (data-path "share/font-16.bmp") 16 #\Space))
           #'srend::delete-font))

(defun load-weapons (render-system model-manager)
  (srend::load-image-from-file (srend::render-system-image-manager render-system) "shotgun.bmp")
  (let ((shotgun-model (smdl:get-model model-manager "../shotgun.obj")))
    ;; TODO: This should be more generic and injected in a better way.
    (setf (smdl::surf-triangles-tex-name (smdl::obj-model-verts shotgun-model)) "shotgun.bmp")))

(defun load-map-textures (render-system bsp)
  (labels ((texture-name (surf)
             (aif (sbsp:sidedef-texinfo surf)
                  (string-downcase (sbsp:texinfo-name it))))
           (leaf-textures (leaf)
             (remove nil (mapcar #'texture-name (sbsp:leaf-surfaces leaf)))))
    (let* ((textures (remove-duplicates (sbsp:bsp-trav bsp #'append
                                                       #'leaf-textures)
                                        :test #'string=)))
      (srend:load-map-images (srend::render-system-image-manager render-system)
                             textures))))

(defun spawn-player (things camera)
  (dolist (thing things)
    (when (eq (sbsp:map-thing-type thing) :player-spawn)
      (let ((pos (sbsp:map-thing-pos thing))
            (angle (sbsp:map-thing-angle thing)))
        (setf (camera-position camera) (v (vx pos) 0.5 (vy pos)))
        (return (nrotate-camera angle #.(shiva-float 0.0) camera))))))

(defun render-weapon (camera model-manager)
  (flet ((calc-kickback ()
           ;; Placeholder for shooting animation
           (if (game-key-down-p :mouse1) 4.0 0.0)))
    (let ((mvp (m* (camera-projection-matrix camera)
                   (m* (m* (translation :x 0.5 :y -0.5 :z #.(shiva-float -1.4d0))
                           (rotation (v 0 1 0) (* deg->rad 182)))
                       (rotation (v 1 0 0) (* deg->rad (- -3 (calc-kickback))))))))
      ;; Hack the projected Z so that we keep the depth of FP weapon small, thus
      ;; drawing over almost everything.
      (zap (lambda (x) (* 0.25 x)) (aref mvp 2 0))
      (zap (lambda (x) (* 0.25 x)) (aref mvp 2 1))
      (zap (lambda (x) (* 0.25 x)) (aref mvp 2 2))
      (zap (lambda (x) (* 0.25 x)) (aref mvp 2 3))
      (srend:render-surface
       (smdl::obj-model-verts (smdl:get-model model-manager "../shotgun.obj"))
       mvp)))
  ;; Placeholder for rendering shot, this should be done elsewhere.
  (when (game-key-down-p :mouse1)
    (let* ((forward-dir (view-dir :forward camera))
           (shot-range #.(shiva-float 20))
           (velocity (vscale shot-range forward-dir))
           ;; TODO: Seperate player position from camera.
           ;; (camera-offset (v 0d0 0.5d0 0d0))
           (origin (camera-position camera)) ;; (v- (camera-position camera) camera-offset))
           ;; (end-pos (v+ origin velocity))
           (hull (smdl:bsp-model-hull smdl:*world-model*)))
      (multiple-value-bind (mtrace hitp) (clip-hull hull origin (v+ origin velocity))
        (when hitp
          (let* ((dest (v+ (mtrace-endpos mtrace)
                           (vscale #.(shiva-float 0.005d0) (mtrace-normal mtrace))))
                 (mvp (m* (m* (camera-projection-matrix camera)
                             (camera-view-transform camera))
                         (m* (translation :x (vx dest) :y (vy dest) :z (vz dest))
                             (scale :x 0.125 :y 0.125 :z 0.125)))))
            (srend:render-surface
             (smdl::obj-model-verts (smdl:model-manager-default-model model-manager))
             mvp)))))))

(defstruct weapon
  (model nil :type smdl::obj-model)
  (position (v 0 0 0) :type (vec 3))
  (scale #.(shiva-float 1.0) :type shiva-float)
  (angle-y #.(shiva-float 0.0) :type shiva-float)
  (bounds-scale (v 1 1 1) :type (vec 3)))

(defun make-shotgun (model-manager position)
  (check-type position (vec 3))
  (let* ((shotgun-model (smdl:get-model model-manager "../shotgun.obj"))
         (scale #.(shiva-float 0.125))
         (bounds-scale (vscale (* 0.5 scale)
                               (v- (smdl::obj-model-max-bounds shotgun-model)
                                   (smdl::obj-model-min-bounds shotgun-model)))))
    (assert (v= (vscale 0.5 (v+ (smdl::obj-model-min-bounds shotgun-model)
                                (smdl::obj-model-max-bounds shotgun-model)))
                (v 0 0 0)))
    (make-weapon :model shotgun-model
                 :position position
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
          (bounds-scale (weapon-bounds-scale weapon)))
      (srend:render-surface
       (smdl::obj-model-verts (smdl:model-manager-default-model model-manager))
       (m* view-project
           (m* (translation :x (vx pos) :y (vy pos) :z (vz pos))
               (m* (rotation (v 0 1 0) (* deg->rad (weapon-angle-y weapon)))
                   (scale :x (vx bounds-scale) :y (vy bounds-scale) :z (vz bounds-scale)))))
       :wireframep t))
    ;; Render the weapon model
    (srend:render-surface (smdl::obj-model-verts (weapon-model weapon)) mvp)))

(defun weapon-think (weapon)
  (check-type weapon weapon)
  (setf (weapon-angle-y weapon) (shiva-float (mod (get-time) 360))))

(defun run-thinkers (thinkers)
  (dolist (thinker thinkers)
    (weapon-think thinker)))

(defun render-things (render-things camera model-manager)
  (check-type camera camera)
  (check-type model-manager smdl::model-manager)
  (dolist (render-thing render-things)
    (weapon-render render-thing camera model-manager)))

(defun call-with-init (function)
  "Initialize everything and run FUNCTION with RENDER-SYSTEM and WINDOW arguments."
  ;; Calling sdl2:with-init will create a SDL2 Main Thread, and the body is
  ;; executed inside that thread.
  (sdl2:with-init (:everything)
    (reset-game-keys)
    (with-data-dirs *base-dir*
      (set-gl-attrs)
      (let* ((*win-width* 1024) (*win-height* 768)
             (*rend-width* *win-width*) (*rend-height* *win-height*))
        (sdl2:with-window (window :title "shake" :w *win-width* :h *win-height*
                                  :flags '(:opengl))
          (srend:with-render-system
              (render-system window *rend-width* *rend-height*)
            (sdl2:set-relative-mouse-mode 1)
            (srend:print-gl-info (srend:render-system-gl-config render-system))
            (funcall function render-system window)))))))

(defmacro with-init ((render-system window) &body body)
  `(call-with-init (lambda (,render-system ,window) ,@body)))

(defun main ()
  (with-init (render-system win)
    (smdl:with-model-manager model-manager
      (with-resources "main"
        (load-main-resources render-system)
        (let* ((proj (make-perspective (* deg->rad #.(shiva-float 60d0))
                                       (/ *win-width* *win-height*)
                                       #.(shiva-float 0.01d0)
                                       #.(shiva-float 100d0)))
               (camera (make-camera :projection proj :position (v 1 0.5 8)))
               (frame-timer (make-timer :name "Main Loop"))
               (smdl:*world-model* (smdl:get-model model-manager "test.bsp"))
               ;; TODO: These should probably be members of some kind of game struct
               (think-things nil)
               (render-things nil))
          (load-weapons render-system model-manager)
          (load-map-textures render-system (smdl:bsp-model-nodes smdl:*world-model*))
          (srend:print-memory-usage render-system)
          (spawn-player (smdl:bsp-model-things smdl:*world-model*) camera)
          ;; Spawn and register shotgun thing
          (let ((shotgun (make-shotgun model-manager (v 17 0.125 51))))
            (push shotgun think-things)
            (push shotgun render-things))
          (let ((shotgun (make-shotgun model-manager (v 15 0.125 48))))
            (push shotgun think-things)
            (push shotgun render-things))
          (symbol-macrolet ((input-focus-p
                             (member :input-focus
                                     (sdl2:get-window-flags win)))
                            (minimized-p
                             (member :minimized
                                     (sdl2:get-window-flags win))))
            (start-game-loop)
            (sdl2:with-event-loop (:method :poll)
              (:quit () t)
              (:keydown
               (:keysym keysym)
               (when input-focus-p
                 (press-game-key (sdl2:scancode keysym))))
              (:keyup
               (:keysym keysym)
               (when input-focus-p
                 (release-game-key (sdl2:scancode keysym))))
              (:mousemotion
               (:xrel xrel :yrel yrel)
               (when input-focus-p
                 (update-mouse-relative xrel yrel)))
              (:mousebuttondown
               (:button button)
               (press-mouse-button button))
              (:mousebuttonup
               (:button button)
               (release-mouse-button button))
              (:idle ()
                     (with-timer (frame-timer)
                       (try-run-tics #'build-ticcmd
                                     (lambda (tic)
                                       (run-tic camera tic)
                                       (run-thinkers think-things)))
                       (unless minimized-p
                         (srend:with-draw-frame (render-system)
                           (render-view camera model-manager render-things)
                           (draw-timer-stats frame-timer)
                           (draw-timer-stats
                            (srend::render-system-swap-timer render-system) :y -36))))))))))))

(defun set-gl-attrs ()
  "Set OpenGL context attributes. This needs to be called before window
  and context creation."
  (sdl2:gl-set-attrs
   :context-major-version 3
   :context-minor-version 3
   ;; set CONTEXT_FORWARD_COMPATIBLE
   :context-flags #x2
   ;; set CONTEXT_PROFILE_CORE
   :context-profile-mask #x1))

(defun render-world (camera world-model)
  "Send geometry for rendering, front to back."
  (declare (optimize (speed 3) (space 3)))
  (check-type camera camera)
  (check-type world-model smdl:bsp-model)
  (with-struct (camera- position) camera
    (let ((pos-2d (make-array 2 :element-type 'shiva-float))
          (frustum (list (left-frustum-plane camera) (right-frustum-plane camera)
                         (near-frustum-plane camera) (far-frustum-plane camera)))
          (mvp (m* (camera-projection-matrix camera)
                   (camera-view-transform camera))))
      (declare (dynamic-extent pos-2d frustum))
      (setf (vx pos-2d) (vx position))
      (setf (vy pos-2d) (vz position))
      (labels ((rec (node &optional (test-frustum-p t))
                 (declare (type (or sbsp:node sbsp:leaf) node))
                 (declare (type boolean test-frustum-p))
                 (if (sbsp:leaf-p node)
                     (when (or (not test-frustum-p)
                               (intersect-frustum-2d frustum
                                                     (sbsp:leaf-bounds node)
                                                     (vy position)))
                       (aif (smdl:mleaf-floor-geometry node)
                            (srend:render-surface it mvp))
                       (aif (smdl:mleaf-ceiling-geometry node)
                            (srend:render-surface it mvp))
                       (dolist (surf (sbsp:leaf-surfaces node))
                         (srend:render-surface (smdl:surface-geometry surf) mvp)))
                     ;; split node
                     (let ((front (sbsp:node-front node))
                           (back (sbsp:node-back node)))
                       (when-let ((intersect (or (not test-frustum-p)
                                                 (intersect-frustum-2d
                                                  frustum (sbsp:node-bounds node)
                                                  (vy position)))))
                         ;; Frustum testing is no longer needed if the bounds
                         ;; are completely inside.
                         (let ((test-p (and test-frustum-p
                                            (not (eq :inside intersect)))))
                           (ecase (sbsp:determine-side (sbsp:node-line node) pos-2d)
                             ((or :front :on-line)
                              (rec front test-p)
                              (rec back test-p))
                             (:back
                              (rec back test-p)
                              (rec front test-p)))))))))
        (rec (smdl:bsp-model-nodes world-model))))))

(defun render-view (camera model-manager render-things)
  (gl:enable :depth-test)
  ;; TODO: Enable this when we correctly generate CCW faces for .bsp models.
  ;; (gl:enable :cull-face)
  ;; (gl:cull-face :back)
  ;; (gl:front-face :ccw)
  (gl:depth-func :less)
  (render-world camera smdl:*world-model*)
  ;; Draw "crosshair"
  (srend::draw-text "+" :x (floor *win-width* 2) :y (floor *win-height* 2)
                    :scale 2.0)
  ;; TODO: This should use currently equipped weapon.
  (render-weapon camera model-manager)
  (render-things render-things camera model-manager))
