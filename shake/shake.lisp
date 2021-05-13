;;;; Entry point for Shake game.

(in-package #:shake)

;; NOTE: This does not work with executable image.
(defvar *base-dir*
  #.(uiop:pathname-directory-pathname (or *compile-file-truename* *load-truename*))
  "Directory pathname where this file is located.")

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
  and rotation as Euler angles."
  (projection nil :type projection)
  (position (v 0 0 0) :type (vec 3))
  ;; degrees
  (angles (v 0 0) :type (vec 2)))

(defun camera-rotation (camera)
  "Return camera angles as a quaternion."
  ;; TODO: Direct conversion from euler angles to quaternion.
  (q* (qrotation (v 0 1 0) (* deg->rad (vx (camera-angles camera))))
      (qrotation (v 1 0 0) (* deg->rad (vy (camera-angles camera))))))

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
  (prog1 camera
    (zap (lambda (old-y)
           (clamp (+ old-y yrel) #.(shiva-float -89d0) #.(shiva-float 89d0)))
         (vy (camera-angles camera)))
    (zap (lambda (old-x)
           (let ((new-x (- old-x xrel)))
             (* #.(shiva-float 360d0)
                (nth-value 1
                           (if (< new-x #.(shiva-float 0d0))
                               (floor (/ new-x #.(shiva-float 360d0)))
                               (truncate (/ new-x #.(shiva-float 360d0))))))))
         (vx (camera-angles camera)))))

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
                          (gl:bind-buffer :array-buffer 0)
                          (gl:draw-arrays :points 0 1))))
     (:delete () (unless deleted
                   (gl:delete-vertex-arrays (list vao))
                   (setf deleted t))))))
(defun renderer-draw (renderer) (funcall renderer :draw))
(defun renderer-delete (renderer) (funcall renderer :delete))

(defun draw-timer-stats (timer &key (x 0) (y -20))
  (with-struct (timer- name max avg) timer
    (srend:draw-text (format nil "~A: ~,2Fms (avg) ~,2Fms (max)"
                             name (* 1s3 avg) (* 1s3 max))
                     :x x :y y)))

(declaim (type (unsigned-byte 32) +max-frame-skip+ +ticrate+
               *last-tic* *base-time* *gametic*))
(defconstant +ticrate+ 60)
(defconstant +max-frame-skip+ 5)
(defvar *base-time* 0 "Time since game started in milliseconds")
(defvar *last-tic* 0)
(defvar *gametic* 0)

(defun get-time ()
  "Return time in 1/TICRATE second tics offset by *BASE-TIME*."
  (declare (optimize (speed 3)))
  (floor (* +ticrate+ (- (the (unsigned-byte 32) (sdl2:get-ticks)) *base-time*))
         1000))

(defun get-game-time ()
  "Return milliseconds of elapsed game time"
  (coerce (round (* 1000 *gametic*) +ticrate+) 'fixnum))

(defun start-game-loop ()
  "Starts the game loop by reseting the game time."
  (setf *base-time* (sdl2:get-ticks)
        *last-tic* 0
        *gametic* 0))

(defun try-run-tics (build-ticcmd run-tic)
  (let ((new-tics (- (get-time) *last-tic*)))
    (incf *last-tic* new-tics)
    (repeat (min new-tics +max-frame-skip+)
      (funcall run-tic (funcall build-ticcmd))
      (incf *gametic*))))

(defvar *mouse* (cons 0 0))

(defun update-mouse-relative (xrel yrel)
  (incf (car *mouse*) xrel)
  (incf (cdr *mouse*) yrel))

(defstruct key-press
  (state :clicked :type (member :clicked :held)))

(defvar *game-keys* (make-hash-table)
  "Mapping of currently pressed keys. Used for building a tick command.")

(defun reset-game-keys ()
  (clrhash *game-keys*))

(defun press-game-key (key)
  (setf (gethash key *game-keys*) (make-key-press)))

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

;;; Player

(defstruct inventory
  (weapons nil :type list))

(defstruct player
  (inventory (make-inventory) :type inventory)
  (current-weapon nil :type (or null weapon))
  (weapon-hidden-p nil :type boolean)
  (position (v 0 0 0) :type (vec 3)))

(defun build-ticcmd ()
  (declare (special *player*))
  (declare (special *game*))
  (declare (special *camera*))
  (check-type *player* player)
  ;; XXX: Hacked toggling PLAYER-WEAPON-HIDDEN-P, use appropriate on-key-down
  ;; event. Also, is this the right place to do the toggle?
  (when (game-key-down-p :scancode-h)
    (zap #'not (player-weapon-hidden-p *player*))
    (release-game-key :scancode-h))
  (when (game-key-down-p :scancode-space)
    (let* (;; Offset origin as if camera
           (origin (v+ (player-position *player*) (v 0 0.5 0)))
           (use-dest (v+ origin (vscale 0.5 (vnormalize (view-dir :forward *camera*))))))
      (dolist (thing (game-think-things *game*))
        (when (subtypep (type-of thing) 'door)
          (multiple-value-bind (mtrace hitp)
              (clip-hull (door-hull thing) origin
                         (mtrace-endpos
                          (clip-hull (smdl:bsp-model-hull smdl:*world-model*)
                                     origin use-dest))
                         :ignore-empty-leaves t)
            (declare (ignore mtrace))
            (when hitp (door-open thing))))))
    (release-game-key :scancode-space))
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
  (let* ((step-origin (v+ origin (v 0.0 +step-height+ 0.0)))
         (height #.(shiva-float 0.5))
         (climb-mtrace (clip-hull hull origin step-origin :height height)))
    (if (/= 1.0 (mtrace-fraction climb-mtrace))
        ;; Unable to move up to climb a stair.
        ;; origin
        (mtrace-endpos (clip-hull hull (mtrace-endpos climb-mtrace)
                                  (v+ (mtrace-endpos climb-mtrace) velocity)
                                  :height height))
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
               (if (mtrace-start-solid-p mtrace)
                   (mtrace-endpos mtrace)
                   (let ((time-left (- 1.0 (mtrace-fraction mtrace)))
                         (new-origin (mtrace-endpos mtrace))
                         (new-vel (clip-velocity velocity
                                                 (mtrace-normal mtrace))))
                     (mtrace-endpos
                      (clip-hull hull new-origin (v+ new-origin (vscale time-left new-vel))
                                 :height height))))))
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

(defun player-touch-triggers (player start end)
  (declare (special *game*))
  (check-type *game* game)
  (check-type player player)
  (check-type start (vec 3))
  (check-type end (vec 3))
  (unless (v= start end)
    (let ((move-dir (vnormalize (v- end start)))
          (move-len (vnorm (v- end start)))
          (touched-things nil))
      (dolist (thing (game-think-things *game*))
        (when (typep thing 'weapon)
          (let* ((hull-expand (* 0.5 sbsp::+clip-square+))
                 (half-height 0.25)
                 (oobb (make-oobb
                       :center (weapon-position thing)
                       :half-lengths
                       (v+ (weapon-bounds-scale thing)
                           ;; TODO: Appropriate collision hull expansion
                           (v hull-expand half-height hull-expand))))
                (rotation (rotation (v 0 1 0) (* deg->rad (weapon-angle-y thing)))))
            (setf (vx (oobb-directions oobb))
                  (vxyz (vtransform rotation (v (vx (vx (oobb-directions oobb)))
                                                (vy (vx (oobb-directions oobb)))
                                                (vz (vx (oobb-directions oobb)))
                                                0.0))))
            (setf (vz (oobb-directions oobb))
                  (vxyz (vtransform rotation (v (vx (vz (oobb-directions oobb)))
                                                (vy (vz (oobb-directions oobb)))
                                                (vz (vz (oobb-directions oobb)))
                                                0.0))))
            (when (ray-oobb-intersect start move-dir oobb :ray-length move-len)
              (push thing touched-things)))))
      ;; Trigger touched things
      (dolist (touched touched-things)
        (when (typep touched 'weapon)
          ;; TODO: Handle stacking same weapons
          (push touched (inventory-weapons (player-inventory player)))
          (unless (player-current-weapon player)
            (setf (player-current-weapon player) touched)))
        ;; NOTE: Removal is horribly inefficient.
        (setf (game-think-things *game*)
              (remove touched (game-think-things *game*)))
        (setf (game-render-things *game*)
              (remove touched (game-render-things *game*)))))))

(defun player-move (camera forward-move side-move &key (noclip nil))
  (declare (special *player*))
  (declare (special *game*))
  (check-type *player* player)
  ;; TODO: Consolidate player and camera
  (check-type camera camera)
  (let ((forward-dir (view-dir :forward camera)))
    (unless noclip
      ;; Project forward-dir to plane of movement.
      (setf forward-dir (vnormalize (v (vx forward-dir) 0 (vz forward-dir)))))
    (let* ;; Intentionally make diagonal movement faster.
        ((velocity (v+ (vscale forward-move forward-dir)
                       (vscale side-move (view-dir :right camera))))
         (camera-offset (v 0.0 0.5 0.0))
         (origin (player-position *player*))
         (end-pos (v+ origin velocity)))
      (if noclip
          (setf (player-position *player*) end-pos)
          ;; TODO: Resolve collision detection with everything properly.
          (let ((destination (apply-gravity (player-ground-move origin velocity))))
            (dolist (thing (game-think-things *game*))
              (when (subtypep (type-of thing) 'door)
                (multiple-value-bind (mtrace hitp)
                    (clip-hull (door-hull thing) origin destination
                               :height #.(shiva-float 0.5)
                               :ignore-empty-leaves t)
                  (when hitp
                    (setf destination (mtrace-endpos mtrace))))))
            (player-touch-triggers *player* origin destination)
            (setf (player-position *player*) destination)))
      (setf (camera-position camera)
            (v+ camera-offset (player-position *player*))))))

(defun run-tic (game camera cmd)
  "Run a single tic/frame of game logic. CMD is used to read and act on player
  controls."
  (check-type game game)
  (check-type camera camera)
  (check-type cmd ticcmd)
  (with-struct (ticcmd- forward-move side-move angle-turn) cmd
    (let ((*time-delta* (shiva-float (/ +ticrate+)))
          (*game* game)
          (noclip nil))
      (declare (special *time-delta*))
      (declare (special *game*))
      (unless (and (zerop forward-move) (zerop side-move))
        (player-move camera (* forward-move *time-delta*) (* side-move *time-delta*) :noclip noclip))
      (destructuring-bind (x-turn . y-turn) angle-turn
        (unless (and (zerop x-turn) (zerop y-turn))
          (nrotate-camera x-turn y-turn camera)))
      (unless noclip
        (let ((camera-offset (v 0.0 0.5 0.0)))
          (setf (camera-position camera)
                (v+ camera-offset (apply-gravity (v- (camera-position camera) camera-offset))))))
      (run-thinkers (game-think-things game)))))

(defun load-main-resources (render-system)
  (add-res "point-renderer" #'make-point-renderer #'renderer-delete)
  (let ((progs (srend:render-system-prog-manager render-system)))
    (srend::get-program progs "billboard" "text" "billboard")
    (srend::get-program progs "pass" "color"))
  (add-res "font"
           (lambda ()
             (srend::load-font (data-path "share/font-16.bmp" :if-does-not-exist :error)
                               16 #\Space))
           #'srend::delete-font))

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
        (setf (camera-angles camera) (v angle #.(shiva-float 0d0)))
        (return (make-player :position (v2->v3 pos)))))))

(defun spawn-things (game image-manager model-manager map-model)
  (check-type game game)
  (check-type image-manager srend::image-manager)
  (check-type model-manager smdl::model-manager)
  (check-type map-model smdl:bsp-model)
  (dolist (thing (smdl:bsp-model-things map-model))
    (let* ((pos-2d (sbsp:map-thing-pos thing))
           (floor-height (sbsp:sector-floor-height
                          (hull-point-sector (smdl:bsp-model-hull map-model)
                                             pos-2d)))
           (angle-y (sbsp:map-thing-angle thing)))
      (ccase (sbsp:map-thing-type thing)
        (:player-spawn) ;; Handled by `SPAWN-PLAYER'.
        (:shotgun
         (game-add-thing game
                         (make-shotgun image-manager model-manager
                                       ;; TODO: Fix magic 0.25
                                       (v2->v3 pos-2d (+ floor-height 0.25))
                                       :angle-y angle-y)))
        (:enemy
         (game-add-thing game
                         (make-enemy image-manager model-manager
                                     (v2->v3 pos-2d floor-height)
                                     :angle-y angle-y)))
        (:door
         (assert (= 1 (list-length (sbsp:map-thing-brushes thing))))
         (game-add-thing game
                         (make-door (first (sbsp:map-thing-brushes thing)))))))))

(defun hit-enemy-p (start end)
  (declare (special *game*))
  (check-type *game* game)
  (check-type start (vec 3))
  (check-type end (vec 3))
  (unless (v= start end)
    (let ((move-dir (vnormalize (v- end start)))
          (move-len (vnorm (v- end start)))
          (hit-distance nil)
          (hit-enemy nil))
      (dolist (thing (game-think-things *game*))
        (when (typep thing 'enemy)
          (let ((oobb (make-oobb
                       ;; XXX: Hack center
                       :center (v+ (enemy-position thing) (v 0 0.5 0))
                       :half-lengths (enemy-bounds-scale thing)))
                (rotation (rotation (v 0 1 0) (* deg->rad (enemy-angle-y thing)))))
            (setf (vx (oobb-directions oobb))
                  (vxyz (vtransform rotation (v (vx (vx (oobb-directions oobb)))
                                                (vy (vx (oobb-directions oobb)))
                                                (vz (vx (oobb-directions oobb)))
                                                0.0))))
            (setf (vz (oobb-directions oobb))
                  (vxyz (vtransform rotation (v (vx (vz (oobb-directions oobb)))
                                                (vy (vz (oobb-directions oobb)))
                                                (vz (vz (oobb-directions oobb)))
                                                0.0))))
            (let ((hitp (ray-oobb-intersect start move-dir oobb :ray-length move-len)))
              (when (and hitp (or (not hit-enemy) (< hitp hit-distance)))
                (setf hit-enemy thing)
                (setf hit-distance hitp))))))
      (values hit-enemy hit-distance))))

(defun player-render-weapon (player camera image-manager model-manager)
  (check-type player player)
  (check-type camera camera)
  (check-type image-manager srend::image-manager)
  (check-type model-manager smdl::model-manager)
  (let ((weapon (player-current-weapon player)))
    (when weapon
      (unless (player-weapon-hidden-p player)
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
             (smdl::obj-model-verts (smdl:get-model model-manager "shotgun.obj"))
             mvp))))
      ;; XXX: Placeholder for rendering shot, this should be done elsewhere.
      ;; Alternate fire (projectile)
      (let ((fire-button (game-key-down-p :mouse3)))
        (when (and fire-button (eq :clicked (key-press-state fire-button)))
          (setf (key-press-state fire-button) :held)
          (let ((rotation (q->mat (camera-rotation camera)))
                (position (camera-position camera)))
            (declare (special *game*))
            (game-add-thing *game*
             (make-projectile image-manager model-manager position :rotation rotation)))))
      ;; Primary fire (hitscan)
      (when (game-key-down-p :mouse1)
        (let* ((forward-dir (vnormalize (view-dir :forward camera)))
               (shot-range #.(shiva-float 20))
               (velocity (vscale shot-range forward-dir))
               ;; TODO: Seperate player position from camera.
               ;; (camera-offset (v 0d0 0.5d0 0d0))
               (origin (camera-position camera)) ;; (v- (camera-position camera) camera-offset))
               ;; (end-pos (v+ origin velocity))
               (hull (smdl:bsp-model-hull smdl:*world-model*)))
          (multiple-value-bind (mtrace hitp) (clip-hull hull origin (v+ origin velocity))
            (multiple-value-bind (hit-enemy dist) (hit-enemy-p origin (mtrace-endpos mtrace))
              (let ((dest (cond
                            (hit-enemy (v+ origin (vscale dist forward-dir)))
                            (hitp
                             (v+ (mtrace-endpos mtrace)
                                 (vscale #.(shiva-float 0.005d0) (mtrace-normal mtrace)))))))
                (declare (special *game*))
                (when hit-enemy
                  ;; NOTE: Removal is horribly inefficient.
                  (setf (game-think-things *game*)
                        (remove hit-enemy (game-think-things *game*)))
                  (setf (game-render-things *game*)
                        (remove hit-enemy (game-render-things *game*))))
                (when dest
                  (let ((mvp (m* (m* (camera-projection-matrix camera)
                                     (camera-view-transform camera))
                                 (m* (translation :x (vx dest) :y (vy dest) :z (vz dest))
                                     (scale :x 0.125 :y 0.125 :z 0.125)))))
                    (srend:render-surface
                     (smdl::obj-model-verts (smdl:model-manager-default-model model-manager))
                     mvp)))))))))))

(defun run-thinkers (thinkers)
  (dolist (thinker thinkers)
    (thing-think thinker)))

(defun render-things (render-things camera model-manager)
  (check-type camera camera)
  (check-type model-manager smdl::model-manager)
  (dolist (render-thing render-things)
    (ctypecase render-thing
      (weapon (weapon-render render-thing camera model-manager))
      (enemy (enemy-render render-thing camera model-manager))
      (projectile (projectile-render render-thing camera model-manager))
      (door (door-render render-thing camera))
      (test-model-thing (test-model-thing-render render-thing camera)))))

(defun restart-map (map-model game camera render-system model-manager)
  "Restart the map and return PLAYER."
  (declare (special *player* smdl:*world-model*))
  (check-type map-model smdl:bsp-model)
  (check-type game game)
  (check-type camera camera)
  (check-type render-system srend:render-system)
  (check-type model-manager smdl::model-manager)
  (setf (game-think-things game) nil)
  (setf (game-render-things game) nil)
  (prog1 (spawn-player (smdl:bsp-model-things map-model) camera)
    (spawn-things game (srend::render-system-image-manager render-system)
                  model-manager map-model)))

(defun load-map (map-fname game camera render-system model-manager)
  "Load a map from MAP-FNAME bsp file and return map model and player. Fills
the data for GAME, CAMERA, RENDER-SYSTEM and MODEL-MANAGER."
  (check-type map-fname string)
  (check-type game game)
  (check-type camera camera)
  (check-type render-system srend:render-system)
  (check-type model-manager smdl::model-manager)
  (let ((map-model (smdl:get-model model-manager map-fname)))
    ;; TODO: Handle map-model defaulted.
    (check-type map-model smdl:bsp-model)
    (load-map-textures render-system (smdl:bsp-model-nodes map-model))
    (values map-model
            (restart-map map-model game camera render-system model-manager))))

(defun call-with-init (function)
  "Initialize everything and run FUNCTION with RENDER-SYSTEM argument."
  ;; Calling sdl2:with-init will create a SDL2 Main Thread, and the body is
  ;; executed inside that thread.
  (sdl2:with-init (:everything)
    (with-cmd-system
      (let ((*console* (make-console))
            (*last-printed-error* nil)
            (*last-printed-warning* nil)
            ;; Make *BASE-DIR* our EXE-DIR in executable images.
            (*base-dir* (or (sdata:exe-dir) *base-dir*)))
        (declare (special *console* *last-printed-error* *last-printed-warning*))
        (reset-game-keys)
        (add-command 'base-dir (lambda () (printf "'~A'~%" *base-dir*)))
        (add-command 'exe-dir (lambda () (printf "'~A'~%" (sdata:exe-dir))))
        (add-command 'pwd (lambda () (printf "'~A'~%" (uiop:getcwd))))
        (with-data-dirs *base-dir*
          (add-command 'data-search-paths
                       (lambda () (printf "~{'~A'~%~}" sdata::*search-paths*)))
          (srend:with-render-system (render-system)
            (when (uiop:os-windows-p)
              (add-command 'show-console-window
                           (lambda ()
                             (cffi:load-foreign-library "User32.dll")
                             (cffi:defcfun "GetConsoleWindow" :pointer)
                             (cffi:defcfun "ShowWindow" :bool (hwnd :pointer) (ncmdshow :int))
                             (showwindow (getconsolewindow) 5)))
              (add-command 'hide-console-window
                           (lambda ()
                             (cffi:load-foreign-library "User32.dll")
                             (cffi:defcfun "GetConsoleWindow" :pointer)
                             (cffi:defcfun "ShowWindow" :bool (hwnd :pointer) (ncmdshow :int))
                             (showwindow (getconsolewindow) 0)))
              (command-progn () (hide-console-window)))
            (command-progn () (exec "default.cfg"))
            (funcall function render-system)))))))

(defmacro with-init ((render-system) &body body)
  `(call-with-init (lambda (,render-system) ,@body)))

(defun main-loop (game camera frame-timer minimized-p render-system model-manager)
  (declare (special *player*))
  (declare (special *console*))
  (with-timer (frame-timer)
    ;; TODO: This is a hack to allow using managers and game in tic logic
    ;; functions. Managers should probably be part of GAME struct and it
    ;; should be global.
    (let ((*image-manager* (srend::render-system-image-manager render-system))
          (*model-manager*  model-manager)
          (*game* game)
          (*camera* camera))
      (declare (special *image-manager* *model-manager* *game* *camera*))
      (if (zerop (- (get-time) *last-tic*))
          ;; Nothing to run, so sleep for a bit to ease off on the CPU.
          ;; This works great on keeping the laptop cool.
          (sleep 0)
          (progn
            (try-run-tics #'build-ticcmd
                          (lambda (ticcmd)
                            (run-tic game camera ticcmd)))
            (unless minimized-p
              (srend:with-draw-frame (render-system)
                (let ((*game* game))
                  (declare (special *game*))
                  (render-view *player* camera render-system
                               model-manager (game-render-things game)))
                (when (console-active-p *console*)
                  (srend:draw-gui-quad 0 0
                                       (srend:render-system-rend-width render-system)
                                       (* 0.5 (srend:render-system-rend-height render-system)))
                  (console-draw *console* render-system))
                (draw-timer-stats frame-timer)
                (draw-timer-stats
                 (srend::render-system-swap-timer render-system) :y -36))))))))

(defun main ()
  (with-init (render-system)
    (declare (special *console*))
    (check-type *console* console)
    (smdl:with-model-manager model-manager
      (with-resources "main"
        (load-main-resources render-system)
        (let* ((win (srend::render-system-window render-system))
               (aspect (/ (srend:render-system-rend-width render-system)
                          (srend:render-system-rend-height render-system)))
               (proj (make-perspective (* deg->rad #.(shiva-float 60d0))
                                       aspect
                                       #.(shiva-float 0.01d0)
                                       #.(shiva-float 100d0)))
               (camera (make-camera :projection proj :position (v 1 0.5 8)))
               (frame-timer (make-timer :name "Main Loop"))
               (game (make-game))
               (*player* nil))
          (declare (special *player*))
          (add-command 'spawn-test-model
                       (lambda (model-file image-file)
                         (let ((test-model (make-test-model-thing
                                            (srend::render-system-image-manager render-system)
                                            model-manager
                                            (camera-position camera)
                                            :rotation (q->mat (camera-rotation camera))
                                            :image-file image-file
                                            :model-file model-file)))
                           (game-add-thing game test-model)
                           test-model)))
          (add-command 'remove-test-models
                       (lambda ()
                         (setf (game-think-things game) (remove-if (lambda (thing)
                                                                     (typep thing 'test-model-thing))
                                                                   (game-think-things game)))
                         (setf (game-render-things game) (remove-if (lambda (thing)
                                                                      (typep thing 'test-model-thing))
                                                                    (game-render-things game)))
                         nil))
          (add-command 'reload-models (lambda ()
                                        (printf "Reloading all models...~%")
                                        (smdl:model-manager-reload-models model-manager)))
          (multiple-value-setq (smdl:*world-model* *player*)
            (load-map "test.bsp" game camera render-system model-manager))
          (srend:print-memory-usage render-system)
          (add-command 'restart-map
                       (lambda ()
                         (printf "Restarting map...~%")
                         (setf *player* (restart-map smdl:*world-model* game camera
                                                     render-system model-manager))
                         (printf "Restarted map.~%")))
          (add-command 'load-map
                       (lambda (map-fname)
                         (printf "Loading map '~A'...~%" map-fname)
                         ;; TODO: Unload old map models and things
                         (multiple-value-setq (smdl:*world-model* *player*)
                           (load-map map-fname game camera render-system model-manager))
                         (printf "Loaded '~A'.~%" map-fname)))
          (add-command 'tbkfa
                       (lambda ()
                         (let ((all-weapons (list (make-shotgun (srend::render-system-image-manager render-system)
                                                                model-manager (v 0 0 0)))))
                           (dolist (weapon all-weapons)
                             ;; TODO: This is copy-pasta
                             (push weapon (inventory-weapons (player-inventory *player*)))
                             (unless (player-current-weapon *player*)
                               (setf (player-current-weapon *player*) weapon))))
                         (printf "Very Happy Ammo Added~%")))
          (add-command 'my-pos
                       (lambda ()
                         (let* ((pos-2d (v3->v2 (player-position *player*)))
                                (map-hull (smdl:bsp-model-hull smdl:*world-model*))
                                (sector (hull-point-sector map-hull pos-2d))
                                (contents (hull-point-contents map-hull pos-2d)))
                           (printf "POS: ~A~%ANGLE: ~A~%~A~%~A~%"
                                   (player-position *player*)
                                   (camera-angles camera)
                                   contents sector))))
          (symbol-macrolet ((input-focus-p
                              (member :input-focus
                                      (sdl2:get-window-flags win)))
                            (minimized-p
                              (member :minimized
                                      (sdl2:get-window-flags win))))
            (start-game-loop)
            (sdl2:stop-text-input)
            (sdl2:with-event-loop (:method :poll)
              (:quit () t)
              (:textinput
               (:text utf8-code)
               (console-append *console* (if (<= 0 utf8-code 255) (code-char utf8-code) #\?))
               (console-autocomplete-clear *console*))
              (:keydown
               (:keysym keysym)
               (when input-focus-p
                 (when (eq :scancode-grave (sdl2:scancode keysym))
                   (if (console-active-p *console*) (sdl2:stop-text-input) (sdl2:start-text-input))
                   (zap #'not (console-active-p *console*)))
                 (if (console-active-p *console*)
                     (console-handle-keydown *console* keysym)
                     (press-game-key (sdl2:scancode keysym)))))
              (:keyup
               (:keysym keysym)
               (when input-focus-p
                 (unless (console-active-p *console*)
                   (release-game-key (sdl2:scancode keysym)))))
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
                     (main-loop game
                                camera
                                frame-timer
                                minimized-p
                                render-system
                                model-manager)))))))))

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
                         (when-let ((geometry (smdl:surface-geometry surf)))
                           (srend:render-surface geometry mvp))))
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

(defun render-view (player camera render-system model-manager render-things)
  (gl:enable :depth-test)
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :ccw)
  (gl:depth-func :less)
  (setf (camera-projection camera)
        (make-perspective (* deg->rad #.(shiva-float 60d0))
                          (/ (srend:render-system-rend-width render-system)
                             (srend:render-system-rend-height render-system))
                          #.(shiva-float 0.01d0)
                          #.(shiva-float 100d0)))
  (render-world camera smdl:*world-model*)
  ;; Draw "crosshair"
  (let ((width (srend:render-system-rend-width render-system))
        (height (srend:render-system-rend-height render-system)))
    ;; NOTE: Offset by half-cell size, to make it centered.
    (srend:draw-text "+" :x (- (floor width 2) 16) :y (- (floor height 2) 16)
                         :scale 2.0))
  (let ((image-manager (srend::render-system-image-manager render-system)))
    (player-render-weapon player camera image-manager model-manager))
  (render-things render-things camera model-manager))

(defun main-exe ()
  #+sbcl
  (if (member "--debug" uiop:*command-line-arguments* :test #'string=)
      (sb-ext:enable-debugger)
      (sb-ext:disable-debugger))
  (when-let ((swank (find-package :swank)))
    (let ((create-server (find-symbol (string :create-server) swank)))
      ;; NOTE: swank:create-server will bind to "localhost", so for non-local
      ;; connection the port needs to be forwarded. Prefer secure forward with
      ;; SSH, over unsecure like `socat` (or `netsh` on Windows).
      (funcall create-server :dont-close t)))
  (sdl2:make-this-thread-main
   (lambda ()
     (handler-bind
         ((error (lambda (e)
                   (sdl2-ffi.functions:sdl-show-simple-message-box
                    sdl2-ffi:+sdl-messagebox-error+ "Fatal Error"
                    (format nil "~A" e)
                    (cffi:null-pointer)))))
       (main)))))
