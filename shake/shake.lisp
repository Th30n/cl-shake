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
  (dist nil :type double-float :read-only t))

(defstruct projection
  (matrix nil :type (mat 4) :read-only t))

(defstruct (perspective (:include projection)
                        (:constructor make-perspective-priv))
  (fovy nil :type (double-float 0d0) :read-only t)
  (aspect nil :type (double-float 0d0) :read-only t)
  (near nil :type (double-float 0d0) :read-only t)
  (far nil :type (double-float 0d0) :read-only t))

(defun make-perspective (fovy aspect near far)
  (assert (double> (coerce far 'double-float) (coerce near 'double-float)))
  (make-perspective-priv :fovy (coerce fovy 'double-float)
                         :aspect (coerce aspect 'double-float)
                         :near (coerce near 'double-float)
                         :far (coerce far 'double-float)
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
  (check-type bound-y double-float)
  (with-struct (plane- normal dist) plane
    (let ((center (make-array 3 :element-type 'double-float))
          (h (make-array 2 :element-type 'double-float))
          (bound-sum (v+ mins maxs))
          (bound-diff (v- maxs mins)))
      (declare (dynamic-extent center h bound-sum bound-diff))
      (setf
       ;; (setf center (vscale 0.5d0 bound-sum))
       (vx center) (* 0.5d0 (vx bound-sum))
       (vy center) bound-y  ; v2->v3
       (vz center) (* 0.5d0 (vy bound-sum))
       ;; NOTE: Using LET* and DYNAMIC-EXTENT breaks for some reason
       ;; (setf h (vscale 0.5d0 bound-diff))
       (vx h) (* 0.5d0 (vx bound-diff))
       (vy h) (* 0.5d0 (vy bound-diff)))
      (let ((s (+ (v3dot center normal) dist))
            (e (+ (* (vx h) (abs (vx normal)))
                  (* bound-y (abs (vy normal)))
                  ;; Note 2D vy iz vz in 3D.
                  (* (vy h) (abs (vz normal))))))
        (cond
          ((double> (- s e) 0d0) nil) ;; outside
          ((double> 0d0 (+ s e)) :inside)
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
  (check-type bound-y double-float)
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
      ((>= old-v-angle 90d0) (decf old-v-angle 180d0))
      ((<= old-v-angle -90d0) (incf old-v-angle 180d0)))
    (let ((v-angle (clamp (+ old-v-angle v-angle-diff) -89d0 89d0)))
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
                              name (* 1d3 avg) (* 1d3 max))
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

(defstruct ticcmd
  (forward-move 0d0 :type double-float)
  (side-move 0d0 :type double-float)
  (angle-turn (cons 0d0 0d0) :type (cons double-float double-float)))

(defun build-ticcmd ()
  (let ((xrel (car *mouse*))
        ;; Invert the Y movement.
        (yrel (- (cdr *mouse*)))
        (sens 1.5d0)
        (move-speed 5d0)
        (cmd (make-ticcmd)))
    (when (game-key-down-p :scancode-w)
      (incf (ticcmd-forward-move cmd) move-speed))
    (when (game-key-down-p :scancode-s)
      (decf (ticcmd-forward-move cmd) move-speed))
    (when (game-key-down-p :scancode-d)
      (incf (ticcmd-side-move cmd) move-speed))
    (when (game-key-down-p :scancode-a)
      (decf (ticcmd-side-move cmd) move-speed))
    (incf (car (ticcmd-angle-turn cmd)) (* xrel (/ sens 10d0)))
    (incf (cdr (ticcmd-angle-turn cmd)) (* yrel (/ sens 10d0)))
    (setf (car *mouse*) 0)
    (setf (cdr *mouse*) 0)
    cmd))

;; Player movement

(defun clip-velocity (velocity normal)
  "Clip the given VELOCITY by projecting it on NORMAL and return a parallel
  vector to the surface."
  (let ((change (vscale (vdot velocity normal) normal)))
    (v- velocity change)))

(defconstant +step-height+ 0.375d0 "Height of a stair step.")

(defun player-climb-move (origin velocity hull)
  (let ((step-origin (v+ origin (v 0d0 +step-height+ 0d0))))
    (if (/= 1d0 (mtrace-fraction (recursive-hull-check hull origin step-origin)))
        ;; Unable to move up to climb a stair.
        origin
        ;; Try climbing a stair with regular movement.
        ;; XXX: What if we need to climb again?
        (mtrace-endpos (recursive-hull-check hull step-origin
                                             (v+ step-origin velocity))))))

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
         (mtrace (recursive-hull-check hull origin (v+ origin velocity))))
    (if (= (mtrace-fraction mtrace) 1d0)
        ;; Completed the whole move.
        (mtrace-endpos mtrace)
        ;; Partial move, try sliding or climbing.
        ;; XXX: Maybe use time-left to scale climb velocity?
        (let ((climb-endpos (player-climb-move origin velocity hull))
              (slide-endpos
               (let ((time-left (- 1d0 (mtrace-fraction mtrace)))
                     (new-origin (mtrace-endpos mtrace))
                     (new-vel (clip-velocity velocity
                                             (mtrace-normal mtrace))))
                 (mtrace-endpos
                  (recursive-hull-check hull new-origin
                                        (v+ new-origin
                                            (vscale time-left new-vel)))))))
          (if (double> (vdistsq (v3->v2 origin) (v3->v2 climb-endpos))
                       (vdistsq (v3->v2 origin) (v3->v2 slide-endpos)))
              climb-endpos
              slide-endpos)))))

(defun groundedp (entity-position)
  (declare (special smdl:*world-model*))
  (check-type entity-position (vec 3))
  (let ((sector (hull-point-sector (smdl:bsp-model-hull smdl:*world-model*)
                                   (v3->v2 entity-position))))
    (or (not sector) ; We are off the map, treat it as grounded.
        (double= (sbsp:sector-floor-height sector) (vy entity-position)))))

(defun apply-gravity (entity-position)
  (declare (special *time-delta* smdl:*world-model*))
  (check-type entity-position (vec 3))
  (flet ((clamp-to-floor (position)
           (let ((floor (sbsp:sector-floor-height (hull-point-sector (smdl:bsp-model-hull smdl:*world-model*)
                                                                     (v3->v2 position)))))
             (v (vx position) (max (vy position) floor) (vz position)))))
    (if (groundedp entity-position)
        entity-position
        (let* ((gravity-speed 10d0)
               ;; TODO: Use remaining time fraction.
               (gravity-velocity (v 0d0 (- (* gravity-speed *time-delta*)) 0d0)))
          ;; TODO: Remove `clamp-to-floor' when `recursive-hull-check'
          ;; correctly clips vertical movement
          (clamp-to-floor
           (mtrace-endpos (recursive-hull-check (smdl:bsp-model-hull smdl:*world-model*)
                                                entity-position
                                                (v+ entity-position gravity-velocity))))))))

(defun move-player (player forward-move side-move &key (noclip nil))
  (let ((forward-dir (view-dir :forward player)))
    (unless noclip
      ;; Project forward-dir to plane of movement.
      (setf forward-dir (vnormalize (v (vx forward-dir) 0 (vz forward-dir)))))
    (let* ;; Intentionally make diagonal movement faster.
        ((velocity (v+ (vscale forward-move forward-dir)
                       (vscale side-move (view-dir :right player))))
         ;; TODO: Seperate player position from camera.
         (camera-offset (v 0d0 0.5d0 0d0))
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
    (let ((*time-delta* (coerce (/ +ticrate+) 'double-float))
          (noclip nil))
      (declare (special *time-delta*))
      (unless (and (zerop forward-move) (zerop side-move))
        (move-player camera (* forward-move *time-delta*) (* side-move *time-delta*) :noclip noclip))
      (destructuring-bind (x-turn . y-turn) angle-turn
        (unless (and (zerop x-turn) (zerop y-turn))
          (nrotate-camera x-turn y-turn camera)))
      (unless noclip
        (let ((camera-offset (v 0d0 0.5d0 0d0)))
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
                             textures)
      (srend:print-memory-usage render-system))))

(defun spawn-player (things camera)
  (dolist (thing things)
    (when (eq (sbsp:map-thing-type thing) :player-spawn)
      (let ((pos (sbsp:map-thing-pos thing))
            (angle (sbsp:map-thing-angle thing)))
        (setf (camera-position camera) (v (vx pos) 0.5 (vy pos)))
        (return (nrotate-camera angle 0d0 camera))))))

(defun call-with-init (function)
  "Initialize everything and run FUNCTION with RENDER-SYSTEM and WINDOW arguments."
  ;; Calling sdl2:with-init will create a SDL2 Main Thread, and the body is
  ;; executed inside that thread.
  (sdl2:with-init (:everything)
    (reset-game-keys)
    (with-data-dirs *base-dir*
      (set-gl-attrs)
      (let ((*win-width* 800) (*win-height* 600)
            (*rend-width* 800) (*rend-height* 600))
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
        (let* ((proj (make-perspective (* deg->rad 60d0)
                                       (/ *win-width* *win-height*)
                                       0.01d0 100d0))
               (camera (make-camera :projection proj :position (v 1 0.5 8)))
               (frame-timer (make-timer :name "Main Loop"))
               (smdl:*world-model* (smdl:get-model model-manager "test.bsp")))
          (load-map-textures render-system (smdl:bsp-model-nodes smdl:*world-model*))
          (spawn-player (smdl:bsp-model-things smdl:*world-model*) camera)
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
              (:idle ()
                     (with-timer (frame-timer)
                       (try-run-tics #'build-ticcmd
                                     (lambda (tic) (run-tic camera tic)))
                       (unless minimized-p
                         (srend:with-draw-frame (render-system)
                           (render camera)
                           (let ((mvp (m* (m* (camera-projection-matrix camera)
                                              (camera-view-transform camera))
                                          (m* (translation :x 17d0 :y 0.25d0 :z 51d0)
                                              (scale :x 0.5d0 :y 0.5d0 :z 0.5d0)))))
                             (srend:render-surface
                              (smdl::obj-model-verts (smdl:model-manager-default-model model-manager))
                              mvp))
                           (draw-timer-stats frame-timer)
                           (draw-timer-stats
                            (srend::render-system-swap-timer render-system) :y -36)
                           )))))))))))

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
    (let ((pos-2d (make-array 2 :element-type 'double-float))
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

(defun render (camera)
  (gl:enable :depth-test)
  (gl:depth-func :less)
  (render-world camera smdl:*world-model*))

(defun uniform-mvp (program mvp)
  (with-uniform-locations program mvp
    (uniform-matrix-4f mvp-loc (list mvp))))
