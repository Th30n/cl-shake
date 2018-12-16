;;;; Game definition and logic

(in-package #:shake)

(defstruct game
  (think-things nil :type list)
  (render-things nil :type list))

(defstruct weapon
  (model nil :type smdl::obj-model)
  (position (v 0 0 0) :type (vec 3))
  (scale #.(shiva-float 1.0) :type shiva-float)
  (angle-y #.(shiva-float 0.0) :type shiva-float)
  (bounds-scale (v 1 1 1) :type (vec 3)))

(defun game-add-weapon (game weapon)
  (check-type game game)
  (check-type weapon weapon)
  (push weapon (game-think-things game))
  (push weapon (game-render-things game)))

(defun make-shotgun (model-manager position &key (angle-y #.(shiva-float 0.0)))
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

(defun weapon-think (weapon)
  (check-type weapon weapon)
  (setf (weapon-angle-y weapon) (shiva-float (mod (get-time) 360))))
