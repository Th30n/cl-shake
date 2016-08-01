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

(in-package #:shake-ed.map-scene)
(in-readtable :qtools)

(defconstant +initial-grid-step+ 64)

(defun scene->map-unit (x)
  "Convert a scene coordinate into a map unit. Each unit in scene coordinates
  consists of 64 map units."
  (round (* 64d0 x)))

(defun map->scene-unit (x)
  "Divides a map unit by 64 to obtain the scene coordinate."
  (/ x 64d0))

(defun snap-scene-to-map-unit (x)
  "Snaps the scene coordinate to match with nearest map unit."
  (map->scene-unit (scene->map-unit x)))

(defun snap-scene-to-grid (x grid-step)
  "Snaps the scene coordinate to grid designated in map units."
  (let ((map-x (scene->map-unit x)))
    (map->scene-unit (* grid-step (round map-x grid-step)))))


(defstruct mbrush
  "Map representation of SBRUSH:BRUSH."
  (brush nil :type sbrush:brush)
  (rotation 0d0 :type double-float))

(defun mbrush-lines (mbrush)
  "Returns LINEDEFS after applying transformations from MBRUSH."
  (sbrush:brush-lines
   (sbrush:brush-rotate (mbrush-brush mbrush)
                        (* deg->rad (mbrush-rotation mbrush)))))

(define-widget map-scene (QGraphicsScene)
  ((draw-info :initform nil)
   (edit-mode :initform :mode-brush-create)
   (graphics-item-brush-map :initform (make-hash-table))
   (graphics-item-thing-map :initform (make-hash-table))
   (view-normals-p :initform nil)
   (grid-step :initform +initial-grid-step+)))

(define-signal (map-scene mouse-scene-pos) (double double))
(define-signal (map-scene grid-step-changed) (int))

(defun scale-grid-step (map-scene &key (scale 2))
  (with-slots (grid-step) map-scene
    (setf grid-step (clamp (round (* scale grid-step)) 1 256))
    (q+:update map-scene (q+:scene-rect map-scene))
    (signal! map-scene (grid-step-changed int) grid-step)))

(defun draw-grid (painter start-x end-x start-y end-y grid-step)
  (flet ((draw-lines (start end axis)
           (loop for x from start upto end by grid-step
              as scene-x = (map->scene-unit x)
              as start-coords = (if (= 0 axis)
                                    (list scene-x start-y)
                                    (list start-x scene-x))
              and end-coords = (if (= 0 axis)
                                   (list scene-x end-y)
                                   (list end-x scene-x))
              do (with-finalizing ((p1 (apply #'q+:make-qpointf start-coords))
                                   (p2 (apply #'q+:make-qpointf end-coords)))
                   (q+:draw-line painter p1 p2))))
         (grid-end (scene-pos round-fun)
           (* grid-step (funcall round-fun
                                 (scene->map-unit scene-pos) grid-step))))
    (draw-lines (grid-end start-x #'ceiling) (grid-end end-x #'floor) 0)
    (draw-lines (grid-end start-y #'ceiling) (grid-end end-y #'floor) 1)))

(define-override (map-scene draw-background) (painter rect)
  (q+:fill-rect painter rect (q+:qt.black))
  (with-finalizing ((color (q+:make-qcolor 40 40 40))
                    (axis-color (q+:make-qcolor 60 60 0)))
    (q+:set-pen painter color)
    (draw-grid painter (q+:left rect) (q+:right rect)
               (q+:top rect) (q+:bottom rect) grid-step)
    ;; Draw axis in different color.
    (q+:set-pen painter axis-color)
    (draw-grid painter (q+:left rect) (q+:right rect) 0 0 grid-step)
    (draw-grid painter 0 0 (q+:top rect) (q+:bottom rect) grid-step)))

(defun v2->qpoint (vector-2d)
  (q+:make-qpointf (vx vector-2d) (vy vector-2d)))

(defun draw-linedef-normal (line painter &key (scale 0.25d0))
  (let* ((center (v+ (sbsp:linedef-start line)
                     (vscale 0.5d0 (sbsp:linedef-vec line))))
         (dest (v+ center (vscale scale (sbsp:linedef-normal line)))))
    (with-finalizing* ((p1 (v2->qpoint center))
                       (p2 (v2->qpoint dest)))
      (q+:draw-line painter p1 p2))))

(define-override (map-scene draw-foreground) (painter rect)
  (when view-normals-p
    (with-finalizing ((color (q+:make-qcolor 0 255 255)))
      (q+:set-pen painter color)
      (let ((items (q+:items map-scene)))
        (dolist (item items)
          (when-let ((mbrush (gethash item graphics-item-brush-map)))
            (dolist (line (mbrush-lines mbrush))
              (draw-linedef-normal line painter)))))))
  (stop-overriding))

(defun cancel-editing (map-scene)
  (with-slots (draw-info) map-scene
    (when draw-info
      (let ((group (first draw-info)))
        (q+:remove-item map-scene group)
        (finalize group)
        (setf draw-info nil)))))

(defun setup-graphicsitem (item)
  (with-finalizing* ((color (q+:make-qcolor (q+:qt.dark-green)))
                     (pen (q+:make-qpen color)))
    (setf (q+:width-f pen) (map->scene-unit 2)
          (q+:pen item) pen
          (q+:flag item) (values (q+:qgraphicsitem.item-is-selectable) t)))
  item)

(defun new-pointitem (center &key radius)
  "Return a QGraphicsEllipseItem representing a point."
  (let* ((r (if radius radius (map->scene-unit 2)))
         (x (- (q+:x center) r))
         (y (- (q+:y center) r))
         (item (q+:make-qgraphicsellipseitem x y (* 2 r) (* 2 r))))
    (with-finalizing* ((color (q+:make-qcolor (q+:qt.dark-green)))
                       (brush (q+:make-qbrush color (q+:qt.solid-pattern))))
      (setf (q+:brush (setup-graphicsitem item)) brush))
    item))

(defun new-lineitem (p1 p2)
  (with-finalizing ((new-line (q+:make-qlinef p1 p2)))
    (setup-graphicsitem (q+:make-qgraphicslineitem new-line))))

(defun linedef->lineitem (line)
  (with-finalizing ((p1 (v2->qpoint (sbsp:linedef-start line)))
                    (p2 (v2->qpoint (sbsp:linedef-end line))))
    (new-lineitem p1 p2)))

(defun make-itemgroup-from-lines (lines)
  (let ((itemgroup (q+:make-qgraphicsitemgroup)))
    (dolist (line lines itemgroup)
      (q+:add-to-group itemgroup (linedef->lineitem line)))))

(defun update-brush-drawing (draw-info scene-pos)
  (let ((group (first draw-info))
        (item (second draw-info))
        (last-pos (lastcar draw-info)))
    (qtypecase item
      (qgraphicsellipseitem
       (unless (v= last-pos scene-pos)
         (q+:remove-from-group group item)
         (finalize item)
         (with-finalizing ((start-point (v2->qpoint last-pos))
                           (end-point (v2->qpoint scene-pos)))
           (let ((line-item (new-lineitem start-point end-point)))
             (setf (second draw-info) line-item)
             (q+:add-to-group group line-item)))))
      (qgraphicslineitem
       (if (not (v= last-pos scene-pos))
           (q+:set-line item
                        (vx last-pos) (vy last-pos)
                        (vx scene-pos) (vy scene-pos))
           (progn
             (q+:remove-from-group group item)
             (finalize item)
             (with-finalizing ((point (v2->qpoint last-pos)))
               (let ((point-item (new-pointitem point)))
                 (setf (second draw-info) point-item)
                 (q+:add-to-group group point-item)))))))))

(defun add-or-update-brush (map-scene scene-pos)
  (with-slots (draw-info graphics-item-brush-map) map-scene
    (with-finalizing ((scene-point (v2->qpoint scene-pos)))
      (if draw-info
          (progn
            (let ((first-pos (third draw-info))
                  (group (first draw-info))
                  (points (cddr draw-info)))
              (cond
                ((and (v= scene-pos first-pos) (length>= 3 points))
                 ;; finish line loop
                 (let ((linedefs (apply #'sbsp:make-linedef-loop points)))
                   (dolist (child (q+:child-items group))
                     (q+:remove-from-group group child)
                     (finalize child))
                   (dolist (line linedefs)
                     (q+:add-to-group group (linedef->lineitem line)))
                   (handler-case
                       (let ((surfs (mapcar #'sbsp:linedef->sidedef linedefs)))
                         (setf (gethash group graphics-item-brush-map)
                               (make-mbrush :brush
                                            (sbrush:make-brush :surfaces surfs))
                               draw-info nil))
                     (sbrush:non-convex-brush-error ()
                       (cancel-editing map-scene)))
                   (q+:update map-scene (q+:scene-rect map-scene))))
                ;; end current line and continue with new
                ((not (v= scene-pos first-pos))
                 (let ((new-point-item (new-pointitem scene-point)))
                   (update-brush-drawing draw-info scene-pos)
                   (q+:add-to-group group new-point-item)
                   (setf draw-info
                         (append (list group new-point-item) points
                                 (list scene-pos))))))))
          (progn
            (let ((group (q+:make-qgraphicsitemgroup))
                  (point-item (new-pointitem scene-point)))
              (setf draw-info (list group point-item scene-pos))
              (q+:set-flag group (q+:qgraphicsitem.item-is-selectable) t)
              (q+:add-to-group group point-item)
              (q+:add-item map-scene group)))))))

(defun within-scene-p (scene point)
  (q+:contains (q+:scene-rect scene) point))

(defun scene-pos-from-mouse (mouse-event grid-step)
  (let* ((scene-pos (q+:scene-pos mouse-event))
         (x (q+:x scene-pos))
         (y (q+:y scene-pos))
         (snap-to-grid-p (enum-equal (q+:modifiers mouse-event)
                                     (q+:qt.control-modifier))))
    (if snap-to-grid-p
        (v (snap-scene-to-grid x grid-step) (snap-scene-to-grid y grid-step))
        (v (snap-scene-to-map-unit x) (snap-scene-to-map-unit y)))))

(define-override (map-scene mouse-press-event) (mouse-event)
  (when (within-scene-p map-scene (q+:scene-pos mouse-event))
    (cond
      ((enum-equal (q+:button mouse-event) (q+:qt.right-button))
       (add-or-update-brush map-scene
                            (scene-pos-from-mouse mouse-event grid-step)))
      ((enum-equal (q+:button mouse-event) (q+:qt.left-button))
       (let* ((view (car (q+:views map-scene)))
              (item (q+:item-at map-scene (q+:scene-pos mouse-event)
                                (q+:transform view))))
         (if (null-qobject-p item)
             (q+:clear-selection map-scene)
             (setf (q+:selected item) t))))
      (t (stop-overriding)))))

(define-override (map-scene mouse-move-event) (mouse-event)
  (when (and draw-info (within-scene-p map-scene (q+:scene-pos mouse-event)))
    (let ((scene-pos (scene-pos-from-mouse mouse-event grid-step)))
      (update-brush-drawing draw-info scene-pos)))
  (signal! map-scene (mouse-scene-pos double double)
           (q+:x (q+:scene-pos mouse-event))
           (q+:y (q+:scene-pos mouse-event))))

(defun remove-selected (scene)
  (let ((items (q+:selected-items scene)))
    (with-slots (draw-info graphics-item-brush-map) scene
      (dolist (item items)
        (when (eq item (car draw-info))
          (setf draw-info nil))
        (remhash item graphics-item-brush-map)
        (q+:remove-item scene item)
        (finalize item))))
  (q+:update scene (q+:scene-rect scene)))

(define-override (map-scene key-press-event) (key-event)
  (cond
    ((q+:matches key-event (q+:qkeysequence.delete))
     (remove-selected map-scene))
    ((enum-equal (q+:key key-event) (q+:qt.key_escape))
     (cancel-editing map-scene))
    (t (stop-overriding))))

(defun toggle-view-normals (scene)
  (with-slots (view-normals-p) scene
    (notf view-normals-p)
    (q+:update scene (q+:scene-rect scene))))

(defun clear-map (scene)
  (with-slots (graphics-item-brush-map graphics-item-thing-map) scene
    (q+:clear scene)
    (clrhash graphics-item-brush-map)
    (clrhash graphics-item-thing-map)))

(defun write-map (stream scene)
  (with-slots (graphics-item-brush-map graphics-item-thing-map) scene
    (let ((mbrushes (hash-table-values graphics-item-brush-map))
          (things (hash-table-values graphics-item-thing-map)))
      (flet ((convert-brush (mbrush)
               (sbrush:brush-rotate (mbrush-brush mbrush)
                                    (* deg->rad (mbrush-rotation mbrush)))))
        (sbsp:write-map
         (sbsp:make-map-file :brushes (mapcar #'convert-brush mbrushes)
                             :things things)
         stream)))))

(defun make-thing-graphics-item (thing)
  (flet ((set-item-pos (item)
           (with-finalizing ((pos (v2->qpoint
                                   ;; Offset the image to center of half-cell.
                                   (v- (sbsp:map-thing-pos thing)
                                       (v 0.25 0.25))))
                             (size (q+:default-size (q+:renderer item))))
             (q+:set-pos item pos)
             ;; Scale the image to fit in half a cell.
             (q+:set-scale item (/ 0.5d0 (max (q+:width size)
                                              (q+:height size))))
             (q+:set-rotation item (sbsp:map-thing-angle thing)))
           item))
    (let ((image-file
           (concatenate 'string ":/things/"
                        (case (sbsp:map-thing-type thing)
                          (:player-spawn "player.svg")
                          (otherwise "invalid.svg")))))
      (set-item-pos (q+:make-qgraphicssvgitem image-file)))))

(defun add-thing-to-scene (scene thing)
  (with-slots (graphics-item-thing-map) scene
    (let ((item (make-thing-graphics-item thing)))
      (q+:add-item scene item)
      (setf (gethash item graphics-item-thing-map) thing))))

(defun read-map (stream scene)
  (clear-map scene)
  (with-slots (graphics-item-brush-map) scene
    (let ((map-file (sbsp:read-map stream)))
      (dolist (brush (sbsp:map-file-brushes map-file))
        (let ((item (make-itemgroup-from-lines (sbrush:brush-lines brush))))
          (q+:set-flag item (q+:qgraphicsitem.item-is-selectable) t)
          (q+:add-item scene item)
          (setf (gethash item graphics-item-brush-map)
                (make-mbrush :brush brush))))
      (dolist (thing (sbsp:map-file-things map-file))
        (add-thing-to-scene scene thing)))))

(defun selected-brushes (scene)
  (with-slots (graphics-item-brush-map) scene
    (mapcar (rcurry #'gethash graphics-item-brush-map)
            (q+:selected-items scene))))

(defun rotate-selected (scene)
  (with-slots (graphics-item-brush-map) scene
    (when-let ((items (q+:selected-items scene)))
      (dolist (item items)
        (let* ((old-deg (q+:rotation item))
               (new-deg (+ old-deg 10))
               (rounds (floor new-deg 360))
               (clamped-deg (- new-deg (* rounds 360))))
          (q+:set-transform-origin-point item (q+:center (q+:bounding-rect item)))
          (q+:set-rotation item clamped-deg)
          (setf (mbrush-rotation (gethash item graphics-item-brush-map))
                new-deg))))))
