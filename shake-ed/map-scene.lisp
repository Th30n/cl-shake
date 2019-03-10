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


(defun convert-brush (mbrush)
  (sbrush:brush-rotate (mbrush-brush mbrush)
                       (* deg->rad (mbrush-rotation mbrush))))

(define-widget map-scene (QGraphicsScene)
  ((draw-info :initform nil)
   (highlighted-item :initform nil)
   ;; Selected graphics items.
   (selected-items :initform nil)
   ;; Selected map data types corresponding to graphics items.
   (selection :initform nil)
   (edit-mode :initform :lines :reader map-scene-edit-mode)
   (brushes :initform (make-array 256 :element-type '(or null mbrush)
                                  :adjustable t :fill-pointer 0))
   ;; Maps qt graphics items to BRUSHES vector index
   (graphics-item-brush-map :initform (make-hash-table))
   (things :initform (make-array 256 :element-type '(or null sbsp:map-thing)
                                 :adjustable t :fill-pointer 0))
   ;; Maps qt graphics items to THINGS vector index
   (graphics-item-thing-map :initform (make-hash-table))
   (view-normals-p :initform nil)
   (grid-step :initform +initial-grid-step+)))

(defun push-selection (map-scene graphics-item data-item)
  (assert (not (null data-item)))
  (assert (qinstancep graphics-item 'qgraphicsitem))
  (with-slots (selected-items selection) map-scene
    (push graphics-item selected-items)
    (push data-item selection)))

(defun clear-selection (map-scene)
  (with-slots (selected-items selection) map-scene
    (q+:clear-selection map-scene)
    (setf selected-items nil)
    (setf selection nil)))

(defun clear-map (scene)
  (with-slots-bound (scene map-scene)
    (clear-selection scene)
    (q+:clear scene)
    (setf highlighted-item nil)
    (setf draw-info nil)
    (setf (fill-pointer brushes) 0)
    (clrhash graphics-item-brush-map)
    (setf (fill-pointer things) 0)
    (clrhash graphics-item-thing-map)))

(defun brush-for-graphics-item (map-scene item)
  (declare (type map-scene map-scene))
  (with-slots (graphics-item-brush-map brushes) map-scene
    (when-let ((i (gethash item graphics-item-brush-map)))
      (assert (and (>= i 0) (< i (length brushes))))
      (aref brushes i))))

(defun map-scene-add-brush (map-scene item brush)
  (declare (type map-scene map-scene))
  (declare (type mbrush brush))
  (with-slots (brushes graphics-item-brush-map) map-scene
    (setf (gethash item graphics-item-brush-map) (length brushes))
    (vector-push-extend brush brushes)))

(defun map-scene-remove-brush (map-scene item)
  (declare (type map-scene map-scene))
  (with-slots (brushes graphics-item-brush-map) map-scene
    (let ((i (gethash item graphics-item-brush-map)))
      (assert (and (>= i 0) (< i (length brushes))))
      ;; TODO: What if the vector get's really large?
      (setf (aref brushes i) nil))
    (remhash item graphics-item-brush-map)))

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

(defun draw-linedef-normal (line painter &key (scale 0.25))
  (let* ((center (v+ (sbsp:linedef-start line)
                     (vscale 0.5 (sbsp:linedef-vec line))))
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
          (when-let ((mbrush (brush-for-graphics-item map-scene item)))
            (dolist (line (sbrush:brush-lines (convert-brush mbrush)))
              (draw-linedef-normal line painter)))))))
  (stop-overriding))

(defun cancel-editing (map-scene)
  (with-slots (draw-info) map-scene
    (when draw-info
      (let ((group (first draw-info)))
        (q+:remove-item map-scene group)
        (finalize group)
        (setf draw-info nil)))))

(defun make-qpen (width color)
  (with-finalizing ((qcolor (q+:make-qcolor color)))
    (let ((pen (q+:make-qpen qcolor)))
      (setf (q+:width-f pen) width)
      pen)))

(defun set-default-pen (item)
  (with-finalizing ((pen (make-qpen (map->scene-unit 2) (q+:qt.dark-green))))
    (setf (q+:pen item) pen)))

(defun setup-graphicsitem (item)
  (set-default-pen item)
  (setf (q+:flag item) (values (q+:qgraphicsitem.item-is-selectable) nil))
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
  (with-slots (draw-info) map-scene
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
                         (map-scene-add-brush
                          map-scene group
                          (make-mbrush :brush (sbrush:make-brush :surfaces surfs)))
                         (setf draw-info nil))
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
              (q+:set-flag group (q+:qgraphicsitem.item-is-selectable) nil)
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

(defun thing-for-graphics-item (map-scene item)
  (declare (type map-scene map-scene))
  (with-slots (graphics-item-thing-map things) map-scene
    (when-let ((i (gethash item graphics-item-thing-map)))
      (assert (and (>= i 0) (< i (length things))))
      (aref things i))))

(defun add-thing-to-scene (scene thing)
  (declare (type map-scene scene))
  (declare (type sbsp:map-thing thing))
  (with-slots (graphics-item-thing-map things) scene
    (let ((item (make-thing-graphics-item thing)))
      (q+:add-item scene item)
      (setf (gethash item graphics-item-thing-map) (length things))
      (vector-push-extend thing things))))

(defun remove-thing-from-scene (map-scene item)
  (declare (type map-scene map-scene))
  (with-slots (graphics-item-thing-map things) map-scene
    (let ((i (gethash item graphics-item-thing-map)))
      (assert (and (>= i 0) (< i (length things))))
      (setf (aref things i) nil)
      (remhash item graphics-item-thing-map))))

(defun insert-player-spawn (scene scene-pos)
  (with-slots (graphics-item-thing-map things) scene
    (flet ((remove-player-spawn (item thing-i)
             (when (eq :player-spawn
                       (sbsp:map-thing-type (thing-for-graphics-item scene item)))
               (assert (and (>= thing-i 0) (< thing-i (length things))))
               (setf (aref things thing-i) nil)
               ;; The function is allowed to remove/modify *currently*
               ;; processed hash entry according to CLHS.
               (remhash item graphics-item-thing-map)
               (q+:remove-item scene item)
               (finalize item))))
      (maphash #'remove-player-spawn graphics-item-thing-map)))
  (add-thing-to-scene scene (sbsp:make-map-thing :type :player-spawn
                                                 :pos scene-pos)))

(defun insert-shotgun (scene scene-pos)
  (with-slots (graphics-item-thing-map things) scene
    (add-thing-to-scene scene (sbsp:make-map-thing :type :shotgun
                                                   :pos scene-pos))))

(defun insert-enemy (scene scene-pos)
  (with-slots (graphics-item-thing-map things) scene
    (add-thing-to-scene scene (sbsp:make-map-thing :type :enemy
                                                   :pos scene-pos))))

(defun show-things-menu (scene scene-pos)
  (with-finalizing* ((pos (q+:qcursor-pos))
                     (menu (q+:make-qmenu "Things"))
                     (player-spawn-action (q+:add-action menu "Set Player Spawn"))
                     (weapons-menu (q+:make-qmenu "Add Weapon"))
                     (shotgun-spawn-action (q+:add-action weapons-menu "Shotgun"))
                     (npcs-menu (q+:make-qmenu "Add NPC"))
                     (enemy-spawn-action (q+:add-action npcs-menu "Enemy")))
    (q+:add-menu menu weapons-menu)
    (q+:add-menu menu npcs-menu)
    (let ((res (q+:exec menu pos)))
      (unless (null-qobject-p res)
        (cond
          ((string= (q+:text player-spawn-action) (q+:text res))
           (insert-player-spawn scene scene-pos))
          ((string= (q+:text shotgun-spawn-action) (q+:text res))
           (insert-shotgun scene scene-pos))
          ((string= (q+:text enemy-spawn-action) (q+:text res))
           (insert-enemy scene scene-pos)))))))

(defun select-line (item)
  (with-finalizing ((pen (make-qpen (map->scene-unit 4)
                                    (q+:qt.blue))))
    (setf (q+:pen item) pen)))

(defun select-sidedef (map-scene highlighted-items)
  (declare (type map-scene map-scene))
  (declare (type list highlighted-items))
  (labels ((lineitem-name (item)
             (let* ((mbrush (brush-for-graphics-item map-scene
                                                     (q+:parent-item item)))
                    (center (sbrush::brush-center (mbrush-brush mbrush))))
               (format nil "Line for brush at (~A, ~A)"
                       (scene->map-unit (vx center))
                       (scene->map-unit (vy center)))))
           (show-choices-menu ()
             (with-finalizing* ((pos (q+:qcursor-pos))
                                (menu (q+:make-qmenu "Lines")))
               (let* ((line-names (mapcar #'lineitem-name highlighted-items))
                      (actions (mapcar (lambda (name)
                                         (q+:add-action menu name))
                                       line-names)))
                 (with-finalizing ((res (q+:exec menu pos)))
                   (unless (null-qobject-p res)
                     (position (q+:text res) actions
                               :key #'q+:text :test #'string=)))))))
    (let (chosen-item)
      (if (null (cdr highlighted-items))
          (setf chosen-item (car highlighted-items))
          (when-let ((item-index (show-choices-menu)))
            (setf chosen-item (nth item-index highlighted-items))))
      (when chosen-item
        (select-line chosen-item)
        (push-selection
         map-scene chosen-item
         (sidedef-for-lineitem map-scene chosen-item))))))

(define-override (map-scene mouse-press-event) (mouse-event)
  (when (within-scene-p map-scene (q+:scene-pos mouse-event))
    (cond
      ((enum-equal (q+:button mouse-event) (q+:qt.right-button))
       (let ((scene-pos (scene-pos-from-mouse mouse-event grid-step)))
         (case edit-mode
           (:lines
            (add-or-update-brush map-scene scene-pos))
           (:things (show-things-menu map-scene scene-pos)))))
      ((enum-equal (q+:button mouse-event) (q+:qt.left-button))
       (case edit-mode
         (:lines
          (if highlighted-item
              (select-sidedef map-scene highlighted-item)
              (progn
                (dolist (item selected-items)
                  (set-default-pen item))
                (clear-selection map-scene))))
         (:brushes
          (if highlighted-item
              (progn
                (dolist (line (q+:child-items highlighted-item))
                  (select-line line))
                (push-selection
                 map-scene highlighted-item
                 (brush-for-graphics-item map-scene highlighted-item)))
              (progn
                (dolist (group selected-items)
                  (dolist (line (q+:child-items group))
                    (set-default-pen line)))
                (clear-selection map-scene))))
         (:sectors
          (if highlighted-item
              (progn
                (with-finalizing*
                    ((color (q+:make-qcolor (q+:qt.cyan)))
                     (brush (q+:make-qbrush color)))
                  (q+:set-brush highlighted-item brush))
                (select-line highlighted-item)
                (push-selection
                 map-scene highlighted-item
                 (with-finalizing ((scene-pos (q+:scene-pos mouse-event)))
                   (nth-value
                    1 (map-scene-hovered-brush map-scene scene-pos)))))
              (progn
                (dolist (item selected-items)
                  (q+:remove-item map-scene item)
                  (finalize item))
                (clear-selection map-scene))))
         (:things nil))
       (signal! map-scene (selection-changed)))
      (t (stop-overriding)))))

(defun items-at (map-scene scene-pos)
  (let* ((size (map->scene-unit 4))
         (offset (* 0.5 size))
         (center-x (- (q+:x scene-pos) offset))
         (center-y (- (q+:y scene-pos) offset)))
    (q+:items map-scene center-x center-y size size
              (q+:qt.intersects-item-shape) (q+:qt.descending-order))))

(defun highlight-line (item-or-items selected-items)
  (flet ((set-pen (item)
           (unless (member item selected-items)
             (with-finalizing ((pen (make-qpen (map->scene-unit 4)
                                               (q+:qt.dark-blue))))
               (setf (q+:pen item) pen)))))
    (if (listp item-or-items)
        (dolist (item item-or-items) (set-pen item))
        (set-pen item-or-items))))

(defun unhighlight-line (item-or-items selected-items)
  (flet ((set-pen (item)
           (unless (member item selected-items)
             (set-default-pen item))))
    (if (listp item-or-items)
        (dolist (item item-or-items) (set-pen item))
        (set-pen item-or-items))))

(defun lines-mode-handle-mouse-move (map-scene mouse-event)
  (with-slots (highlighted-item selected-items draw-info grid-step) map-scene
    (flet ((hovered-lineitems ()
             (with-finalizing ((scene-pos (q+:scene-pos mouse-event)))
               (loop for item in (items-at map-scene scene-pos)
                  when (qinstancep item 'qgraphicslineitem)
                  collect item))))
      (when highlighted-item
        (unhighlight-line highlighted-item selected-items)
        (setf highlighted-item nil))
      (unless draw-info
        (when-let ((hover-items (hovered-lineitems)))
          (highlight-line hover-items selected-items)
          (setf highlighted-item hover-items))))
    (when (and draw-info (within-scene-p map-scene (q+:scene-pos mouse-event)))
      (let ((scene-pos (scene-pos-from-mouse mouse-event grid-step)))
        (update-brush-drawing draw-info scene-pos)))))

(defun map-scene-hovered-brush (map-scene scene-pos)
  "Return graphics item and its `MBRUSH' under the given SCENE-POS."
  (flet ((point-in-mbrush-p (point mbrush)
           (sbsp:point-in-hull-p point
                                 (mapcar #'sbsp:sidedef-lineseg
                                         (sbrush:brush-surfaces
                                          (convert-brush mbrush))))))
    (let (hovered-brush)
      (dolist (item (items-at map-scene scene-pos))
        (let ((mbrush (brush-for-graphics-item map-scene item)))
          (when (and (qinstancep item 'qgraphicsitemgroup)
                     (point-in-mbrush-p
                      (v (q+:x scene-pos) (q+:y scene-pos))
                      mbrush))
            (if (not hovered-brush)
                (setf hovered-brush (list item mbrush))
                (let ((hovered-bounds (sbrush::brush-bounds
                                       (mbrush-brush (second hovered-brush))))
                      (bounds (sbrush::brush-bounds (mbrush-brush mbrush))))
                  (when (shiva:float<
                         (shiva:vdistsq (car bounds) (cdr bounds))
                         (shiva:vdistsq (car hovered-bounds) (cdr hovered-bounds)))
                    (setf hovered-brush (list item mbrush))))))))
      (values-list hovered-brush))))

(defun unhighlight-brush (group selected-items)
  (unless (member group selected-items)
    (dolist (line (q+:child-items group))
      (unhighlight-line line selected-items))))

(defun brushes-mode-handle-mouse-move (map-scene mouse-event)
  (with-slots (highlighted-item selected-items) map-scene
    (flet ((highlight-brush (group)
             (unless (member group selected-items)
               (dolist (line (q+:child-items group))
                 (highlight-line line selected-items))))
           (hovered-brush ()
             (with-finalizing ((scene-pos (q+:scene-pos mouse-event)))
               (map-scene-hovered-brush map-scene scene-pos))))
      (when highlighted-item
        (unhighlight-brush highlighted-item selected-items)
        (setf highlighted-item nil))
      (when-let ((hover-item (hovered-brush)))
        (highlight-brush hover-item)
        (setf highlighted-item hover-item)))))

(defun sectors-mode-handle-mouse-move (map-scene mouse-event)
  (with-slots (highlighted-item selected-items) map-scene
    (when (and highlighted-item
               (not (member highlighted-item selected-items)))
      (q+:remove-item map-scene highlighted-item)
      (finalize highlighted-item)
      (setf highlighted-item nil))
    (multiple-value-bind (hover-item mbrush)
        (with-finalizing ((scene-pos (q+:scene-pos mouse-event)))
          (map-scene-hovered-brush map-scene scene-pos))
      (when hover-item
        (let* ((brush (mbrush-brush mbrush))
               (brush-points (mapcar #'sbsp:linedef-end
                                     (sbrush:brush-lines brush)))
               (first-point (sbsp:linedef-start (car (sbrush:brush-lines brush)))))
          (with-finalizing*
              ((start-point (v2->qpoint first-point))
               (shape (q+:make-qpainterpath start-point)))
            (dolist (point brush-points)
              (q+:line-to shape (vx point) (vy point)))
            (with-finalizing*
                ((polygon (q+:to-fill-polygon shape))
                 (color (q+:make-qcolor (q+:qt.dark-cyan)))
                 (brush (q+:make-qbrush color))
                 (pen (make-qpen (map->scene-unit 4) (q+:qt.dark-blue))))
              (let ((poly-item (q+:make-qgraphicspolygonitem polygon)))
                (q+:set-pen poly-item pen)
                (q+:set-brush poly-item brush)
                (setf highlighted-item poly-item)
                (q+:add-item map-scene poly-item)))))))))

(define-override (map-scene mouse-move-event) (mouse-event)
  (case edit-mode
    (:lines (lines-mode-handle-mouse-move map-scene mouse-event))
    (:brushes (brushes-mode-handle-mouse-move map-scene mouse-event))
    (:sectors (sectors-mode-handle-mouse-move map-scene mouse-event)))
  (signal! map-scene (mouse-scene-pos double double)
           (q+:x (q+:scene-pos mouse-event))
           (q+:y (q+:scene-pos mouse-event))))

(defun remove-selected (scene)
  (with-slots (selected-items edit-mode) scene
    (case edit-mode
      (:brushes
       (dolist (group selected-items)
         (dolist (line (q+:child-items group))
           (q+:remove-from-group group line)
           (finalize line))
         (q+:remove-item scene group)
         (finalize group)
         (map-scene-remove-brush scene group))
       (clear-selection scene)))))

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

(defun write-map (stream scene)
  (with-slots (brushes things) scene
    (sbsp:write-map
     (sbsp:make-map-file :brushes (loop for brush across brushes when brush
                                     collect (convert-brush brush))
                         :things (loop for thing across things when thing collect it))
     stream)))

(defun make-thing-graphics-item (thing)
  (flet ((set-item-pos (item)
           (with-finalizing ((pos (v2->qpoint
                                   ;; Offset the image to center of half-cell.
                                   (v- (sbsp:map-thing-pos thing)
                                       (v 0.25 0.25))))
                             (size (q+:default-size (q+:renderer item))))
             (q+:set-pos item pos)
             ;; Scale the image to fit in half a cell.
             (q+:set-scale item (/ 0.5 (max (q+:width size)
                                            (q+:height size))))
             (q+:set-rotation item (sbsp:map-thing-angle thing)))
           item))
    (let ((image-file
           (concatenate 'string ":/things/"
                        (ccase (sbsp:map-thing-type thing)
                          (:player-spawn "player.svg")
                          (:shotgun "weapon.svg")
                          (:enemy "enemy.svg")))))
      (set-item-pos (q+:make-qgraphicssvgitem image-file)))))

(defun read-map (stream scene)
  (clear-map scene)
  (let ((map-file (sbsp:read-map stream)))
    (dolist (brush (sbsp:map-file-brushes map-file))
      (let ((item (make-itemgroup-from-lines (sbrush:brush-lines brush))))
        (q+:set-flag item (q+:qgraphicsitem.item-is-selectable) nil)
        (q+:add-item scene item)
        (map-scene-add-brush scene item (make-mbrush :brush brush))))
    (dolist (thing (sbsp:map-file-things map-file))
      (unless (eq :door (sbsp:map-thing-type thing))
        ;; TODO: Handle doors, i.e. brush based things
        (add-thing-to-scene scene thing)))))

(defun sidedef-for-lineitem (scene lineitem)
  (when-let ((mbrush (brush-for-graphics-item scene (q+:parent-item lineitem))))
    (with-finalizing ((qline (q+:line lineitem)))
      (let ((p1 (v (q+:x1 qline) (q+:y1 qline)))
            (p2 (v (q+:x2 qline) (q+:y2 qline)))
            (surf-pairs (mapcar #'cons
                                (sbrush:brush-surfaces (convert-brush mbrush))
                                (sbrush:brush-surfaces (mbrush-brush mbrush)))))
        (flet ((line-match-p (seg)
                 (let ((start (sbsp:lineseg-start seg))
                       (end (sbsp:lineseg-end seg)))
                   (or (and (v= p1 start) (v= p2 end))
                       (and (v= p1 end) (v= p2 start))))))
          (dolist (surf-pair surf-pairs)
            (destructuring-bind (rotated-surf . orig-surf) surf-pair
              (when (line-match-p (sbsp:sidedef-lineseg rotated-surf))
                (return orig-surf)))))))))

(defun map-scene-selected-brushes (scene)
  (with-slots (selection edit-mode) scene
    (when (member edit-mode '(:brushes :sectors))
      selection)))

(defun selected-sidedefs (scene)
  (with-slots (selection edit-mode) scene
    (when (eq edit-mode :lines)
      selection)))

(defun rotate-selected (scene)
  (with-slots (selected-items edit-mode) scene
    (case edit-mode
      (:brushes
       (dolist (item selected-items)
         (let* ((old-deg (q+:rotation item))
                (new-deg (+ old-deg 10))
                (rounds (floor new-deg 360))
                (clamped-deg (- new-deg (* rounds 360))))
           (q+:set-transform-origin-point item (q+:center (q+:bounding-rect item)))
           (q+:set-rotation item clamped-deg)
           (setf (mbrush-rotation (brush-for-graphics-item scene item))
                 new-deg)))))))

(defun change-mode (scene mode)
  (with-slots (edit-mode highlighted-item selected-items) scene
    (unless (eq edit-mode mode)
      ;; TODO: Move this to some kind of `on-mode-exit' function.
      (case edit-mode
        (:sectors
         (when highlighted-item
           (when (not (member highlighted-item selected-items))
             (q+:remove-item scene highlighted-item)
             (finalize highlighted-item))
           (setf highlighted-item nil))
         (dolist (item selected-items)
           (q+:remove-item scene item)
           (finalize item)))
        (:brushes
         (when highlighted-item
           (unhighlight-brush highlighted-item selected-items)
           (setf highlighted-item nil)))
        (:lines
         (when highlighted-item
           (unhighlight-line highlighted-item selected-items)
           (setf highlighted-item nil))))
      (cancel-editing scene)
      (q+:clear-selection scene)
      (setf edit-mode mode))))
