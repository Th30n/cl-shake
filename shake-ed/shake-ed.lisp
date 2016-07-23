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

(in-package #:shake-ed)
(in-readtable :qtools)

(defconstant +initial-scale+ 50d0)
(defconstant +initial-grid-step+ 64)

(defun qcolor->vector (qcolor)
  (v (q+:red-f qcolor) (q+:green-f qcolor) (q+:blue-f qcolor)))

(defun vector->qcolor (color)
  (let ((qcolor (q+:make-qcolor)))
    (setf (q+:rgb-f qcolor)
          (values (vx color) (vy color) (vz color)))
    qcolor))

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
   (sbrush::brush-rotate (mbrush-brush mbrush)
                         (* deg->rad (mbrush-rotation mbrush)))))

(define-widget map-scene (QGraphicsScene)
  ((draw-info :initform nil)
   (edit-mode :initform :mode-brush-create)
   (graphics-item-brush-map :initform (make-hash-table))
   (view-normals-p :initform t)
   (grid-step :initform +initial-grid-step+)))

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

(defun v2->qpoint (vector-2d)
  (q+:make-qpointf (vx vector-2d) (vy vector-2d)))

(defun linedef->lineitem (line)
  (with-finalizing ((p1 (v2->qpoint (sbsp:linedef-start line)))
                    (p2 (v2->qpoint (sbsp:linedef-end line))))
    (new-lineitem p1 p2)))

(defun make-itemgroup-from-lines (lines)
  (let ((itemgroup (q+:make-qgraphicsitemgroup)))
    (dolist (line lines itemgroup)
      (q+:add-to-group itemgroup (linedef->lineitem line)))))

(defun within-scene-p (scene point)
  (q+:contains (q+:scene-rect scene) point))

(define-signal (map-scene mouse-scene-pos) (double double))

(defun scene-pos-from-mouse (mouse-event grid-step)
  (let* ((scene-pos (q+:scene-pos mouse-event))
         (x (q+:x scene-pos))
         (y (q+:y scene-pos))
         (snap-to-grid-p (enum-equal (q+:modifiers mouse-event)
                                     (q+:qt.control-modifier))))
    (if snap-to-grid-p
        (v (snap-scene-to-grid x grid-step) (snap-scene-to-grid y grid-step))
        (v (snap-scene-to-map-unit x) (snap-scene-to-map-unit y)))))

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
                       (setf (gethash group graphics-item-brush-map)
                             (make-mbrush :brush
                                          (sbrush:make-brush :lines linedefs))
                             draw-info nil)
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

(defun cancel-editing (map-scene)
  (with-slots (draw-info) map-scene
    (when draw-info
      (let ((group (first draw-info)))
        (q+:remove-item map-scene group)
        (finalize group)
        (setf draw-info nil)))))

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

(define-widget map-view (QGraphicsView)
  ((zoom-lvl :initform 4)))

(define-signal (map-view zoom-lvl-changed) (double))

(defun map-view-scale-zoom (map-view)
  (with-slots (zoom-lvl) map-view
    (let ((scale (if (>= 4 zoom-lvl)
                     (* +initial-scale+ (/ zoom-lvl 4))
                     (* 2d0 +initial-scale+ (- zoom-lvl 4)))))
      (with-finalizing ((transform (q+:transform map-view)))
        (let ((scale-factor (/ scale (q+:m11 transform))))
          (q+:scale map-view scale-factor scale-factor)))
      (signal! map-view (zoom-lvl-changed double) scale))))

(define-override (map-view wheel-event) (event)
  (let ((zoom-p (and (enum-equal (q+:modifiers event)
                                 (q+:qt.control-modifier))
                     (enum-equal (q+:orientation event)
                                 (q+:qt.vertical))))
        (max-zoom 20) (min-zoom 1))
    (if zoom-p
        ;; zoom
        (let ((prev-zoom-lvl zoom-lvl))
          (setf zoom-lvl (clamp (if (plusp (q+:delta event))
                                    (1+ zoom-lvl)
                                    (1- zoom-lvl))
                                min-zoom max-zoom))
          (unless (= prev-zoom-lvl zoom-lvl)
            ;; Zoom in towards or out of mouse position.
            (q+:set-transformation-anchor map-view (q+:qgraphicsview.anchor-under-mouse))
            (map-view-scale-zoom map-view)))
        (stop-overriding))))

(defstruct status-info
  (map-pos (cons 0 0))
  (zoom-lvl +initial-scale+)
  (grid-step +initial-grid-step+))

(define-widget main (QMainWindow)
  ((map-file :initform nil)
   (status-info :initform (make-status-info))))

(defun show-status-info (main)
  (with-slots (status-info) main
    (with-struct (status-info- map-pos zoom-lvl grid-step) status-info
      (let ((map-x (car map-pos))
            (map-y (cdr map-pos)))
        (q+:show-message (q+:status-bar main)
                         (format nil "Pos ~4D, ~4D Zoom: ~,2F% Grid: ~3D"
                                 map-x map-y zoom-lvl grid-step))))))

(define-subwidget (main scene) (make-instance 'map-scene))

(define-slot (main mouse-scene-pos) ((x double) (y double))
  (declare (connected scene (mouse-scene-pos double double)))
  (let ((map-x (scene->map-unit x))
        (map-y (scene->map-unit y)))
    (setf (status-info-map-pos status-info) (cons map-x map-y))
    (show-status-info main)))

(define-subwidget (main map-view) (make-instance 'map-view)
  (with-finalizing ((cursor (q+:make-qcursor (q+:qt.cross-cursor))))
    (setf (q+:minimum-size map-view) (values 200 200)
          (q+:mouse-tracking map-view) t
          (q+:cursor map-view) cursor))
  (map-view-scale-zoom map-view)
  (with-finalizing ((rect (q+:make-qrectf -200 -200 400 400))
                    (brush (q+:make-qbrush (q+:qt.black) (q+:qt.solid-pattern))))
    (setf (q+:scene-rect scene) rect
          (q+:background-brush scene) brush))
  (setf (q+:scene map-view) scene))

(define-slot (main zoom-lvl-changed) ((zoom-lvl double))
  (declare (connected map-view (zoom-lvl-changed double)))
  (setf (status-info-zoom-lvl status-info) zoom-lvl)
  (show-status-info main))

(define-slot (main grid-step-changed) ((grid-step int))
  (declare (connected scene (grid-step-changed int)))
  (setf (status-info-grid-step status-info) grid-step)
  (show-status-info main))

(define-subwidget (main layout) (q+:make-qvboxlayout)
  (let ((widget (q+:make-qwidget)))
    (q+:add-widget layout map-view)
    (setf (q+:layout widget) layout)
    (setf (q+:central-widget main) widget)))

(defun clear-map (scene)
  (with-slots (graphics-item-brush-map) scene
    (q+:clear scene)
    (setf graphics-item-brush-map (make-hash-table))))

(defun new-map (w)
  (with-slots (scene map-file) w
    (clear-map scene)
    (setf map-file nil)))

(defun write-map (stream scene)
  (with-slots (graphics-item-brush-map) scene
    (let ((mbrushes (hash-table-values graphics-item-brush-map)))
      (sbsp:write-map
       (mapcar (lambda (mbrush)
                 (sbrush::brush-rotate (mbrush-brush mbrush)
                                       (* deg->rad (mbrush-rotation mbrush))))
               mbrushes) stream))))

(defun read-map (stream scene)
  (clear-map scene)
  (with-slots (graphics-item-brush-map) scene
    (let ((brushes (sbsp:read-map stream)))
      (dolist (brush brushes)
        (let ((item (make-itemgroup-from-lines (sbrush:brush-lines brush))))
          (q+:set-flag item (q+:qgraphicsitem.item-is-selectable) t)
          (q+:add-item scene item)
          (setf (gethash item graphics-item-brush-map)
                (make-mbrush :brush brush)))))))

(defun save-map (w &optional filename)
  (let ((filepath filename))
    (unless filepath
      (setf filepath
            (q+:qfiledialog-get-save-file-name w "Save Map" "" "Maps (*.map)")))
    (unless (emptyp filepath)
      (unless (ends-with-subseq ".map" filepath)
        (setf filepath (concatenate 'string filepath ".map")))
      (with-slots (scene map-file) w
        (with-open-file (file filepath :direction :output
                              :if-exists :supersede :if-does-not-exist :create)
          (write-map file scene)
          (q+:show-message (q+:status-bar w)
                           (format nil "Saved '~S'" filepath))
          (setf map-file filepath))))))

(defun open-map (w)
  (let ((filepath (q+:qfiledialog-get-open-file-name
                   w "Open Map" "" "Maps (*.map)")))
    (when (and filepath (ends-with-subseq ".map" filepath))
      (with-slots (scene map-file) w
        (with-open-file (file filepath)
          (read-map file scene)
          (q+:show-message (q+:status-bar w)
                           (format nil "Loaded '~S'" filepath))
          (setf map-file filepath))))))

(defun compile-map (w)
  (with-slots (map-file) w
    (unless (emptyp map-file)
      (let ((bsp-file (concatenate 'string
                                   (subseq map-file 0 (- (length map-file) 4))
                                   ".bsp")))
        (sbsp:compile-map-file map-file bsp-file)
        (q+:show-message (q+:status-bar w)
                         (format nil "Compiled to '~S'" bsp-file))))))

(define-menu (main File)
  (:item ("New" (ctrl n)) (new-map main))
  (:item ("Open..." (ctrl o)) (open-map main))
  (:separator)
  (:item ("Save" (ctrl s)) (save-map main map-file))
  (:item ("Save As..." (shift ctrl s)) (save-map main))
  (:item ("Compile" (f5)) (compile-map main))
  (:separator)
  (:item ("Quit" (ctrl q))
         (q+:close main)))

(defun set-brush-color (brush color)
  (flet ((change-line-color (line)
           (sbsp:make-linedef :start (sbsp:linedef-start line)
                              :end (sbsp:linedef-end line)
                              :color color)))
    (setf (sbrush:brush-lines brush)
          (mapcar #'change-line-color (sbrush:brush-lines brush)))))

(defun selected-brushes (scene)
  (with-slots (graphics-item-brush-map) scene
    (mapcar (rcurry #'gethash graphics-item-brush-map)
            (q+:selected-items scene))))

(defun edit-color (w)
  (with-slots-bound (w main)
    (when-let* ((brushes (mapcar #'mbrush-brush (selected-brushes scene)))
                (old-color (sbsp::linedef-color
                            (car (sbrush:brush-lines (car brushes))))))
      (with-finalizing* ((qcolor (vector->qcolor old-color))
                         (new-qcolor (q+:qcolordialog-get-color qcolor w)))
        (when (q+:is-valid new-qcolor)
          (dolist (brush brushes)
            (set-brush-color brush (qcolor->vector new-qcolor))))))))

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

(define-menu (main Edit)
  (:item ("Color" (c)) (edit-color main))
  (:item ("Rotate" (r)) (rotate-selected scene))
  (:item ("Delete" (backspace)) (remove-selected scene))
  (:item ("Increase Grid" (])) (scale-grid-step scene :scale 0.5))
  (:item ("Decrease Grid" ([)) (scale-grid-step scene :scale 2)))

(define-menu (main View)
  (:item "Normals" (toggle-view-normals scene)))

(define-initializer (main setup)
  (setf (q+:window-title main) "ShakeEd")
  (q+:resize main 800 600)
  (show-status-info main))

(defun main ()
  (let ((gl-format (q+:make-qglformat)))
    (q+:set-version gl-format 3 3)
    ;; Core Profile.
    (q+:set-profile gl-format 1)
    (q+:qglformat-set-default-format gl-format)
    (with-main-window (window (make-instance 'main)))))
