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

(defun qcolor->vector (qcolor)
  (v (q+:red-f qcolor) (q+:green-f qcolor) (q+:blue-f qcolor)))

(defun vector->qcolor (color)
  (let ((qcolor (q+:make-qcolor)))
    (setf (q+:rgb-f qcolor)
          (values (vx color) (vy color) (vz color)))
    qcolor))

(define-widget map-scene (QGraphicsScene)
  ((draw-info :initform nil)
   (edit-mode :initform :mode-brush-create)
   (graphics-item-brush-map :initform (make-hash-table))
   (view-normals-p :initform t)))

(define-override (map-scene draw-background) (painter rect)
  (q+:fill-rect painter rect (q+:qt.black))
  (with-finalizing ((color (q+:make-qcolor 40 40 40)))
    (q+:set-pen painter color)
    ;; draw grid
    (loop for x from (ceiling (q+:left rect)) upto (floor (q+:right rect)) do
         (with-finalizing ((p1 (q+:make-qpointf x (q+:top rect)))
                           (p2 (q+:make-qpointf x (q+:bottom rect))))
           (q+:draw-line painter p1 p2)))
    (loop for y from (ceiling (q+:top rect)) upto (floor (q+:bottom rect)) do
         (with-finalizing ((p1 (q+:make-qpointf (q+:left rect) y))
                           (p2 (q+:make-qpointf (q+:right rect) y)))
           (q+:draw-line painter p1 p2)))))

(defun make-linedef-loop (p1 p2 p3 &rest points)
  (let ((start-points (append (list p1 p2 p3) points))
        (end-points (append (list p2 p3) points (list p1))))
    (mapcar (lambda (start end) (sbsp:make-linedef :start start :end end))
            start-points end-points)))

(defun qrect->linedefs (rect)
  (let ((right (q+:right rect))
        (left (q+:left rect))
        (top (q+:top rect))
        (bot (q+:bottom rect)))
    (make-linedef-loop (v right bot) (v left bot) (v left top) (v right top))))

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
          (when-let ((brush (gethash item graphics-item-brush-map)))
            (dolist (line (sbrush:brush-lines brush))
              (draw-linedef-normal line painter)))))))
  (stop-overriding))

(defun new-lineitem (p1 p2)
  (with-finalizing ((new-line (q+:make-qlinef p1 p2)))
    (let ((item (q+:make-qgraphicslineitem new-line)))
      (with-finalizing* ((color (q+:make-qcolor (q+:qt.dark-green)))
                         (pen (q+:make-qpen color)))
        (setf (q+:width-f pen) 0.05
              (q+:pen item) pen
              (q+:flag item) (values (q+:qgraphicsitem.item-is-selectable) t)))
      item)))

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

(defun new-rectitem (top-left bot-right)
  (with-finalizing ((rect (q+:make-qrectf top-left bot-right)))
    (let* ((lines (qrect->linedefs (q+:normalized rect)))
           (item (make-itemgroup-from-lines lines)))
      (q+:set-flag item (q+:qgraphicsitem.item-is-selectable) t)
      item)))

(defun update-rectitem (rectitem-and-start-pos bot-right)
  (destructuring-bind (rectitem . start-pos) rectitem-and-start-pos
    (with-finalizing* ((top-left (v2->qpoint start-pos))
                       (new-rect (q+:make-qrectf top-left bot-right)))
      (let ((lines (qrect->linedefs (q+:normalized new-rect))))
        (dolist (child (q+:child-items rectitem))
          (q+:remove-from-group rectitem child)
          (finalize child))
        (dolist (line lines)
          (q+:add-to-group rectitem (linedef->lineitem line)))
        lines))))

(defun within-scene-p (scene point)
  (q+:contains (q+:scene-rect scene) point))

(define-signal (map-scene mouse-scene-pos) (double double))

(defun scene-pos-from-mouse (mouse-event)
  (let* ((scene-pos (q+:scene-pos mouse-event))
         (x (q+:x scene-pos))
         (y (q+:y scene-pos))
         (snap-to-grid-p (enum-equal (q+:modifiers mouse-event)
                                     (q+:qt.control-modifier))))
    (if snap-to-grid-p
        (v (round x) (round y))
        (v x y))))

(defun add-or-update-rect (map-scene scene-pos)
  (with-slots (draw-info graphics-item-brush-map) map-scene
    (with-finalizing ((scene-point (v2->qpoint scene-pos)))
      (if draw-info
          (progn
            (let ((linedefs (update-rectitem draw-info scene-point)))
              (setf (gethash (car draw-info) graphics-item-brush-map)
                    (sbrush:make-brush :lines linedefs))
              (setf draw-info nil)
              (q+:update map-scene (q+:scene-rect map-scene))))
          (progn
            (setf draw-info
                  (cons (new-rectitem scene-point scene-point) scene-pos))
            (q+:add-item map-scene (car draw-info)))))))

(defun update-brush-drawing (draw-info scene-pos)
  (let ((line-item (second draw-info))
        (last-pos (lastcar draw-info)))
    (q+:set-line line-item
                 (vx last-pos) (vy last-pos)
                 (vx scene-pos) (vy scene-pos))))

(defun add-or-update-brush (map-scene scene-pos)
  (with-slots (draw-info graphics-item-brush-map) map-scene
    (with-finalizing ((scene-point (v2->qpoint scene-pos)))
      (if draw-info
          (progn
            (let ((first-pos (third draw-info)))
              (if (v= scene-pos first-pos)
                  ;; finish line loop
                  (let* ((group (first draw-info))
                         (points (cddr draw-info))
                         (linedefs (apply #'make-linedef-loop points)))
                    (dolist (child (q+:child-items group))
                      (q+:remove-from-group group child)
                      (finalize child))
                    (dolist (line linedefs)
                      (q+:add-to-group group (linedef->lineitem line)))
                    (setf (gethash group graphics-item-brush-map)
                          (sbrush:make-brush :lines linedefs)
                          draw-info nil)
                    (q+:update map-scene (q+:scene-rect map-scene)))
                  ;; end current line and continue with new
                  (let* ((group (first draw-info))
                         (points (cddr draw-info))
                         (new-line-item (new-lineitem scene-point scene-point)))
                    (update-brush-drawing draw-info scene-pos)
                    (q+:add-to-group group new-line-item)
                    (setf draw-info
                          (append (list group new-line-item) points
                                  (list scene-pos)))))))
          (progn
            (let ((group (q+:make-qgraphicsitemgroup))
                  (line-item (new-lineitem scene-point scene-point)))
              (setf draw-info (list group line-item scene-pos))
              (q+:set-flag group (q+:qgraphicsitem.item-is-selectable) t)
              (q+:add-to-group group line-item)
              (q+:add-item map-scene group)))))))

(define-override (map-scene mouse-press-event) (mouse-event)
  (when (within-scene-p map-scene (q+:scene-pos mouse-event))
    (cond
      ((enum-equal (q+:button mouse-event) (q+:qt.right-button))
       (add-or-update-brush map-scene (scene-pos-from-mouse mouse-event)))
       ;;(add-or-update-rect map-scene (scene-pos-from-mouse mouse-event)))
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
    (let ((scene-pos (scene-pos-from-mouse mouse-event)))
      (with-finalizing ((scene-point (v2->qpoint scene-pos)))
        (update-brush-drawing draw-info scene-pos))))
        ;; (update-rectitem draw-info scene-point))))
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
    (t (stop-overriding))))

(defun toggle-view-normals (scene)
  (with-slots (view-normals-p) scene
    (setf view-normals-p (not view-normals-p))
    (q+:update scene (q+:scene-rect scene))))

(define-widget main (QMainWindow)
  ((map-file :initform nil)))

(define-subwidget (main scene) (make-instance 'map-scene))

(define-slot (main mouse-scene-pos) ((x double) (y double))
  (declare (connected scene (mouse-scene-pos double double)))
  (q+:show-message (q+:status-bar main) (format nil "Pos ~,2F, ~,2F" x y)))

(define-subwidget (main map-view) (q+:make-qgraphicsview)
  (with-finalizing ((cursor (q+:make-qcursor (q+:qt.cross-cursor))))
    (setf (q+:minimum-size map-view) (values 200 200)
          (q+:mouse-tracking map-view) t
          (q+:cursor map-view) cursor))
  (q+:scale map-view 25 25)
  (with-finalizing ((rect (q+:make-qrectf -200 -200 400 400))
                    (brush (q+:make-qbrush (q+:qt.black) (q+:qt.solid-pattern))))
    (setf (q+:scene-rect scene) rect
          (q+:background-brush scene) brush))
  (setf (q+:scene map-view) scene))

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
    (let ((brushes (hash-table-values graphics-item-brush-map)))
      (sbsp:write-map brushes stream))))

(defun read-map (stream scene)
  (clear-map scene)
  (with-slots (graphics-item-brush-map) scene
    (let ((brushes (sbsp:read-map stream)))
      (dolist (brush brushes)
        (let ((item (make-itemgroup-from-lines (sbrush:brush-lines brush))))
          (q+:set-flag item (q+:qgraphicsitem.item-is-selectable) t)
          (q+:add-item scene item)
          (setf (gethash item graphics-item-brush-map) brush))))))

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
    (when-let* ((brushes (selected-brushes scene))
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
        (let* ((brush (gethash item graphics-item-brush-map))
               (old-deg (q+:rotation item))
               (new-deg (+ old-deg 10))
               (rounds (floor new-deg 360))
               (clamped-deg (- new-deg (* rounds 360))))
          (format t "~S From ~S To ~S Rounds ~S~%"
                  item old-deg clamped-deg rounds)
          (q+:set-transform-origin-point item (q+:center (q+:bounding-rect item)))
          (q+:set-rotation item clamped-deg)
          (setf (gethash item graphics-item-brush-map)
                (sbrush::brush-rotate brush (* deg->rad 10))))))))

(define-menu (main Edit)
  (:item ("Color" (c)) (edit-color main))
  (:item ("Rotate" (r)) (rotate-selected scene))
  (:item ("Delete" (backspace)) (remove-selected scene)))

(define-menu (main View)
  (:item "Normals" (toggle-view-normals scene)))

(define-initializer (main setup)
  (setf (q+:window-title main) "ShakeEd")
  (q+:resize main 800 600)
  (q+:show-message (q+:status-bar main) "Status"))

(defun main ()
  (let ((gl-format (q+:make-qglformat)))
    (q+:set-version gl-format 3 3)
    ;; Core Profile.
    (q+:set-profile gl-format 1)
    (q+:qglformat-set-default-format gl-format)
    (with-main-window (window (make-instance 'main)))))
