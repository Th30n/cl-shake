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
  (shiva:v (q+:red-f qcolor) (q+:green-f qcolor) (q+:blue-f qcolor)))

(defun vector->qcolor (color)
  (let ((qcolor (q+:make-qcolor)))
    (setf (q+:rgb-f qcolor)
          (values (shiva:vx color) (shiva:vy color) (shiva:vz color)))
    qcolor))

(define-widget gl-editor (QGLWidget)
  ())

(define-initializer (gl-editor setup)
  (setf (q+:minimum-size gl-editor) (values 200 200)))

(define-override (gl-editor "paintGL" paint-gl) ()
  (sgl:clear-buffer-fv :color 0 0 0 0))

(define-widget map-scene (QGraphicsScene)
  ((drawing-line :initform nil)
   (line-color-map :initform (make-hash-table))))

(defun update-lineitem-p2 (lineitem p2)
  (with-finalizing ((new-line (q+:make-qlinef (q+:p1 (q+:line lineitem)) p2)))
    (setf (q+:line lineitem) new-line)))

(defun new-lineitem (p1 p2)
  (with-finalizing ((new-line (q+:make-qlinef p1 p2)))
    (let ((lineitem (q+:make-qgraphicslineitem new-line)))
      (setf (q+:flag lineitem) (values (q+:qgraphicsitem.item-is-selectable) t))
      lineitem)))

(defun within-scene-p (scene point)
  (q+:contains (q+:scene-rect scene) point))

(define-signal (map-scene mouse-scene-pos) (double double))

(define-override (map-scene mouse-press-event) (mouse-event)
  (when (within-scene-p map-scene (q+:scene-pos mouse-event))
    (cond
      ((enum-equal (q+:button mouse-event) (q+:qt.right-button))
       (if drawing-line
           (progn
             (update-lineitem-p2 drawing-line (q+:scene-pos mouse-event))
             (setf drawing-line nil))
           (progn
             (setf drawing-line (new-lineitem (q+:scene-pos mouse-event)
                                              (q+:scene-pos mouse-event)))
             (with-finalizing* ((color (q+:make-qcolor (q+:qt.dark-green)))
                                (pen (q+:make-qpen color)))
               (setf (q+:width-f pen) 0.5
                     (q+:pen drawing-line) pen))
             (q+:add-item map-scene drawing-line))))
      ((enum-equal (q+:button mouse-event) (q+:qt.left-button))
       (let* ((view (car (q+:views map-scene)))
              (item (q+:item-at map-scene (q+:scene-pos mouse-event)
                                (q+:transform view))))
         (q+:clear-selection map-scene)
         (unless (null-qobject-p item)
           (setf (q+:selected item) t))))
      (t (stop-overriding)))))

(define-override (map-scene mouse-move-event) (mouse-event)
  (when (and drawing-line (within-scene-p map-scene (q+:scene-pos mouse-event)))
    (update-lineitem-p2 drawing-line (q+:scene-pos mouse-event)))
  (signal! map-scene (mouse-scene-pos double double)
           (q+:x (q+:scene-pos mouse-event))
           (q+:y (q+:scene-pos mouse-event))))

(define-widget main (QMainWindow)
  ((map-file :initform nil)))

(define-subwidget (main editor) (make-instance 'gl-editor))

(define-subwidget (main scene) (make-instance 'map-scene))

(define-slot (main mouse-scene-pos) ((x double) (y double))
  (declare (connected scene (mouse-scene-pos double double)))
  (q+:show-message (q+:status-bar main) (format nil "Pos ~,2F, ~,2F" x y)))

(define-subwidget (main button) (q+:make-qpushbutton "Click Me!"))

(define-subwidget (main map-view) (q+:make-qgraphicsview)
  (with-finalizing ((cursor (q+:make-qcursor (q+:qt.cross-cursor))))
    (setf (q+:minimum-size map-view) (values 200 200)
          (q+:mouse-tracking map-view) t
          (q+:cursor map-view) cursor))
  (q+:scale map-view 4 4)
  (with-finalizing ((rect (q+:make-qrectf -200 -200 400 400))
                    (brush (q+:make-qbrush (q+:qt.black) (q+:qt.solid-pattern))))
    (setf (q+:scene-rect scene) rect
          (q+:background-brush scene) brush))
  (setf (q+:scene map-view) scene))

(define-subwidget (main layout) (q+:make-qvboxlayout)
  (q+:add-widget layout editor)
  (q+:add-widget layout button)
  (let ((widget (q+:make-qwidget)))
    (q+:add-widget layout map-view)
    (setf (q+:layout widget) layout)
    (setf (q+:central-widget main) widget)))

(define-slot (main button-pressed) ()
  (declare (connected button (pressed)))
  (q+:qmessagebox-information
   main "Hello World!"
   (format nil "Hello dear sir/madam.")))

(defun new-map (w)
  (q+:qmessagebox-information
   w "New Map"
   "Create a new map."))

(defun open-map (w)
  (q+:qmessagebox-information
   w "Open Map"
   "Open an existing map."))

(defun write-map (stream scene)
  (with-slots (line-color-map) scene
    (let* ((items (q+:items scene))
           (items-count (length items)))
      (format stream "~S~%" items-count)
      (loop for i from 0 and lineitem in items do
           (let ((p1 (q+:p1 (q+:line lineitem)))
                 (p2 (q+:p2 (q+:line lineitem)))
                 (color (gethash lineitem line-color-map (shiva:v 1 0 1))))
             (format stream "~S~%~S ~S ~S~%"
                     (list (q+:x p1) (q+:y p1) (q+:x p2) (q+:y p2))
                     (shiva:vx color) (shiva:vy color) (shiva:vz color)))))))

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

(defun edit-color (w)
  (with-slots-bound (w main)
    (when-let ((item (car (q+:selected-items scene))))
      (with-slots (line-color-map) scene
        (let ((old-color (gethash item line-color-map (shiva:v 1 0 1))))
          (with-finalizing* ((qcolor (vector->qcolor old-color))
                             (new-qcolor (q+:qcolordialog-get-color qcolor w)))
            (when (q+:is-valid new-qcolor)
              (setf (gethash item line-color-map)
                    (qcolor->vector new-qcolor)))))))))

(define-menu (main Edit)
  (:item "Color" (edit-color main)))

(define-initializer (main setup)
  (setf (q+:window-title main) "ShakeEd")
  (q+:resize main 480 320)
  (q+:show-message (q+:status-bar main) "Status"))

(defun main ()
  (let ((gl-format (q+:make-qglformat)))
    (q+:set-version gl-format 3 3)
    ;; Core Profile.
    (q+:set-profile gl-format 1)
    (q+:qglformat-set-default-format gl-format)
    (with-main-window (window (make-instance 'main)))))
