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

(defvar *base-dir*
  #.(uiop:pathname-directory-pathname (or *compile-file-truename* *load-truename*)))

(defconstant +initial-scale+ 50d0)

(define-widget map-view (QGraphicsView)
  ((zoom-lvl :initform 4)
   (translate-mouse-last-pos :initform nil)))

(define-signal (map-view zoom-lvl-changed) (double))

(define-override (map-view mouse-move-event) (mouse-event)
  (cond
    ((enum-equal (q+:buttons mouse-event) (q+:qt.mid-button))
     (when translate-mouse-last-pos
       (let* ((x (q+:global-x mouse-event))
              (y (q+:global-y mouse-event))
              (dx (- x (car translate-mouse-last-pos)))
              (dy (- y (cdr translate-mouse-last-pos))))
         (setf translate-mouse-last-pos (cons x y))
         (with-finalizing ((transform (q+:transform map-view)))
           (let ((scale (q+:m11 transform)))
             (q+:set-transformation-anchor map-view (q+:qgraphicsview.no-anchor))
             (q+:translate map-view (/ dx scale) (/ dy scale)))))))
    (t (stop-overriding))))

(define-override (map-view mouse-press-event) (mouse-event)
  (cond
    ((enum-equal (q+:button mouse-event) (q+:qt.mid-button))
     (setf translate-mouse-last-pos (cons (q+:global-x mouse-event)
                                          (q+:global-y mouse-event))))
    (t (stop-overriding))))

(define-override (map-view mouse-release-event) (mouse-event)
  (cond
    ((enum-equal (q+:button mouse-event) (q+:qt.mid-button))
     (setf translate-mouse-last-pos nil))
    (t (stop-overriding))))

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

(defun center-view-to-map (map-view)
  (with-finalizing* ((rect (q+:items-bounding-rect (q+:scene map-view)))
                     (center (q+:center rect)))
    (q+:center-on map-view center)))

(defstruct status-info
  (map-pos (cons 0 0))
  (zoom-lvl +initial-scale+)
  (grid-step +initial-grid-step+))

(define-widget main (QMainWindow)
  ((map-file :initform nil)
   (status-info :initform (make-status-info))
   (mode-action-group :initform nil)))

(defun show-status-info (main)
  (with-slots (status-info) main
    (with-struct (status-info- map-pos zoom-lvl grid-step) status-info
      (let ((map-x (car map-pos))
            (map-y (cdr map-pos)))
        (q+:show-message (q+:status-bar main)
                         (format nil "Pos ~4D, ~4D Zoom: ~,2F% Grid: ~3D"
                                 map-x map-y zoom-lvl grid-step))))))

(define-subwidget (main props-dock) (q+:make-qdockwidget "Properties" main))
(define-subwidget (main props-ed) (make-instance 'properties-editor))

(define-subwidget (main scene) (make-instance 'map-scene))

(define-slot (main scene-selection-changed) ()
  (declare (connected scene (selection-changed)))
  (setf (target props-ed) (selected-sidedefs scene)))

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
      (with-slots (scene map-view map-file) w
        (with-open-file (file filepath)
          (read-map file scene)
          (q+:show-message (q+:status-bar w)
                           (format nil "Loaded '~S'" filepath))
          (setf map-file filepath)
          (center-view-to-map map-view))))))

(defun compile-map (w)
  (with-slots (map-file) w
    (unless (emptyp map-file)
      (let ((bsp-file (concatenate 'string
                                   (subseq map-file 0 (- (length map-file) 4))
                                   ".bsp")))
        (handler-case
            (progn
              (sbsp:compile-map-file map-file bsp-file)
              (q+:show-message (q+:status-bar w)
                               (format nil "Compiled to '~S'" bsp-file)))
          (sbsp:invalid-map-file-error (err)
            (q+:qmessagebox-critical w "Compile Error" (sbsp:message err))))))))

(defun new-map (w)
  (with-slots (scene map-file) w
    (clear-map scene)
    (setf map-file nil)))

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
    (when-let* ((sides (selected-sidedefs scene))
                (old-color (sbsp:sidedef-color (first sides))))
      (with-finalizing* ((qcolor (vector->qcolor old-color))
                         (new-qcolor (q+:qcolordialog-get-color qcolor w)))
        (when (q+:is-valid new-qcolor)
          (dolist (side sides)
            (setf (sbsp:sidedef-color side) (qcolor->vector new-qcolor))))))))

(define-menu (main Edit)
  (:item ("Color" (c)) (edit-color main))
  (:item ("Rotate" (r)) (rotate-selected scene))
  (:item ("Delete" (backspace)) (remove-selected scene))
  (:item ("Increase Grid" (])) (scale-grid-step scene :scale 0.5))
  (:item ("Decrease Grid" ([)) (scale-grid-step scene :scale 2)))

(define-menu (main View)
  (:item "Normals" (toggle-view-normals scene))
  (:item "Center on Map" (center-view-to-map map-view)))

(defun add-toggle-action (toolbar text &key icon (checked nil))
  (let ((action (q+:add-action toolbar text)))
    (q+:set-checkable action t)
    (q+:set-checked action checked)
    (when icon
      (with-finalizing ((qicon (q+:make-qicon icon)))
        (q+:set-icon action qicon)))
    action))

(define-slot (main mode-changed) ((checked bool))
  (let ((text (q+:text (q+:checked-action mode-action-group))))
    (change-mode scene (cond
                         ((string= text "Lines") :lines)
                         ((string= text "Brushes") :brushes)
                         ((string= text "Things") :things)
                         (t :invalid)))))

(define-initializer (main setup)
  (setf (q+:window-title main) "ShakeEd"
        mode-action-group (q+:make-qactiongroup main))
  (q+:resize main 800 600)
  (show-status-info main)
  (let ((toolbar (q+:add-tool-bar main "Modes")))
    (flet ((add-action (text &key icon (checked nil))
             (let ((action (add-toggle-action toolbar text :icon icon
                                              :checked checked)))
               (q+:add-action mode-action-group action)
               (connect! action (triggered bool) main (mode-changed bool)))))
      (add-action "Lines" :checked t :icon ":/modes/lines.svg")
      (add-action "Brushes" :icon ":/modes/brushes.svg")
      (add-action "Sectors" :icon ":/modes/sectors.svg")
      (add-action "Things" :icon ":/modes/things.svg")))
  (q+:set-widget props-dock props-ed)
  (q+:add-dock-widget main (q+:qt.right-dock-widget-area) props-dock))

(define-finalizer (main destroy)
  (dolist (action (q+:actions mode-action-group))
    (finalize action))
  (finalize mode-action-group)
  (setf mode-action-group nil))

(defun main ()
  (let ((gl-format (q+:make-qglformat)))
    (q+:set-version gl-format 3 3)
    ;; Core Profile.
    (q+:set-profile gl-format 1)
    (q+:qglformat-set-default-format gl-format)
    ;; Register image resources
    (let ((resource-pathname (merge-pathnames #p"resource.rcc" *base-dir*)))
      (unless (q+:qresource-register-resource (uiop:native-namestring resource-pathname))
        (format t "Failed to load resources from '~A'!" resource-pathname)))
    (edk.data:with-change-tracker (change-tracker)
      (with-main-window (window (make-instance 'main))))))
