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

(define-widget gl-editor (QGLWidget)
  ())

(define-initializer (gl-editor setup)
  (setf (q+:minimum-size gl-editor) (values 200 200)))

(define-override (gl-editor "paintGL" paint-gl) ()
  (gl:clear-color 0 0 0 1)
  (gl:clear :color-buffer-bit))

(define-widget main (QMainWindow)
  ())

(define-subwidget (main editor) (make-instance 'gl-editor))

(define-subwidget (main button) (q+:make-qpushbutton "Click Me!"))

(define-subwidget (main layout) (q+:make-qvboxlayout)
  (q+:add-widget layout editor)
  (q+:add-widget layout button)
  (let ((widget (q+:make-qwidget main)))
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

(defun save-map (w)
  (q+:qmessagebox-information
   w "Save Map"
   "Save to existing file."))

(defun save-as-map (w)
  (q+:qmessagebox-information
   w "Save As Map"
   "Save to a new file."))

(define-menu (main File)
  (:item ("New" (ctrl n)) (new-map main))
  (:item ("Open..." (ctrl o)) (open-map main))
  (:separator)
  (:item ("Save" (ctrl s)) (save-map main))
  (:item ("Save As..." (shift ctrl s)) (save-as-map main))
  (:separator)
  (:item ("Quit" (ctrl q))
         (q+:close main)))

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
