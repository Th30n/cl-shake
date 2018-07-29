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

(in-package #:shake-ed.props-ed)
(in-readtable :qtools)

(defgeneric target (editor) (:documentation "Read the editor target"))
(defgeneric (setf target) (target editor)
  (:documentation "Set the editor target and update widget contents"))

(defun draw-mode->combo-index (draw-mode)
  (ecase draw-mode
    (:tile 0)
    (:scale-to-fit 1)))

(defun combo-index->draw-mode (index)
  (ecase index
    (0 :tile)
    (1 :scale-to-fit)))

(define-widget texture-editor (QWidget)
  ((texinfo :initform nil)
   (choose-form :initform nil)
   (name-lbl :initform (edk.forms:label "No texture"))
   (x-offset-lbl :initform (edk.forms:label "X offset"))
   (x-offset-spinner
    :initform (edk.forms:double-spinner nil :min -1 :max 1 :step 0.05))
   (y-offset-lbl :initform (edk.forms:label "Y offset"))))

(defun choose-btn-clicked (texture-editor)
  (with-all-slots-bound (texture-editor texture-editor)
    (let ((filepath (q+:qfiledialog-get-open-file-name
                     texture-editor "Select Texture" "" "Textures (*.bmp)")))
      (when (and filepath (ends-with-subseq ".bmp" filepath))
        (let ((name (file-namestring filepath)))
          (unless texinfo
            (let ((offset (shiva:v 0d0 0d0))
                  (draw-mode (combo-index->draw-mode
                              (q+:current-index draw-mode-combo))))
              (setf texinfo (sbsp:make-texinfo :name name :offset offset
                                               :draw-mode draw-mode))
              (setf (edk.forms::data x-offset-spinner)
                    (make-instance 'edk.data:boxed-double
                                   :value (shiva:vx offset)))))
          (setf (sbsp:texinfo-name texinfo) name
                (edk.forms:text name-lbl) name))
        (signal! texture-editor (target-changed))))))

(define-subwidget (texture-editor draw-mode-combo) (q+:make-qcombobox)
  (q+:add-items draw-mode-combo (list "Tile" "Scale To Fit")))

(define-subwidget (texture-editor x-offset-widget) (q+:make-qwidget)
  (let ((hbox (q+:make-qhboxlayout)))
    (q+:set-layout x-offset-widget hbox)
    (q+:add-widget hbox (edk.forms:build-widget x-offset-lbl))
    (q+:add-widget hbox (edk.forms:build-widget x-offset-spinner))))

(define-subwidget (texture-editor y-offset-spinbox) (q+:make-qdoublespinbox)
  (setf (q+:range y-offset-spinbox) (values -1d0 1d0)
        (q+:single-step y-offset-spinbox) 0.05d0))
(define-subwidget (texture-editor y-offset-widget) (q+:make-qwidget)
  (let ((hbox (q+:make-qhboxlayout)))
    (q+:set-layout y-offset-widget hbox)
    (q+:add-widget hbox (edk.forms:build-widget y-offset-lbl))
    (q+:add-widget hbox y-offset-spinbox)))

(define-signal (texture-editor target-changed) ())

(define-slot (texture-editor draw-mode-changed) ((index int))
  (declare (connected draw-mode-combo (current-index-changed int)))
  (let ((new-draw-mode (combo-index->draw-mode index)))
    (unless (or (not texinfo)
                (eq (sbsp:texinfo-draw-mode texinfo) new-draw-mode))
      (setf (sbsp:texinfo-draw-mode texinfo) new-draw-mode)
      (signal! texture-editor (target-changed)))))

(define-slot (texture-editor y-offset-changed) ((val double))
  (declare (connected y-offset-spinbox (value-changed double)))
  (when texinfo
    (setf (shiva:vy (sbsp:texinfo-offset texinfo)) val)
    (signal! texture-editor (target-changed))))

(define-initializer (texture-editor setup)
  (let ((layout (q+:make-qvboxlayout))
        (choose-btn (edk.forms:button
                     "Texture" (lambda () (choose-btn-clicked texture-editor)))))
    (q+:set-layout texture-editor layout)
    (setf choose-form (edk.forms:left-right choose-btn name-lbl))
    (q+:add-widget layout (edk.forms:build-widget choose-form))
    (q+:add-widget layout draw-mode-combo)
    (q+:add-widget layout x-offset-widget)
    (q+:add-widget layout y-offset-widget)))

(defmethod target ((editor texture-editor))
  (with-slots (texinfo x-offset-spinner) editor
    (when texinfo
      (setf (shiva:vx (sbsp:texinfo-offset texinfo))
            (edk.data:value (edk.forms::data x-offset-spinner))))
    texinfo))

(defmethod (setf target) (target (editor texture-editor))
  ;; XXX: Hack for propagating edkit value, fix this by converting the data
  ;; model to edkit.data.
  (target editor)
  (with-slots (texinfo name-lbl draw-mode-combo x-offset-spinner
                       y-offset-spinbox) editor
    (setf texinfo target
          (edk.forms:text name-lbl) (if texinfo
                                        (sbsp:texinfo-name texinfo)
                                        "No texture")
          (q+:current-index draw-mode-combo) (if texinfo
                                                 (draw-mode->combo-index
                                                  (sbsp:texinfo-draw-mode texinfo))
                                                 0))
    (setf (edk.forms::data x-offset-spinner)
          (if texinfo
              (make-instance 'edk.data:boxed-double
                             :value (shiva:vx (sbsp:texinfo-offset texinfo)))))
    (let ((offset (if texinfo
                      (sbsp:texinfo-offset texinfo)
                      (shiva:v 0 0))))
      (setf (q+:value y-offset-spinbox) (shiva:vy offset))))
  target)

(define-widget properties-editor (QWidget)
  ((sidedefs :initform nil :accessor sidedefs)
   (color-btn :initform nil)))

(defun color-btn-clicked (properties-editor)
  (when-let ((sidedef (first (sidedefs properties-editor))))
    (with-finalizing* ((qcolor (vector->qcolor (sbsp:sidedef-color sidedef)))
                       (new-qcolor (q+:qcolordialog-get-color
                                    qcolor properties-editor)))
      (when (q+:is-valid new-qcolor)
        (dolist (side (sidedefs properties-editor))
          (setf (sbsp:sidedef-color side) (qcolor->vector new-qcolor)))))))

(define-subwidget (properties-editor tex-ed) (make-instance 'texture-editor))

(define-slot (properties-editor texinfo-changed) ()
  (declare (connected tex-ed (target-changed)))
  (let ((texinfo (target tex-ed)))
    (unless (sbsp:sidedef-texinfo (first sidedefs))
      (setf (sbsp:sidedef-texinfo (first sidedefs)) texinfo))
    (dolist (side (cdr sidedefs))
      (let ((tex-copy (sbsp:copy-texinfo texinfo)))
        (zap #'copy-seq (sbsp:texinfo-offset tex-copy))
        (setf (sbsp:sidedef-texinfo side) tex-copy)))))

(define-initializer (properties-editor setup)
  (let ((layout (q+:make-qvboxlayout)))
    (q+:set-layout properties-editor layout)
    (setf color-btn
          (edk.forms:button
           "Color" (lambda () (color-btn-clicked properties-editor))))
    (q+:add-widget layout (edk.forms:build-widget color-btn))
    (q+:add-widget layout tex-ed 0 (q+:qt.align-top)))
  (unless sidedefs
    (q+:set-enabled properties-editor nil)))

(defmethod (setf target) (target (editor properties-editor))
  (with-slots (sidedefs tex-ed) editor
    (setf sidedefs target)
    (when sidedefs
      (setf (target tex-ed) (sbsp:sidedef-texinfo (first sidedefs))))
    (q+:set-enabled editor (when sidedefs t)))
  target)
