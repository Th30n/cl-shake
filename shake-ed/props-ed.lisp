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
    (:scale-to-fit 0)
    (:tile 1)))

(defun combo-index->draw-mode (index)
  (ecase index
    (0 :scale-to-fit)
    (1 :tile)))

(define-widget texture-editor (QWidget)
  ((texinfo :initform nil)))

(define-subwidget (texture-editor choose-button)
    (q+:make-qpushbutton "Texture"))

(define-subwidget (texture-editor name-label) (q+:make-qlabel "No texture"))

(define-subwidget (texture-editor choose-widget) (q+:make-qwidget)
  (let ((hbox (q+:make-qhboxlayout)))
    (q+:set-layout choose-widget hbox)
    (q+:add-widget hbox choose-button)
    (q+:add-widget hbox name-label)))

(define-subwidget (texture-editor draw-mode-combo) (q+:make-qcombobox)
  (q+:add-items draw-mode-combo (list "Scale To Fit" "Tile")))

(define-signal (texture-editor target-changed) ())

(define-slot (texture-editor choose-button-clicked) ((checked bool))
  (declare (connected choose-button (clicked bool)))
  (let ((filepath (q+:qfiledialog-get-open-file-name
                   texture-editor "Select Texture" "" "Textures (*.bmp)")))
    (when (and filepath (ends-with-subseq ".bmp" filepath))
      (let ((name (file-namestring filepath)))
        (unless texinfo
          (setf texinfo (sbsp:make-texinfo :name name)))
        (setf (sbsp:texinfo-name texinfo) name
              (q+:text name-label) name))
      (signal! texture-editor (target-changed)))))

(define-slot (texture-editor draw-mode-changed) ((index int))
  (declare (connected draw-mode-combo (current-index-changed int)))
  (let ((new-draw-mode (combo-index->draw-mode index)))
    (unless (eq (sbsp:texinfo-draw-mode texinfo) new-draw-mode)
      (setf (sbsp:texinfo-draw-mode texinfo) new-draw-mode))))

(define-initializer (texture-editor setup)
  (let ((layout (q+:make-qvboxlayout)))
    (q+:set-layout texture-editor layout)
    (q+:add-widget layout choose-widget)
    (q+:add-widget layout draw-mode-combo)))

(defmethod target ((editor texture-editor))
  (with-slots (texinfo) editor
    texinfo))

(defmethod (setf target) (target (editor texture-editor))
  (with-slots (texinfo name-label draw-mode-combo) editor
    (setf texinfo target
          (q+:text name-label) (if texinfo
                                   (sbsp:texinfo-name texinfo)
                                   "No texture")
          (q+:current-index draw-mode-combo) (if texinfo
                                                 (draw-mode->combo-index
                                                  (sbsp:texinfo-draw-mode texinfo))
                                                 0)))
  target)

(define-widget properties-editor (QWidget)
  ((sidedefs :initform nil)))

(define-subwidget (properties-editor color-button)
    (q+:make-qpushbutton "Color"))
(define-subwidget (properties-editor tex-ed) (make-instance 'texture-editor))

(define-slot (properties-editor color-button-clicked) ((checked bool))
  (declare (connected color-button (clicked bool)))
  (when-let ((sidedef (first sidedefs)))
    (with-finalizing* ((qcolor (vector->qcolor (sbsp:sidedef-color sidedef)))
                       (new-qcolor (q+:qcolordialog-get-color
                                    qcolor properties-editor)))
      (when (q+:is-valid new-qcolor)
        (dolist (side sidedefs)
          (setf (sbsp:sidedef-color side) (qcolor->vector new-qcolor)))))))

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
    (q+:add-widget layout color-button)
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
