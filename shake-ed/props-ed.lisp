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

(defun unobserve-slots (data accessors &key tag)
  (when data
    (dolist (accessor accessors)
      (edk.data:unobserve (funcall accessor data) :tag tag))))

(defun observe-slots (data accessors observer &key tag)
  (when data
    (dolist (accessor accessors)
      (edk.data:observe (funcall accessor data) observer :tag tag))))

(defgeneric target (editor) (:documentation "Read the editor target"))
(defgeneric (setf target) (target editor)
  (:documentation "Set the editor target and update widget contents"))

;; `texture-editor'

(defclass texture-editor (edk.forms:compound-editor)
  ((texinfo :initform nil)))

(defmethod initialize-instance :after ((ed texture-editor) &key)
  (flet ((choose-texture ()
           (let ((filepath (q+:qfiledialog-get-open-file-name
                            (edk.forms::widget ed)
                            "Select Texture" "" "Textures (*.bmp)")))
             (when (and filepath (ends-with-subseq ".bmp" filepath))
               (let ((name (file-namestring filepath))
                     (texture (edk.forms:editor-data ed)))
                 (edk.data:with-change-operation ("select texture")
                   (setf (edk.data:value (texture-name texture)) name)))))))
    (setf (edk.forms:layout-info ed)
          `(edk.forms:top-down
            (edk.forms:left-right
             (:form choose-btn edk.forms:button "Texture" ,#'choose-texture)
             (:edit name edk.forms:text-entry))
            (:edit draw-mode edk.forms:selector nil :tile :scale-to-fit)
            (edk.forms:left-right
             (:form x-lbl edk.forms:label "X offset")
             (:edit offset-x edk.forms:double-spinner :min -1 :max 1 :step 0.05))
            (edk.forms:left-right
             (:form y-lbl edk.forms:label "Y offset")
             (:edit offset-y edk.forms:double-spinner :min -1 :max 1 :step 0.05))))))

(defmethod target ((editor texture-editor))
  (when-let ((texture (edk.forms:editor-data editor)))
    (with-slots (texinfo) editor
      (when (and (> (length (edk.data:value (texture-name texture))) 0)
                 (not texinfo))
        (setf texinfo (sbsp:make-texinfo :name "")))
      (when texinfo
        (setf (sbsp:texinfo-name texinfo)
              (edk.data:value (texture-name texture)))
        (setf (shiva:vx (sbsp:texinfo-offset texinfo))
              (edk.data:value (texture-offset-x texture)))
        (setf (shiva:vy (sbsp:texinfo-offset texinfo))
              (edk.data:value (texture-offset-y texture)))
        (setf (sbsp:texinfo-draw-mode texinfo)
              (edk.data:value (texture-draw-mode texture))))
      texinfo)))

(defmethod (setf target) (target (editor texture-editor))
  ;; XXX: Hack for propagating edkit value, fix this by converting the data
  ;; model to edkit.data.
  (target editor)
  (with-slots (texinfo) editor
    (setf texinfo target)
    (setf
     (edk.forms:editor-data editor)
     (if (not texinfo)
         (make-instance 'texture)
         (make-instance
          'texture
          :name (make-instance 'edk.data:boxed-string
                               :value (sbsp:texinfo-name texinfo))
          :offset-x (make-instance 'edk.data:boxed-double
                                   :value (shiva:vx (sbsp:texinfo-offset texinfo)))
          :offset-y (make-instance 'edk.data:boxed-double
                                   :value (shiva:vy (sbsp:texinfo-offset texinfo)))
          :draw-mode (make-instance 'edk.data::boxed-symbol
                                    :value (sbsp:texinfo-draw-mode texinfo))))))
  target)

;;; End `texture-editor'

;;; `sector-editor'

(defclass sector-editor (edk.forms:compound-editor)
  ((brushes :initform nil :reader sector-editor-brushes)))

(defmethod initialize-instance :after ((ed sector-editor) &key)
  (setf (edk.forms:layout-info ed)
        '(edk.forms:top-down
          (:edit contents edk.forms:selector nil :contents-empty :contents-solid)
          (edk.forms:left-right
           (:form floor-height-lbl edk.forms:label "Floor height")
           (:edit floor-height edk.forms:double-spinner
            :min -5 :max 5 :step 0.125))
          (edk.forms:left-right
           (:form ceiling-height-lbl edk.forms:label "Ceiling height")
           (:edit ceiling-height edk.forms:double-spinner
            :min -5 :max 5 :step 0.125)))))

(defmethod (setf sector-editor-brushes) (new-brushes (ed sector-editor))
  (let ((sector-accessors (list #'sector-floor-height #'sector-ceiling-height
                                #'sector-contents)))
    (unobserve-slots (edk.forms:editor-data ed) sector-accessors :tag ed)
    (with-slots (brushes) ed
      (setf brushes new-brushes)
      (if (not brushes)
          (setf (edk.forms:editor-data ed) nil)
          (let ((contents (and brushes
                               (sbrush:brush-contents (mbrush-brush (car brushes)))))
                (back-sector
                 (and brushes
                      (sbsp:sidedef-back-sector
                       (car (sbrush:brush-surfaces (mbrush-brush (car brushes))))))))
            (setf (edk.forms:editor-data ed)
                  (make-instance
                   'sector
                   :contents (make-instance 'edk.data:boxed-symbol
                                            :value (or contents :contents-empty))
                   :floor-height
                   (make-instance 'edk.data:boxed-double
                                  :value (if back-sector
                                             (sbsp:sector-floor-height back-sector)
                                             0d0))
                   :ceiling-height
                   (make-instance 'edk.data:boxed-double
                                  :value (if back-sector
                                             (sbsp:sector-ceiling-height back-sector)
                                             1d0)))))))
    (flet ((sector-changed ()
             (with-slots (brushes) ed
               (when brushes
                 (dolist (mbrush brushes)
                   (setf (sbrush:brush-contents (mbrush-brush mbrush))
                         (edk.data:value (sector-contents (edk.forms:editor-data ed)))))
                 (let ((sidedefs
                        (loop for mbrush in brushes appending
                             (sbrush:brush-surfaces (mbrush-brush mbrush)))))
                   (dolist (sidedef sidedefs)
                     (let* ((back-sector (sbsp:sidedef-back-sector sidedef))
                            (sector (edk.forms:editor-data ed))
                            (floor-height (edk.data:value (sector-floor-height sector)))
                            (ceiling-height (edk.data:value (sector-ceiling-height sector)))
                            (contents (edk.data:value (sector-contents sector))))
                       (if back-sector
                           (setf (sbsp:sector-floor-height back-sector) floor-height
                                 (sbsp:sector-ceiling-height back-sector) ceiling-height
                                 (sbsp:sector-contents back-sector) contents)
                           (setf (sbsp:sidedef-back-sector sidedef)
                                 (sbsp:make-sector :floor-height floor-height
                                                   :ceiling-height ceiling-height
                                                   :contents contents))))))))))
      (observe-slots (edk.forms:editor-data ed) sector-accessors
                     #'sector-changed :tag ed)))
  new-brushes)

;;; End `sector-editor'

;;; `properties-editor'

(define-widget properties-editor (QWidget)
  ((sidedefs :initform nil :accessor sidedefs)
   (color-btn :initform nil)
   (tex-ed :initform (make-instance 'texture-editor))))

(defun color-btn-clicked (properties-editor)
  (when-let ((sidedef (first (sidedefs properties-editor))))
    (with-finalizing* ((qcolor (vector->qcolor (sbsp:sidedef-color sidedef)))
                       (new-qcolor (q+:qcolordialog-get-color
                                    qcolor properties-editor)))
      (when (q+:is-valid new-qcolor)
        (dolist (side (sidedefs properties-editor))
          (setf (sbsp:sidedef-color side) (qcolor->vector new-qcolor)))))))

(define-initializer (properties-editor setup)
  (let ((layout (q+:make-qvboxlayout)))
    (q+:set-layout properties-editor layout)
    (setf color-btn
          (edk.forms:button
           "Color" (lambda () (color-btn-clicked properties-editor))))
    (q+:add-widget layout (edk.forms:build-widget color-btn))
    (q+:add-widget layout (edk.forms:build-widget tex-ed) 0 (q+:qt.align-top)))
  (unless sidedefs
    (q+:set-enabled properties-editor nil)))

(defmethod (setf target) (target (editor properties-editor))
  (with-slots (sidedefs tex-ed) editor
    (let ((texture-accessors (list #'texture-name #'texture-draw-mode
                                   #'texture-offset-x #'texture-offset-y)))
      (unobserve-slots (edk.forms:editor-data tex-ed) texture-accessors
                       :tag editor)
      (setf sidedefs target)
      (when sidedefs
        (setf (target tex-ed) (sbsp:sidedef-texinfo (first sidedefs)))
        (flet ((texture-changed ()
                 (when-let ((texinfo (target tex-ed)))
                   (unless (sbsp:sidedef-texinfo (first sidedefs))
                     (setf (sbsp:sidedef-texinfo (first sidedefs)) texinfo))
                   (dolist (side (cdr sidedefs))
                     (let ((tex-copy (sbsp:copy-texinfo texinfo)))
                       (zap #'copy-seq (sbsp:texinfo-offset tex-copy))
                       (setf (sbsp:sidedef-texinfo side) tex-copy))))))
          (observe-slots (edk.forms:editor-data tex-ed) texture-accessors
                         #'texture-changed :tag editor))))
    (q+:set-enabled editor (when sidedefs t)))
  target)

;;; End `properties-editor'
