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

(define-widget properties-editor (QWidget)
  ((sidedef :initform nil)))

(define-subwidget (properties-editor color-button)
    (q+:make-qpushbutton "Color"))

(define-slot (properties-editor color-button-clicked) ((checked bool))
  (declare (connected color-button (clicked bool)))
  (when sidedef
    (with-finalizing* ((qcolor (vector->qcolor (sbsp:sidedef-color sidedef)))
                       (new-qcolor (q+:qcolordialog-get-color
                                    qcolor properties-editor)))
      (when (q+:is-valid new-qcolor)
        (setf (sbsp:sidedef-color sidedef) (qcolor->vector new-qcolor))))))

(define-initializer (properties-editor setup)
  (let ((layout (q+:make-qvboxlayout)))
    (q+:set-layout properties-editor layout)
    (q+:add-widget layout color-button))
  (unless sidedef
    (q+:set-enabled properties-editor nil)))

(defun set-target (properties-editor target)
  (with-slots (sidedef) properties-editor
    (setf sidedef (when (and (listp target) (length= 1 target)
                             (sbsp:sidedef-p (first target)))
                    (first target)))
    (q+:set-enabled properties-editor (when sidedef t))))
