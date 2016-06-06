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

(in-package #:shake)

(defun hull-point-contents (hull point)
  "Traverse the HULL to the leaf where POINT is located and return
  LEAF-CONTENTS. Splitting line is offset by given RADIUS."
  (if (sbsp:leaf-p hull)
      (sbsp:leaf-contents hull)
      (let* ((lineseg (sbsp:node-line hull))
             (d (vdot (sbsp:lineseg-normal lineseg)
                      (v- point (sbsp:lineseg-start lineseg)))))
        (if (minusp d)
            (hull-point-contents (sbsp:node-back hull) point)
            (hull-point-contents (sbsp:node-front hull) point)))))
