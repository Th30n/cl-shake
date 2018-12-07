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

(in-package #:cl-user)

(defpackage #:shiva
  (:use #:cl
        #:iterate
        #:alexandria
        #:metabang.bind)
  (:export #:shiva-float
           #:float= #:float> #:float>= #:float< #:float<=
           #:rad->deg #:deg->rad
           #:vec #:v #:vx #:vy #:vz #:vw
           #:v3->v2 #:v2->v3
           #:v- #:v+ #:v= #:vnorm #:vnormalize
           #:vscale #:vdot #:vcross #:vtransform #:vrotate
           #:v3dot
           #:vdist #:vdistsq
           #:quat #:q #:q+ #:q* #:qconj #:qrotation #:q->mat #:q->euler-x
           #:mat #:mat-row #:mat-col #:m*
           #:translation #:scale #:rotation
           #:ortho #:perspective)
  ;; swizzle operators
  (:export #:vxx #:vxy #:vyx #:vyy
           #:vxxx #:vxxy #:vxxz #:vxyx #:vxyy #:vxyz #:vxzx #:vxzy #:vxzz
           #:vyxx :vyxy :vyxz :vyyx :vyyy :vyyz :vyzx :vyzy :vyzz :vzxx :vzxy
           #:vzxz :vzyx :vzyy :vzyz :vzzx :vzzy :vzzz)
  (:export #:vrrr #:vrrg #:vrrb #:vrgr #:vrgg #:vrgb #:vrbr #:vrbg #:vrbb
           #:vgrr #:vgrg #:vgrb #:vggr #:vggg #:vggb #:vgbr #:vgbg #:vgbb
           #:vbrr #:vbrg :vbrb #:vbgr #:vbgg #:vbgb #:vbbr #:vbbg #:vbbb))
