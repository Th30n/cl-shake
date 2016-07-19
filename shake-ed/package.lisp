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

(defpackage #:shake-ed
  (:use #:cl+qt #:shake-utils)
  (:export #:main)
  (:import-from #:shiva
                #:deg->rad
                #:v #:vx #:vy #:vz #:v=
                #:vnormalize
                #:v+ #:v- #:vscale)
  (:import-from #:alexandria
                #:clamp
                #:emptyp
                #:ends-with-subseq
                #:hash-table-values
                #:lastcar
                #:rcurry
                #:when-let
                #:when-let*))
