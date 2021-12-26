;;;; camera.lisp
;;;; Camera class for Raytracing in One Weekend
;;;;
;;;; Written in 2021 based on work by Peter Shirley
;;;;
;;;; To the extent possible under law, the author(s) have dedicated all
;;;; copyright and related and neighboring rights to this software to the
;;;; public domain worldwide. This software is distributed without any
;;;; warranty.
;;;;
;;;; You should have received a copy of the CC0 Public Domain Dedication
;;;; along with this software. If not, see
;;;; <http://creativecommons.org/publicdomain/zero/1.0/>.

(in-package #:rtiow)

(defclass camera ()
  ((origin :initform (make-vec3) :initarg :origin :accessor camera-origin)
   (horizontal :initarg :horizontal :accessor camera-horizontal)
   (vertical :initarg :vertical :accessor camera-vertical)
   (lower-left-corner :initarg :lower-left-corner
		      :accessor camera-lower-left-corner)
   (u :initarg :u :initform (make-vec3 1.0d0 0.0d0 0.0d0) :accessor camera-u)
   (v :initarg :v :initform (make-vec3 0.0d0 1.0d0 0.0d0) :accessor camera-v)
   (w :initarg :w :initform (make-vec3 0.0d0 0.0d0 -1.0d0) :accessor camera-w)
   (lens-radius :initarg :lens-radius :initform 0.0d0 :accessor camera-lens-radius))
  (:documentation "Class for managing a virtual camera."))

(defun make-camera (&optional
		      (look-from (make-point))
		      (look-at (make-point 0.0d0 0.0d0 -1.0d0))
		      (view-up (make-point 0.0d0 1.0d0 0.0d0))
		      (vfov 90.0d0)
		      (aspect-ratio (/ 16.0d0 9.0d0))
		      (aperture 0.0d0)
		      (focus-dist 1.0d0))
  "Return a camera with given vertical field of view in degrees."
  (let* ((theta (degrees-to-radians vfov))
	 (h (tan (/ theta 2.0d0)))
	 (viewport-height (* 2.0d0 h))
	 (viewport-width (* aspect-ratio viewport-height))
	 (w (unit-vec3 (sub-vec3 look-from look-at)))
	 (u (unit-vec3 (cross-vec3 view-up w)))
	 (v (cross-vec3 w u))
	 (origin look-from)
	 (horizontal (scale-vec3 (* focus-dist viewport-width) u))
	 (vertical (scale-vec3 (* focus-dist viewport-height) v))
	 (lower-left-corner (sub-vec3
			     (sub-vec3
			      (sub-vec3 origin
					(scale-vec3 0.5d0 horizontal))
			      (scale-vec3 0.5d0 vertical))
			     (scale-vec3 focus-dist w)))
	 (lens-radius (/ aperture 2)))
    (make-instance 'camera :origin origin
			   :horizontal horizontal
			   :vertical vertical
			   :lower-left-corner lower-left-corner
			   :u u :v v :w w
			   :lens-radius lens-radius)))

(defgeneric camera-get-ray (camera u v)
  (:documentation "Return ray from origin in direction of pixel at u,v"))

(defmethod camera-get-ray ((camera camera) u v)
  (let* ((rd (scale-vec3 (camera-lens-radius camera) (random-vec3-in-unit-disk)))
	 (offset (add-vec3 (scale-vec3 (vec3-x rd) (camera-u camera))
			   (scale-vec3 (vec3-y rd) (camera-v camera)))))
    (make-ray (add-vec3 (camera-origin camera) offset)
	      (sub-vec3
	       (sub-vec3
		(add-vec3
		 (add-vec3 (camera-lower-left-corner camera)
			   (scale-vec3 u (camera-horizontal camera)))
		 (scale-vec3 v (camera-vertical camera)))
		(camera-origin camera))
	       offset))))
