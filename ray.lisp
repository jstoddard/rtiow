;;;; ray.lisp
;;;; ray class for Raytracing in One Weekend
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

(defclass ray ()
  ((origin :initarg :origin :initform (make-point) :accessor ray-origin)
   (direction :initarg :direction :initform (make-vec3 0.0d0 0.0d0 1.0d0) :accessor ray-direction))
  (:documentation "A ray."))

(defun make-ray (&optional (origin (make-point)) (direction (make-vec3 0.0d0 0.0d0 1.0d0)))
  "Create a ray with given origin and direction."
  (make-instance 'ray :origin origin :direction direction))

(defgeneric ray-at (r s)
  (:documentation "Returns point along ray r at s."))

(defmethod ray-at ((r ray) s)
  (add-vec3 (ray-origin r)
	    (scale-vec3 s (ray-direction r))))
