;;;; material.lisp
;;;; Material class for Raytracing in One Weekend
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

;;; hit-record struct
;;; Not currently enforced, but point of type point, normal of type
;;; vec3, material material, s number (ideally double), front-face
;;; boolean (nil if ray is inside related hittable object).
(defstruct hit-record
  point
  normal
  material
  s
  front-face)

(defun copy-rec (src dst)
  "Copy the contents of slots of hit-record src to hit-record dst."
  (setf (hit-record-point dst) (hit-record-point src)
	(hit-record-normal dst) (hit-record-normal src)
	(hit-record-material dst) (hit-record-material src)
	(hit-record-s dst) (hit-record-s src)
	(hit-record-front-face dst) (hit-record-front-face src)))

(declaim (inline set-face-normal))

(defun set-face-normal (rec r outward-normal)
  (setf (hit-record-front-face rec) (< (dot-vec3 (ray-direction r) outward-normal) 0.0d0)
	(hit-record-normal rec) (if (hit-record-front-face rec)
				    outward-normal
				    (sub-vec3 (make-vec3) outward-normal))))

(defclass material ()
  ()
  (:documentation "Base class for materials."))

(defgeneric scatter (material ray-in record attenuation scattered-ray)
  (:documentation "Scatter or absorb ray hitting material."))

(defclass lambertian (material)
  ((albedo :initarg :albedo :initform (make-color) :accessor material-albedo))
  (:documentation "A diffuse material."))

(defun make-lambertian (&optional (albedo (make-color)))
  "Make a lambertian with given albedo."
  (make-instance 'lambertian :albedo albedo))

(defmethod scatter ((material lambertian) ray-in rec attenuation scattered-ray)
  (let ((scatter-direction (add-vec3 (hit-record-normal rec) (random-unit-vec3))))
    (when (vec3-near-zero? scatter-direction) ; avoid zero vector for scatter
      (setf scatter-direction (hit-record-normal rec)))
    (setf (ray-origin scattered-ray) (hit-record-point rec)
	  (ray-direction scattered-ray) scatter-direction)
    (copy-color (material-albedo material) attenuation))
  t) ; always return t

(defclass metal (material)
  ((albedo :initarg :albedo :initform (make-color) :accessor material-albedo)
  ;; fuzz - small inconsistencies in direction of reflected rays
   (fuzz :initarg :fuzz :initform 0.0d0 :accessor material-fuzz))
  (:documentation "A reflective material."))

(defun make-metal (&optional (albedo (make-color)) (fuzz 0.0d0))
  "Make a metal with given albedo."
  (make-instance 'metal :albedo albedo :fuzz fuzz))

(defmethod scatter ((material metal) ray-in rec attenuation scattered-ray)
  (setf (ray-origin scattered-ray) (hit-record-point rec)
	(ray-direction scattered-ray) (add-vec3 (reflect-vec3
						 (unit-vec3 (ray-direction ray-in))
						 (hit-record-normal rec))
						(scale-vec3 (material-fuzz material)
							    (random-vec3-in-unit-sphere))))
  (copy-color (material-albedo material) attenuation)
  (> (dot-vec3 (ray-direction scattered-ray) (hit-record-normal rec)) 0.0d0)) ;return t or nil

(defclass dielectric (material)
  ((ir :initarg :ir :initform 1.0d0 :accessor material-ir))
  (:documentation "Material that partly refracts with index of refraction ir."))

(defun make-dielectric (&optional (index-of-refraction 1.0d0))
  "Make a dielectric material with given index of refraction."
  (make-instance 'dielectric :ir index-of-refraction))

(defun reflectance (cosine ir)
  "Return reflectance estimated by Schlick's approximation."
  (let ((r0 (expt (/ (- 1.0d0 ir) (+ 1.0d0 ir)) 2)))
    (+ r0
       (* (- 1.0d0 r0) (expt (- 1 cosine) 5)))))

(defmethod scatter ((material dielectric) ray-in rec attenuation scattered-ray)
  (let* ((refraction-ratio (if (hit-record-front-face rec)
			       (/ (material-ir material))
			       (material-ir material)))
	 (unit-direction (unit-vec3 (ray-direction ray-in)))
	 (cos-theta (min (dot-vec3 (neg-vec3 unit-direction) (hit-record-normal rec))
			 1.0d0))
	 (sin-theta (sqrt (- 1.0 (expt cos-theta 2))))
	 (cannot-refract (> (* refraction-ratio sin-theta) 1.0d0))
	 direction)
    (if (or cannot-refract
	    (> (reflectance cos-theta refraction-ratio) (random 1.0d0)))
	(setf direction (reflect-vec3 unit-direction (hit-record-normal rec)))
	(setf direction (refract-vec3 unit-direction
				      (hit-record-normal rec)
				      refraction-ratio)))
    (copy-color (make-color 1.0d0 1.0d0 1.0d0) attenuation)
    (setf (ray-origin scattered-ray) (hit-record-point rec)
	  (ray-direction scattered-ray) direction)
    t))
