;;;; hittable.lisp
;;;; Class for things a ray might hit for Raytracing in One Weekend
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

(defclass hittable ()
  ()
  (:documentation "Base class for objects that can be hit by a ray."))

(defgeneric hit (h r s-min s-max rec)
  (:documentation "Determine whether ray r hits hittable h. Updates hit-record rec."))

(defclass sphere (hittable)
  ((center :initarg :center :initform (make-point) :accessor sphere-center)
   (radius :initarg :radius :initform 0.1d0 :accessor sphere-radius)
   (material :initarg :material :initform nil :accessor sphere-material))
  (:documentation "Sphere class."))

(defun make-sphere (&optional (center (make-point)) (radius 0.1d0) (material nil))
  "Return a new sphere"
  (make-instance 'sphere :center center :radius radius :material material))

(defmethod hit ((h sphere) (r ray) s-min s-max rec)
  (let* ((oc (sub-vec3 (ray-origin r) (sphere-center h)))
	 (a (vec3-length-squared (ray-direction r)))
	 (half-b (dot-vec3 oc (ray-direction r)))
	 (c (- (vec3-length-squared oc) (expt (sphere-radius h) 2)))
	 (discriminant (- (expt half-b 2)
			  (* a c))))
    (if (< discriminant 0)
	nil
	(let* ((sqrtd (sqrt discriminant))
	       (root (/ (- (- half-b) sqrtd) a)))
	  (when (or (< root s-min) (> root s-max))
	    (setf root (/ (+ (- half-b) sqrtd) a)))
	  (if (or (< root s-min) (> root s-max))
	      nil
	      (progn
		(setf (hit-record-s rec) root
		      (hit-record-point rec) (ray-at r (hit-record-s rec))
		      (hit-record-material rec) (sphere-material h))
		(let ((outward-normal (scale-vec3 (/ (sphere-radius h))
						  (sub-vec3 (hit-record-point rec)
							    (sphere-center h)))))
		  (set-face-normal rec r outward-normal))
		t))))))

(defclass hittable-list (hittable)
  ((objs :initform nil :accessor hittable-objs))
  (:documentation "A convenience class to treat a group of hittable instances as one."))

(defun make-hittable-list ()
  "Return a new, empty hittable list"
  (make-instance 'hittable-list))

(defgeneric hittable-list-clear (l)
  (:documentation "Clear list of objects from hittable-list l."))

(defmethod hittable-list-clear ((l hittable-list))
  (setf (hittable-objs l) nil))

(defgeneric hittable-list-add (l obj)
  (:documentation "Add hittable obj to hittable-list l"))

(defmethod hittable-list-add ((l hittable-list) (obj hittable))
  (push obj (hittable-objs l)))

(defmethod hit ((l hittable-list) (r ray) s-min s-max rec)
  (let ((temp-rec (make-hit-record))
	(hit-anything nil)
	(closest-so-far s-max))
    (dolist (obj (hittable-objs l) hit-anything)
      (when (hit obj r s-min closest-so-far temp-rec)
	(setf hit-anything t closest-so-far (hit-record-s temp-rec))
	(copy-rec temp-rec rec)))))
