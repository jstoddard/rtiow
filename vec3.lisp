;;;; vec3.lisp
;;;; 3D vector class for Raytracing in One Weekend
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

(defun make-vec3 (&optional (x 0.0d0) (y 0.0d0) (z 0.0d0))
  "Create a 3-element vector with coordinates x, y, z"
  (make-array 3
	      :element-type 'double-float
	      :initial-contents (list x y z)))

(defmacro vec3-x (v)
  `(aref ,v 0))

(defmacro vec3-y (v)
  `(aref ,v 1))

(defmacro vec3-z (v)
  `(aref ,v 2))

(defgeneric vec3-length-squared (vec)
  (:documentation "Returns the square of the length of vec3."))

(defmethod vec3-length-squared ((vec vector))
  (+ (expt (vec3-x vec) 2)
     (expt (vec3-y vec) 2)
     (expt (vec3-z vec) 2)))

(defgeneric vec3-length (vec)
  (:documentation "Returns length of vec3."))

(defmethod vec3-length ((vec vector))
  (sqrt (vec3-length-squared vec)))

(defgeneric vec3-near-zero? (vec)
  (:documentation "Return true if vec3 is very small"))

(defmethod vec3-near-zero? ((vec vector))
  (and
   (< (vec3-x vec) 0.00000001)
   (< (vec3-y vec) 0.00000001)
   (< (vec3-z vec) 0.00000001)))

(defun add-vec3 (u v)
  "Returns new vec3 by summing u and v, both of which must be of type vec3."
  (make-vec3 (+ (vec3-x u) (vec3-x v))
	     (+ (vec3-y u) (vec3-y v))
	     (+ (vec3-z u) (vec3-z v))))

(defun sub-vec3 (u v)
  "Returns new vec3 by subtracting v from u. Both u and v must be of type vec3."
  (make-vec3 (- (vec3-x u) (vec3-x v))
	     (- (vec3-y u) (vec3-y v))
	     (- (vec3-z u) (vec3-z v))))

(defun neg-vec3 (vec)
  "Returns 0,0,0 - vec."
  (sub-vec3 (make-vec3) vec))

(defun scale-vec3 (s v)
  "Returns new vec3 with coordinates of vec3 v multiplied by scalar s."
  (make-vec3 (* s (vec3-x v))
	     (* s (vec3-y v))
	     (* s (vec3-z v))))

(defun dot-vec3 (u v)
  "Returns the dot product of u and v, both of which must be of type vec3."
  (+ (* (vec3-x u) (vec3-x v))
     (* (vec3-y u) (vec3-y v))
     (* (vec3-z u) (vec3-z v))))

(defun cross-vec3 (u v)
  "Returns the cross product of u and v, both of which must be of type vec3."
  (make-vec3 (- (* (vec3-y u) (vec3-z v))
		(* (vec3-z u) (vec3-y v)))
	     (- (* (vec3-z u) (vec3-x v))
		(* (vec3-x u) (vec3-z v)))
	     (- (* (vec3-x u) (vec3-y v))
		(* (vec3-y u) (vec3-x v)))))

(defun unit-vec3 (vec)
  "Returns unit vector with direction of vec, which must be of type vec3."
  (scale-vec3 (/ (vec3-length vec)) vec))

(defun reflect-vec3 (vec normal)
  "Reflect vec3 vec from surface with normal normal."
  (sub-vec3 vec
	    (scale-vec3 (* 2 (dot-vec3 vec normal))
			normal)))

(defun refract-vec3 (vec normal etai/etat)
  (let* ((cos-theta (min (dot-vec3 (neg-vec3 vec) normal) 1.0d0))
	 (r-out-perp (scale-vec3 etai/etat
				 (add-vec3 vec (scale-vec3 cos-theta normal))))
	 (r-out-par (scale-vec3 (- (sqrt
				    (abs
				     (- 1.0d0 (vec3-length-squared r-out-perp)))))
				normal)))
    (add-vec3 r-out-perp r-out-par)))

(defun random-vec3 (&optional (min 0.0d0) (max 1.0d0))
  "Return a vec3 with random coordinates."
  (let ((difference (- max min)))
    (unless (> difference 0.0d0) (error "max must be greater than min"))
    (let ((x (+ (random difference) min))
	  (y (+ (random difference) min))
	  (z (+ (random difference) min)))
      (make-vec3 x y z))))

(defun random-vec3-in-unit-sphere ()
  "Return a vec3 with random components within a unit sphere."
  (let ((p (random-vec3)))
    (if (< (vec3-length p) 1)
	p
	(random-vec3-in-unit-sphere))))

(defun random-unit-vec3 ()
  "Return a unit vec3 with random direction."
  (unit-vec3 (random-vec3-in-unit-sphere)))

(defun random-vec3-in-unit-disk ()
  "Return a vec3 with random x and y components between -1 and 1."
  (let ((p (make-vec3 (- (random 2.0d0) 1.0d0) (- (random 2.0d0) 1.0d0) 0.0d0)))
    (if (< (vec3-length p) 1)
	p
	(random-vec3-in-unit-disk))))

;;; point and color are effectively aliases of vec3
(defun make-point (&optional (x 0.0d0) (y 0.0d0) (z 0.0d0))
  "Create a point with coordinates x, y, z."
  (make-vec3 x y z))

(defun make-color (&optional (r 0.0d0) (g 0.0d0) (b 0.0d0))
  "Create a color with red, green, and blue components r, g, and b."
  (make-vec3 r g b))

(defmacro color-r (v)
  `(aref ,v 0))

(defmacro color-g (v)
  `(aref ,v 1))

(defmacro color-b (v)
  `(aref ,v 2))

(defun copy-color (src dst)
  "Put the r, g, and b components from src into dst."
  (setf (color-r dst) (color-r src)
	(color-g dst) (color-g src)
	(color-b dst) (color-b src)))

(defun add-color (u v)
  "Like add-vec3, but returns a color."
  (make-color (+ (vec3-x u) (vec3-x v))
	      (+ (vec3-y u) (vec3-y v))
	      (+ (vec3-z u) (vec3-z v))))

(defun random-color (&optional (min 0.0d0) (max 1.0d0))
  (let* ((diff (- max min))
	 (r (+ (random diff) min))
	 (g (+ (random diff) min))
	 (b (+ (random diff) min)))
    (make-color r g b)))

(defgeneric write-color (stream pixel-color samples-per-pixel)
  (:documentation "Write the translated [0,255] value of each color component to stream."))

(defmethod write-color (stream (pixel-color vector) samples-per-pixel)
  (let* ((scale (/ 1.0d0 samples-per-pixel))
	 ;; Divide by number of samples and gamma correct for gamma = 2.0
	 (r (sqrt (* scale (color-r pixel-color))))
	 (g (sqrt (* scale (color-g pixel-color))))
	 (b (sqrt (* scale (color-b pixel-color)))))
    (format stream "~d ~d ~d~%"
	    (floor (* 256.0d0 (clamp r 0.0 0.999)))
	    (floor (* 256.0d0 (clamp g 0.0 0.999)))
	    (floor (* 256.0d0 (clamp b 0.0 0.999))))))

