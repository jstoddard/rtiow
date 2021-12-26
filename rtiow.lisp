;;;; Ray Tracing in One Weekend
;;;; Common Lisp source code
;;;; Written in 2021 by Jeremiah Stoddard, based on the book "Ray Tracing
;;;; in One Weekend" by Peter Shirley and available online at
;;;; <https://raytracing.github.io/books/RayTracingInOneWeekend.html>.
;;;;
;;;; To the extent possible under law, the author(s) have dedicated# all
;;;; copyright and related and neighboring rights to this software to the
;;;; public domain worldwide. This software is distributed without any
;;;; warranty.
;;;;
;;;; You should have received a copy of the CC0 Public Domain Dedication
;;;; along with this software. If not, see
;;;; <http://creativecommons.org/publicdomain/zero/1.0/>.

(in-package #:rtiow)

(defparameter +aspect-ratio+ (/ 3.0d0 2.0d0))
(defparameter +image-width+ 600)
(defparameter +image-height+ (round (/ +image-width+ +aspect-ratio+)))
(defparameter +infinity+ most-positive-double-float) ; best approximation
(defparameter +samples-per-pixel+ 100)
(defparameter +max-depth+ 50)

(defun attenuate (attenuation color)
  "Helper function for ray-color."
  (make-color (* (color-r attenuation) (color-r color))
	      (* (color-g attenuation) (color-g color))
	      (* (color-b attenuation) (color-b color))))

(defun ray-color (r world &optional (depth +max-depth+))
  "Compute color in direction of ray r with hittable world."
  (if (<= depth 0)
      (make-color)
      (let ((rec (make-hit-record)))
	(if (hit world r 0.001d0 +infinity+ rec)
	    (let ((scattered (make-ray))
		  (attenuation (make-color)))
	      (if (scatter (hit-record-material rec)
			   r rec attenuation scattered) ; return attenuated color?
		  (attenuate attenuation (ray-color scattered world (- depth 1)))
		  (make-color))) ; ray absorbed, return plain black
	    (let* ((unit-direction (unit-vec3 (ray-direction r)))
		   (s (* 0.5d0 (+ (vec3-y unit-direction) 1.0d0)))) 
	      (vec3->color
	       (add-vec3 (scale-vec3 (- 1.0d0 s) (make-color 1.0d0 1.0d0 1.0d0))
			 (scale-vec3 s (make-color 0.5d0 0.7d0 1.0d0)))))))))

(defun random-scene ()
  "Return hittable-list with a bunch of spheres."
  (let ((world (make-hittable-list))
	(ground-material (make-lambertian (make-color 0.5d0 0.5d0 0.5d0)))
	(material1 (make-dielectric 1.5d0))
	(material2 (make-lambertian (make-color 0.4d0 0.2d0 0.1d0)))
	(material3 (make-metal (make-color 0.7d0 0.6d0 0.5d0) 0.0d0)))

    ;; Ground
    (hittable-list-add
     world
     (make-sphere (make-point 0.0d0 -1000.0d0 0.0d0) 1000.0d0 ground-material))

    ;; Nested loop to make many small spheres
    (do ((a -11 (1+ a)))
	((>= a 11))
      (do ((b -11 (1+ b)))
	  ((>= b 11))
	(let ((choose-mat (random 1.0d0))
	      (center (make-point (+ a (random 0.9d0))
				  0.2d0
				  (+ b (random 0.9d0))))
	      albedo fuzz sphere-material)
	  (when (> (vec3-length (sub-vec3 center
					  (make-vec3 4.0d0 0.2d0 0.0d0)))
		   0.9d0)
	    (cond
	      ((< choose-mat 0.8d0)
	       (setf albedo (attenuate (random-color) (random-color))
		     sphere-material (make-lambertian albedo))
	       (hittable-list-add world (make-sphere center 0.2 sphere-material)))
	      ((< choose-mat 0.95d0)
	       (setf albedo (random-color 0.5d0 1.0d0)
		     fuzz (random 0.5d0)
		     sphere-material (make-metal albedo fuzz))
	       (hittable-list-add world (make-sphere center 0.2 sphere-material)))
	      (t
	       (setf sphere-material (make-dielectric 1.5d0))
	       (hittable-list-add world (make-sphere center 0.2 sphere-material))))))))

    ;; Three larger spheres
    (hittable-list-add world (make-sphere (make-point 0.0d0 1.0d0 0.0d0) 1.0d0 material1))
    (hittable-list-add world (make-sphere (make-point -4.0d0 1.0d0 0.0d0) 1.0d0 material2))
    (hittable-list-add world (make-sphere (make-point 4.0d0 1.0d0 0.0d0) 1.0d0 material3))
    world))

(defun main ()
  (let* ((world (random-scene))
	 (look-from (make-point 13.0d0 2.0d0 3.0d0))
	 (look-at (make-point 0.0d0 0.0d0 0.0d0))
	 (view-up (make-vec3 0.0d0 1.0d0 0.0d0))
	 (dist-to-focus 10.0d0)
	 (aperture 0.1d0)
	 (camera (make-camera look-from look-at view-up
			      20.0d0 +aspect-ratio+
			      aperture dist-to-focus)))
    (with-open-file (stream "~/image.ppm"
			    :direction :output
			    :if-exists :supersede)
      (format stream "P3~%~d ~d~%255~%" +image-width+ +image-height+)
      (do ((row (- +image-height+ 1) (1- row)))
	  ((< row 0))
	#-swank (format t "~cScanlines remaining: ~d   " #\Return row)
	#+swank (format t ".") ; In Emacs, show progress but don't bloat buffer
	(dotimes (col +image-width+)
	  (let ((pixel-color (make-color))) ; start at 0,0,0
	    (dotimes (s +samples-per-pixel+)
	      (let* ((u (/ (+ col (random 1.0d0)) (- +image-width+ 1.0d0)))
		     (v (/ (+ row (random 1.0d0)) (- +image-height+ 1.0d0)))
		     (r (camera-get-ray camera u v)))
		(setf pixel-color (add-color pixel-color (ray-color r world)))))
	    (write-color stream pixel-color +samples-per-pixel+))))))
  (format t "~%Done.~%"))
