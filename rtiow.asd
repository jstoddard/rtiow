(asdf:defsystem #:rtiow
  :components ((:file "package")
	       (:file "utility")
	       (:file "vec3":depends-on ("utility"))
	       (:file "ray" :depends-on ("vec3"))
	       (:file "material" :depends-on ("vec3" "ray"))
	       (:file "hittable" :depends-on ("vec3" "ray" "material"))
	       (:file "camera" :depends-on ("vec3" "ray"))
	       (:file "rtiow" :depends-on ("vec3" "ray" "camera" "hittable"))))
