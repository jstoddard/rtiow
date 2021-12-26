;;;; utility.lisp
;;;; Small utility functions for Raytracing in One Weekend
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

(declaim (inline degrees-to-radians))

(defun degrees-to-radians (degrees)
  (/ (* degrees pi) 180))

(declaim (inline clamp))

(defun clamp (x min max)
  (cond
    ((< x min) min)
    ((> x max) max)
    (t x)))

