;;;
;;; Copyright (c) 2016, Gayane Kazhoyan <kazhoyan@cs.uni-bremen.de>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;;     * Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;;     * Redistributions in binary form must reproduce the above copyright
;;;       notice, this list of conditions and the following disclaimer in the
;;;       documentation and/or other materials provided with the distribution.
;;;     * Neither the name of the Institute for Artificial Intelligence/
;;;       Universitaet Bremen nor the names of its contributors may be used to
;;;       endorse or promote products derived from this software without
;;;       specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package :helicopter)

(defparameter *visibility-range* 0.4
  "Radius that camera can see while scanning. In meters")

;;; Might as well use def-cram-function-s but they're not as convenient

(defun land (?pose)
  (declare (type cl-transforms-stamped:pose-stamped ?pose))
  (format t "land ~a~%" ?pose)
  (when ?pose
    (perform (a motion (to fly) (to ?pose))))
  (perform (a motion (to set-altitude) (to 0)))
  (perform (a motion (to switch-engine) (state off))))

(defun take-off (?altitude)
  (declare (type number ?altitude))
  (format t "take-off ~a~%" ?altitude)
  (perform (a motion (to switch-engine) (state on)))
  (perform (a motion (to set-altitude) (to ?altitude))))

(defun calculate-area-via-points (area delta)
  "`area' is a list with 3d-vector of dimensions and pose-stamped.
E.g. (#<3D-VECTOR (d w h)> #<POSE-STAMPED ('frame' stamp (x y z) (q1 q2 q3 w))>)"
  (declare (type list area))
  (destructuring-bind (dimensions stamped-pose)
      area
    (flet ((make-coordinate (x y theta)
             (cl-transforms-stamped:pose->pose-stamped
              (cl-transforms-stamped:frame-id stamped-pose)
              (cl-transforms-stamped:stamp stamped-pose)
              (cl-transforms:transform
               (cl-transforms:pose->transform stamped-pose)
               (cl-transforms:make-pose
                (cl-transforms:make-3d-vector x y 0)
                (cl-transforms:axis-angle->quaternion
                 (cl-transforms:make-3d-vector 0 0 1)
                 theta))))))
      (let* ((dimensions/2 (cl-transforms:v* dimensions 0.5))
             (initial-goal-y (- delta (cl-transforms:y dimensions/2)))
             (initial-goal-x (- delta (cl-transforms:x dimensions/2))))
        (loop for goal-y-sign = 1 then (* goal-y-sign -1)
              for goal-y = initial-goal-y then (* initial-goal-y goal-y-sign)
              for goal-x = initial-goal-x then (+ goal-x (* 2 delta))
              while (<= goal-x (cl-transforms:x dimensions/2))
              collect (make-coordinate goal-x goal-y (* (/ pi 2) goal-y-sign))
              collect (make-coordinate goal-x (- goal-y) 0.0))))))

(defun scan (area)
  "`area' is a list with 3d-vector of dimensions and pose-stamped.
E.g. (#<3D-VECTOR (d w h)> #<POSE-STAMPED ('frame' stamp (x y z) (q1 q2 q3 w))>)"
  (declare (type list area))
  (format t "scan ~a~%" area)
  (mapc (lambda (?goal)
          (perform (a motion (to fly) (to ?goal))))
        (calculate-area-via-points area *visibility-range*)))

(defun continuously-perceive (object-name)
  (format t "continuously-perceive ~a~%" object-name)
  ;; TODO
  )

(defparameter *navigation-altitude* 2
  "In meters. Will be changed into a Prolog rule.")

(defun navigate (?location)
  (format t "go ~a~%" ?location)
  (perform (an action (to take-off) (to *navigation-altitude*)))
  (perform (a motion (to fly) (to ?location))))
