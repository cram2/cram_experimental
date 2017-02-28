;;;
;;; Copyright (c) 2017, Gayane Kazhoyan <kazhoyan@cs.uni-bremen.de>
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

(in-package :blue-wasp)

(defun main ()
  (let ((agent-ros-name (rosify (current-robot-symbol))))
    (roslisp-utilities:startup-ros :name agent-ros-name))
  (roslisp:wait-duration 5.0)
  (cpm:with-process-modules-running
      (blue-wasp-sensors helicopter:helicopter-actuators robosherlock-pm)
    ;; (run-reference-server "blue_wasp")
    (run-perform-server "blue_wasp")
    (roslisp:spin-until nil 100)))

(defparameter *pub-transmit* nil)

(defun ensure-transmit-pub ()
  (unless *pub-transmit*
    (setf *pub-transmit*
          (roslisp:advertise "blue_wasp/image_transmitted" "std_msgs/Bool"))
    (roslisp:wait-duration 1))
  *pub-transmit*)

(defun clear-transmit-pub ()
  (setf *pub-transmit* nil))

(roslisp-utilities:register-ros-cleanup-function clear-transmit-pub)

(defun can-send ()
  (let* ((object-individual-string
           (robots-common:loggable-robot-individual-name :blue-wasp))
         (query-string (format nil
                               "action_feasible_on_robot(knowrob:'SendingAHighResPicture', '~a')."
                               object-individual-string)))
    (json-prolog:prolog-simple query-string)))

(defun move-closer ()
  (let* ((wasp-tran (cl-transforms-stamped:lookup-transform
                     cram-tf:*transformer*
                     cram-tf:*fixed-frame*
                     "blue_wasp/base_link"))
         (donkey-tran (cl-transforms-stamped:lookup-transform
                       cram-tf:*transformer*
                       cram-tf:*fixed-frame*
                       "donkey/base_link"))
         (?target-pose (cl-transforms-stamped:make-pose-stamped
                        (cl-transforms-stamped:frame-id wasp-tran)
                        0
                        (cl-transforms-stamped:v*
                         (cl-transforms-stamped:v-
                          (cl-transforms-stamped:translation donkey-tran)
                          (cl-transforms-stamped:translation wasp-tran))
                         0.5)
                        (cl-transforms-stamped:make-identity-rotation))))
    (perform (desig:an action (to fly) (destination (desig:a location (pose ?target-pose)))))))

(defun transmit-image ()
  (roslisp:publish
   (ensure-transmit-pub)
   (roslisp:make-message "std_msgs/bool")))

(defun take-picture ()
  (perform (desig:a motion (to take-picture)))
  ;; (loop until (can-send) do
  ;;   (move-closer))
  ;; (transmit-image)
  )

(defun look-for (?object-name)
  (perform (desig:an action (to take-picture)))
  (perform (desig:a motion (to look-for) (object ?object-name))))

(defun sherpa-search (?object ?area)
  (format t "search for ~a at ~a~%" ?object ?area)
  (cpl:pursue
    (progn ; if scan action finishes first, means look-for didn't finish so no object
      (perform (desig:an action (to scan) (area ?area)))
      (helicopter:say (format nil "Blue Wasp could not find ~a." ?object)))
    (progn ; if look-for finished first, we found victim before scanning was over
      (perform (desig:an action (to look-for) (object ?object)))
      (helicopter:say (format nil "Blue Wasp FOUND ~a." ?object))
      (perform (desig:an action (to take-picture))))))
