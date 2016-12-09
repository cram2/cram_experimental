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

(defun spawn-object-aabb-box (object-name
                              &key
                                (box-name (intern (concatenate
                                                   'string
                                                   (symbol-name object-name)
                                                   "-BOX")))
                                (world btr:*current-bullet-world*))
  (let* ((aabb (btr:aabb (btr-utils:object-instance object-name)))
         (aabb-pose (cl-transforms:make-pose
                     (cl-bullet:bounding-box-center aabb)
                     (cl-transforms:make-identity-rotation)))
         (aabb-size (with-slots (cl-transforms:x cl-transforms:y cl-transforms:z)
                        (cl-bullet:bounding-box-dimensions aabb)
                      (list cl-transforms:x cl-transforms:y cl-transforms:z))))
    (btr:add-object world :box box-name aabb-pose :mass 0.0 :size aabb-size)
    box-name))

(defun translate-object (object-name &optional (pose-z-delta 0.0))
  (let* ((object-pose (btr-utils:object-pose object-name))
         (new-pose (cl-transforms:copy-pose
                    object-pose
                    :origin (cl-transforms:copy-3d-vector
                             (cl-transforms:origin object-pose)
                             :z (+ pose-z-delta
                                   (cl-transforms:z (cl-transforms:origin object-pose)))))))
    (btr-utils:move-object object-name new-pose)))

(defun calculate-altitude-bullet (object-name &optional (z-delta 0.1))
  (let ((box-name (spawn-object-aabb-box object-name))
        (terrain-name (cut:var-value '?terrain
                                     (car (prolog:prolog
                                           '(robots-common:terrain-name ?terrain))))))
    ;; hack to make collision detection acknowledge the box
    (translate-object box-name 0.1)
    (translate-object box-name -0.1)
    (let ((steps-to-collision
            (loop for i = 0 then (+ 1 i)
                  until (member (btr-utils:object-instance box-name)
                                (btr:find-objects-in-contact btr:*current-bullet-world*
                                                             (btr-utils:object-instance
                                                              terrain-name)))
                  do (translate-object box-name (- z-delta))
                  finally (return i))))
      (btr-utils:kill-object box-name)
      (* z-delta steps-to-collision))))

(defun projection-altitude (altitude)
  (declare (type number altitude))
  (format t "projection altitude ~a~%" altitude)
  (let* ((my-name (cut:var-value '?name
                                 (car (prolog:prolog
                                       '(cram-robot-interfaces:robot ?name)))))
         (current-altitude (calculate-altitude-bullet my-name)))
    (translate-object my-name (- altitude current-altitude))))

(defun projection-engine (on?)
  (declare (type boolean on?))
  (format t "projection engine ~a~%" on?)
  (unless on?
    (projection-altitude -2)))

(defun projection-fly (goal)
  (declare (type cl-transforms-stamped:pose-stamped goal))
  (format t "projection fly ~a~%" goal)
  ;; the names are in the sandbox
  (assert
   (prolog:prolog
    `(and (cram-robot-interfaces:robot ?robot)
          (btr:bullet-world ?w)
          (btr:assert (btr:object-pose ?w ?robot ,goal))))))

;; (defun projection-set-beacon (on?)
;;   (declare (type boolean on?))
;;   (format t "projection set-beacon ~a~%" on?)
;;   )


(cpm:def-process-module helicopter-projection-actuators (motion-designator)
  (destructuring-bind (command parameter)
      (desig:reference motion-designator)
    (ecase command
      (fly
       (projection-fly parameter))
      (switch-engine
       (projection-engine parameter))
      (set-altitude
       (projection-altitude parameter)))))

;; (cpm:def-process-module helicopter-projection-sensors (motion-designator)
;;   (destructuring-bind (command parameter)
;;       (desig:reference motion-designator)
;;     (ecase command
;;       (switch-beacon
;;        (projection-set-beacon parameter)))))


(cram-projection:define-projection-environment helicopter-bullet-projection-environment
  ;; :special-variable-initializers
  ;; ((*transformer* (make-instance 'cl-tf:transformer))
  ;;  (*current-bullet-world* (cl-bullet:copy-world *current-bullet-world*))
  ;;  (*current-timeline* (btr:timeline-init *current-bullet-world*))
  ;;  (desig:*default-role* 'projection-role)
  ;;  (*projection-clock* (make-instance 'partially-ordered-clock))
  ;;  (cut:*timestamp-function* #'projection-timestamp-function))
  :process-module-definitions (helicopter-projection-actuators ;; helicopter-projection-sensors
                                                               )
  ;; :startup (update-tf)
  ;; :shutdown (setf *last-timeline* *current-timeline*)
  )

(def-fact-group helicopter-projection-pms (cpm:matching-process-module
                                     cpm:projection-running
                                     cpm:available-process-module)

  (<- (cpm:matching-process-module ?motion-designator helicopter-projection-actuators)
    (or (desig:desig-prop ?motion-designator (:type :flying))
        (desig:desig-prop ?motion-designator (:to :fly))))

  (<- (cpm:matching-process-module ?motion-designator helicopter-projection-actuators)
    (or (desig:desig-prop ?motion-designator (:type :switching-engine))
        (desig:desig-prop ?motion-designator (:to :switch-engine))))

  (<- (cpm:matching-process-module ?motion-designator helicopter-projection-actuators)
    (or (desig:desig-prop ?motion-designator (:type :setting-altitude))
        (desig:desig-prop ?motion-designator (:to :set-altitude))))

  ;; (<- (cpm:matching-process-module ?motion-designator wasp-projection-sensors)
  ;;   (or (desig:desig-prop ?motion-designator (:type :switching-beacon))
  ;;       (desig:desig-prop ?motion-designator (:to :switch-beacon))))

  (<- (cpm:projection-running ?_)
    (symbol-value cram-projection:*projection-environment*
                  helicopter-bullet-projection-environment))

  (<- (cpm:available-process-module helicopter-projection-actuators)
    (symbol-value cram-projection:*projection-environment*
                  helicopter-bullet-projection-environment))

  ;; (<- (cpm:available-process-module wasp-projection-sensors)
  ;;   (symbol-value cram-projection:*projection-environment*
  ;;                 helicopter-bullet-projection-environment))
  )

