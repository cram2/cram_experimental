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

(in-package :wasp)

(defun projection-altitude (altitude)
  (declare (type number altitude))
  (format t "projection altitude ~a~%" altitude)
  )

(defun projection-engine (on?)
  (declare (type boolean on?))
  (format t "projection engine ~a~%" on?)
  (unless on?
    ;;set altitude to 0.1
    ))

(defun projection-fly (goal)
  (declare (type cl-transforms-stamped:pose-stamped goal))
  (format t "projection fly ~a~%" goal)
  ;; the names are in the sandbox
  ;; (btr-utils:move-object *red-wasp-name* goal)
  )

(defun projection-set-beacon (on?)
  (declare (type boolean on?))
  (format t "projection set-beacon ~a~%" on?)
  )


(cram-projection:define-projection-environment wasp-bullet-projection-environment
  ;; :special-variable-initializers
  ;; ((*transformer* (make-instance 'cl-tf:transformer))
  ;;  (*current-bullet-world* (cl-bullet:copy-world *current-bullet-world*))
  ;;  (*current-timeline* (btr:timeline-init *current-bullet-world*))
  ;;  (desig:*default-role* 'projection-role)
  ;;  (*projection-clock* (make-instance 'partially-ordered-clock))
  ;;  (cut:*timestamp-function* #'projection-timestamp-function))
  :process-module-definitions (wasp-projection-actuators wasp-projection-sensors)
  ;; :startup (update-tf)
  ;; :shutdown (setf *last-timeline* *current-timeline*)
  )

(def-fact-group wasp-projection-pms (cpm:matching-process-module
                                     cpm:projection-running
                                     cpm:available-process-module)

  (<- (cpm:matching-process-module ?motion-designator wasp-projection-actuators)
    (or (desig:desig-prop ?motion-designator (:type :flying))
        (desig:desig-prop ?motion-designator (:to :fly))))

  (<- (cpm:matching-process-module ?motion-designator wasp-projection-actuators)
    (or (desig:desig-prop ?motion-designator (:type :switching-engine))
        (desig:desig-prop ?motion-designator (:to :switch-engine))))

  (<- (cpm:matching-process-module ?motion-designator wasp-projection-actuators)
    (or (desig:desig-prop ?motion-designator (:type :setting-altitude))
        (desig:desig-prop ?motion-designator (:to :set-altitude))))

  (<- (cpm:matching-process-module ?motion-designator wasp-projection-sensors)
    (or (desig:desig-prop ?motion-designator (:type :switching-beacon))
        (desig:desig-prop ?motion-designator (:to :switch-beacon))))

  (<- (cpm:projection-running ?_)
    (symbol-value cram-projection:*projection-environment* wasp-bullet-projection-environment))

  (<- (cpm:available-process-module wasp-projection-actuators)
    (symbol-value cram-projection:*projection-environment* wasp-bullet-projection-environment))

  (<- (cpm:available-process-module wasp-projection-sensors)
    (symbol-value cram-projection:*projection-environment* wasp-bullet-projection-environment)))


(cpm:def-process-module wasp-projection-actuators (motion-designator)
  (destructuring-bind (command parameter)
      (desig:reference motion-designator)
    (ecase command
      (fly
       (projection-fly parameter))
      (switch-engine
       (projection-engine parameter))
      (set-altitude
       (projection-altitude parameter)))))

(cpm:def-process-module wasp-projection-sensors (motion-designator)
  (destructuring-bind (command parameter)
      (desig:reference motion-designator)
    (ecase command
      (switch-beacon
       (projection-set-beacon parameter)))))
