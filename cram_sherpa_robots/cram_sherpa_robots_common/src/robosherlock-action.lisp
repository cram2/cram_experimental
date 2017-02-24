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

(in-package :robots-common)

(defparameter *robosherlock-action-name* "RoboSherlock/sherpa_color_object")
(defparameter *robosherlock-action-type* "iai_robosherlock_msgs/HighlightObjectAction")

(defparameter *robosherlock-action-timeout* 5000000000
  "How many seconds to wait before returning from the robosherlock action.")

(defvar *robosherlock-action-client* nil)

(defun init-robosherlock-action-client ()
  (setf *robosherlock-action-client*
        (actionlib:make-action-client
         *robosherlock-action-name*
         *robosherlock-action-type*))
  (loop until (actionlib:wait-for-server *robosherlock-action-client* 5.0)
        do (roslisp:ros-info (robots-common robosherlock-client)
                             "Waiting for robosherlock action server..."))
  ;; (dotimes (seconds 5) ; give the client some time to settle down
  ;;     (roslisp:ros-info (robots-common action-client)
  ;;                       "Goal subscribers: ~a~%"
  ;;                       (mapcar (lambda (connection)
  ;;                                 (roslisp::subscriber-uri connection))
  ;;                               (roslisp::subscriber-connections
  ;;                                (actionlib::goal-pub ,action-var-name))))
  ;;     (cpl:sleep 1))
  (roslisp:ros-info (robots-common robosherlock-client) "robosherlock action client created.")
  *robosherlock-action-client*)

(defun destroy-robosherlock-action-clients ()
  (setf *robosherlock-action-client* nil))

(roslisp-utilities:register-ros-cleanup-function destroy-robosherlock-action-clients)
;; (roslisp-utilities:register-ros-init-function ,init-function-name)
;; The init function is necessary because there is a bug when
;; nodes don't subscribe to a publisher started from a terminal executable

(defun get-robosherlock-action-client ()
  (or *robosherlock-action-client* (init-robosherlock-action-client)))

(defun call-robosherlock-action (object-name object-cram-name &optional action-timeout)
  (declare (type string object-name)
           (type (or null number) action-timeout))
  (roslisp:ros-info (robots-common robosherlock-client)
                    "Calling CALL-ROBOSHERLOCK-ACTION with object ~a" object-name)
  (unless action-timeout
    (setf action-timeout *robosherlock-action-timeout*))
  (cpl:with-failure-handling
      ((simple-error (e)
         (format t "Actionlib error occured!~%~a~%Reinitializing...~%~%" e)
         (init-robosherlock-action-client)
         (cpl:retry)))
    (let ((actionlib:*action-server-timeout* 10.0))
      (multiple-value-bind (result status)
          (actionlib:call-goal
           (get-robosherlock-action-client)
           (robots-common:make-symbol-type-message
            'iai_robosherlock_msgs-msg:HighlightObjectGoal
            :obj_name object-name)
           :timeout action-timeout)
        (roslisp:ros-info (robots-common action-client)
                          "Done with RoboSherlock action with status ~a and result:~%~a"
                          status result)
        (let ((object-pose (cl-transforms-stamped:from-msg
                            (roslisp:msg-slot-value
                             result
                             'iai_robosherlock_msgs-msg:pose))))
          (cram-occasions-events:on-event (make-instance 'robots-common:object-found
                                          :object object-cram-name :pose object-pose))
          object-pose)))))



(def-fact-group robosherlock-desigs (desig:motion-desig)
  (<- (motion-desig ?motion-designator (look-for ?object-name ?object-cram-name))
    (or (desig-prop ?motion-designator (:type :looking-for))
        (desig-prop ?motion-designator (:to :look-for)))
    (desig-prop ?motion-designator (:object ?object-cram-name))
    (lisp-fun cram-owl-name ?object-cram-name ?object-name)))

(def-fact-group robosherlock-pm (cpm:matching-process-module cpm:available-process-module)
  (<- (cpm:matching-process-module ?motion-designator robosherlock-pm)
    (or (desig-prop ?motion-designator (:type :looking-for))
        (desig-prop ?motion-designator (:to :look-for))))
  (<- (cpm:available-process-module robosherlock-pm)
    (not (cpm:projection-running ?_))))

(cpm:def-process-module robosherlock-pm (motion-designator)
  (destructuring-bind (command argument-1 argument-2) (reference motion-designator)
    (ecase command
      (look-for
       (handler-case
           (call-robosherlock-action argument-1 argument-2)
         ;; (cram-plan-failures:look-at-failed ()
         ;;   (cpl:fail 'cram-plan-failures:look-at-failed :motion motion-designator))
         )))))

;; example:
;; (cpm:with-process-modules-running
;;     (robosherlock-pm)
;;   (cpl:top-level
;;     (perform (an action (to look-for) (object "Bridge_1")))))
