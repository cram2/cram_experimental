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

(defmacro define-action-client (agent-name name type timeout)
  "Creates this and that."
  (let* ((package (symbol-package name))
         (param-name (intern (format nil "*~a-ACTION-TIMEOUT*" name) package))
         (action-var-name (intern (format nil "*~a-ACTION-CLIENT*" name) package))
         (init-function-name (intern (format nil "INIT-~a-ACTION-CLIENT" name) package))
         (destroy-function-name (intern (format nil "DESTROY-~a-ACTION-CLIENT" name) package))
         (getter-function-name (intern (format nil "GET-~a-ACTION-CLIENT" name) package))
         (call-function-name (intern (format nil "CALL-~a-ACTION" name) package))
         (ros-name (concatenate 'string (rosify agent-name) "/"
                                (roslisp-utilities:rosify-lisp-name name))))
    `(progn

       (defparameter ,param-name ,timeout
         ,(format nil "How many seconds to wait before returning from ~a action." name))

       (defvar ,action-var-name nil)

       (defun ,init-function-name ()
         (setf ,action-var-name (actionlib:make-action-client ,ros-name ,type))
         (loop until (and ,action-var-name
                          (actionlib:wait-for-server ,action-var-name 5.0))
               do (roslisp:ros-info (robots-common action-client)
                                    "Waiting for ~a action server..." ',name)
               unless ,action-var-name do (return))
         ;; (dotimes (seconds 5) ; give the client some time to settle down
         ;;     (roslisp:ros-info (robots-common action-client)
         ;;                       "Goal subscribers: ~a~%"
         ;;                       (mapcar (lambda (connection)
         ;;                                 (roslisp::subscriber-uri connection))
         ;;                               (roslisp::subscriber-connections
         ;;                                (actionlib::goal-pub ,action-var-name))))
         ;;     (cpl:sleep 1))
         (if ,action-var-name
             (roslisp:ros-info (robots-common action-client)
                               "~a action client created."  ',name)
             (roslisp:ros-info (robots-common action-client)
                               "Waiting for ~a action client cancelled." ',name))
         ,action-var-name)

       (defun ,destroy-function-name ()
         (setf ,action-var-name nil))

       (roslisp-utilities:register-ros-cleanup-function ,destroy-function-name)
       ;; (roslisp-utilities:register-ros-init-function ,init-function-name)
       ;; The init function is necessary because there is a bug when
       ;; nodes don't subscribe to a publisher started from a terminal executable

       (defun ,getter-function-name ()
         (or ,action-var-name (,init-function-name)))

       (defun ,call-function-name (&key action-goal action-timeout)
         (declare (type (or null ,(action-type-class-name type)) action-goal)
                  (type (or null number) action-timeout))
         (roslisp:ros-info (robots-common action-client)
                           "Calling ~a with goal:~%~a" ',call-function-name action-goal)
         (unless action-timeout
           (setf action-timeout ,param-name))
         (cpl:with-failure-handling
             ((simple-error (e)
                (format t "Actionlib error occured!~%~a~%Reinitializing...~%~%" e)
                (,init-function-name)
                (cpl:retry)))
           (let ((actionlib:*action-server-timeout* 10.0)
                 (client (,getter-function-name)))
             (if client
                 (multiple-value-bind (result status)
                     (actionlib:call-goal client action-goal :timeout action-timeout)
                   (roslisp:ros-info (robots-common action-client)
                                     "Done with ~a with status ~a and result:~%~a"
                                     ',call-function-name status result)
                   result)
                 (roslisp:ros-info (robots-common action-client)
                                   "~a aborted because the client was destroyed."
                                   ',call-function-name))))))))


; /RoboSherlock/sherpa_color_object/goal

#+hohohohoandabottleofrum
(
 /donkey/mount  sherpa_msgs/MountAction
 /donkey/drive              MoveToAction

 /hawk/fly                  MoveToAction
       set_altitude         SetAltitudeAction
       toggle_engine        ToggleActuatorAction
       toggle_camera        ToggleActuatorAction
       take_picture         TakePictureAction

 /red_wasp/fly              MoveToAction
           set_altitude     SetAltitudeAction
           toggle_engine    ToggleActuatorAction
           toggle_beacon    ToggleActuatorAction

 /blue_wasp/fly
            set_altitude    SetAltitudeAction
            toggle_engine   ToggleActuatorAction
            toggle_camera   ToggleActuatorAction
            take_picture    TakePictureAction
            detect_victim   ToggleVictimTrackingAction
)

(defun make-mount-goal (target-name mount?-otherwise-unmount)
  (declare (type (or symbol string) target-name)
           (type boolean mount?-otherwise-unmount))
  (make-symbol-type-message
   'sherpa_msgs-msg:MountGoal
   :target_name (format nil "~a" target-name)
   :mounted_state mount?-otherwise-unmount))

(defun make-move-to-goal (goal)
  (declare (type (or cl-transforms-stamped:pose-stamped cl-transforms:pose) goal))
  (make-symbol-type-message
   'sherpa_msgs-msg:MoveToGoal
   :goal (cl-transforms-stamped:to-msg
          (cl-transforms-stamped:ensure-pose-stamped
           goal cram-tf:*fixed-frame* 0.0))))

(defun make-set-altitude-goal (altitude)
  (declare (type number altitude))
  (make-symbol-type-message
   'sherpa_msgs-msg:SetAltitudeGoal
   :altitude altitude))

(defun make-take-picture-goal (agent-name)
  (declare (type symbol agent-name))
  (make-symbol-type-message
   'sherpa_msgs-msg:TakePictureGoal
   :picture_id (format nil "~a_~d"
                       (rosify agent-name)
                       (floor (* (roslisp:ros-time) 1000000)))))

(defun make-toggle-actuator-goal (on?-otherwise-off)
  (declare (type boolean on?-otherwise-off))
  (make-symbol-type-message
   'sherpa_msgs-msg:ToggleActuatorGoal
   :command on?-otherwise-off))

(defun make-toggle-victim-tracking-goal (on?-otherwise-off)
  (declare (type boolean on?-otherwise-off))
  (make-symbol-type-message
   'sherpa_msgs-msg:ToggleVictimTrackingGoal
   :command on?-otherwise-off))
