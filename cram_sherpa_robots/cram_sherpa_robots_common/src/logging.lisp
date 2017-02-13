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

(defparameter *unique-id-length* 14
  "How many characters to append to create unique IDs for OWL individuals")

(defparameter *cram-owl-action-names* nil
  "An association list of CRAM name to OWL name mappings")

(defparameter *logging-action-name* "???")
(defparameter *logging-action-type* "sherpa_msgs/???Action")

(defparameter *logging-action-timeout* 5
  "How many seconds to wait before returning from the logging action.")

(defvar *logging-action-client* nil)

(defun init-logging-action-client ()
  (setf *logging-action-client*
        (actionlib:make-action-client *logging-action-name* *logging-action-type*))
  (loop until (actionlib:wait-for-server *logging-action-client* 5.0)
        do (roslisp:ros-info (robots-common logging-client)
                             "Waiting for logging action server..."))
  ;; (dotimes (seconds 5) ; give the client some time to settle down
  ;;     (roslisp:ros-info (robots-common action-client)
  ;;                       "Goal subscribers: ~a~%"
  ;;                       (mapcar (lambda (connection)
  ;;                                 (roslisp::subscriber-uri connection))
  ;;                               (roslisp::subscriber-connections
  ;;                                (actionlib::goal-pub ,action-var-name))))
  ;;     (cpl:sleep 1))
  (roslisp:ros-info (robots-common logging-client) "logging action client created.")
  *logging-action-client*)

(defun destroy-logging-action-clients ()
  (setf *logging-action-client* nil))

(roslisp-utilities:register-ros-cleanup-function destroy-logging-action-clients)
;; (roslisp-utilities:register-ros-init-function ,init-function-name)
;; The init function is necessary because there is a bug when
;; nodes don't subscribe to a publisher started from a terminal executable

(defun get-logging-action-client ()
  (or *logging-action-client* (init-logging-action-client)))

(defun call-logging-action (action-goal &optional action-timeout)
  (declare ;(type sherpa_msgs-msg:LogGoal action-goal)
           (type (or null number) action-timeout))
  (roslisp:ros-info (robots-common logging-client)
                    "Calling CALL-LOGGING-ACTION with ~a" action-goal)
  (unless action-timeout (setf action-timeout *logging-action-timeout*))
  (multiple-value-bind (result status)
      (cpl:with-failure-handling
          ((simple-error (e)
             (format t "Actionlib error occured!~%~a~%Reinitializing...~%~%" e)
             (init-logging-action-client)
             (cpl:retry)))
        (let ((actionlib:*action-server-timeout* 10.0))
          (actionlib:call-goal
           (get-logging-action-client) action-goal :timeout action-timeout)))
    status))


(defun random-string (length)
  (let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"))
    (coerce (loop repeat length collect (aref chars (random (length chars))))
            'string)))

(defun loggable-timepoint ()
  (format nil "timepoint_~,3f" (roslisp:ros-time)))

(defun loggable-robot-name (cram-robot-name)
  ;; todo: json query to find out the instance of robot
  )

(defun loggable-action-name (cram-action-name)
  (concatenate 'string
               (cdr (assoc *cram-owl-action-names* cram-action-name))
               (random-string *unique-id-length*)))

;; (defun make-logging-goal ()
;;   (make-symbol-type-message
;;    'sherpa_msgs-msg:LogGoal
;;    :obj_name object-name))


;;; TODO: macro to wrap perform action designator in logging
