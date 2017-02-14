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

(defparameter *owl-namespaces*
  '((:rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    (:rdfs "http://www.w3.org/2000/01/rdf-schema#")
    (:owl "http://www.w3.org/2002/07/owl#")
    (:xsd "http://www.w3.org/2001/XMLSchema#")
    (:knowrob "http://knowrob.org/kb/knowrob.owl#")
    (:log "http://knowrob.org/kb/unreal_log.owl#")
    (:u-map "http://knowrob.org/kb/u_map.owl#")
    (:swrl "http://www.w3.org/2003/11/swrl#")
    (:computable "http://knowrob.org/kb/computable.owl#")))

(defparameter *cram-owl-names*
  '((:red-wasp "SherpWaspRed" )
    (:blue-wasp "SherpaWaspBlue")
    (:hawk "SherpaHawk")
    (:donkey "SherpaDonkey")

    (:go "Movement-TranslationEvent")
    (:going "Movement-TranslationEvent")
    (:scan "Scanning")
;    (:scanning "Scanning")
    ;; TODO: talk to Daniel about TurningOn/Off
    ;; (:switch-on "TurningOnPoweredDevice")
    ;; (:switch-off "TurningOffPoweredDevice")
    (:look-for "Perceiving-Voluntary")
;    (:looking-for "Perceiving-Voluntary")
    (:land "Landing-Aircraft")
;    (:landing "Landing-Aircraft")
    (:mount "MountingSomething")
;    (:mounting "MountingSomething")
    (:take-off "TakingOffAircraft")
;    (:taking-off "TakingOffAircraft")
    )
  "An association list of CRAM name to OWL name mappings")

(defgeneric log-owl (object &key &allow-other-keys)
  (:documentation "call logging action on `object'"))

(defparameter *logging-action-name* "???")
(defparameter *logging-action-type* "sherpa_msgs/LogEventAction")

(defparameter *logging-action-timeout* 3
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
  (declare (type sherpa_msgs-msg:LogEventGoal action-goal)
           (type (or null number) action-timeout))
  (roslisp:ros-info (robots-common logging-client)
                    "Calling CALL-LOGGING-ACTION with ~a" action-goal)
  (unless action-timeout (setf action-timeout *logging-action-timeout*))
  ;; (multiple-value-bind (result status)
  ;;     (cpl:with-failure-handling
  ;;         ((simple-error (e)
  ;;            (format t "Actionlib error occured!~%~a~%Reinitializing...~%~%" e)
  ;;            (init-logging-action-client)
  ;;            (cpl:retry)))
  ;;       (let ((actionlib:*action-server-timeout* 10.0))
  ;;         (actionlib:call-goal
  ;;          (get-logging-action-client) action-goal :timeout action-timeout)))
  ;;   (print result)
  ;;   status)
  )

(defun make-property-msg (name &key resource type value)
  (declare (type string name)
           (type (or null string) resource type value))
  (make-symbol-type-message
   'sherpa_msgs-msg:LoggedRDFEntry
   :property_name name
   :rdf_resource (or resource "")
   :rdf_datatype (or type "")
   :value (or value "")
   :use_resource (if resource T NIL)))

(defun make-logging-goal (name type property-msgs)
  (declare (type string name type)
           (type vector property-msgs))
  (make-symbol-type-message
   'sherpa_msgs-msg:LogEventGoal
   :name name
   :name_id_flag (roslisp:symbol-code 'sherpa_msgs-msg:LogEventGoal :USE_ID_IN_NAME)
   :type type
   :rdf_entries property-msgs))


;;; TODO: macro to wrap perform action designator in logging

(defun namespaced (namespace string)
  (concatenate 'string (cadr (assoc namespace *owl-namespaces*)) string))

(defun unique-id-ed (string)
  (flet ((random-string (&optional (length *unique-id-length*))
           (let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"))
             (coerce (loop repeat length collect (aref chars (random (length chars))))
                     'string))))
    (concatenate 'string string "_" (random-string))))

(defun loggable-timepoint (&optional time)
  (declare (type (or null double-float) time))
  (format nil "timepoint_~,3f" (or time (roslisp:ros-time))))

(defun loggable-location-name ()
  (namespaced :log (unique-id-ed "location")))

(defun cram-owl-name (cram-name)
  (declare (type keyword cram-name))
  (cadr (assoc cram-name *cram-owl-names*)))

(defun loggable-robot-type (cram-robot-type)
  (declare (type symbol cram-robot-type))
  ;; todo: json query to find out the instance of robot
  (namespaced :knowrob (cram-owl-name (intern (symbol-name cram-robot-type) :keyword))))

(defun loggable-robot-instance (&optional cram-robot-type)
  (declare (type (or null symbol) cram-robot-type))
  (let ((owl-robot-name
            (cut:var-value
             :?name
             (car
              (json-prolog:prolog
               `("owl_individual_of" ?name ,(robots-common::loggable-robot-type
                                             (or cram-robot-type (current-robot-symbol))))
               :package :keyword)))))
      (if (cut:is-var owl-robot-name)
          NIL
          (string-trim "'" (symbol-name owl-robot-name)))))

(defun loggable-action-individual-name (cram-action-type)
  (namespaced :log (unique-id-ed (cram-owl-name cram-action-type))))

(defun loggable-action-type (cram-action-type)
  (namespaced :knowrob (cram-owl-name cram-action-type)))

(defmethod log-owl ((designator location-designator) &key name-with-id)
  (call-logging-action
   (make-logging-goal
    name-with-id
    (namespaced :knowrob "Location")
    (vector (make-property-msg "knowrob:startTime" :resource (loggable-timepoint))
          (make-property-msg "knowrob:endTime" :resource (loggable-timepoint))
          (make-property-msg "knowrob:taskSuccess" :type (namespaced :xsd "boolean")
                                                   :value "true")))))

(defmethod log-owl ((designator action-designator) &key start-time agent)
  (log-owl-action
   (or (desig:desig-prop-value designator :to)
       (desig:desig-prop-value designator :type))
   designator
   :start-time start-time
   :agent agent))

(defgeneric log-owl-action (action-type action-designator &key start-time agent))

;; TODO: ask Mihai to return the NAME+ID of logged entry

(defmethod log-owl-action ((type (eql :go)) designator &key start-time agent)
  (let ((goal-location-name (loggable-location-name)))
    (call-logging-action
     (make-logging-goal
      (loggable-action-individual-name type)
      (loggable-action-type type)
      (vector (make-property-msg "knowrob:startTime" :resource (loggable-timepoint start-time))
              (make-property-msg "knowrob:endTime" :resource (loggable-timepoint))
              (make-property-msg "knowrob:taskSuccess" :type (namespaced :xsd "boolean")
                                                       :value "true")
              (make-property-msg "knowrob:performedBy" :resource (loggable-robot-instance agent))
              (make-property-msg "knowrob:goalLocation" :resource goal-location-name))))
    (log-owl (or (car (remove :go (desig:desig-prop-values designator :to)))
                 (desig:desig-prop-value designator :destination))
             :name-with-id goal-location-name)))

(defmethod log-owl-action ((type (eql :going)) designator &key start-time agent)
  (log-owl-action :go designator :start-time start-time :agent agent))
