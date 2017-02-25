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

(defparameter *logging-enabled* t)

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
    (:computable "http://knowrob.org/kb/computable.owl#")
    (:srdl2-action "http://knowrob.org/kb/srdl2-action.owl#")))

(defparameter *cram-owl-names*
  '((:red-wasp "SherpaWaspRed" :knowrob :log)
    (:blue-wasp "SherpaWaspBlue" :knowrob :log)
    (:hawk "SherpaHawk" :knowrob :log)
    (:donkey "SherpaDonkey" :knowrob :log)

    (:engine "Engine" :knowrob :log)
    (:beacon "Beacon" :knowrob :log)

    (:victim "SherpaVictim" :knowrob :log)
    (:kite "SherpaHangGlider" :knowrob :log)

    (:go "Movement-TranslationEvent" :knowrob :log)
    (:going "Movement-TranslationEvent" :knowrob :log)
    (:fly "Flying" :knowrob :log)
    (:take-off "TakeOff-Flight" :knowrob :log)
    (:set-altitude "ChangingAircraftAltitude" :knowrob :log)
    (:land "Landing-Aircraft" :knowrob :log)
    (:scan "ScanningArea" :knowrob :log)
    (:switch "TogglingDeviceState" :knowrob :log)
    (:drive "Driving" :knowrob :log)
    (:mount "InstallingHardwareComponent" :srdl2-action :log)
    (:unmount "SeparationIntoConstituentParts" :knowrob :log)
    (:look-for "LookingForSomething" :knowrob :log)
    (:take-picture "Perceiving-Voluntary" :knowrob :log))
  "An association list of CRAM name to OWL name mappings")

(defparameter *logging-namespace-default* "knowrob")

(defparameter *link-to-image-file* "")

(defgeneric log-owl (object &key &allow-other-keys)
  (:documentation "call logging action on `object'")
  (:method ((object null) &key &allow-other-keys)
    (roslisp:ros-info (common owl) "Got a NIL object to log. Ignoring.")))

(defparameter *logging-action-name* "ue_semlog/LogEvent")
(defparameter *logging-action-type* "sherpa_msgs/LogEventAction")

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
  (declare (type sherpa_msgs-msg:LogEventGoal action-goal)
           (type (or null number) action-timeout))
  (roslisp:ros-info (robots-common logging-client) "Calling CALL-LOGGING-ACTION")
  (when *logging-enabled*
    (unless action-timeout (setf action-timeout *logging-action-timeout*))
    (nth-value
     1
     (cpl:with-failure-handling
         ((simple-error (e)
            (format t "Actionlib error occured!~%~a~%Reinitializing...~%~%" e)
            (init-logging-action-client)
            (cpl:retry)))
       (let ((actionlib:*action-server-timeout* 10.0))
         (actionlib:call-goal
          (get-logging-action-client) action-goal :timeout action-timeout))))))

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

(defun loggable-location-type ()
  (namespaced :knowrob "Location"))

(defun loggable-pose-name ()
  (namespaced :u-map (unique-id-ed "Pose")))

(defun loggable-pose-type ()
  (namespaced :knowrob "Pose"))

(defun loggable-image-name ()
  (namespaced :log (unique-id-ed "CameraImage")))

(defun loggable-image-type ()
  (namespaced :knowrob "CameraImage"))

(defun cram-owl-name (cram-name &optional namespace-id)
  (declare (type keyword cram-name)
           (type (or null keyword) namespace-id))
  (let ((name-entry (cdr (assoc cram-name *cram-owl-names*))))
    (if namespace-id
        (namespaced (nth (case namespace-id (:class 1) (:individual 2)) name-entry)
                    (first name-entry))
        (first name-entry))))

(defun owl-individual-of-class (class)
  (let ((name (cut:var-value
               '?name
               (car
                (json-prolog:prolog
                 `("owl_individual_of" ?name ,class)
                 :package :robots-common)))))
    (if (cut:is-var name)
        NIL
        (string-trim "'" (symbol-name name)))))

(defun owl-object-bounding-box (owl-name)
  "Returns list of (<3dvector> <pose>)."
  (let ((owl-namespaced-name (namespaced :log owl-name)))
    (cut:with-vars-bound (?pose ?dim)
        (cut:lazy-car
         (json-prolog:prolog
          `(and ("current_object_pose" ,owl-namespaced-name ?pose)
                ("object_dimensions" ,owl-namespaced-name ?d ?w ?h)
                (= '(?d ?w ?h) ?dim))
          :package :robots-common))
      (if (or (cut:is-var ?pose) (cut:is-var ?dim))
          NIL
          (list (apply #'cl-transforms:make-3d-vector ?dim)
                (destructuring-bind (x y z q1 q2 q3 w)
                    ?pose
                  (cl-transforms:make-pose
                   (cl-transforms:make-3d-vector x y z)
                   (cl-transforms:make-quaternion q1 q2 q3 w))))))))

(defun loggable-robot-type (cram-robot-type)
  (declare (type symbol cram-robot-type))
  ;; todo: json query to find out the instance of robot
  (cram-owl-name (intern (symbol-name cram-robot-type) :keyword) :class))

(defun loggable-robot-individual-name (&optional cram-robot-type)
  (declare (type (or null symbol) cram-robot-type))
  (owl-individual-of-class
   (loggable-robot-type (or cram-robot-type (current-robot-symbol)))))

(defun loggable-action-individual-name (cram-action-type)
  (unique-id-ed (cram-owl-name cram-action-type :individual)))

(defun loggable-action-type (cram-action-type)
  (cram-owl-name cram-action-type :class))

(defun loggable-action (cram-action-type start-time end-time task-success agent
                        &rest loggable-properties)
  (make-logging-goal
   (loggable-action-individual-name cram-action-type)
   (loggable-action-type cram-action-type)
   (apply #'vector
          (loggable-property :|startTime| :time start-time)
          (loggable-property :|endTime| :time end-time)
          (loggable-property :|taskSuccess| :true-or-false task-success)
          (loggable-property :|performedBy| :agent agent)
          loggable-properties)))


(defgeneric log-owl-action (action-type action-designator &key start-time agent)
  (:documentation "Calls the logging action. `action-type' is :TO or :TYPE property.")
  (:method (action-type action-designator &key start-time agent)
    (declare (ignore action-designator start-time agent))
    (roslisp:ros-warn (common cram-owl)
                      "Action-type ~a not supported. Skipping" action-type)
    NIL))

(defmethod log-owl :before (object &key &allow-other-keys)
  (roslisp:ros-info (common owl) "Logging object: ~a~%" object))

(defmethod log-owl ((designator action-designator) &key start-time agent)
  (log-owl-action
   (or (car (remove-if-not #'keywordp (desig:desig-prop-values designator :to)))
       (desig:desig-prop-value designator :type))
   designator
   :start-time start-time
   :agent agent))

(defmethod log-owl ((designator motion-designator) &key start-time agent)
  (log-owl-action
   (or (car (remove-if-not #'keywordp (desig:desig-prop-values designator :to)))
       (desig:desig-prop-value designator :type))
   designator
   :start-time start-time
   :agent agent))


(defun loggable-property-with-resource (name resource &optional name-namespace)
  (declare (type keyword name)
           (type (or null string) resource name-namespace))
  (make-property-msg
   (concatenate 'string
                (or name-namespace *logging-namespace-default*)
                ":"
                (symbol-name name))
   :resource (or resource "")))

(defun loggable-property-with-value (name value-type value &optional name-namespace)
  (declare (type keyword name)
           (type (or null string) value-type value name-namespace))
  (make-property-msg
   (concatenate 'string
                (or name-namespace *logging-namespace-default*)
                ":"
                (symbol-name name))
   :type (or value-type "")
   :value (or value "")))

(defgeneric loggable-property (name &key &allow-other-keys))

(defmethod loggable-property ((name (eql :|startTime|)) &key time)
  (declare (type (or null double-float) time))
  (loggable-property-with-resource name (loggable-timepoint time)))

(defmethod loggable-property ((name (eql :|endTime|)) &key time)
  (declare (type (or null double-float) time))
  (loggable-property-with-resource name (loggable-timepoint time)))

(defmethod loggable-property ((name (eql :|taskSuccess|)) &key true-or-false)
  (declare (type boolean true-or-false))
  (loggable-property-with-value name
                                (namespaced :xsd "boolean")
                                (if true-or-false
                                    "true"
                                    "false")))

(defmethod loggable-property ((name (eql :|performedBy|)) &key agent)
  (declare (type (or null symbol) agent))
  (loggable-property-with-resource name (loggable-robot-individual-name agent)))

(defmethod loggable-property ((name (eql :|translation|)) &key pose)
  (declare (type cl-transforms:pose pose))
  (loggable-property-with-value name
                                (namespaced :xsd "string")
                                (with-slots ((x cl-transforms:x)
                                             (y cl-transforms:y)
                                             (z cl-transforms:z))
                                    (cl-transforms:origin pose)
                                  (format nil "~a ~a ~a" x y z))))

(defmethod loggable-property ((name (eql :|quaternion|)) &key pose)
  (declare (type cl-transforms:pose pose))
  (loggable-property-with-value name
                                (namespaced :xsd "string")
                                (with-slots ((x cl-transforms:x)
                                             (y cl-transforms:y)
                                             (z cl-transforms:z)
                                             (w cl-transforms:w))
                                    (cl-transforms:orientation pose)
                                  (format nil "~a ~a ~a ~a" w x y z))))


(defmethod log-owl ((pose cl-transforms:pose) &key name-with-id)
  (call-logging-action
   (make-logging-goal
    name-with-id
    (loggable-pose-type)
    (vector (loggable-property :|translation| :pose pose)
            (loggable-property :|quaternion| :pose pose)))))

(defmethod log-owl ((designator location-designator) &key name-with-id)
  (let ((pose-name (loggable-pose-name)))
    (call-logging-action
     (make-logging-goal
      name-with-id
      (loggable-location-type)
      (vector (loggable-property-with-resource :|pose| pose-name))))
    (log-owl (desig:desig-prop-value designator :pose)
             :name-with-id pose-name)))

(defmethod log-owl-action ((type (eql :go)) designator &key start-time agent)
  (let ((goal-location-name (loggable-location-name)))
    (call-logging-action
     (loggable-action
      type start-time NIL T agent
      (loggable-property-with-resource :|goalLocation| goal-location-name)))
    (log-owl (or (car (remove :go (desig:desig-prop-values designator :to)))
                 (desig:desig-prop-value designator :destination))
             :name-with-id goal-location-name)))

(defmethod log-owl-action ((type (eql :going)) designator &key start-time agent)
  (log-owl-action :go designator :start-time start-time :agent agent))

(defmethod log-owl-action ((type (eql :fly)) designator &key start-time agent)
  (let ((goal-location-name (loggable-location-name)))
    (call-logging-action
     (loggable-action
      type start-time NIL T agent
      (loggable-property-with-resource :|goalLocation| goal-location-name)))
    (log-owl (or (car (remove :fly (desig:desig-prop-values designator :to)))
                 (desig:desig-prop-value designator :destination))
             :name-with-id goal-location-name)))

(defmethod log-owl-action ((type (eql :flying)) designator &key start-time agent)
  (log-owl-action :fly designator :start-time start-time :agent agent))

(defmethod log-owl-action ((type (eql :drive)) designator &key start-time agent)
  (let ((goal-location-name (loggable-location-name)))
    (call-logging-action
     (loggable-action
      type start-time NIL T agent
      (loggable-property-with-resource :|goalLocation| goal-location-name)))
    (log-owl (or (car (remove :drive (desig:desig-prop-values designator :to)))
                 (desig:desig-prop-value designator :destination))
             :name-with-id goal-location-name)))

(defmethod log-owl-action ((type (eql :driving)) designator &key start-time agent)
  (log-owl-action :drive designator :start-time start-time :agent agent))

(defmethod log-owl-action ((type (eql :switch)) designator &key start-time agent)
  (let* ((device-cram (desig:desig-prop-value designator :device))
         (device-type-owl (cram-owl-name device-cram :class))
         (device-individual (owl-individual-of-class device-type-owl))
         (state-cram (desig:desig-prop-value designator :state)))
    (call-logging-action
     (loggable-action
      type start-time NIL T agent
      (loggable-property-with-resource :|objectActedOn| device-individual)
      (loggable-property-with-resource :|deviceState|
                                       (if (eql state-cram :on)
                                           (namespaced :knowrob "DeviceStateOn")
                                           (namespaced :knowrob "DeviceStateOff")))))))

(defmethod log-owl-action ((type (eql :switching)) designator &key start-time agent)
  (log-owl-action :switch designator :start-time start-time :agent agent))

(defmethod log-owl-action ((type (eql :land)) designator &key start-time agent)
  (let ((goal-location-name (loggable-location-name)))
    (call-logging-action
     (loggable-action
      type start-time NIL T agent
      (loggable-property-with-resource :|goalLocation| goal-location-name)))
    (log-owl (or (desig:desig-prop-value designator :at)
                 (desig:desig-prop-value designator :destination))
             :name-with-id goal-location-name)))

(defmethod log-owl-action ((type (eql :landing)) designator &key start-time agent)
  (log-owl-action :land designator :start-time start-time :agent agent))

(defmethod log-owl-action ((type (eql :take-off)) designator &key start-time agent)
  (let ((altitude (or (car (remove :take-off (desig:desig-prop-values designator :to)))
                      (desig:desig-prop-value designator :altitude))))
    (call-logging-action
     (loggable-action
      type start-time NIL T agent
      (loggable-property-with-value :|altitude| (namespaced :xsd "double")
                                    (write-to-string altitude))))))

(defmethod log-owl-action ((type (eql :taking-off)) designator &key start-time agent)
  (log-owl-action :take-off designator :start-time start-time :agent agent))

(defmethod log-owl-action ((type (eql :set-altitude)) designator &key start-time agent)
  (let ((altitude (or (car (remove :set-altitude (desig:desig-prop-values designator :to)))
                      (desig:desig-prop-value designator :value))))
    (call-logging-action
     (loggable-action
      type start-time NIL T agent
      (loggable-property-with-value :|altitude| (namespaced :xsd "double")
                                    (write-to-string altitude))))))

(defmethod log-owl-action ((type (eql :setting-altitude)) designator &key start-time agent)
  (log-owl-action :set-altitude designator :start-time start-time :agent agent))

(defmethod log-owl-action ((type (eql :mount)) designator &key start-time agent)
  (let* ((designator-agent (desig:desig-prop-value designator :agent))
         (cram-agent-to-mount (typecase designator-agent
                                (string (derosify designator-agent))
                                (keyword designator-agent)
                                (symbol (intern (symbol-name :key) :keyword))
                                (t nil))))
    (call-logging-action
     (loggable-action
      type start-time NIL T agent
      (loggable-property-with-resource :|objectActedOn|
                                       (loggable-robot-individual-name cram-agent-to-mount))))))

(defmethod log-owl-action ((type (eql :mounting)) designator &key start-time agent)
  (log-owl-action :mount designator :start-time start-time :agent agent))

(defmethod log-owl-action ((type (eql :unmount)) designator &key start-time agent)
  (let* ((designator-agent (desig:desig-prop-value designator :agent))
         (cram-agent-to-mount (typecase designator-agent
                                (string (derosify designator-agent))
                                (keyword designator-agent)
                                (symbol (intern (symbol-name :key) :keyword))
                                (t nil))))
    (call-logging-action
     (loggable-action
      type start-time NIL T agent
      (loggable-property-with-resource :|objectActedOn|
                                       (loggable-robot-individual-name cram-agent-to-mount))))))

(defmethod log-owl-action ((type (eql :unmounting)) designator &key start-time agent)
  (log-owl-action :unmount designator :start-time start-time :agent agent))

(defmethod log-owl-action ((type (eql :scan)) designator &key start-time agent)
  (let ((owl-area (desig:desig-prop-value designator :area)))
    (call-logging-action
     (loggable-action
      type start-time NIL T agent
      (loggable-property-with-resource :|objectActedOn| owl-area)))))

(defmethod log-owl-action ((type (eql :scanning)) designator &key start-time agent)
  (log-owl-action :scan designator :start-time start-time :agent agent))

(defmethod log-owl-action ((type (eql :look-for)) designator &key start-time agent)
  (let ((object (desig:desig-prop-value designator :object)))
    (call-logging-action
     (loggable-action
      type start-time NIL T agent
      (loggable-property-with-resource :|objectActedOn| (if (keywordp object)
                                                            (cram-owl-name object :class)
                                                            object))))))

(defmethod log-owl-action ((type (eql :looking-for)) designator &key start-time agent)
  (log-owl-action :mount designator :start-time start-time :agent agent))

(defmethod log-owl-action ((type (eql :take-picture)) designator &key start-time agent)
  (let ((image-name (loggable-image-name))
        (end-time (roslisp:ros-time)))
    (call-logging-action
     (loggable-action
      type start-time end-time T agent
      (loggable-property-with-resource :|perceptionResult| "")
      (loggable-property-with-resource :|capturedImage| image-name)))
    (call-logging-action
     (make-logging-goal
      image-name
      (loggable-image-type)
      (vector (loggable-property-with-resource :|captureTime| (loggable-timepoint end-time))
              (loggable-property-with-value :|rosTopic| (namespaced :xsd "string") "RoboSherlock/output_image")
              (loggable-property-with-value :|linkToImageFile| (namespaced :xsd "string") *link-to-image-file*))))))

(defmethod log-owl-action ((type (eql :taking-picture)) designator &key start-time agent)
  (log-owl-action :take-picture designator :start-time start-time :agent agent))
