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

(defgeneric main ()
  (:method :before ()
    (let ((agent-ros-name (rosify (current-robot-symbol))))
      (roslisp-utilities:startup-ros :name agent-ros-name))))

;;;;;;;;;; referencing

(roslisp:def-service-callback sherpa_msgs-srv:ReferenceDesignator (designator)
  (roslisp:ros-info (robots-common reference-server) "Referencing designator ~a" designator)
  (roslisp:make-response :success (try-reference-designator-json designator)))

(defun run-reference-server (agent-namespace)
  (roslisp:register-service
   (concatenate 'string
                agent-namespace
                "/reference_designator")
   'sherpa_msgs-srv:ReferenceDesignator)
  (roslisp:ros-info (robots-common reference-server) "Ready to reference designators."))

;;;;;;;;;;;; performing

(defvar *performing-threads* nil)

(defun stop-all-actions ()
  (roslisp:ros-info (common performing) "stopping all ~a threads" (length *performing-threads*))
  (maphash #'(lambda (tree-name task-tree)
               (declare (ignore tree-name))
               (when (and (cpl:task-tree-node-p task-tree)
                          (cpl:task-tree-node-children task-tree))
                (let* ((code (cpl:task-tree-node-effective-code
                              (cdr (car (cpl:task-tree-node-children task-tree))))))
                  (assert code () "Task tree node doesn't contain a CODE.")
                  (assert (cpl:code-task code) () "Task tree code empty.")
                  (format t "EVAPORATING CHILDREN~%")
                  (cpl:evaporate (cpl:code-task code)))))
           cpl-impl::*top-level-task-trees*)
  (mapc (lambda (thread)
          (when (sb-thread:thread-alive-p thread)
            (sb-thread:terminate-thread thread)))
        *performing-threads*)
  (setf *performing-threads* nil))

(def-fact-group robot-agnostic-desigs (desig:action-desig)
  (<- (action-desig ?action-designator (stop-all-actions))
    (or (desig-prop ?action-designator (:type :stoppping))
        (desig-prop ?action-designator (:to :stop)))))

(roslisp:def-service-callback sherpa_msgs-srv:PerformDesignator (designator)
  (roslisp:ros-info (robots-common perform-server) "Referencing designator ~a" designator)
  (multiple-value-bind (success parsed-designator grounding)
      (try-reference-designator-json designator)
    (declare (ignore grounding))
    (if success
        (if (or (member :stop (desig:desig-prop-values parsed-designator :to))
                (eq :stopping (desig:desig-prop-value parsed-designator :type)))
            (progn
             (stop-all-actions)
             (roslisp:make-response :result (format nil "Stopping all actions.")))
            (progn
              ;; (when (some #'(lambda (process-module-name)
              ;;                 (cpm::check-process-module-running process-module-name :throw-error nil))
              ;;             (cpm:get-process-module-names))
              ;;   (roslisp:ros-info (common perform-server) "Agent already has goals. Cancelling.")
              ;;   (stop-all-actions)
              ;;   (cpl:sleep 1) ; give one second to kill all threads
              ;;   )
              (let ((worker-thread nil)
                    (result nil))
                (setf worker-thread
                      (sb-thread:make-thread
                       (lambda ()
                         (handler-case
                             (progn
                               (setf result (cpl-impl::named-top-level (:name red-wasp-tasks)
                                              (perform parsed-designator))))
                           (condition (error-object)
                             (format t "CONDITION: ~a~%" error-object)
                             (setf result error-object)))
                         (format t "finished with result : ~a~%~%" result))))
                (push worker-thread *performing-threads*)
                (roslisp:make-response :result
                                       (format nil "Performing designator ~a."
                                               parsed-designator)))))
        (roslisp:make-response :result
                               (format nil
                                       "Designator ~a could not be referenced."
                                       parsed-designator)))))

(defun run-perform-server (agent-namespace)
  (roslisp:register-service
   (concatenate 'string
                agent-namespace
                "/perform_designator")
   'sherpa_msgs-srv:PerformDesignator)
  (roslisp:ros-info (robots-common perform-server) "Ready to perform designators."))
