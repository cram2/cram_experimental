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

(defun main ()
  (let ((agent-ros-name (rosify_ (current-robot-symbol))))
    (let (perform-server-status-thread perform-server-callback-thread)
      (unwind-protect
           (progn
             (roslisp-utilities:startup-ros :name agent-ros-name)
             (run-reference-server agent-ros-name)
             (multiple-value-setq
                 (perform-server-status-thread perform-server-callback-thread)
               (run-perform-server agent-ros-name))
             (roslisp:spin-until nil 100))
        (when (sb-thread:thread-alive-p perform-server-status-thread)
          (sb-thread:terminate-thread perform-server-status-thread))
        (when (sb-thread:thread-alive-p perform-server-callback-thread)
          (sb-thread:terminate-thread perform-server-callback-thread))
        (roslisp-utilities:shutdown-ros)))))

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

(defgeneric perform-with-pms-running (designator)
  (:documentation "Each robot should define this function with its corresponding PMs.")
  (:method (designator)
    (cpl:fail "PERFORM-WITH-PMS-RUNNING is not defined for this robot. Please do!")))

(actionlib:def-exec-callback perform-server-callback (designator)
  (roslisp:ros-info (robots-common perform-server) "Performing action designator ~a" designator)
  (multiple-value-bind (success parsed-designator grounding)
      (try-reference-designator-json designator)
    (declare (ignore grounding))
    (unless success
      (actionlib:abort-current :result
                               (format nil
                                       "Designator ~a could not be referenced."
                                       designator)))
    (let ((worker-thread nil)
          (result nil))
      (unwind-protect
           (progn
             (setf worker-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (handler-case
                          (setf result (perform-with-pms-running parsed-designator))
                        ;; (cpl:plan-failure (error-object)
                        ;;   (roslisp:ros-error (robots-common reference-server)
                        ;;                      "Could not perform designator ~a: ~a"
                        ;;                      parsed-designator error-object)
                        ;;   (setf result error-object))
                        (condition (error-object)
                          (setf result error-object))))))
             (roslisp:loop-at-most-every 0.1
               (when (actionlib:cancel-request-received)
                 (actionlib:abort-current))
               (unless (sb-thread:thread-alive-p worker-thread)
                 (typecase result
                   (condition
                    (actionlib:abort-current :result (format nil "Could not perform designator ~a: ~a"
                                                             parsed-designator result)))
                   (t (when (eq (roslisp:node-status) :RUNNING) ; in case one pressed Ctrl-C-C
                        (actionlib:succeed-current :result (format nil "~a" result))))))))
        (when (and worker-thread (sb-thread:thread-alive-p worker-thread))
          (maphash #'(lambda (tree-name task-tree)
                       (declare (ignore tree-name))
                       (let* ((code (cpl:task-tree-node-effective-code
                                     (cdr (car (cpl:task-tree-node-children task-tree))))))
                         (assert code () "Task tree node doesn't contain a CODE.")
                         (assert (cpl:code-task code) () "Task tree code empty.")
                         (cpl:evaporate (cpl:code-task code))))
                   cpl-impl::*top-level-task-trees*)
          (sb-thread:terminate-thread worker-thread))))))

(defun run-perform-server (agent-namespace)
  (multiple-value-bind
        (perform-server-status-thread perform-server-callback-thread)
      (actionlib:start-action-server
       (concatenate 'string
                    agent-namespace
                    "/perform_designator")
       "sherpa_msgs/PerformDesignatorAction"
       #'perform-server-callback
       :separate-thread t)
    (roslisp:ros-info (robots-common perform-server) "Ready to perform designators.")
    (values perform-server-status-thread perform-server-callback-thread)))
