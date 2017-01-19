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

(defun try-reference-designator-json (designator-json-string)
  (let (success designator grounding)
    (handler-case
        (progn (roslisp:ros-info
                (robots-common reference-server)
                "Designator resolved to: ~a"
                (setf grounding (desig:reference
                                 (setf designator (parse-action-json designator-json-string)))))
               (setf success T))

      (desig:designator-error (error-object)
        (roslisp:ros-error (robots-common reference-server)
                           "Could not resolve designator: ~a"
                           error-object))

      (json-parser-failed (error-object)
        (roslisp:ros-error (robots-common reference-server)
                           "Could not parse JSON designator: ~a"
                           error-object)))
    (values success designator grounding)))

(roslisp:def-service-callback sherpa_msgs-srv:ReferenceDesignator (designator)
  (roslisp:ros-info (robots-common reference-server) "Referencing designator ~a" designator)
  (roslisp:make-response :success (try-reference-designator-json designator)))

(defun run-reference-server (actor-namespace)
  (roslisp:with-ros-node ((concatenate 'string
                                       actor-namespace
                                       "/designator_reference_server")
                          :spin t)
    (roslisp:register-service
     (concatenate 'string
                  actor-namespace
                  "/reference_designator")
     'sherpa_msgs-srv:ReferenceDesignator)
    (roslisp:ros-info (robots-common reference-server) "Ready to reference designators.")))


(roslisp:def-service-callback sherpa_msgs-srv:PerformDesignator (designator)
  (roslisp:ros-info (robots-common perform-server) "Performing action designator ~a" designator)
  (multiple-value-bind (success parsed-designator grounding)
      (try-reference-designator-json designator)
    (declare (ignore grounding))
    (if success
        (roslisp:make-response :ack (handler-case
                                        (cram-sherpa-robots-common::perform parsed-designator)
                                      (cpl:plan-failure (error-object)
                                        (roslisp:ros-error (robots-common reference-server)
                                                           "Could not perform designator ~a: ~a"
                                                           parsed-designator error-object)
                                        NIL)))
        (roslisp:make-response :ack NIL))))

(defun run-perform-server (actor-namespace)
  (roslisp:with-ros-node ((concatenate 'string
                                       actor-namespace
                                       "/perform_designator_server")
                          :spin t)
    (roslisp:register-service
     (concatenate 'string
                  actor-namespace
                  "/perform_designator")
     'sherpa_msgs-srv:PerformDesignator)
    (roslisp:ros-info (robots-common perform-server) "Ready to reference designators.")))
