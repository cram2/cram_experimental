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

(roslisp:def-service-callback sherpa_msgs-srv:ReferenceDesignator (designator)
  (roslisp:ros-info (robots-common reference-server) "Referencing designator ~a" designator)
  (handler-case
      (prog1 (desig:reference (parse-action-json designator))
        (roslisp:make-response :success T))

    (desig:designator-error (error-object)
      (roslisp:ros-error (robots-common reference-server)
                         "Could not resolve designator: ~a"
                         error-object)
      (roslisp:make-response :success NIL))

    (json-parser-failed (error-object)
      (roslisp:ros-error (robots-common reference-server)
                         "Could not parse JSON designator: ~a"
                         error-object)
      (roslisp:make-response :success NIL))))


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
  (roslisp:make-response :ack T))

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
