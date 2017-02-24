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

(in-package :cl-user)

(defpackage cram-sherpa-robots-common
  (:nicknames #:robots-common)
  (:use #:common-lisp #:cram-designators #:cram-process-modules #:cram-prolog)
  (:export
   ;; action-designator-server
   #:main
   #:run-reference-server
   #:run-perform-server
   ;; action-json-parser
   #:*show-json-warnings*
   #:json-key-not-supported
   #:json-parser-failed
   ;; cram-owl
   #:*logging-enabled*
   #:log-owl
   #:cram-owl-name
   #:owl-individual-of-class
   #:owl-object-bounding-box
   ;; low-level-action-clients
   #:define-action-client
   #:make-mount-goal
   #:make-move-to-goal
   #:make-set-altitude-goal
   #:make-take-picture-goal
   #:make-toggle-actuator-goal
   #:make-toggle-victim-tracking-goal
   ;; plan-library
   #:perform
   ;; prolog
   #:location-pose
   #:terrain-name #:terrain
   ;; robosherlock-action
   #:robosherlock-pm
   ;; belief-state
   #:*found-objects*
   #:object-found
   ;; utils
   #:try-reference-designator-json
   #:current-robot-symbol
   #:current-robot-package
   #:current-robot-name
   #:rosify
   #:derosify
   #:make-symbol-type-message))
