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

(defvar *found-objects* (make-hash-table)
  "Key is :victim or :kite, value is pose-stamped in map")

(defun reset-found-objects ()
  (setf *found-objects* (make-hash-table)))

(defun reset-beliefstate ()
  (reset-found-objects))

(roslisp-utilities:register-ros-init-function reset-beliefstate)

(defun get-found-object-pose (object-name)
  (gethash object-name *found-objects*))

(defun set-found-object-pose (object-name object-pose)
  (setf (gethash object-name *found-objects*) object-pose))

(def-fact-group sherpa-occasions (object-found)

  (<- (object-found ?object)
    (bound ?object)
    (lisp-fun get-found-object-pose ?object ?pose)
    (lisp-pred identity ?pose))

  ;; (<- (object-found ?object)
  ;;   (not (bound ?object))
  ;;   todo
  )

(defclass object-found (cram-occasions-events:event)
  ((object :initarg :object
           :reader event-object
           :initform (error 'simple-error
                            :format-control "OBJECT-FOUND event requires OBJECT."))
   (pose :initarg :pose
         :reader event-pose
         :initform (error 'simple-error
                          :format-control "OBJECT-FOUND event requires POSE.")))
  (:documentation "Event that is generated whenever an object is detected by whatever sensor."))

(defmethod cram-occasions-events:on-event object-found ((event object-found))
  (roslisp:ros-info (common beliefstate) "Found an object ~a." (event-object event))
  (set-found-object-pose (event-object event) (event-pose event)))

;; (cram-occasions-events:on-event
;;            (make-instance 'object-gripped
;;              :object ?carried-object
;;              :arm :left-or-right
;;              :grasp :top-of-side-or-front))

;; (prolog:prolog `(object-in-hand ?object :left))
