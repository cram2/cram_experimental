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

;; (defvar *found-objects* (make-hash-table)
;;   "Key is :victim or :kite, value is pose-stamped in map")

;; (defun reset-found-objects ()
;;   (setf *found-objects* (make-hash-table)))

;; (defun reset-beliefstate ()
;;   (reset-found-objects))

;; (roslisp-utilities:register-ros-init-function reset-beliefstate)


#+aasdfasdf
(
 (json-prolog:prolog-simple "entity_assert(POSE, [a, pose, [0.0,2.0,0.0], [1.0,0.0,0.1,0.2]])")
 (json-prolog:prolog-simple
   (format nil "current_object_pose('~a', POSE)" "http://knowrob.org/kb/unreal_log.owl#SherpaVictim_VAZg"))
 (json-prolog:prolog-simple
  "entity_assert(Pose, [a, pose, [0.0,2.0,0.0], [1.0,0.0,0.1,0.2]]),
entity_assert(Obj, [an, object, [type, dough]]),
rdf_assert(Obj, knowrob:pose,Pose),
current_object_pose(Obj, Obj_pose).")
)


(defun pose->flat-list (pose)
  (let* ((xyz (cl-transforms:origin pose))
         (qqqw (cl-transforms:orientation pose))
         (x (cl-transforms:x xyz))
         (y (cl-transforms:y xyz))
         (z (cl-transforms:z xyz))
         (q1 (cl-transforms:x qqqw))
         (q2 (cl-transforms:y qqqw))
         (q3 (cl-transforms:z qqqw))
         (w (cl-transforms:w qqqw)))
    (list x y z q1 q2 q3 w)))

(defun pose->flat-list-w-first (pose)
  (let* ((xyz (cl-transforms:origin pose))
         (qqqw (cl-transforms:orientation pose))
         (x (cl-transforms:x xyz))
         (y (cl-transforms:y xyz))
         (z (cl-transforms:z xyz))
         (q1 (cl-transforms:x qqqw))
         (q2 (cl-transforms:y qqqw))
         (q3 (cl-transforms:z qqqw))
         (w (cl-transforms:w qqqw)))
    (list x y z w q1 q2 q3)))

(defun flat-list->pose (pose-list)
  (destructuring-bind (x y z q1 q2 q3 w)
      pose-list
    (cl-transforms:make-pose
     (cl-transforms:make-3d-vector x y z)
     (cl-transforms:make-quaternion q1 q2 q3 w))))

(defun flat-list-w-first->pose (pose-list)
  (destructuring-bind (x y z w q1 q2 q3)
      pose-list
    (cl-transforms:make-pose
     (cl-transforms:make-3d-vector x y z)
     (cl-transforms:make-quaternion q1 q2 q3 w))))

(defun set-found-object-pose (object-name object-pose)
  (let* ((pose-prolog-string
           (apply #'format nil
                  "[a, pose, [~,2f,~,2f,~,2f],[~,2f,~,2f,~,2f,~,2f]]"
                  (cram-sherpa-robots-common::pose->flat-list-w-first object-pose)))
         (object-individual-string
           (owl-individual-of-class
            (namespaced :knowrob (cram-owl-name object-name))))
         (query-string
           (format nil
                   "entity_assert(POSE, ~a), rdf_assert('~a', knowrob:pose, POSE)."
                   pose-prolog-string object-individual-string)))
    (json-prolog:prolog-simple query-string))
  ;; (setf (gethash object-name *found-objects*) object-pose)
  )

(defun get-found-object-pose (object-name)
  (let* ((object-individual-string
           (owl-individual-of-class
            (namespaced :knowrob (cram-owl-name object-name))))
         (query-result-bindings
           (json-prolog:prolog `("current_object_pose" ,object-individual-string ?pose)
                               :package :robots-common))
         (pose-list (cut:var-value '?pose (cut:lazy-car query-result-bindings))))
    (unless (cut:is-var pose-list)
      (cl-transforms-stamped:pose->pose-stamped
       cram-tf:*fixed-frame*
       (roslisp:ros-time)
       (flat-list-w-first->pose pose-list))))
  ;; (gethash object-name *found-objects*)
  )


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
