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

(in-package :robots-common)

(defun get-object-pose (object-name)
  (let ((tf-frame (typecase object-name
                    (keyword (case object-name
                               (:victim "victim")
                               (:kite "hang_glider")
                               (t nil)))
                    (string (concatenate 'string object-name "/base_link"))
                    (t nil))))
    (when tf-frame
      (let ((object-transform (cl-transforms-stamped:lookup-transform
                               cram-tf:*transformer*
                               cram-tf:*fixed-frame*
                               tf-frame
                               :time 0.0
                               :timeout cram-tf:*tf-default-timeout*)))
        (cl-transforms-stamped:make-pose-stamped
         cram-tf:*fixed-frame*
         (cl-transforms-stamped:stamp object-transform)
         (cl-transforms-stamped:translation object-transform)
         (cl-transforms-stamped:rotation object-transform))))))

(def-fact-group location-designator-generators (desig-solution)
  (<- (desig-solution ?desig ?solution)
    (loc-desig? ?desig)
    (desig-prop ?desig (:of ?obj))
    (lisp-type ?obj string)
    (lisp-fun get-object-pose ?obj ?solution))

  (<- (desig-solution ?desig ?solution)
    (loc-desig? ?desig)
    (desig-prop ?desig (:of ?obj))
    (lisp-type ?obj keyword)
    (lisp-fun get-object-pose ?obj ?solution)))
