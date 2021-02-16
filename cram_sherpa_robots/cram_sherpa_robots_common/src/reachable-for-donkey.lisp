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

(defun calculate-reachable-for-pose (reference-pose agent-name)
  (let* ((offset (if (string-equal agent-name "red_wasp")
                     (cl-transforms:make-3d-vector 1.5 -0.5 0)
                     (if (string-equal agent-name "blue_wasp")
                         (cl-transforms:make-3d-vector 1.5 0.5 0)
                         (error "reachable-for poses can only be for wasps"))))
         (reference-transform (cl-tf:pose->transform reference-pose)))
    (cl-transforms-stamped:transform-pose
     reference-transform
     (cl-transforms:make-pose offset (cl-transforms:make-identity-rotation)))))

(def-fact-group location-designator-generators (desig-solution)
  (<- (desig-solution ?desig ?solution)
    (loc-desig? ?desig)
    (desig-prop ?desig (:reachable-for ?reference-name))
    (desig-prop ?desig (:viewpoint ?agent))
    (lisp-fun get-object-pose ?reference-name ?reference-pose)
    (lisp-fun calculate-reachable-for-pose ?reference-pose ?agent ?solution)))
