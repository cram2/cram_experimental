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

(in-package :hawk)

(def-fact-group hawk-motions (desig:motion-desig)

  (<- (desig:motion-desig ?motion-designator (switch-camera ?on-or-off))
    (or (desig:desig-prop ?motion-designator (:type :switching))
        (desig:desig-prop ?motion-designator (:to :switch)))
    (desig:desig-prop ?motion-designator (:device :camera))
    (or (and (desig:desig-prop ?motion-designator (:state :on))
             (equal ?on-or-off T))
        (and (desig:desig-prop ?motion-designator (:state :off))
             (equal ?on-or-off NIL))))

  (<- (desig:motion-desig ?motion-designator (take-picture NIL))
    (or (desig:desig-prop ?motion-designator (:type :taking-picture))
        (desig:desig-prop ?motion-designator (:to :take-picture)))))

(def-fact-group hawk-actions (desig:action-desig)

  (<- (desig:action-desig ?action-designator (take-picture))
    (or (desig:desig-prop ?action-designator (:type :taking-picture))
        (desig:desig-prop ?action-designator (:to :take-picture))))

  (<- (desig:action-desig ?action-designator (sherpa-search ?object ?area))
    (or (desig:desig-prop ?action-designator (:type :searching))
        (desig:desig-prop ?action-designator (:to :search)))
    (desig:desig-prop ?action-designator (:area ?area))
    (desig:desig-prop ?action-designator (:object ?object))))
