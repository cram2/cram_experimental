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

(in-package :helicopter)

(def-fact-group helicopter-motions (motion-desig)

  (<- (motion-desig ?motion-designator (fly ?pose))
    (or (desig-prop ?motion-designator (:type :flying))
        (desig-prop ?motion-designator (:to :fly)))
    (or (and (desig-prop ?motion-designator (:to ?location))
             (not (equal ?location :fly)))
        (desig-prop ?motion-designator (:destination ?location)))
    (location-pose ?location ?pose))

  (<- (motion-desig ?motion-designator (switch-engine ?on-or-off))
    (or (desig-prop ?motion-designator (:type :switching))
        (desig-prop ?motion-designator (:to :switch)))
    (desig-prop ?motion-designator (:device :engine))
    (or (and (desig-prop ?motion-designator (:state :on))
             (equal ?on-or-off T))
        (and (desig-prop ?motion-designator (:state :off))
             (equal ?on-or-off NIL))))

  (<- (motion-desig ?motion-designator (set-altitude ?altitude))
    (or (desig-prop ?motion-designator (:type :setting-altitude))
        (desig-prop ?motion-designator (:to :set-altitude)))
    (or (desig-prop ?motion-designator (:value ?altitude))
        (and (desig-prop ?motion-designator (:to ?altitude))
             (not (equal ?altitude :set-altitude))))))



(def-fact-group helicopter-actions (action-desig)

  (<- (action-desig ?action-designator (land ?pose))
    ;; DESTINATION/AT are optional
    (or (desig-prop ?action-designator (:type :landing))
        (desig-prop ?action-designator (:to :land)))
    (-> (or (desig-prop ?action-designator (:destination ?location))
            (desig-prop ?action-designator (:at ?location)))
        (location-pose ?location ?pose)
        (equal ?pose NIL)))

  (<- (action-desig ?action-designator (take-off ?altitude))
    (or (desig-prop ?action-designator (:type :taking-off))
        (desig-prop ?action-designator (:to :take-off)))
    (or (desig-prop ?action-designator (:altitude ?altitude))
        (and (desig-prop ?action-designator (:to ?altitude))
             (not (equal ?altitude :take-off)))))

  (<- (action-desig ?action-designator (scan ?area))
    (or (desig-prop ?action-designator (:type :scanning))
        (desig-prop ?action-designator (:to :scan)))
    (desig-prop ?action-designator (:area ?area-name-string))
    (lisp-fun robots-common:owl-object-bounding-box ?area-name-string ?area))

  (<- (action-desig ?action-designator (continuously-perceive ?object-name))
    (or (desig-prop ?action-designator (:type :looking-for))
        (desig-prop ?action-designator (:to :look-for)))
    (desig-prop ?action-designator (:object ?object-name)))

  (<- (action-desig ?action-designator (navigate ?pose))
    (or (desig-prop ?action-designator (:type :going))
        (desig-prop ?action-designator (:to :go)))
    (or (desig-prop ?action-designator (:destination ?location))
        (and (desig-prop ?action-designator (:to ?location))
             (not (equal ?location :go))))
    (location-pose ?location ?pose)))
