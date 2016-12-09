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

(def-fact-group helicopter-motions (desig:motion-desig)

  (<- (desig:motion-desig ?motion-designator (fly ?pose))
    (or (and (desig:desig-prop ?motion-designator (:type :flying))
             (desig:desig-prop ?motion-designator (:destination ?location)))
        (and (desig:desig-prop ?motion-designator (:to :fly))
             (desig:desig-prop ?motion-designator (:to ?location))
             (not (equal ?location :fly))))
    (cram-sherpa-robots-common:location-pose ?location ?pose))

  (<- (desig:motion-desig ?motion-designator (switch-engine ?on-or-off))
    (or (desig:desig-prop ?motion-designator (:type :switching-engine))
        (desig:desig-prop ?motion-designator (:to :switch-engine)))
    (or (and (desig:desig-prop ?motion-designator (:state :on))
             (equal ?on-or-off T))
        (and (desig:desig-prop ?motion-designator (:state :off))
             (equal ?on-or-off NIL))))

  (<- (desig:motion-desig ?motion-designator (set-altitude ?altitude))
    (or (and (desig:desig-prop ?motion-designator (:type :setting-altitude))
             (desig:desig-prop ?motion-designator (:value ?altitude)))
        (and (desig:desig-prop ?motion-designator (:to :set-altitude))
             (desig:desig-prop ?motion-designator (:to ?altitude))
             (not (equal ?altitude :set-altitude)))))

  (<- (desig:motion-desig ?motion-designator (switch-beacon ?on-or-off))
    (or (desig:desig-prop ?motion-designator (:type :switching-beacon))
        (desig:desig-prop ?motion-designator (:to :switch-beacon)))
    (or (and (desig:desig-prop ?motion-designator (:state :on))
             (equal ?on-or-off T))
        (and (desig:desig-prop ?motion-designator (:state :off))
             (equal ?on-or-off NIL)))))



(def-fact-group helicopter-actions (desig:action-desig)

  ;;;;;;;;;;;;;;;;;;;;;; land ;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; (<- (cpm:matching-process-module ?motion-designator donkey-nav)
  ;;   (or (desig-prop ?motion-designator (:type :landing))
  ;;       (desig-prop ?motion-designator (:to :land))))

  ;; (<- (motion-desig ?motion-designator (drive ?pose))
  ;;   (or
  ;;    (and (desig-prop ?motion-designator (:type :driving))
  ;;         (desig-prop ?motion-designator (:destination ?location)))
  ;;    (and (desig-prop ?motion-designator (:to :drive))
  ;;         (desig-prop ?motion-designator (:to ?location))
  ;;         (not (equal ?location :drive))))
  ;;   (cram-sherpa-robots-common:location-pose ?location ?pose))


  ;;;;;;;;;;;;;;;;;;;;;; take-off ;;;;;;;;;;;;;;;;;;;;;;

  ;; (<- (cpm:matching-process-module ?motion-designator donkey-manip)
  ;;   (or (desig-prop ?motion-designator (:type :taking-off))
  ;;       (desig-prop ?motion-designator (:to :take-off))))

  ;; (<- (motion-desig ?motion-designator (take-off))
  ;;   (or (desig-prop ?motion-designator (:type :taking-off))
  ;;       (desig-prop ?motion-designator (:to :take-off))))


  ;;;;;;;;;;;;;;;;;;;;;; sense ;;;;;;;;;;;;;;;;;;;;;;

  ;; (<- (cpm:matching-process-module ?motion-designator donkey-manip)
  ;;   (or (desig-prop ?motion-designator (:type :sensing))
  ;;       (desig-prop ?motion-designator (:to :sense))))

  ;; (<- (motion-desig ?motion-designator (mount ?robot-name))
  ;;   (or (desig-prop ?motion-designator (:type :mounting))
  ;;       (desig-prop ?motion-designator (:to :mount)))

  )
