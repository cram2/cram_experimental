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

(def-fact-group helicopter-pms (cpm:matching-process-module
                                cpm:available-process-module)

  (<- (cpm:matching-process-module ?motion-designator helicopter-actuators)
    (or (desig:desig-prop ?motion-designator (:type :flying))
        (desig:desig-prop ?motion-designator (:to :fly))))

  (<- (cpm:matching-process-module ?motion-designator helicopter-actuators)
    (or (desig:desig-prop ?motion-designator (:type :switching))
        (desig:desig-prop ?motion-designator (:to :switch)))
    (desig-prop ?motion-designator (:device :engine)))

  (<- (cpm:matching-process-module ?motion-designator helicopter-actuators)
    (or (desig:desig-prop ?motion-designator (:type :setting-altitude))
        (desig:desig-prop ?motion-designator (:to :set-altitude))))

  (<- (cpm:available-process-module helicopter-actuators)
    (not (cpm:projection-running ?_))))

(cpm:def-process-module helicopter-actuators (motion-designator)
  (destructuring-bind (command argument) (reference motion-designator)
    (ecase command
      (fly
       (handler-case
           (funcall (symbol-function
                     (intern (symbol-name '#:call-fly-action) (current-robot-package)))
                    :action-goal (make-move-to-goal argument))
         ;; (cram-plan-failures:look-at-failed ()
         ;;   (cpl:fail 'cram-plan-failures:look-at-failed :motion motion-designator))
         ))
      (switch-engine
       (handler-case
           (funcall (symbol-function
                     (intern (symbol-name '#:call-toggle-engine-action) (current-robot-package)))
                    :action-goal (make-toggle-actuator-goal argument))))
      (set-altitude
       (handler-case
           (funcall (symbol-function
                     (intern (symbol-name '#:call-set-altitude-action) (current-robot-package)))
                    :action-goal (make-set-altitude-goal argument)))))))

;; Examples:
;;
;; (cpm:with-process-modules-running
;;     (helicopter::helicopter-actuators)
;;            (cpl:top-level
;;              (cram-sherpa-robots-common:perform
;;               (desig:an motion
;;                         (to fly)
;;                         (to ((582.640 -302.901 185.935) (0 0 0 1)))))))
;;
;; (cpm:with-process-modules-running
;;     (helicopter::helicopter-actuators)
;;   (cpl:top-level
;;     (cram-sherpa-robots-common:perform
;;      (desig:an motion
;;                (to switch)
;;                (device engine)
;;                (state on)))))
;;
;; (cpm:with-process-modules-running
;;     (helicopter::helicopter-actuators)
;;   (cpl:top-level
;;     (cram-sherpa-robots-common:perform
;;      (desig:an motion
;;                (to set-altitude)
;;                (to 5)))))
