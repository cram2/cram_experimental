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

(in-package :donkey)

(def-fact-group donkey-pms (cpm:matching-process-module
                            cpm:available-process-module)

  (<- (cpm:matching-process-module ?motion-designator donkey-navigation)
    (or (desig-prop ?motion-designator (:type :driving))
        (desig-prop ?motion-designator (:to :drive))))

  (<- (cpm:matching-process-module ?motion-designator donkey-manipulation)
    (or (desig-prop ?motion-designator (:type :mounting))
        (desig-prop ?motion-designator (:to :mount))
        (desig-prop ?motion-designator (:type :unmounting))
        (desig-prop ?motion-designator (:to :unmount))))

  (<- (cpm:available-process-module donkey-navigation)
    (not (cpm:projection-running ?_)))
  (<- (cpm:available-process-module donkey-manipulation)
    (not (cpm:projection-running ?_))))


(cpm:def-process-module donkey-navigation (motion-designator)
  (destructuring-bind (command argument) (reference motion-designator)
    (ecase command
      (drive
       (handler-case
           (call-drive-action
            :action-goal (cram-sherpa-robots-common:make-move-to-goal argument))
         ;; (cram-plan-failures:look-at-failed ()
         ;;   (cpl:fail 'cram-plan-failures:look-at-failed :motion motion-designator))
         )))))

(cpm:def-process-module donkey-manipulation (motion-designator)
  (destructuring-bind (command argument mount?-otherwise-unmount)
      (reference motion-designator)
    (ecase command
      (mount
       (handler-case
           (call-mount-action
            :action-goal (cram-sherpa-robots-common:make-mount-goal
                          argument mount?-otherwise-unmount))
         ;; (cram-plan-failures:look-at-failed ()
         ;;   (cpl:fail 'cram-plan-failures:look-at-failed :motion motion-designator))
         )))))

;; Examples:
;;
;; (cpm:with-process-modules-running
;;   (donkey::donkey-navigation)
;; (cpl:top-level
;;   (cpm:pm-execute-matching
;;    (desig:a motion
;;             (to drive)
;;             (to ((1 1 1) ( 0 0 0 1)))))))
;;
;; (cpm:with-process-modules-running
;;     (donkey::donkey-navigation donkey::donkey-manipulation)
;;   (cpl:top-level
;;     (cpm:pm-execute-matching
;;      (desig:an motion
;;                (to unmount)
;;                (agent red-wasp)))))
