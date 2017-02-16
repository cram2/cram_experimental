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

(defsystem cram-sherpa-robots-common
  :author "Gayane Kazhoyan"
  :maintainer "Gayane Kazhoyan"
  :license "BSD"

  :depends-on (roslisp
               cram-designators
               cram-process-modules
               cram-language
               cram-occasions-events
               cram-plan-occasions-events
               cram-prolog
               yason ; for parsing json action designators
               cram-tf ; for parsing json pose-stamped-s
               sherpa_msgs-msg ; for communication between robots and commander
               sherpa_msgs-srv
               actionlib
;               geometry_msgs-msg ; to represent poses, currently using cl-trans-stamped
               cl-transforms-stamped
               roslisp-utilities ; for starting a node
               iai_robosherlock_msgs-msg ; for communicating to robosherlock
               cram-json-prolog ; for logging
               cram-utilities ; for json-prolog var-value
               )

  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "plan-library" :depends-on ("package"))
     (:file "prolog" :depends-on ("package"))
     (:file "action-json-parser" :depends-on ("package"))
     (:file "utils" :depends-on ("package"))
     (:file "action-designator-server" :depends-on ("package" "action-json-parser" "utils"))
     (:file "define-action-client-and-make-goals" :depends-on ("package" "utils"))
     (:file "cram-owl" :depends-on ("package" "utils"))
     (:file "robosherlock-action" :depends-on ("package" "plan-library" "utils"
                                                         "cram-owl" ; for mapping cram to RS names
                                                         ))))))
