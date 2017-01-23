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

(defmacro define-action-client (name timeout ros-name type)
  "Creates this and that."
  (let* ((package (symbol-package name))
         (param-name (intern (format nil "*~a-ACTION-TIMEOUT*" name) package))
         (action-var-name (intern (format nil "*~a-ACTION-CLIENT*" name) package))
         (init-function-name (intern (format nil "INIT-~a-ACTION-CLIENT" name) package))
         (destroy-function-name (intern (format nil "DESTROY-~a-ACTION-CLIENT" name) package))
         (getter-function-name (intern (format nil "GET-~a-ACTION-CLIENT" name) package)))
    `(progn

       (defparameter ,param-name ,timeout
         ,(format nil "How many seconds to wait before returning from ~a action." name))

       (defvar ,action-var-name nil)

       (defun ,init-function-name ()
         (setf ,action-var-name (actionlib:make-action-client ,ros-name ,type))
         (loop until (actionlib:wait-for-server ,action-var-name 5.0)
               do (roslisp:ros-info (ptu-action-client)
                                    "Waiting for ~a action server..." ',name))
         (roslisp:ros-info (robots-common action-client)
                           "~a action client created." ',name)
         ,action-var-name)

       (defun ,destroy-function-name ()
         (setf ,action-var-name nil))

       (roslisp-utilities:register-ros-cleanup-function ,destroy-function-name)

       (defun ,getter-function-name ()
         (or ,action-var-name (,init-function-name))))))
