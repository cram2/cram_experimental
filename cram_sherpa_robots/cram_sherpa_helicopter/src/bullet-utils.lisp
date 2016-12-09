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

(defun spawn-object-aabb-box (object-name
                              &key
                                (box-name (intern (concatenate
                                                   'string
                                                   (symbol-name object-name)
                                                   "-BOX")))
                                (world btr:*current-bullet-world*))
  (let* ((aabb (btr:aabb (btr-utils:object-instance object-name)))
         (aabb-pose (cl-transforms:make-pose
                     (cl-bullet:bounding-box-center aabb)
                     (cl-transforms:make-identity-rotation)))
         (aabb-size (with-slots (cl-transforms:x cl-transforms:y cl-transforms:z)
                        (cl-bullet:bounding-box-dimensions aabb)
                      (list cl-transforms:x cl-transforms:y cl-transforms:z))))
    (btr:add-object world :box box-name aabb-pose :mass 0.0 :size aabb-size)
    box-name))

(defun translate-object (object-name &optional (pose-z-delta 0.0))
  (let* ((object-pose (btr-utils:object-pose object-name))
         (new-pose (cl-transforms:copy-pose
                    object-pose
                    :origin (cl-transforms:copy-3d-vector
                             (cl-transforms:origin object-pose)
                             :z (+ pose-z-delta
                                   (cl-transforms:z (cl-transforms:origin object-pose)))))))
    (btr-utils:move-object object-name new-pose)))

(defun calculate-altitude-bullet (object-name &key (z-delta 0.1) (nudge-robot T))
  (let ((box-name (spawn-object-aabb-box object-name))
        (terrain-name (cut:var-value '?terrain
                                     (car (prolog:prolog
                                           '(robots-common:terrain-name ?terrain))))))
    ;; hack to make collision detection acknowledge the box
    (translate-object box-name 0.1)
    (translate-object box-name -0.1)
    (if (member (btr-utils:object-instance box-name)
                (btr:find-objects-in-contact btr:*current-bullet-world*
                                             (btr-utils:object-instance terrain-name)))
        ;; somehow underneath the terrain -> go up first
        (progn
          (when nudge-robot
            (loop for i = 0 then (1+ i)
                  while (member (btr-utils:object-instance box-name)
                                (btr:find-objects-in-contact btr:*current-bullet-world*
                                                             (btr-utils:object-instance
                                                              terrain-name)))
                  do (translate-object box-name z-delta)
                  finally (translate-object object-name (* z-delta (1- i)))))
            (btr-utils:kill-object box-name)
            0.0)
        ;; above terrain -> go down
        (loop for i = 0 then (1+ i)
                      until (member (btr-utils:object-instance box-name)
                                    (btr:find-objects-in-contact btr:*current-bullet-world*
                                                                 (btr-utils:object-instance
                                                                  terrain-name)))
                      do (translate-object box-name (- z-delta))
                      finally (btr-utils:kill-object box-name)
                              (return (* z-delta i))))))
