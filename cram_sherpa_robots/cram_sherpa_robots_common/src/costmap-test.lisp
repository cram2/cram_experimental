;;;
;;; Copyright (c) 2010, Lorenz Moesenlechner <moesenle@in.tum.de>
;;;               2017, Gayane Kazhoyan <kazhoyan@cs.uni-bremen.de>
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
;;;     * Neither the name of Willow Garage, Inc. nor the names of its
;;;       contributors may be used to endorse or promote products derived from
;;;       this software without specific prior written permission.
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
;;;

(in-package :robots-common)

(defvar *location-costmap-publisher* nil)
(defvar *marker-publisher* nil)
(defparameter *last-published-marker-index* nil)

(defun costmap-marker-pub-init ()
  (setf *location-costmap-publisher*
        (roslisp:advertise "visualization_marker_array" "visualization_msgs/MarkerArray"))
  (setf *marker-publisher*
        (roslisp:advertise "visualization_marker" "visualization_msgs/Marker")))

(roslisp-utilities:register-ros-init-function costmap-marker-pub-init)

(defun remove-markers-up-to-index (index)
  (let ((removers
          (loop for i from 0 to index
                collect (roslisp:make-message "visualization_msgs/Marker"
                                              (std_msgs-msg:frame_id header) cram-tf:*fixed-frame*
                                              (visualization_msgs-msg:ns) ""
                                              (visualization_msgs-msg:id) i
                                              (visualization_msgs-msg:action)
                                              (roslisp-msg-protocol:symbol-code
                                               'visualization_msgs-msg:marker
                                               :delete)))))
    (when removers
      (roslisp:publish *location-costmap-publisher*
                       (roslisp:make-message
                        "visualization_msgs/MarkerArray"
                        (visualization_msgs-msg:markers)
                        (map 'vector #'identity removers))))))

(defun publish-location-costmap (map &key (frame-id cram-tf:*fixed-frame*) (threshold 0.0005))
  (when *location-costmap-publisher*
    (multiple-value-bind (markers last-index)
        (location-costmap::location-costmap->marker-array
         map :frame-id frame-id
             :threshold threshold
             :z (slot-value map 'location-costmap:visualization-z)
             :hsv-colormap t
             :elevate-costmap nil)
      (when *last-published-marker-index*
        (remove-markers-up-to-index *last-published-marker-index*))
      (setf *last-published-marker-index* last-index)
      (roslisp:publish *location-costmap-publisher* markers))))

(defun costmap-metadata (diameter z)
  (list :width diameter :height diameter :resolution 0.1
          :origin-x (/ diameter -2.0) :origin-y (/ diameter -2.0) :visualization-z z)
  ;; (with-vars-bound (?width ?height ?resolution ?origin-x ?origin-y)
  ;;     (lazy-car (prolog `(and (costmap-size ?width ?height)
  ;;                             (costmap-origin ?origin-x ?origin-y)
  ;;                             (costmap-resolution ?resolution))))
  ;;   (check-type ?width number)
  ;;   (check-type ?height number)
  ;;   (check-type ?resolution number)
  ;;   (check-type ?origin-x number)
  ;;   (check-type ?origin-y number)
  ;;   )
  )

(def-prolog-handler dynamic-costmap (bdgs ?size ?z ?cm)
  (list
   (if (or (not bdgs) (cut:is-var (cut:var-value ?cm bdgs)))
       (prog1
           (cut:add-bdg (cut:var-value ?cm bdgs)
                        (apply #'make-instance 'location-costmap:location-costmap
                               (costmap-metadata (cut:var-value ?size bdgs)
                                                 (cut:var-value ?z bdgs)))
                        bdgs))
       (when (typep (cut:var-value ?cm bdgs) 'location-costmap:location-costmap)
         bdgs))))

;; (def-fact-group costmap-metadata ()
;;   (<- (location-costmap:costmap-size ?diameter ?diameter)
;;     (lisp-fun get-costmap-size ?diameter))
;;   (<- (location-costmap:costmap-origin -5 -5))
;;   (<- (location-costmap:costmap-resolution 1.0))
;;   (<- (location-costmap:costmap-padding 0.1)))

(defmethod location-costmap:costmap-generator-name->score ((name (eql 'test-costmap-generator)))
  5)

(def-fact-group test-costmap (location-costmap:desig-costmap)

  (<- (location-costmap:desig-costmap ?desig ?cm)
    (desig:desig-prop ?desig (:to :test))
    (desig:desig-prop ?desig (:size ?size))
    (desig:desig-prop ?desig (:z ?z))
    (format "size: ~a~%" ?size)
    (dynamic-costmap ?size ?z ?cm)
    (location-costmap:costmap-add-function
     test-costmap-generator
     (location-costmap:make-gauss-cost-function (0.0 0.0) ((1.0 0.0) (0.0 1.0)))
     ?cm)))

(defmethod location-costmap:on-visualize-costmap sherpa ((map location-costmap:location-costmap))
  (publish-location-costmap map :threshold 0.0005))

(defun publish-test-cm ()
  (loop for ?i from 1 to 5
        do (desig:reference (desig:a location (to test) (size ?i) (z ?i)))
           (sleep 1.0)
           (when (= ?i 5)
             (setf ?i 0))))
