
(in-package :cram-plan-transformations)

(defun test-query ()
  (force-ll
   (json-prolog:prolog
    `(and ("add_world_clauses")
          ("model" ?model)
          (= ?model "mug")
          ("get_model_pose" "mug" 0 ?pose))
    :package :cram-plan-transformations)))

(defun get-spatula-height-in-collection (collection-id)
  (with-vars-bound (?spatula_pose ?pancake_maker_pose ?pancake_maker_bb)
      (lazy-car
       (json-prolog:prolog
        `(and ("get_model_pose" ,collection-id "pancake_maker" 0 ?pancake_maker_pose)
              ("get_model_bb" ,collection-id "pancake_maker" 0 ?pancake_maker_bb)
              ;; ("occurs" "flipping" '("during" ?start_time ?end_time))
              ;; ("lost_spatula_contact_pancake" 2 ?time)
              ("=" ?end_time ,(cond ((= collection-id 1) 33370000000)
                                    ((= collection-id 2) 23338000000)
                                    ((= collection-id 3) 38742000000)
                                    ((= collection-id 4) 35873000000)
                                    ((= collection-id 5) 21598000000)))
              ("get_link_pose" ,collection-id "spatula_head_link" ?end_time ?spatula_pose))))
    (let* ((pm-rotation
             (cl-transforms:euler->quaternion
              :ax (fourth ?pancake_maker_pose)
              :ay (fifth ?pancake_maker_pose)
              :az (sixth ?pancake_maker_pose)))
           (pm-inverse-rotation
             (cl-transforms:q-inv pm-rotation))
           (pm-bb-bottom
             (cl-transforms:rotate
              pm-inverse-rotation
              (cl-transforms:make-3d-vector
               (first ?pancake_maker_bb)
               (second ?pancake_maker_bb)
               (third ?pancake_maker_bb))))
           (pm-bb-top
             (cl-transforms:rotate
              pm-inverse-rotation
              (cl-transforms:make-3d-vector
               (fourth ?pancake_maker_bb)
               (fifth ?pancake_maker_bb)
               (sixth ?pancake_maker_bb))))
           (pm-bb-height/2
             (/ (- (cl-transforms:z pm-bb-top) (cl-transforms:z pm-bb-bottom)) 2))
           (pm-top-center
             (cl-transforms:v+
              (cl-transforms:make-3d-vector
               (first ?pancake_maker_pose)
               (second ?pancake_maker_pose)
               (third ?pancake_maker_pose))
              (cl-transforms:make-3d-vector 0 0 pm-bb-height/2)))
           (pm-top-center-transformed (cl-transforms:rotate pm-rotation pm-top-center)))
      (- (third ?spatula_pose) (cl-transforms:z pm-top-center-transformed)))))

(defun get-spatula-height ()
  (init)
  (let* ((collections (alexandria:iota 5 :start 1))
         (heights (mapcar 'get-spatula-height-in-collection collections))
         (sum (reduce '+ heights))
         (avg (/ sum (length collections))))
    avg))

;; get_model_bb(2,pancake_maker, 0, BB).
;; get_model_pose(2,pancake_maker,0,Pose).
;; get_link_pose(2,spatula_head_link,10000000000,Pose).

;; x1: 26267000000 : 33370000000
;; x2: 19643000000 : 23338000000
;; x3: 31046000000 : 38742000000
;; x4: 27713000000 : 35873000000
;; x5: 17235000000 : 21598000000
