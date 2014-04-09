
(in-package :cram-plan-transformations)

(defun test-connection ()
  (init "192.168.100.104")
  (roslisp:call-service "/game_log_server/query"
                        'json_prolog_msgs-srv:PrologQuery
                        :id (json-prolog::make-query-id)
                        :query (json-prolog::prolog->json
                                '("=" ?x 1) :prologify t)))

(defun test-query ()
  (force-ll
   (json-prolog:prolog
    `("=" ?x 1)
    :package :cram-plan-transformations)))

(defun get-spatula-height-in-collection (collection-id)
  (with-vars-bound (?pancake_maker_pose ?pancake_maker_bb ?end_time)
      (lazy-car
       (json-prolog:prolog
        `(and ("get_model_pose" ,collection-id "pancake_maker" 0 ?pancake_maker_pose)
              ("get_model_bb" ,collection-id "pancake_maker" 0 ?pancake_maker_bb)
              ("occurs" ,collection-id "flip"  ?start_time ?end_time)) :lispify t))
    (with-vars-bound (?spatula_pose)
        (lazy-car
         (json-prolog:prolog
          `("get_link_pose" ,collection-id "spatula_head_link" ,?end_time ?spatula_pose)))
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
        (- (third ?spatula_pose) (cl-transforms:z pm-top-center-transformed))))))

(defvar *errors* nil)

(defun get-spatula-height ()
  (init)
  (setf json-prolog::*service-namespace* "/game_log_server")
  (let* ((collections (alexandria:iota 5 :start 1))
             (heights (mapcar 'get-spatula-height-in-collection collections))
             (sum (reduce '+ heights))
             (avg (/ sum (length collections))))
        avg))

(defun game-log-param-value (param)
  (cond ((eq param 'instrument-relative-height)
         (get-spatula-height))))