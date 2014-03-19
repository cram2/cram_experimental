
;; Andrei's IP: 192.168.100.65

(in-package :cram-plan-transformations)

(defun init ()
  (unless (eq roslisp::*node-status* :running)
    (roslisp-utilities:startup-ros :anonymous nil)))

(defun test-query ()
  (force-ll
   (json-prolog:prolog
    `(and ("mongo_prolog:add_world_clauses")
          ("mongo_prolog:model" ?model)
          (= ?model "mug")
          ("get_model_pose" "mug" 0 ?pose))
    :package :cram-plan-transformations)))

