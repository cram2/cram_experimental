(in-package :reas-inf)

(defun init-interface ()
  "Initializes the bullet reasoning interface."
  (if (eq (node-status) :RUNNING)
      (out-info "No need to start another node.")
      (start-ros-node "bullet-reasoning-interface"))
  (spatial-relations-demo::start-ros-and-bullet)
  (set-transform-listener)
  (out-info "Initialized bullet-reasoning-interface."))

(defun start-service (&key (debug-level :info))
  "Starts the service."
  (out-info "Starting service.")
  (out-info "Setting debug level to ~a" debug-level)
  (roslisp:set-debug-level '(bullet-reasoning-interface) debug-level)
  (interaction-server))

(def-service-callback bullet_reasoning_interface-srv:Interaction (operationsWithCopy
                                                                  removeAllObjects
                                                                  operations
                                                                  updateCameraPose
                                                                  cameraPose
                                                                  simulate
                                                                  simulateWithCopy
                                                                  computeStability
                                                                  computeContactWithKitchenBeforeSimulation
                                                                  computeContactWithKitchenAfterSimulation
                                                                  computeCollisionsBeforeSimulation
                                                                  computeCollisionsAfterSimulation
                                                                  computeVisibility
                                                                  computeOcclusions
                                                                  duration)
  (setf *start-time* (get-internal-real-time))
  (out-info "Incoming Request:")
  (out-info "operationsWithCopy: ~a" operationsWithCopy)
  (out-info "removeAllObjects: ~a" removeAllObjects)
  (out-debug "operations: ~a" operations)
  (out-info "updateCameraPose: ~a" updateCameraPose)
  (out-debug "cameraPose: ~a" cameraPose)
  (out-info "simulate: ~a" simulate)
  (out-info "simulateWithCopy: ~a" simulateWithCopy)
  (out-info "computeStability: ~a" computeStability)
  (out-info "computeContactWithKitchenBeforeSimulation: ~a" computeContactWithKitchenBeforeSimulation)
  (out-info "computeContactWithKitchenAfterSimulation: ~a" computeContactWithKitchenAfterSimulation)
  (out-info "computeCollisionsBeforeSimulation: ~a" computeCollisionsBeforeSimulation)
  (out-info "computeCollisionsAfterSimulation: ~a" computeCollisionsAfterSimulation)
  (out-info "computeVisibility: ~a" computeVisibility)
  (out-info "computeOcclusions: ~a" computeOcclusions)
  (out-info "duration: ~a" duration)
  (let* (;; operation constants
         (spawn-object-const (get-operation-constant ':SPAWN_OBJECT))
         (move-object-const (get-operation-constant ':MOVE_OBJECT))
         (remove-object-const (get-operation-constant ':REMOVE_OBJECT))
         (query-status-const (get-operation-constant ':QUERY_STATUS))
         ;; errorLevel/confirmation constants
         (success-const (get-error-level-constant ':SUCCESS))
         (failed-const (get-error-level-constant ':FAILED))
         (error-const (get-error-level-constant ':ERROR))
         (unhandled-value-const (get-error-level-constant ':UNHANDLED_VALUE))
         (remove-all-objects-failed-const (get-error-level-constant ':REMOVE_ALL_OBJECTS_FAILED))
         ;; world types
         (current-world-const (get-world-type-constant ':CURRENT_WORLD))
         (operated-world-const (get-world-type-constant ':OPERATED_WORLD))
         (simulated-world-const (get-world-type-constant ':SIMULATED_WORLD))
         ;; camera pose
         (camera-pose (when updateCameraPose
                        (cl-transforms-stamped:from-msg cameraPose)))
         ;; responses
         (intel-current-world (make-msg "bullet_reasoning_interface/WorldIntel" worldType current-world-const))
         (intel-operations-world (make-msg "bullet_reasoning_interface/WorldIntel" worldType operated-world-const))
         (intel-simulated-world (make-msg "bullet_reasoning_interface/WorldIntel" worldType simulated-world-const))
         (object-intel-current-world (if (not operationsWithCopy)
                                         (make-array (length operations) :initial-element (make-msg "BULLET_REASONING_INTERFACE/ObjectIntel"))
                                         '#()))
         (object-intel-operations (if operationsWithCopy
                                      (make-array (length operations) :initial-element (make-msg "BULLET_REASONING_INTERFACE/ObjectIntel"))
                                      object-intel-current-world))
         (object-intel-simulated (if simulateWithcopy
                                     (make-array (length operations) :initial-element (make-msg "BULLET_REASONING_INTERFACE/ObjectIntel"))
                                     object-intel-operations))
         (object-ids-symbol (make-array (length operations) :fill-pointer 0))
         (error-level success-const)
         ;; current world
         (world-current btr:*current-bullet-world*)
         ;; world to operate on
         (world-operations (if operationsWithcopy
                               (copy-world)
                               world-current))
         ;; world to simulate
         (world-simulate nil)
         (remove-all-objects-result (if removeAllObjects
                                        (remove-all-objects world-operations)
                                        t)))
    (out-info "Prepared all variables.")
    (get-elapsed-time)
    (when updateCameraPose
      (update-camera-pose camera-pose))
    (if (not remove-all-objects-result)
        (progn
          (out-error "Removing all objects failed!")
          (make-response :errorLevel remove-all-objects-failed-const))
        (progn
          ;; Applying operations
          (out-info "Number operations: ~a" (length operations))
          (loop for i from 0 to (- (length operations) 1) do
            (get-elapsed-time)
            (with-fields ((object-id Id)
                          operation
                          (object-type type)
                          (object-pose-stamped poseStamped)
                          (object-color color)
                          (bounding-box boundingBox))
                (elt operations i)
              (let ((object-bounding-box (cl-transforms-stamped:from-msg bounding-box)))
                (out-info "######## operations loop ########")
                (out-info "Applying operation ~a to object ~a." operation object-id)
                (update-message-arrays i `(:id ,object-id)
                                       object-intel-current-world (not operationsWithCopy)
                                       object-intel-operations operationsWithcopy
                                       object-intel-simulated simulateWithCopy)
                (vector-push (make-keyword object-id) object-ids-symbol)
                (cond
                  ((eq operation spawn-object-const)
                   (update-message-array i object-intel-operations
                                         :operation operation
                                         :operationSucceeded
                                         (spawn-object object-id object-type object-pose-stamped
                                                       object-color object-bounding-box :world world-operations)
                                         :contactWithKitchenBeforeSimulation (when computeContactWithKitchenBeforeSimulation
                                                                               (object-has-contact-with-kitchen object-id))
                                         :collisionWithBeforeSimulation (when computeCollisionsBeforeSimulation
                                                                          (object-get-collisions object-id :world world-operations :elem-type :string))))
                  ((eq operation move-object-const)
                   (update-message-array i object-intel-operations
                                         :operation operation
                                         :operationSucceeded
                                         (move-object object-id object-pose-stamped object-bounding-box :world world-operations)
                                         :contactWithKitchenBeforeSimulation (when computeContactWithKitchenBeforeSimulation
                                                                               (object-has-contact-with-kitchen object-id))
                                         :collisionWithBeforeSimulation (when computeCollisionsBeforeSimulation
                                                                          (object-get-collisions object-id :world world-operations :elem-type :string))))
                  ((eq operation remove-object-const)
                   (update-message-array i object-intel-operations
                                         :operation operation
                                         :operationSucceeded
                                         (remove-obj object-id world-operations)))
                  ((eq operation query-status-const)
                   (update-message-array i object-intel-operations
                                         :contactWithKitchenBeforeSimulation (when computeContactWithKitchenBeforeSimulation
                                                                               (object-has-contact-with-kitchen object-id))
                                         :collisionWithBeforeSimulation (when computeCollisionsBeforeSimulation
                                                                          (object-get-collisions object-id :world world-operations :elem-type :string))
                                         :operation operation
                                         :operationSucceeded t)
                   (out-info "Only querying status for object ~a" object-id))
                  (t
                   (out-error "Unhandled operation.")
                   (update-message-array i object-intel-operations :operation operation
                                                                   :operationSucceeded nil))))
              (out-info "---------------------------------")))
          (out-info "######## Finished operations loop ########")
          (get-elapsed-time)
          (out-info "Simulating.")
          ;; simulating
          (when simulate
            (setf world-simulate (simulate-world duration :world world-operations :copy simulateWithCopy)))
          (get-elapsed-time)
          ;; getting data from current world
          (out-info "---------------------------------")
          (out-info "Getting data from current world ~a" world-current)
          (out-info "---------------------------------")
          (setf object-intel-current-world (add-remaining-objects-to-intel world-current
                                                                           object-intel-current-world
                                                                           (if operationsWithCopy
                                                                               '#()
                                                                               object-ids-symbol)))
          (multiple-value-bind (data stable)
              (get-data-from-world world-current object-intel-current-world
                                   :camera-pose camera-pose
                                   :stability computeStability
                                   :kitchen-contact (when (and simulate computeContactWithKitchenAfterSimulation)
                                                        :after)
                                   :collisions (when (and simulate computeCollisionsAfterSimulation
                                                   :after))
                                   :visibility computeVisibility
                                   :occlusions computeOcclusions)
            (setf object-intel-current-world data)
            (setf intel-current-world (modify-message-copy intel-current-world
                                                           isStable stable
                                                           objects object-intel-current-world)))
          (get-elapsed-time)
          ;; getting data from operations world
          (out-info "---------------------------------")
          (out-info "Getting data from operations world ~a" world-operations)
          (out-info "---------------------------------")
          (when operationsWithCopy
            (setf object-intel-operations (add-remaining-objects-to-intel world-operations
                                                                          object-intel-operations
                                                                          object-ids-symbol))
            (multiple-value-bind (data stable)
                (get-data-from-world world-operations object-intel-operations
                                     :camera-pose camera-pose
                                     :stability computeStability
                                     :kitchen-contact (when (and simulate computeContactWithKitchenAfterSimulation
                                                          :after))
                                     :collisions (when (and simulate computeCollisionsAfterSimulation
                                                     :after))
                                     :visibility computeVisibility
                                     :occlusions computeOcclusions)
              (setf object-intel-operations data)
              (setf intel-operations-world (modify-message-copy intel-operations-world
                                                                isStable stable
                                                                objects object-intel-operations))))
          (get-elapsed-time)
          ;; getting data from simulated world
          (out-info "---------------------------------")
          (out-info "Getting data from simulated world ~a" world-simulate)
          (out-info "---------------------------------")
          (when (and simulate simulateWithCopy)
            (setf object-intel-simulated (add-remaining-objects-to-intel world-simulate
                                                                         object-intel-simulated
                                                                         object-ids-symbol))
            (multiple-value-bind (data stable)
                (get-data-from-world world-simulate object-intel-simulated
                                     :camera-pose camera-pose
                                     :stability computeStability
                                     :kitchen-contact (when (and simulate computeContactWithKitchenAfterSimulation
                                                          :after))
                                     :collisions (when (and simulate computeCollisionsBeforeSimulation
                                                     :after))
                                     :visibility computeVisibility
                                     :occlusions computeOcclusions)
              (setf object-intel-simulated data)
              (setf intel-simulated-world (modify-message-copy intel-simulated-world
                                                               isStable stable
                                                               objects object-intel-simulated))))
          (out-info "Terminating service call.")
          (get-elapsed-time)
          (out-info "########################################")
          (make-response :errorLevel error-level
                         :computedStability computeStability
                         :computedContactWithKitchenBeforeSimulation computeContactWithKitchenBeforeSimulation
                         :computedContactWithKitchenAfterSimulation computeContactWithKitchenAfterSimulation
                         :computedCollisionsBeforeSimulation computeCollisionsBeforeSimulation
                         :computedCollisionsAfterSimulation computeCollisionsAfterSimulation
                         :computedVisibility computeVisibility
                         :computedOcclusions computeOcclusions
                         :worlds (remove-if #'null
                                            `(,intel-current-world
                                              ,(when operationsWithCopy
                                                 intel-operations-world)
                                              ,(when (and simulate simulateWithCopy)
                                                 intel-simulated-world))))))))

(defun add-remaining-objects-to-intel (world intel annotated-objects)
  "Adds intel about objects which exist in the world `world' to `intel' if they are not inside `annotated-objects'.
`world' is expected to be a bullet world instance.
`intel' is expected to be a vector of ObjectIntel messages.
`annotated-objects' is expected to be a vector containing symbols representing object ids.
Returns `intel' unified with the new intel (as a vector)."
  (out-info "add-remaining-objects-to-intel()")
  (let* ((query-status-const (get-operation-constant ':QUERY_STATUS))
         (all-objects-in-world (get-all-objects world))
         (remaining-objects (remove-duplicates
                             (set-difference all-objects-in-world
                                             (vector-to-list annotated-objects))))
         (remaining-objects-intel (make-array (length remaining-objects)
                                              :initial-element (make-msg "BULLET_REASONING_INTERFACE/ObjectIntel"))))
    (out-info "annotated-objects: ~a~%remaining-objects: ~a" annotated-objects remaining-objects)
    (loop for object in remaining-objects
          for i from 0 to (- (length remaining-objects) 1)
          do (update-message-array i remaining-objects-intel :id (resolve-keyword object)
                                                             :operation query-status-const
                                                             :operationSucceeded t))
    (concatenate 'vector intel remaining-objects-intel)))

(defun get-data-from-world (world intel &key stability kitchen-contact collisions visibility occlusions camera-pose)
  "Gets information about objects in the world `world'.
`world' is expected to be a bullet world instance.
`intel' is expected to be a vector of ObjectIntel messages. The id field of every message inside this vecor should be set.
`stability' -- `t' to calcualte stability for each object, `NIL' otherwise.
`kitchen-contact' -- `:before' to calcualte contact with kitchen for each object and put result into field `contactWithKitchenBeforeSimulation',
                     `:after' to calcualte contact with kitchen for each object and put result into field `contactWithKitchenAfterSimulation',
                     `NIL' otherwise.
`collisions' -- `:before' to calcualte collisions for each object and put result into field `collisionWithBeforeSimulation',
                `:after' to calcualte collisions for each object and put result into field `collisionWithAfterSimulation',
                `NIL' otherwise.
`visibility' -- `t' to calcualte visibility for each object, `NIL' otherwise.
`occlusions' -- `t' to calcualte occlusions for each object, `NIL' otherwise.
`camera-pose' -- User for everytihng related with visibility/occlusions. When `NIK', the robot's point of view will be used.
Returns `intel' with aggregated information about every object
and as second value it returns a boolean which indicates, if `world' is stable."
  (out-info "Getting data from world ~a" world)
  (let* ((remove-object-const (get-operation-constant ':REMOVE_OBJECT))
         (visible-objects (when visibility
                            (get-visible-objects :world world :camera-pose camera-pose)))
         (world-is-stable (when stability
                            (is-stable-world :world world)))
         (stable-objects (when stability
                           (unless world-is-stable
                             (get-stable-objects :world world))))
         (occluded-objects (when occlusions
                             (get-occlusions :world world :camera-pose camera-pose))))
    (loop for object-intel across intel
          for i from 0 to (- (length intel) 1)
          do (with-fields ((object-id id) operation) object-intel
               (out-info "Updating data for object ~a" object-id)
               (if (eq operation remove-object-const)
                   (out-info "Skipping removed object.")
                   (let* ((object-id-symbol (make-keyword object-id))
                          (new-object-pose-stamped (get-object-pose-stamped object-id world))
                          (object-dimensions (get-object-dimensions object-id world))
                          (collision-objects (when collisions
                                               (object-get-collisions object-id :world world :elem-type :string))))
                     (when new-object-pose-stamped
                       (update-message-array
                        i intel
                        :poseStamped (cl-transforms-stamped:to-msg new-object-pose-stamped)))
                     (when object-dimensions
                       (update-message-array
                        i intel
                        :boundingBox (cl-transforms-stamped:to-msg object-dimensions)))
                     (when (or world-is-stable (find object-id-symbol stable-objects))
                       (update-message-array i intel :isStable t))
                     (when (find object-id-symbol visible-objects)
                       (update-message-array i intel :isVisible t))
                     (when kitchen-contact
                       (when (eq kitchen-contact :before)
                         (update-message-array i intel :contactWithKitchenBeforeSimulation (object-has-contact-with-kitchen object-id)))
                       (when (eq kitchen-contact :after)
                         (update-message-array i intel :contactWithKitchenAfterSimulation (object-has-contact-with-kitchen object-id))))
                     (when collision-objects
                       (when (eq collisions :before)
                         (update-message-array i intel :collisionWithBeforeSimulation collision-objects))
                       (when (eq collisions :after)
                         (update-message-array i intel :collisionWithAfterSimulation collision-objects)))
                     (when occluded-objects
                       (update-message-array i intel :occludedBy (list-to-vector
                                                                  (mapcar #'resolve-keyword
                                                                          (gethash object-id-symbol occluded-objects)))))))))
    (out-info "Leaving get-data-from-world().")
    (get-elapsed-time)
    (values intel world-is-stable)))
  
(defun interaction-server ()
  (with-ros-node ("interaction_server" :spin t)
    (set-transform-listener)
    (register-service "bullet_reasoning_interface/interaction" 'bullet_reasoning_interface-srv:Interaction)
    (out-info "Ready to receive requests.")))
