;;; todo license

(in-package :cram-plan-transformations)

(defun transform-into-plist (a-tree &optional (a-plist nil))
  "Transforms a tree into a property list. `a-tree' is something like
   `((action-role flip-1      actionverb)
     (has-sense   flip-1      flip-1-8)
     (is-a        flip-1-8    flip.v.08)
     (action-role pancake-3   theme)
     (has-sense   pancake-3   pancake-3-1)
     (is-a        pancake-3-1 pancake.n.01))
   for which the result would be
   `((THEME PANCAKE.N.01) (ACTIONVERB FLIP.V.08))"
  (let ((tripple (car a-tree)))
   (if (null a-tree)
       a-plist
       (cond ((member "action_role" tripple :test #'equal)
              (transform-into-plist
               (cdr a-tree)
               (cons (list (third tripple) (second tripple)) a-plist)))
             ((or (member "has_sense" tripple :test #'equal)
                  (member "is_a" tripple :test #'equal))
              (transform-into-plist
               (cdr a-tree)
               (mapcar #'(lambda (x) ; maybe it could stop at the first occurrence
                           (if (equal (cadr x) (second tripple))
                               (list (car x) (third tripple))
                               x)) a-plist)))
             (t
              (transform-into-plist (cdr a-tree) a-plist))))))

(defun get-owl-name (wordnet-name)
  (declare (ignore wordnet-name))
  ;; (gensym "owl-bla")
  :owl-name)

;; (defun extract-the-actual-name (name-with-id-stuff)
;;   "E.g. flip.v.08 -> flip, everything is a symbol."
;;   (let* ((a-string (symbol-name name-with-id-stuff))
;;          (index (position #\. a-string :from-end t :end (position #\. a-string :from-end t)))
;;          (a-substring (subseq a-string 0 index)))
;;     (intern a-substring)))

(defun extract-the-actual-name (name-with-id-stuff)
  "E.g. flip.v.08 -> flip, everything is a string."
  (subseq name-with-id-stuff 0
          (position #\. name-with-id-stuff :from-end t
                                           :end (position #\. name-with-id-stuff
                                                          :from-end t))))

(def-fact-group cram-plan-transformations ()
  ;; TODO comments
  (<- (transform-plist-into-descriptions ?plist ?descriptions)
    (findall ?description
             (and (member ?key-value-pair ?plist)
                  (description-for-key-value-pair ?key-value-pair ?description))
             ?descriptions-lazy)
    (lisp-fun cut:force-ll ?descriptions-lazy ?descriptions))

  (<- (description-for-key-value-pair ("ActionVerb" ?wordnet-name) ?description)
    (lisp-fun extract-the-actual-name ?wordnet-name ?name)
    (lisp-fun get-owl-name ?wordnet-name ?owl-name)
    (equal ?description (instruction (an action (name ?owl-name) (type ?name)))))

  (<- (description-for-key-value-pair ("Theme" ?wordnet-name) ?description)
    (lisp-fun extract-the-actual-name ?wordnet-name ?name)
    (lisp-fun get-owl-name ?wordnet-name ?owl-name)
    (equal ?description (theme (an object (name ?owl-name) (type ?name)))))

  (<- (description-for-key-value-pair ("Instrument" ?wordnet-name) ?description)
    (lisp-fun extract-the-actual-name ?wordnet-name ?name)
    (lisp-fun get-owl-name ?wordnet-name ?owl-name)
    (equal ?description (instrument (an object (name ?owl-name) (type ?name)))))

  (<- (transform-descriptions-into-action-description ?descriptions ?action-description)
    (member ?description ?descriptions)
    (equal ?description (instruction (an action . ?action-properties)))
    (findall ?remaining-description
             (and (member ?remaining-description ?descriptions)
                  (not (equal ?remaining-description ?description)))
             ?remaining-descriptions-lazy)
    (lisp-fun cut:force-ll ?remaining-descriptions-lazy ?remaining-descriptions)
    (append (an action) ?action-properties ?bla)
    (append ?bla ?remaining-descriptions ?all-descriptions)
    (equal ?action-description (instruction ?all-descriptions)))

  (<- (transform-plist-into-action-description ?plist ?action-description)
    (transform-plist-into-descriptions ?plist ?descriptions)
    (transform-descriptions-into-action-description ?descriptions ?action-description))

  (<- (add-items-to-action-description ?description ?items-plist ?new-description)
    ;; first transform the `?items-plist' into descriptions
    (transform-plist-into-descriptions ?items-plist ?items-description)
    ;; then append the descriptions to the action description
    (equal ?description (instruction (an action . ?key-value-pairs)))
    (append (an action) ?key-value-pairs ?bla)
    (append ?bla ?items-description ?all-descriptions)
    (lisp-fun remove-duplicates ?all-descriptions :test equal ?no-duplicates-desc)
    (equal ?new-description (instruction ?no-duplicates-desc))))

(defun test-stuff ()
  (let* ((atom-prob-pairs `(("action_role(Flip-1, ActionVerb)" 1.0)
                            ("action_role(pancake-3, Theme)" 1.0)
                            ("has_sense(Flip-1, flip-1-8)" 1.0)
                            ("is_a(flip-1-8, flip.v.08)" 1.0)
                            ("!is_a(flip-1-8, pancake.n.01)" 1.0)
                            ("is_a(pancake-3-1, pancake.n.01)" 1.0)
                            ("!is_a(pancake-3-1, flip.v.08)" 1.0)
                            ("has_sense(pancake-3, pancake-3-1)" 1.0)
                            ("action_core(Flip-1, Flipping)" 1.0)))
         (only-1-prob-list
           (mapcar #'(lambda (string-number-pair)
                       (unless (< (second string-number-pair) 1.0)
                         (car string-number-pair)))
                   atom-prob-pairs))
         (a-tree
           (mapcar #'function-notation-string->prefix-notation-list
                   only-1-prob-list))
         (a-plist (transform-into-plist a-tree))
         (action-desc-prolog (cut:force-ll
                              (prolog `(transform-plist-into-action-description
                                        ,a-plist
                                        ?action-description))))
         (action-desc (cut:var-value '?action-description (car action-desc-prolog)))
         (additional-tree (query-prac-with-atom-prob-pairs atom-prob-pairs))
         (additional-plist (transform-into-plist additional-tree))
         (additional-description (cut:force-ll
                                  (prolog `(transform-plist-into-descriptions
                                            ,additional-plist ?additional-description))))
         (new-desc-prolog
           (cut:var-value '?new-description
                          (car (cut:force-ll
                                (prolog `(add-items-to-action-description
                                          ,action-desc
                                          ,additional-plist
                                          ?new-description))))))
         (new-desc-after-duplicate
           (cut:var-value '?new-description-duplicate
                          (car (cut:force-ll
                                (prolog `(add-items-to-action-description
                                          ,new-desc-prolog
                                          ,additional-plist
                                          ?new-description-duplicate)))))))
    (format t "a-tree: ~a~%~%" a-tree)
    (format t "a-plist: ~a~%~%" a-plist)
    (format t "action-desc: ~a~%~%" action-desc)
    (format t "additionall-tree: ~a~%~%" additional-tree)
    (format t "additional-plist: ~a~%~%" additional-plist)
    (format t "additional-description: ~a~%~%"
            (cut:force-ll
             (cut:var-value
              '?additional-description (car (cut:force-ll additional-description)))))
    (format t "new-description: ~a~%~%" new-desc-prolog)
    (format t "new-desc-after-duplicate: ~a~%~%" new-desc-after-duplicate)))

;;; STEP 4: compare goal description to general plan and do something ...:D
;;; either use create-designator or compare the two and transform the plan as a list

;; (INSTRUCTION (A ACTION
;;                (TYPE FLIP)
;;                (NAME OWLBLABLA)
;;                (THEME (A OBJECT
;;                         (NAME #:|owl-bla1268|)
;;                         (TYPE PANCAKE)))
;;                (INSTRUMENT (A OBJECT
;;                              (NAME #:|owl-bla122369|)
;;                              (TYPE SPATULA)))
;;                (GOAL (-> (holds (and (at theme ?location)
;;                                      (orientation theme ?ori))
;;                                 (task-start OWLBLABLA))
;;                          (holds (and (at theme ?location)
;;                                      (orientation theme 90flipped))
;;                                 (task-end OWLBLABLA))
            
;;              )) ;; goal comes from action plan 
;;    (FLAWS (pancake-lost-shape))))

;; (achieve flip pancake-orientation-90degrees)
;; as in (achieve action with-goal)


;; ?instr = "flip the pancake" | (frame-representation  ;  <- call
;; --------- v --------------  |   ?instr               ;      to
;; (a action                   |   ?slot-value-pairs    ;     Daniel
;;   (name owlrelatedname)     |                        ;
;;   (type ?action-core)       |                        ; ?slot-value-pairs:
;;   ?slot-value-pairs)        |                        ;  ((verb flip)
;;                             |                        ;   (theme pancake))

;; (a action                   | (needed-roles          ; <- cram plans
;;   (name owlrelatedname)     |   ?action-core         ;
;;   (type ?action-core)       |   ?needed-roles)       ;
;;   ?slot-value-pairs)        | (given-roles           ;
;; ----------- v ------------- |   ?slot-value-pairs)   ; <- prolog
;; (with-designators           | (missing-roles         ;
;;     ...                     |   ?needed-roles        ; <- gaya
;;   (achieve ...))            |   ?slot-value-pairs    ;
;;                             |   ?missing-roles)      ;

;; (type ?old) -> (type ?new)  | (more-appropriate-in-context ?old ?new)
