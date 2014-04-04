
(in-package :cram-plan-transformations)

(defparameter *result* nil
  "result of calling DEMO-PLAN-TRANSFORMATIONS")

(defun demo-plan-transformations ()
  (init)

  (let (nl-sentence nested-list action-plist action-description additional-nested-list
        additional-plist additional-description combined-description
        game-log-transformed-description)

    (setf nl-sentence "Flip the pancake.")
    (format t "~%~%NL sentence:~%~a~%~%" nl-sentence)

    (setf nested-list (prac-nl->atom-nested-list nl-sentence))
    (setf action-plist (transform-into-plist nested-list))
    (setf action-description
          (cut:var-value '?action-description
                         (cut:lazy-car
                          (prolog `(transform-plist-into-action-description
                                    ,action-plist ?action-description)))))
    (format t "Action raw data:~%~a~%~%" action-plist)
    (format t "Action description:~%~a~%~%" action-description)

    (break)

    (setf additional-nested-list (prac-get-all-known-roles nested-list))
    (setf additional-plist ; ToDo: overwriting would be nicer than set-difference
          (set-difference (transform-into-plist additional-nested-list)
                          action-plist :test #'equal :key #'car))
    (setf additional-description
          (cut:var-value '?additional-description
                         (cut:lazy-car
                          (prolog `(transform-plist-into-descriptions
                                    ,additional-plist ?additional-description)))))
    (format t "Missing roles raw data:~%~a~%~%" additional-plist)
    (format t "Missing roles description:~%~a~%~%" additional-description)

    (break)

    (setf combined-description
          (cut:var-value '?new-description
                         (cut:lazy-car
                          (prolog `(add-items-to-action-description
                                    ,action-description ,additional-plist
                                    ?new-description)))))
    (format t "Combined description:~%~a~%~%" combined-description)

    (break)

    (setf game-log-transformed-description
          (cut:var-value '?new-description
                         (cut:lazy-car
                          (prolog `(update-parameters-in-action-description
                                    ,combined-description game-log-param-value
                                    ?new-description)))))
    (format t "Transformed description from game log data:~%~a~%~%"
            game-log-transformed-description)

    (setf *result* game-log-transformed-description)
    nil))

(defvar *combined-description-example*
  `(INSTRUCTION
    (AN ACTION (URI "knowrob:'FlippingAnObject'") (TYPE FLIP)
      (THEME (AN OBJECT (URI "knowrob:'Pancake'") (TYPE PANCAKE)))
      (PARAMETERS ((INSTRUMENT-RELATIVE-HEIGHT 0.2)))
      (INSTRUMENT (AN OBJECT (URI "knowrob:'Spatula'") (TYPE SPATULA))))))