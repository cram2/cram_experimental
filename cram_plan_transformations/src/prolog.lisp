
(in-package :cram-plan-transformations)

(def-fact-group cram-plan-transformations ()
  ;; TODO comments
  (<- (transform-plist-into-descriptions ?plist ?descriptions)
    (findall ?description
             (and (member ?key-value-pair ?plist)
                  (description-for-key-value-pair ?key-value-pair ?description))
             ?descriptions-lazy)
    (lisp-fun cut:force-ll ?descriptions-lazy ?descriptions))

  (<- (description-for-key-value-pair ("ActionVerb" ?wordnet-name) ?description)
    (lisp-fun wordnet-name->symbol ?wordnet-name ?name)
    (lisp-fun get-owl-name ?wordnet-name ?owl-name)
    (equal ?description (instruction (an action (uri ?owl-name) (type ?name)))))

  (<- (description-for-key-value-pair ("Theme" ?wordnet-name) ?description)
    (lisp-fun wordnet-name->symbol ?wordnet-name ?name)
    (lisp-fun get-owl-name ?wordnet-name ?owl-name)
    (equal ?description (theme (an object (uri ?owl-name) (type ?name)))))

  (<- (description-for-key-value-pair ("Instrument" ?wordnet-name) ?description)
    (lisp-fun wordnet-name->symbol ?wordnet-name ?name)
    (lisp-fun get-owl-name ?wordnet-name ?owl-name)
    (equal ?description (instrument (an object (uri ?owl-name) (type ?name)))))

  (<- (parameters-for-action-core flipping ?parameters)
    (equal ?parameters (instrument-relative-height 0.2)))

  (<- (description-for-key-value-pair ("ActionCore" ?wordnet-name) ?description)
    (lisp-fun wordnet-name->symbol ?wordnet-name ?name)
    (parameters-for-action-core ?name ?parameters)
    (equal ?description (parameters ?parameters)))

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
    (equal ?new-description (instruction ?no-duplicates-desc)))

  (<- (update-parameters-in-action-description ?description ?function ?new-description)
    (equal ?description (instruction (an action . ?key-value-pairs)))
    (setof ?new-key-value-pair
           (and (member ?key-value-pair ?key-value-pairs)
                (-> (equal ?key-value-pair (parameters . ?params-key-value-pairs))
                    (and (setof ?new-params-key-value-pair
                                (and (member ?params-key-value-pair ?params-key-value-pairs)
                                     (equal (?key ?value) ?params-key-value-pair)
                                     (lisp-fun ?function ?key ?new-value)
                                     (equal ?new-params-key-value-pair (?key ?new-value)))
                                ?new-params-key-value-pairs)
                         (equal ?new-key-value-pair
                                (parameters . ?new-params-key-value-pairs)))
                    (equal ?new-key-value-pair ?key-value-pair)))
           ?new-key-value-pairs)
    (equal ?new-description (instruction (an action . ?new-key-value-pairs)))))


(defun test-add-items-to-action-description ()
  (let* ((action-desc
           `(INSTRUCTION
             (AN ACTION (NAME "knowrob:'FlippingAnObject'") (TYPE flip)
               (THEME (AN OBJECT (NAME "knowrob:'Pancake'") (TYPE pancake))))))
         (additional-plist '(("Instrument" "spatula.n.01")))
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
    (equalp new-desc-prolog new-desc-after-duplicate)))