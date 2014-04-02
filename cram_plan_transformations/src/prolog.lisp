
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