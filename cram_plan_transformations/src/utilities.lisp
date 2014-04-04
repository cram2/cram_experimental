
(in-package :cram-plan-transformations)

;;; URIs
;;; either "http://ias.cs.tum.edu/kb/knowrob.owl#NAME"
;;; or "knowrob:'NAME'"
;;; flipping: knowrob:'FlippingAnObject'
;;; pancake: knowrob:'Pancake'
;;; spatula: knowrob:'Spatula'


(defun init (&optional (ip "192.168.100.194"))
  (let ((uri (roslisp:make-uri ip 11311)))
   (unless (and (equalp roslisp:*master-uri* uri)
                (eq roslisp::*node-status* :running))
       (roslisp-utilities:startup-ros :anonymous nil :master-uri uri))))


(defun transform-into-plist (predicates-nested-list)
  "Transforms a nested list into a property list. `a-nested-list' is something like
   ((action_role flip-1 ActionVerb)
    (has_sense   flip-1      flip-1-8)
    (is_a        flip-1-8    flip.v.08)
    (action_role pancake-3   Theme)
    (has_sense   pancake-3   pancake-3-1)
    (is_a        pancake-3-1 pancake.n.01))
   for which the result would be
   ((Theme pancake.n.01) (ActionVerb flip.v.08))"
  (labels ((parse-nested-list (a-nested-list &optional (a-plist nil))
             (let ((tripple (car a-nested-list)))
               (if (null a-nested-list)
                   a-plist
                   (cond ((member "action_core" tripple :test #'equal)
                          (parse-nested-list
                           (cdr a-nested-list)
                           (cons (list "ActionCore" (third tripple)) a-plist)))
                         ((member "action_role" tripple :test #'equal)
                          (parse-nested-list
                           (cdr a-nested-list)
                           (cons (list (third tripple) (second tripple)) a-plist)))
                         ((or (member "has_sense" tripple :test #'equal)
                              (member "is_a" tripple :test #'equal))
                          (parse-nested-list
                           (cdr a-nested-list)
                           (mapcar #'(lambda (x) ; maybe it could stop at the first occurrence
                                       (if (equal (cadr x) (second tripple))
                                           (list (car x) (third tripple))
                                           x)) a-plist)))
                         (t
                          (parse-nested-list (cdr a-nested-list) a-plist))))))
           (precedance-p (string-1 string-2)
             (let* ((scores-small-is-earlier
                      '("action_core" 1 "action_role" 2 "has_sense" 3 "is_a" 4))
                    (string-score-map
                      (alexandria:plist-hash-table scores-small-is-earlier :test 'equal))
                    (-inf most-negative-fixnum))
               (< (gethash string-1 string-score-map -inf)
                  (gethash string-2 string-score-map -inf)))))
    ;; first arrange the nested list such that all "action_role" appear before
    ;; all "has_sense" and the latter before "is_a"
    (let ((sorted-list (sort (map 'list #'identity predicates-nested-list)
                             #'precedance-p :key #'car))) ; sort is destructive
     ;; go through the sorted list a generate the plist
      (parse-nested-list sorted-list))))

(defun test-transform-into-plist ()
  (transform-into-plist `(("is_a"        "pancake-3-1" "pancake.n.01")
                          ("has_sense"   "Flip-1"      "flip-1-8")
                          ("action_role" "Flip-1"      "ActionVerb")
                          ("is_a"        "flip-1-8"    "flip.v.08")
                          ("action_role" "pancake-3"   "Theme")
                          ("action_core" "Flip-1"      "Flipping")
                          ("garbage"     "foo"         "bar")
                          ("has_sense"   "pancake-3"   "pancake-3-1"))))

(defun get-owl-name (wordnet-name)
  (let* ((stripped-name
           (wordnet-name->symbol wordnet-name))
         (wordnet->owl-plist
              '(flip "FlippingAnObject"
                pancake "Pancake"
                spatula "Spatula"))
         (wordnet->owl-hashmap
           (alexandria:plist-hash-table wordnet->owl-plist :test 'equal)))
    (concatenate 'string
                 "knowrob:'" (gethash stripped-name wordnet->owl-hashmap) "'")))

(defun wordnet-name->symbol (name-with-id-stuff)
  "E.g. flip.v.08 -> flip, everything is a string."
  (intern
   (string-upcase
    (subseq name-with-id-stuff 0
            (position #\. name-with-id-stuff :from-end t
                                             :end (position #\. name-with-id-stuff
                                                            :from-end t))))))

(defun test-get-owl-name ()
  (get-owl-name "flip.v.08"))

