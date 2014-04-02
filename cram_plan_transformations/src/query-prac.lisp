
(in-package :cram-plan-transformations)

(defun query-prac (atom-string-probability-pairs-list)
  (roslisp:wait-for-service "/PRACInfer" 1)
  (let* ((atom-probability-message-list
           (mapcar #'(lambda (atom-probability-pair)
                       (roslisp:make-msg
                        "rosmln/AtomProbPair"
                        :atom (first atom-probability-pair)
                        :prob (second atom-probability-pair)))
                   atom-string-probability-pairs-list))
         (mln-database-input
           (roslisp:make-msg
            "rosmln/MLNDatabase"
            :evidence (make-array
                       (length atom-probability-message-list)
                       :initial-contents atom-probability-message-list)))
         (mln-database-output-message
           ;;rosprac-srv:output_dbs
           (aref
            (rosprac-srv:output_dbs
             (roslisp:call-service
              "/PRACInfer" 'rosprac-srv:pracinfer
              :params "missing=True"
              :pracmodule "senses_and_roles"
              :input_dbs (make-array 1 :initial-contents (list mln-database-input))))
            0))
         (atom-probability-pairs-output-list
           (map 'list #'(lambda (atomprobpair-message)
                          (list (rosmln-msg:atom atomprobpair-message)
                                (rosmln-msg:prob atomprobpair-message)))
                (rosmln-msg:evidence mln-database-output-message))))
    atom-probability-pairs-output-list))

(defun test-query-prac-c-style ()
  (let ((atom-prob-pairs '(("action_role(Flip-1, ActionVerb)" 1.0)
                           ("action_role(pancake-3, Theme)" 1.0)
                           ("has_sense(Flip-1, flip-1-8)" 1.0)
                           ("is_a(flip-1-8, flip.v.08)" 1.0)
                           ("!is_a(flip-1-8, pancake.n.01)" 1.0)
                           ("is_a(pancake-3-1, pancake.n.01)" 1.0)
                           ("!is_a(pancake-3-1, flip.v.08)" 1.0)
                           ("has_sense(pancake-3, pancake-3-1)" 1.0)
                           ("action_core(Flip-1, Flipping)" 1.0))))
    (query-prac atom-prob-pairs)))

(defun query-prac-with-atom-prob-pairs
    (&optional (atom-prob-pairs '(("action_role(Flip-1, ActionVerb)" 1.0)
                                  ("action_role(pancake-3, Theme)" 1.0)
                                  ("has_sense(Flip-1, flip-1-8)" 1.0)
                                  ("is_a(flip-1-8, flip.v.08)" 1.0)
                                  ("!is_a(flip-1-8, pancake.n.01)" 1.0)
                                  ("is_a(pancake-3-1, pancake.n.01)" 1.0)
                                  ("!is_a(pancake-3-1, flip.v.08)" 1.0)
                                  ("has_sense(pancake-3, pancake-3-1)" 1.0)
                                  ("action_core(Flip-1, Flipping)" 1.0))))
  (let* ((only-1-prob-list
           (mapcar #'(lambda (string-number-pair)
                       (unless (< (second string-number-pair) 1.0)
                         (car string-number-pair)))
                      atom-prob-pairs))
         (assertions-tree
           (mapcar #'function-notation-string->prefix-notation-list
                   only-1-prob-list)))
    (get-all-known-roles-p-list assertions-tree)))

(defun prefix-notation-list->function-notation-string (prefixed-list)
  (labels ((accumulate (strings-list concatenated-string)
             (if (null strings-list)
                 (concatenate 'string concatenated-string ")")
                 (accumulate (cdr strings-list)
                             (concatenate 'string
                                          concatenated-string
                                          "," (car strings-list))))))
    (accumulate (cdr (cdr prefixed-list))
                (concatenate 'string (car prefixed-list) "(" (second prefixed-list)))))

;; (defun string-to-symbol (a-string)
;;   (intern
;;    (string-upcase
;;     (substitute #\- #\_
;;                 (string-trim '(#\Space #\Newline #\Backspace #\Tab
;;                                #\Linefeed #\Page #\Return #\Rubout) a-string)))))

(defun string-trim-whitespace (a-string)
  (string-trim '(#\Space #\Newline #\Backspace #\Tab
                 #\Linefeed #\Page #\Return #\Rubout) a-string))

(defun function-notation-string->prefix-notation-list (a-string)
  "\"action_core(Flip-1,Flipping)\" -> '(action_core Flip-1 Flipping)"
  (let* ((opening-bracket-position (position #\( a-string))
         (closing-bracket-position (position #\) a-string :from-end t))
         (function-name
           (string-trim-whitespace (subseq a-string 0 opening-bracket-position)))
         (arguments (mapcar #'string-trim-whitespace
                            (split-sequence:split-sequence
                             #\Comma (subseq a-string
                                             (+ 1 opening-bracket-position)
                                             closing-bracket-position)
                             :remove-empty-subseqs t))))
    (append (list function-name) arguments)))

(defun get-all-known-roles-p-list (assertions-tree)
  (let* ((strings-list
           (mapcar #'prefix-notation-list->function-notation-string assertions-tree))
         (string-number-pairs-list
           (mapcar #'(lambda (a-string) (list a-string 1.0)) strings-list))
         (string-number-pairs-output-list
           (query-prac string-number-pairs-list))
         (only-1-probability-strings-list
           (mapcar #'car (remove-if #'(lambda (string-number-pair)
                                        (< (cadr string-number-pair) 1.0))
                                    string-number-pairs-output-list)))
         (output-assertions-tree
           (mapcar #'function-notation-string->prefix-notation-list
                   only-1-probability-strings-list)))
    (format t "~%~%daniel says: ~%~a~%~%" output-assertions-tree)
    output-assertions-tree))