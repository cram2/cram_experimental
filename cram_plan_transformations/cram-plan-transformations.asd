
;; TODO: fix the bsd macro snippet thing

(defsystem cram-plan-transformations
  :author "gaya"
  :license "BSD"
  :description "ToDo"

  :depends-on (cram-reasoning
               cram-language
               designators
               cram-plan-library
               cram-plan-knowledge
               cram-utilities
               alexandria
               split-sequence
               cram-json-prolog
               json_prolog_msgs-srv
               rosprac-srv
               rosmln-srv
               roslisp-utilities)
  :components
  ((:module "src"
           :components
           ((:file "package")
            (:file "utilities" :depends-on ("package"))
            (:file "prolog" :depends-on ("package"))
            (:file "query-game-log" :depends-on ("package"))
            (:file "query-prac" :depends-on ("package"))
            (:file "demo" :depends-on ("package"))))))

