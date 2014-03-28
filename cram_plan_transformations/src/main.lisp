
(in-package :cram-plan-transformations)

(defun init (&optional (ip "192.168.100.194"))
  (let ((uri (roslisp:make-uri ip 11311)))
   (unless (and (equal roslisp:*master-uri* uri)
                (eq roslisp::*node-status* :running))
       (roslisp-utilities:startup-ros :anonymous nil :master-uri uri))))

