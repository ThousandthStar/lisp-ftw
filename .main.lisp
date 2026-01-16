

(load "tags.lisp")
(load "route.lisp")
(ql:quickload '("hunchentoot" "caveman2" "spinneret"
                "djula" "easy-routes"))

(use-package :tags)

(defun main ()
  
  (load-route ""))

(main)
