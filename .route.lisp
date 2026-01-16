

(ql:quickload :hunchentoot)

(defun create-route-handler (path handler-fn)
  "Programmatically create a route"
  (push
   (hunchentoot:create-prefix-dispatcher path handler-fn)
   hunchentoot:*dispatch-table*))

(defun load-route (path)
  (let ((source-file (merge-pathnames "./routes/" (merge-pathnames path "index.lisp"))))
    (create-route-handler 
     path
     (lambda ()
       (setf (hunchentoot:content-type*) "text/html")
       (render-component path)))))

(defun render-component (path)
  (let ((source-file (merge-pathnames "./routes/" (merge-pathnames path "index.lisp"))))
    (when (probe-file source-file)
      (load source-file)
      (if (fboundp 'component)
          (funcall 'component)
          "No component function found"))))

(defun output-component (path)
  (let ((destination-file (merge-pathnames "./output/" (merge-pathnames path "index.html"))))
    ))