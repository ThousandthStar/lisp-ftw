(defvar *routes-dir* (merge-pathnames "src/routes/" (uiop:getcwd)))
(defvar *output-dir* (merge-pathnames "output/" (uiop:getcwd)))

;; Convert route path to flat filename with $
;; "/home/about/" → "home$about$index.html"
;; "/" → "index.html"
(defun route-to-filename (route)
    (let ((route (namestring route)))
    (let* ((clean (string-trim "/" route))
            (parts (uiop:split-string clean :separator "/")))
        (if (or (string= clean "") (null parts) (equal parts '("")))
            "index.html"
            (concatenate 'string 
                        (format nil "~{~a~^$~}" parts) 
                        "$index.html")))))

;; Get the output file path
(defun get-output-file (route)
  (merge-pathnames (route-to-filename route) *output-dir*))

(defun path-to-route (path)
  (uiop:enough-pathname (uiop:truename* path) *routes-dir*))

(defun path-to-string-route (path)
  (namestring (path-to-route path)))