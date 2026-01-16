(ql:quickload '(:cl-ppcre :hunchentoot :easy-routes))

(load "helpers.lisp")
(load "tags.lisp")

(defvar *slug-replace-regex* "([a-zA-Z0-9]+)")
(defvar *slug-capture-regex* ":(\\w+)")
(defvar *content-regex* "\\{\\{\\s*\%content\%\\s*\\}\\}")
(defvar *acceptor* (make-instance 'easy-routes:easy-routes-acceptor :port 4242))
(defvar *template-string* (uiop:read-file-string (merge-pathnames "template.html" *routes-dir*)))
(defvar *static-file-table* (make-hash-table :test #'equalp))

(defun build-route-capture (path)
    (declare (type pathname path))
    (format t "Building route capture for ~a~%" (path-to-string-route path))
    (let* ((route (path-to-string-route path)) (slugs (cl-ppcre:all-matches *slug-capture-regex* route)))
    (if (null slugs)
        (progn (format t "No slugs found for ~a ; returning original route~%" route) route)
        (let ((new-route (cl-ppcre:regex-replace *slug-capture-regex* route *slug-replace-regex*)))
           (progn (format t "New route: ~a~%" new-route) new-route)))))

(defun route-to-output (path)
  (merge-pathnames (path-to-route path) *output-dir*))

(defun output-to-route (path)
  (merge-pathnames (uiop:enough-pathname path *output-dir*) *routes-dir*))

(defun render-component (path)
  (format t "Rendering component: ~a~%" path)
  (let ((source-file (merge-pathnames (merge-pathnames "index.lisp" path) *routes-dir*)))
    (if (probe-file source-file) (progn (format t "Source file found: ~a~%" source-file)
      (load source-file)
      (if (fboundp 'component)
          (progn (format t "Component function found: ~a~%" path) (cl-ppcre:regex-replace *content-regex* *template-string* (funcall 'component)))
          (progn (format t "Error: no component function found in ~a~%" path) nil)))
      (progn (format t "Source file not found: ~a~%" source-file) nil))))

(defun output-component (path template-string)
  (format t "Outputting component: ~a~%" path)
  (format t "Route string: ~a~%" (build-route-capture path))
  (let ((destination-file (merge-pathnames (get-output-file (path-to-route path)) *output-dir*))
        (content (render-component path template-string)))
    (when content
      (ensure-directories-exist destination-file)
      (with-open-file (stream destination-file :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-string content stream)))))

;(defun build-directory (dir-path template-string)
;  (format t "Building directory: ~a~%" dir-path)
;  (output-component dir-path template-string)
;  (loop for subdir in (uiop:subdirectories dir-path)
;        do (build-directory subdir template-string)))

(defun build-all-components (template-path)
  (let ((template-string (uiop:read-file-string template-path)))
    (build-directory *routes-dir* template-string)))
  
(defun build-tailwind ()
  (uiop:run-program "tailwind -i src/routes/style.css -o output/style.css")
  (setf (gethash "style.css" *static-file-table*) (uiop:read-file-string (merge-pathnames "style.css" *output-dir*)))
)

(defun handle-request (path)
  (let* ((relative-path (string-left-trim "/" path))
         (source-file-path (merge-pathnames (merge-pathnames "index.lisp" (pathname relative-path)) *routes-dir*)))
    (if (probe-file source-file-path)
      (progn 
        (format t "Handling request with source file: ~a~%" source-file-path)
        (let ((content (render-component source-file-path)))
          (if content 
            (progn 
              (setf (hunchentoot:content-type*) "text/html")
              (cl-ppcre:regex-replace-all *content-regex* *template-string* content)
            )
            (progn 
              (setf (hunchentoot:return-code*) 505)
              "505 Server Error: component function not found"
            )
          )
        )
      )
      (progn 
        (format t "404 Not Found: ~a~%" path)
        (setf (hunchentoot:return-code*) 404)
        "404 Not Found"
      )
    )
  )
)

(defun is-static-file (path)
  (cl-ppcre:scan "\\.(css|js|ico|png|jpg|jpeg|gif|svg|woff2?|ttf|eot)$" path))

(defun handle-static-file (path)
  (let ((result (gethash (string-left-trim "/" path) *static-file-table*)))
    (if result
      (progn
        (setf (hunchentoot:return-code*) 200)
        (let ((file-ext (last (uiop:split-string path :separator "."))))
          (setf (hunchentoot:content-type*) (cond(
            (equalp file-ext "css") "text/css"
            (equalp file-ext "html") "text/html"
            (equalp file-ext "htm") "text/html"
            (equalp file-ext "js") "application/javascript"
            (equalp file-ext "json") "application/json"
            (equalp file-ext "txt") "text/plain"
            (equalp file-ext "xml") "application/xml"
            (equalp file-ext "png") "image/png"
            (equalp file-ext "jpg") "image/jpeg"
            (equalp file-ext "jpeg") "image/jpeg"
            (equalp file-ext "gif") "image/gif"
            (equalp file-ext "svg") "image/svg+xml"
            (equalp file-ext "ico") "image/x-icon"
            (equalp file-ext "webp") "image/webp"
            (equalp file-ext "woff") "font/woff"
            (equalp file-ext "woff2") "font/woff2"
            (equalp file-ext "ttf") "font/ttf"
            (equalp file-ext "otf") "font/otf"
            (equalp file-ext "eot") "application/vnd.ms-fontobject"
            (equalp file-ext "pdf") "application/pdf"
            (equalp file-ext "zip") "application/zip"
            (equalp file-ext "mp3") "audio/mpeg"
            (equalp file-ext "mp4") "video/mp4"
            (equalp file-ext "webm") "video/webm"
            (equalp file-ext "wasm") "application/wasm"
            t "application/octet-stream"
          )))
        )
        result
      )
      (progn
        (setf (hunchentoot:return-code*) 404)
        (format nil "404: Static file at ~a not found~%" path)
      )
    )
  )
)

(easy-routes:defroute index ("/") ()
  (handle-request "/"))

(easy-routes:defroute catch-all ("/*path") ()
  (let ((path (hunchentoot:script-name*)))
    (format t "Processing path ~a~%" path)
    (if (is-static-file path)
        (handle-static-file path)
        (handle-request path))))

(defun startup-server() 
  (build-tailwind)
  (push (hunchentoot:create-static-file-dispatcher-and-handler "/style.css" (merge-pathnames "style.css" *output-dir*)) hunchentoot:*dispatch-table*)
  (hunchentoot:start *acceptor*))








