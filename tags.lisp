
; NOTE: REMOVED TAGS TIME, SEARCH, MAP, VAR, SLOT BECAUSE THEY ARE COMMON LISP FUNCTIONS

(defpackage :tags
  (:use :cl)
  (:export :import-tags)
)

(in-package :tags)

(defmacro define-tag (tag)
  `(progn
     (defun ,tag (&rest args)
       (let* ((attrs '())
              (body '()))
         (loop while (and args (keywordp (first args)))
               do (push (pop args) attrs)
                  (push (pop args) attrs))
         (setf body args
               attrs (nreverse attrs))
         (let ((attr-string 
                (with-output-to-string (s)
                  (loop for (k v) on attrs by #'cddr
                        do (format s " ~a=\"~a\"" 
                                  (string-downcase (symbol-name k)) v))))
               (body-string (if body
                               (format nil "~{~a~}" body)
                               "")))
           (format nil "<~a~a>~a</~a>" (string-downcase ',tag) attr-string body-string (string-downcase ',tag)))))
     (export ',tag)))

(defmacro define-tags (&rest tags)
  `(progn
     ,@(loop for tag in tags
             collect `(define-tag ,tag))))

(define-tags
  ;; Main root
  html
  
  ;; Document metadata
  base head link meta style title
  
  ;; Sectioning root
  body
  
  ;; Content sectioning
  address article aside footer header h1 h2 h3 h4 h5 h6
  hgroup main nav section
  
  ;; Text content
  blockquote dd div dl dt figcaption figure hr li menu ol p pre ul
  
  ;; Inline text semantics
  a abbr b bdi bdo br cite code data dfn em i kbd mark q rp rt
  ruby s samp small span strong sub sup u wbr
  
  ;; Image and multimedia
  area audio img track video
  
  ;; Embedded content
  embed iframe object picture portal source
  
  ;; SVG and MathML
  svg math
  
  ;; Scripting
  canvas noscript script
  
  ;; Demarcating edits
  del ins
  
  ;; Table content
  caption col colgroup table tbody td tfoot th thead tr
  
  ;; Forms
  button datalist fieldset form input label legend meter
  optgroup option output progress select textarea
  
  ;; Interactive elements
  details dialog summary
  
  ;; Web Components
  template
)
