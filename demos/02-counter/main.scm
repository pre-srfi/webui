(define (scm2host obj)
  (##inline-host-expression "g_scm2host(@1@)" obj))

(define (scm2children obj)
  (cond
   ((string? obj) (scm2host obj))
   ((number? obj) (scm2host obj))
   (else obj)))

(define (e* name props children)
  (let ((children* (scm2children children)))
    (##inline-host-expression
     "helpers.default.e(g_scm2host(@1@), helpers.default.makeprops(@2@), @3@)"
     name props children*)))

(define (render! element container)
  (##inline-host-expression "helpers.default.render(@1@, @2@)"
                            element container))

(define (document-get-element-by-id id)
  (##inline-host-expression "document.getElementById(g_scm2host(@1@))" id))

(define container (document-get-element-by-id "root"))

(define (webui-patch! element)
  (render! element container))

(define (webui-app patch! init view)
  (define model (init))

  (define (make-controller proc)
    (lambda (event)
      (let ((new (proc model event)))
        (set! model new)
        (render))))

  ;; rendering pipeline
  (define (render)
    (patch! (view model make-controller)))

  ;; change procedure allows to sneak into the app closure
  (define (change proc)
    (let ((new (proc model)))
      (set! model new)
      (render)))

  (change values)
  
  change)

(define (on-click model event)
  (+ model 1))

(define (init) 0)

(define (view model mc)
  (e* "div" #f
      (vector (e* "p" #f (string-append "counter is " (number->string model)))
              (e* "button" `(("onClick" . ,(mc on-click))) "increment"))))

(webui-app webui-patch! init view)
