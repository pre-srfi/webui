(define (scm2host obj)
  (##inline-host-expression "g_scm2host(@1@)" obj))

(define (e* name props children)
  (if (null? children)
      (##inline-host-expression
       "helpers.default.e(g_scm2host(@1@), helpers.default.makeprops(@2@), null)"
       name props)
      (##inline-host-expression
       "helpers.default.e(g_scm2host(@1@), helpers.default.makeprops(@2@), @3@)"
       name props (list->vector children))))

  
(define (make-prop attr mc)
  (let ((key (symbol->string (car attr)))
        (value (cadr attr)))
    (if (string=? (substring key 0 2) "on")
        (cons (string-append "on"
                             (list->string (list (char-upcase (string-ref key 3))))
                             (substring key 4 (string-length key)))
              (mc value))
        (if (string=? "class" key)
            (cons "className" value)
            (cons key value)))))

(define (make-props attrs mc)
  (map (lambda (x) (make-prop x mc)) attrs))

(define (pk . args)
  (println args)
  (car (reverse args)))

(define (sxml->vdom sxml mc)
  (if (string? sxml)
      (scm2host sxml)
      (if (and (pair? (cadr sxml)) (eq? '@ (caadr sxml)))
          (e* (symbol->string (car sxml))
              (make-props (cdadr sxml) mc)
              (map (lambda (x) (sxml->vdom x mc)) (cddr sxml)))
          (e* (symbol->string (car sxml))
              #f
              (map (lambda (x) (sxml->vdom x mc)) (cdr sxml))))))

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
    (patch! (sxml->vdom (view model) make-controller)))

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

(define (view model)
  `(section (@ (class "todoapp"))
            (header (@ (class "header"))
                    (h1 "todos")
                    (input (@ (class "new-todo")
                              (placeholder "What needs to be done?")
                              (autofocus "t"))))
            
            (section (@ (class "main"))
                     (ul (@ (class "todo-list"))
                         (li (@ (class "completed"))
                             (div (@ (class "view"))
                                  (input (@ (class "toggle")
                                             (type "checkbox")
                                             (checked "t")))
                                   (label "Learn Scheme")
                                   (button (@ (class "destroy")))))))

            (footer (@ (class "footer"))
                    (span (@ (class "todo-count"))
                          "42 items left")
                    (ul (@ (class "filters"))
                        (li (a (@ (href "#")
                                  (class "selected"))
                               "All"))
                        (li (a (@ (href "#"))
                               "Active"))
                        (li (a (@ (href "#"))
                               "Completed")))
                    (button (@ (class "clear-completed"))
                            "Clear completed"))))

(webui-app webui-patch! init view)
