;; helpers

(define (pk . args)
  (println args)
  (car (reverse args)))

(define (ref alist key)
  (let loop ((alist alist))
    (if (null? alist)
        #f
        (if (equal? key (caar alist))
            (cdar alist)
            (loop (cdr alist))))))

(define (filter predicate? lst)
  (let loop ((lst lst)
             (out '()))
    (if (null? lst)
        (reverse out)
        (if (predicate? (car lst))
            (loop (cdr lst) (cons (car lst) out))
            (loop (cdr lst) out)))))

(define (scm2host obj)
  (##inline-host-expression "g_scm2host(@1@)" obj))

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

;; bindings

(define (e* name props children)
  (if (null? children)
      (##inline-host-expression
       "helpers.default.e(g_scm2host(@1@), helpers.default.makeprops(@2@), null)"
       name props)
      (##inline-host-expression
       "helpers.default.e(g_scm2host(@1@), helpers.default.makeprops(@2@), @3@)"
       name props (list->vector children))))

(define (render! element container)
  (##inline-host-expression "helpers.default.render(@1@, @2@)"
                            element container))
(define (document-get-element-by-id id)
  (##inline-host-expression "document.getElementById(g_scm2host(@1@))" id))

(define (webui-event-key event)
  (##inline-host-expression "g_host2scm(@1@.key)" event))

(define (webui-event-target-value event)
  (##inline-host-expression "g_host2scm(@1@.target.value)" event))

;; framework

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

;; app

(define make-uid
  (let ((uid 0))
    (lambda ()
      (set! uid (+ uid 1))
      uid)))

(define (make-todo title)
  (vector (make-uid) title #f))

(define (todo-uid todo)
  (vector-ref todo 0))

(define (todo-title todo)
  (vector-ref todo 1))

(define (todo-done? todo)
  (vector-ref todo 2))

(define (todo-toggle! todo)
  (vector-set! todo 2 (not (todo-done? todo))))

(define (init)
  (let ((learn-scheme (make-todo "Learn Scheme")))
    (todo-toggle! learn-scheme)
    (vector "" 'all (list learn-scheme))))

(define %status->predicate
  `((all . ,(lambda (x) #t))
    (todo . ,(lambda (x) (not (todo-done? x))))
    (done . ,todo-done?)))

(define (on-todo-toggle todo)
  (lambda (model event)
    (todo-toggle! todo)
    model))

(define (on-todo-destroy todo)
  (lambda (model event)
    (let* ((todos (vector-ref model 2))
           (todos (filter (lambda (x) (not (= (todo-uid x) (todo-uid todo))))
                          todos)))
      (vector-set! model 2 todos)
      model)))

(define (%todo-view todo)
  `(li ,@(if (todo-done? todo) `((@ (class "completed"))) '())
       (div (@ (class "view"))
            (input (@ (class "toggle")
                      (type "checkbox")
                      (on-change ,(on-todo-toggle todo))))
            (label ,(todo-title todo))
            (button (@ (class "destroy")
                       (on-click ,(on-todo-destroy todo)))))))

(define (todo-view model)
  (let* ((predicate? (ref %status->predicate (vector-ref model 1)))
         (todos (filter predicate? (vector-ref model 2))))
    (map %todo-view todos)))

(define (on-all-selected model event)
  (vector-set! model 1 'all)
  model)

(define (on-todo-selected model event)
  (vector-set! model 1 'todo)
  model)

(define (on-done-selected model event)
  (vector-set! model 1 'done)
  model)

(define (on-input model event)
  (let* ((title (webui-event-target-value event)))
    (vector-set! model 0 title)
    model))

(define (on-key-press model event)
  (let* ((key (webui-event-key event)))
    (if (and (string=? key "Enter") (not (string=? "" (vector-ref model 0))))
        (let ((todo (make-todo (vector-ref model 0))))
          (vector-set! model 0 "")
          (vector-set! model 2 (cons todo (vector-ref model 2)))
          model)
        model)))

(define (on-clear-completed model event)
  (let* ((todos (vector-ref model 2))
         (todos (filter (lambda (x) (not (todo-done? x))) todos)))
    (vector-set! model 2 todos)
    model))

(define (view model)
  `(section (@ (class "todoapp"))
            (header (@ (class "header"))
                    (h1 "todos")
                    (input (@ (class "new-todo")
                              (placeholder "What needs to be done?")
                              (autoFocus #t)
                              (on-change ,on-input)
                              (on-keyPress ,on-key-press)
                              (value ,(vector-ref model 0)))))
            (section (@ (class "main"))
                     (ul (@ (class "todo-list"))
                         ,@(todo-view model)))
            (footer (@ (class "footer"))
                    (span (@ (class "todo-count"))
                          ,(string-append (number->string (length (filter (lambda (x) (not (todo-done? x))) (vector-ref model 2))))
                                          " items left"))
                    (ul (@ (class "filters"))
                        (li (a (@ (href "#")
                                  ,@(if (eq? (vector-ref model 1) 'all)
                                        '((class "selected")) '())
                                  (on-click ,on-all-selected))
                               "All"))
                        (li (a (@ (href "#")
                                  ,@(if (eq? (vector-ref model 1) 'todo)
                                        '((class "selected")) '())
                                  (on-click ,on-todo-selected))
                               "Todo"))
                        (li (a (@ (href "#")
                                  ,@(if (eq? (vector-ref model 1) 'done)
                                        '((class "selected")) '())
                                  (on-click ,on-done-selected))
                               "Done")))
                    (button (@ (class "clear-completed")
                               (on-click ,on-clear-completed))
                            "Clear completed"))))

(webui-app webui-patch! init view)
