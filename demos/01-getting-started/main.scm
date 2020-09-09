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
     "helpers.default.e(g_scm2host(@1@), g_scm2host(@2@), @3@)"
     name props children*)))

(define (render element container)
  (##inline-host-expression "helpers.default.render(@1@, @2@)"
                            element container))

(define (document-get-element-by-id id)
  (##inline-host-expression "document.getElementById(g_scm2host(@1@))" id))

(define container (document-get-element-by-id "root"))
(define hello (e* "p" #f "hello"))
(define from (e* "p" #f "from"))
(define scheme (e* "p" #f "scheme"))
(define element (e* "div" #f (vector hello from scheme)))
(render element container)
