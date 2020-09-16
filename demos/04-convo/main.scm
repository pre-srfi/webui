;; helpers

(define (pk . args)
  (println (call-with-output-string (lambda (port) (write args port))))
  (car (reverse args)))

(define ref
  (case-lambda
    ((alist key)
     (ref alist key #f))
    ((alist key default)
     (let loop ((alist alist))
       (if (null? alist)
           default
           (if (equal? key (caar alist))
               (cdar alist)
               (loop (cdr alist))))))))

(define (any predicate? lst)
  (let loop ((lst lst))
    (if (null? lst)
        #f
        (if (predicate? (car lst))
            #t
            (loop (cdr lst))))))

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

(define make-uid
  (let ((uid 0))
    (lambda ()
      (set! uid (+ uid 1))
      uid)))

;; generator

(define (generator->list-reversed g)
  (let loop ((out '()))
    (let ((obj (g)))
      (if (eof-object? obj)
          out
          (loop (cons obj out))))))

(define (list->generator lst)
  (lambda ()
    (if (null? lst)
        (eof-object)
        (let ((out (car lst)))
          (set! lst (cdr lst))
          out))))

(define (generator->list g)
  (reverse (generator->list-reversed g)))

(define (gmap proc g)
  (lambda ()
    (let ((item (g)))
      (if (eof-object? item)
          item
          (proc item)))))

(define (gconcatenate generator)
  ;; Return a generator that yields the elements of the generators
  ;; produced by the given GENERATOR. Similar to gflatten but
  ;; GENERATOR contains other generators instead of lists.
  (let ((state eof-object))
    (lambda ()
      (let ((value (state)))
        (if (eof-object? value)
            (let loop ((new (generator)))
              (if (eof-object? new)
                  new
                  (let ((value (new)))
                    (if (eof-object? value)
                        (loop (generator))
                        (begin (set! state new)
                               value)))))
            value)))))

(define (generator-until predicate? g)
  (lambda ()
    (let ((item (g)))
      (if (eof-object? item)
          item
          (if (predicate? item)
              item
              (eof-object))))))

;; bindings

;; reactjs

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

;; browser

(define (document-get-element-by-id id)
  (##inline-host-expression "document.getElementById(g_scm2host(@1@))" id))

(define (webui-event-key event)
  (##inline-host-expression "g_host2scm(@1@.key)" event))

(define (webui-event-target-value event)
  (##inline-host-expression "g_host2scm(@1@.target.value)" event))

;; functional-red-black-tree

(define (make-frbt) ;; functional-red-black-tree
  (##inline-host-expression "helpers.default.createTree()"))

(define (frbt-end frbt)
  (##inline-host-expression "g_host2scm(@1@.end)" frbt))

(define (frbt-ref frbt key)
  (##inline-host-expression "@1@.get(g_scm2host(@2@))" frbt key))

(define (frbt-insert frbt key value)
  (##inline-host-expression "@1@.insert(g_scm2host(@2@), @3@)" frbt key value))

(define (frbt-remove frbt key)
  (##inline-host-expression "@1@.remove(g_scm2host(@2@))" frbt key))

(define (frbt-find frbt key)
  (##inline-host-expression "@1@.find(g_scm2host(@2@))" frbt key))

(define (frbt-ge frbt key)
  (##inline-host-expression "@1@.ge(g_scm2host(@2@))" frbt key))

(define (frbt-gt frbt key)
  (##inline-host-expression "@1@.gt(g_scm2host(@2@))" frbt key))

(define (frbt-lt frbt key)
  (##inline-host-expression "@1@.lt(g_scm2host(@2@))" frbt key))

(define (frbt-le frbt key)
  (##inline-host-expression "@1@.le(g_scm2host(@2@))" frbt key))

(define (frbt-iter-valid? iter)
  (##inline-host-expression "g_host2scm(@1@.valid)" iter))

(define (frbt-iter-key iter)
  (##inline-host-expression "g_host2scm(@1@.key)" iter))

(define (frbt-iter-value iter)
  (##inline-host-expression "@1@.value" iter))

(define (frbt-iter-next! iter)
  (##inline-host-expression "@1@.next()" iter))

(define (frbt-iter-previous! iter)
  (##inline-host-expression "@1@.previous()" iter))

(define (frbt-iter-next? iter)
  (##inline-host-expression "g_host2scm(@1@.hasNext)" iter))

(define (frbt-iter-previous? iter)
  (##inline-host-expression "g_host2scm(@1@.hasPrev)" iter))

;; store
;;
;; Inspired from SRFI-167, the main differences are that it is not
;; transactional, instead of bytevectors, it stores scheme objects,
;; also there is no need for engine, and it is strictly functional.

(define make-store make-frbt)

(define store-ref frbt-ref)

(define store-set frbt-insert)

(define store-delete frbt-remove)

(define (compare a b)
  (##inline-host-expression "helpers.default.compare(@1@, @2@)" a b))

(define (store-generator store start start-include? end end-include?)

  (define (next)
    (define key (frbt-iter-key iter))

    (define delta (compare key end))

    (cond
     ((= delta -1)
      (if (frbt-iter-next? iter)
          (let ((value (frbt-iter-value iter)))
            (frbt-iter-next! iter)
            (cons key value))
          (let ((value (frbt-iter-value iter)))
            (set! continue eof-object)
            (cons key value))))
     ((= delta 0)
      (set! continue eof-object?)
      (if (not end-include?)
          (eof-object)
          (cons key (frbt-iter-value iter))))
     (else (set! continue eof-object?)
           (eof-object))))

  (define continue #f)

  (define iter (frbt-ge store start))

  (when (and (not start-include?)
             (frbt-iter-valid? iter)
             (equal? start (frbt-iter-key iter)))
    (frbt-iter-next! iter))

  (if (frbt-iter-valid? iter)
      (set! continue next)
      (set! continue eof-object))

  (lambda ()
    (continue)))

(define (store-generator-reversed store start start-include? end end-include?)
  ;; TODO: optimize: avoid the conversion to a list.
  (list->generator
   (generator->list-reversed
    (store-generator store start start-include? end end-include?))))

;; nstore
;;
;; mostly based on SRFI-168 adapted to work with the above store


(define (permutations s)
  ;; http://rosettacode.org/wiki/Permutations#Scheme
  (cond
   ((null? s) '(()))
   ((null? (cdr s)) (list s))
   (else ;; extract each item in list in turn and permutations the rest
    (let splice ((l '()) (m (car s)) (r (cdr s)))
      (append
       (map (lambda (x) (cons m x)) (permutations (append l r)))
       (if (null? r) '()
           (splice (cons m l) (car r) (cdr r))))))))

(define (combination k lst)
  (cond
   ((= k 0) '(()))
   ((null? lst) '())
   (else
    (let ((head (car lst))
          (tail (cdr lst)))
      (append (map (lambda (y) (cons head y)) (combination (- k 1) tail))
              (combination k tail))))))

(define (combinations lst)
  (if (null? lst) '(())
      (let* ((head (car lst))
             (tail (cdr lst))
             (s (combinations tail))
             (v (map (lambda (x) (cons head x)) s)))
        (append s v))))

;; make-indices will compute smallest set of indices required to bind
;; any pattern in one hop. The math behind this computation is
;; explained at:
;;
;;   https://math.stackexchange.com/q/3146568/23663
;;
;; make-indices will return the smallest set of permutations in
;; lexicographic order of the base index ie. the output of (iota
;; n) where n is the length of ITEMS ie. the n in nstore.

(define (prefix? lst other)
  "Return #t if LST is prefix of OTHER"
  (let loop ((lst lst)
             (other other))
    (if (null? lst)
        #t
        (if (= (car lst) (car other))
            (loop (cdr lst) (cdr other))
            #f))))

(define (permutation-prefix? c o)
  (any (lambda (p) (prefix? p o)) (permutations c)))

(define (ok? combinations candidate)
  (every (lambda (c) (any (lambda (p) (permutation-prefix? c p)) candidate)) combinations))

(define (findij L)
  (let loop3 ((x L)
              (y '()))
    (if (or (null? x) (null? (cdr x)))
        (values #f (append (reverse y) x) #f #f)
        (if (and (not (cdr (list-ref x 0))) (cdr (list-ref x 1)))
            (values #t
                    (append (cddr x) (reverse y))
                    (car (list-ref x 0))
                    (car (list-ref x 1)))
            (loop3 (cdr x) (cons (car x) y))))))

(define (lex< a b)
  (let loop ((a a)
             (b b))
    (if (null? a)
        #t
        (if (not (= (car a) (car b)))
            (< (car a) (car b))
            (loop (cdr a) (cdr b))))))

(define (make-indices n)
  ;; This is based on:
  ;;
  ;;   https://math.stackexchange.com/a/3146793/23663
  ;;
  (let* ((tab (iota n))
         (cx (combination (floor (/ n 2)) tab)))
    (let loop1 ((cx cx)
                (out '()))
      (if (null? cx)
          (list-sort lex< out)
          (let loop2 ((L (map (lambda (i) (cons i (not (not (memv i (car cx)))))) tab))
                      (a '())
                      (b '()))
            (call-with-values (lambda () (findij L))
              (lambda (continue? L i j)
                (if continue?
                    (loop2 L (cons j a) (cons i b))
                    (loop1 (cdr cx)
                           (cons (append (reverse a) (map car L) (reverse b))
                                 out))))))))))

(define (make-nstore prefix n)
  (vector 'nstore prefix n (make-indices n)))

(define (nstore-prefix nstore) (vector-ref nstore 1))

(define (nstore-n nstore) (vector-ref nstore 2))

(define (nstore-indices nstore) (vector-ref nstore 3))

(define (nstore-ask? store nstore tuple)
  (store-ref store (vector-append (nstore-prefix nstore) (vector 0) tuple)))

(define (make-tuple v permutation)
  ;; Construct a permutation of V based on PERMUTATION
  (let ((tuple (make-vector (length permutation))))
    (for-each (lambda (index value) (vector-set! tuple index value)) permutation (vector->list v))
    tuple))

(define (permute items index)
  ;; inverse of `make-tuple`
  (let loop ((index index)
             (out '()))
    (if (null? index)
        (list->vector (reverse out))
        (loop (cdr index)
              (cons (vector-ref items (car index)) out)))))

(define (nstore-add store nstore tuple)
  (define prefix (nstore-prefix nstore))
  (let loop ((indices (nstore-indices nstore))
             (subspace 0)
             (store store))
    (if (null? indices)
        store
        (let ((key (vector-append prefix
                                  (vector subspace)
                                  (permute tuple (car indices)))))

          (loop (cdr indices)
                (+ 1 subspace)
                (store-set store key #t))))))

(define (nstore-delete store nstore tuple)
  (define prefix (nstore-prefix nstore))
  (let loop ((indices (nstore-indices nstore))
             (subspace 0)
             (store store))
    (if (null? indices)
        store
        (let ((key (vector-append prefix
                                  (vector subspace)
                                  (permute tuple (car indices)))))
          (loop (cdr indices) (+ subspace 1) (store-delete store key))))))

(define (nstore-var name)
  (vector 'var name))

(define var nstore-var)

(define (nstore-var-name var)
  (vector-ref var 1))

(define (nstore-var? obj)
  (and (vector? obj)
       (eq? (vector-ref obj 0) 'var)))

(define (bind* pattern tuple seed)
  ;; Associate variables of PATTERN to value of TUPLE with SEED.
  (let loop ((tuple (vector->list tuple))
             (pattern (vector->list pattern))
             (out seed))
    (if (null? tuple)
        out
        (if (nstore-var? (car pattern)) ;; only bind variables
            (loop (cdr tuple)
                  (cdr pattern)
                  (cons (cons (nstore-var-name (car pattern))
                              (car tuple))
                        out))
            (loop (cdr tuple) (cdr pattern) out)))))

(define (pattern->combination pattern)
  (let loop ((pattern (vector->list pattern))
             (index 0)
             (out '()))
    (if (null? pattern)
        (reverse out)
        (loop (cdr pattern)
              (+ 1 index)
              (if (nstore-var? (car pattern))
                  out
                  (cons index out))))))

(define (pattern->index pattern indices)
  ;; Retrieve the index and subspace that will allow to bind
  ;; PATTERN in one hop. This is done by getting all non-variable
  ;; items of PATTERN and looking up the first index that is
  ;; permutation-prefix...
  (let ((combination (pattern->combination pattern)))
    (let loop ((indices indices)
               (subspace 0))
      (if (null? indices)
          (error 'nstore "oops!")
          (if (permutation-prefix? combination (car indices))
              (values (car indices) subspace)
              (loop (cdr indices) (+ subspace 1)))))))

(define (pattern->prefix pattern index)
  ;; Return the list that correspond to INDEX, that is the items
  ;; of PATTERN that are not variables. This is used as the prefix
  ;; for the range query done later.
  (let loop ((index index)
             (out '()))
    (let ((v (vector-ref pattern (car index))))
      (if (nstore-var? v)
          (list->vector (reverse out))
          (loop (cdr index) (cons v out))))))

(define (nstore-prefix-generator store prefix)

  (define (prefix? pair)
    (define key (car pair))

    (if (< (vector-length key) (vector-length prefix))
        #f
        (let loop ((index (- (vector-length prefix) 1)))
          (if (= index -1)
              #t
              (if (equal? (vector-ref prefix index) (vector-ref key index))
                  (loop (- index 1))
                  #f)))))

  (generator-until prefix? (store-generator store prefix #t (frbt-end store) #t)))


(define (%from store nstore pattern seed)
  (call-with-values (lambda () (pattern->index pattern (nstore-indices nstore)))
    (lambda (index subspace)
      (let ((prefix (vector-append (nstore-prefix nstore)
                                   (vector subspace)
                                   (pattern->prefix pattern index))))
        (gmap (lambda (pair)
                (bind* pattern
                       (make-tuple (subvector (car pair)
                                              (+ (vector-length (nstore-prefix nstore)) 1)
                                              (vector-length (car pair)))
                                   index)
                       seed))
              (nstore-prefix-generator store prefix))))))

(define (nstore-from store nstore pattern)
  (%from store nstore pattern '()))

(define (pattern-bind pattern seed)
  ;; Return a pattern where variables that have a binding in SEED
  ;; are replaced with the associated value. In practice, most of
  ;; the time, it is the same pattern with less variables.
  (vector-map (lambda (item) (or (and (nstore-var? item)
                                      (ref seed (nstore-var-name item) item))
                                 item))
              pattern))

(define (nstore-where store nstore pattern)
  (lambda (from)
    (gconcatenate
     (gmap (lambda (bindings)
             (%from store
                    nstore
                    (pattern-bind pattern bindings)
                    bindings))
           from))))


(define (nstore-query from where . wheres)
  (let loop ((seed from)
             (wheres (cons where wheres)))
    (if (null? wheres)
        seed
        (loop ((car wheres) seed) (cdr wheres)))))

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

(define db (make-store))
(define space (make-nstore (vector) 3))

(define (on-input model event)
  (let* ((message (webui-event-target-value event)))
    (vector-set! model 1 message)
    model))

(define (new-message! model)
  (define parent (vector-ref model 0))
  (define message (vector-ref model 1))
  (define uid (make-uid))
  (set! db (nstore-add db space (vector uid "parent" parent)))
  (set! db (nstore-add db space (vector uid "message" message)))
  uid)

(define (new-reply! parent text)
  (define uid (make-uid))
  (set! db (nstore-add db space (vector uid "parent" parent)))
  (set! db (nstore-add db space (vector uid "message" text))))

(define (on-key-press model event)
  (let* ((key (webui-event-key event)))
    (if (and (string=? key "Enter") (not (string=? "" (vector-ref model 1))))
      (let ((msg-uid (new-message! model)))
        (begin
          (vector-set! model 1 "")
          (schedule-a-mock-reply msg-uid)
        )
      )
    )
  )
  model
)

(define (schedule-a-mock-reply msg-uid)
  (define text (db-query-message msg-uid))
  (##inline-host-expression "helpers.default.schedule_a_mock_reply(@1@, @2@)"
    text
    (lambda (reply-text)
      (begin
        (log reply-text)
        (new-reply! msg-uid reply-text)
      )
    )
  )
)

(define (log obj)
  (##inline-host-expression "console.log(@1@)" obj))

(define (init) (vector #f ""))

(define (on-parent-click uid)
  (lambda (model event)
    (vector-set! model 0 (db-query-parent uid))
    model))

(define (view-parent model)
  (if (vector-ref model 0)
      `(div (@ (class "message")
               (on-click ,(on-parent-click (vector-ref model 0))))
            ,(db-query-message (vector-ref model 0)))
      `(div "You are at the root of the conversation")))

(define (db-query-messages parent)
  (generator->list
   (nstore-query
    (nstore-from db space (vector (var 'uid) "parent" parent))
    (nstore-where db space (vector (var 'uid) "message" (var 'message))))))

(define (db-query-message uid)
  (ref ((nstore-from db space (vector uid "message" (var 'message))))
       'message))

(define (db-query-parent uid)
  (ref ((nstore-from db space (vector uid "parent" (var 'parent))))
       'parent))

(define (dive-into-message uid)
  (lambda (model event)
    (vector-set! model 0 uid)
    model))

(define (view-thread-message message)
  `(div (@ (class "message")
           (on-click ,(dive-into-message (ref message 'uid))))
        ,(ref message 'message)))

(define (view-thread model)
  (map view-thread-message (db-query-messages (vector-ref model 0))))

(define (view model)
  `(div
    (div (h1 "Parent")
         ,(view-parent model))
    (div (h1 "Thread")
         ,@(view-thread model))
    (div (h1 "Reply")
         (div (input (@ (placeholder "Malfunction! Need input!")
                        (autoFocus #t)
                        (on-change ,on-input)
                        (on-keyPress ,on-key-press)
                        (value ,(vector-ref model 1))))))))


(webui-app webui-patch! init view)
