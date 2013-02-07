#lang racket

(require data/queue)
(require syntax/to-string)
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define-syntax println
  (syntax-rules ()
    ((_ str ...)
     (begin
       (for-each (lambda (arg)
                   (display arg))
                 `(,str ...))
       (newline)))))

(define-syntax-rule (str string1 ...)
  (string-append string1 ...))

(define-syntax clojure:if
  (syntax-rules ()
    ((_ test then)
     (if test then null))
    ((_ test then else)
     (if test then else))))

(define-syntax-rule (clojure:do mexpr ...)
  (begin
    mexpr ...))

(define-syntax-rule (vec a ...)
  (vector a ...))

(define-syntax-rule (get vec key)
  (cond
   ((> key (sub1 (vector-length vec))) null)
   (else
    (vector-ref vec key))))

(define-syntax-rule (clojure:def id expr)
  (define id expr))

(define-syntax-rule (fn #(arg ...) body ...)
  (#%plain-lambda (arg ...)
		  (begin body ...)))

(define-syntax-rule (defn id #(arg ...) body ...)
  (define id (fn #(arg ...) body ...)))

(define-syntax clojure:inner-let
  (syntax-rules () 
    ((_ (id v) . body)
     (begin
       (define id v)
       (clojure:inner-let () . body)))
     ((_ (id v id2 ...) . body)
      (begin
        (define id v)
        (clojure:inner-let (id2 ...) . body)
        ))
    ((_ () . body)
     (let () . body))))

(define-syntax-rule (clojure:let #(bindings ...) . body)
  (let ()
    (clojure:inner-let (bindings ...) . body)))

(define-syntax-rule (letfn #(funcs ...) . body)
  (let ((q (make-queue))
        (paren-count 0)
        (in-func #f))
    (for-each (lambda (arg)
                (cond
                 ((vector? arg)
                  (enqueue! q "(")
                  (set! paren-count (add1 paren-count))
                  (set! in-func #t)
                  (enqueue! q "#%plain-lambda")
                  (enqueue! q "(")
                  (set! paren-count (add1 paren-count))
                  (for ((x (in-vector arg)))
                    (enqueue! q (symbol->string x)))
                  (enqueue! q ")")
                  (set! paren-count (sub1 paren-count)))
                 ((regexp-match #px"^[ \t\n\r]+$" (symbol->string arg))
                  'whitespace)
                 ((symbol? arg)
                  (let ((arg-str (symbol->string arg)))
                    (set! arg-str (regexp-replace* "\\(" arg-str " ( "))
                    (set! arg-str (regexp-replace* "\\)" arg-str " ) "))
                    (set! arg-str (regexp-replace* "^[ ]" arg-str ""))
                        (for ((element (regexp-split #rx" +" arg-str)))
                          (cond
                           ((equal? element "(")
                            (set! paren-count (add1 paren-count)))
                           ((equal? element ")")
                            (set! paren-count (sub1 paren-count))))
                          (enqueue! q element)
                          (if (and (equal? element "(") (= paren-count 1) (not in-func))
                              (enqueue! q "define")
                              'nothing
                              )
                          (cond
                           ((and (= paren-count 2) in-func)
                            (enqueue! q ")")
                            (set! paren-count (sub1 paren-count))
                            (set! in-func #f)))
                          (if (and (= paren-count 2) in-func)
                              (begin
                                (enqueue! q ")")
                                (set! paren-count (sub1 paren-count))
                                (set! in-func #f))
                              'wecool))))))
                  (vector->list #(funcs ...)))
        (letrec ((create-str (lambda (the-queue str)
                               (cond
                                ((queue-empty? the-queue)
                                 (eval (read (open-input-string (string-append
                                                                 "(begin " str ")"
                                                                 ))) ns)
                                 (eval '(let () . body) ns))
                                (else
                                 (set! str (string-append str " " (dequeue! q)))
                                 (create-str q str))))))
          (create-str q "" ))))

(define-syntax clojure:cond
  (syntax-rules (:else)
    ((_ :else else-expr)
     (cond (else else-expr)))
    ((_ e1 e2 e3 ... :else else-expr)
     (if (= 0 (modulo (length '(e1 e2 e3 ...)) 2))
         (if e1 e2
             (clojure:cond e3 ... :else else-expr))
         (raise-syntax-error #f "cond requires an even number of forms")))))

(define-syntax conj
  (syntax-rules ()
    ((_ #(e ...) x ...)
     (vector e ... x ...))
    ((_ '(e ...) x ...)
     (list x ... e ...))
    ((_ coll x ...)
     (cond
      ((vector? coll)
       (vector-append coll #(x ...)))
      ((list? coll)
       (append '(x ...) coll))))))

(define-syntax clojure:count
  (syntax-rules ()
    ((_ #(e ...))
     (vector-length #(e ...)))
    ((_ coll)
     (if (string? coll)
	 (length (string->list coll))
	 (length coll)))))

(define-syntax pop
  (syntax-rules ()
    ((_ #(a ...))
     (vector-take #(a ...) (sub1 (vector-length #(a ...)))))
    ((_ '(a ...))
     (cdr '(a ...)))))

(provide println
         str
	 vec
	 fn
	 get
	 pop
	 conj
	 defn
         letfn
         (except-out (all-from-out racket)
		     add1
                     begin
                     car
                     cdr
                     cond
		     count
                     display
		     do
                     if
                     lambda
		     length
                     let
		     modulo
                     null
		     remainder
		     sub1
		     vector-copy
		     vector-ref)
         (rename-out
          (clojure:do do)
          (clojure:cond cond)
	  (clojure:count count)
          (clojure:let let)
	  (vector-ref nth)
          (null nil)
	  (sub1 dec)
	  (add1 inc)
	  (remainder rem)
	  (modulo mod)
	  (vector-copy subvec)
          (clojure:def def)
          (clojure:if if)))
