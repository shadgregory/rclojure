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

(define-syntax str
  (syntax-rules ()
    ((_) "")
    ((_ string1 . rest)
     (cond
       ((empty? string1)
	(string-append "" (str . rest)))
       ((number? string1)
	(string-append (number->string string1) (str  . rest)))
       (else
	(string-append string1 (str . rest)))))))

(define-syntax clojure:if
  (syntax-rules ()
    ((_ test then)
     (if test then null))
    ((_ test then else)
     (if test then else))))

(define-syntax-rule (clojure:do mexpr ...) (begin mexpr ...))

(define-syntax-rule (vec a ...) (vector a ...))

(define-syntax-rule (get coll key)
  (cond
   ((vector? coll) 
    (cond
     ((> key (sub1 (vector-length coll))) null)
     (else
      (vector-ref coll key))))
   ((hash? coll)
    (hash-ref coll key))))

(define-syntax-rule (clojure:def id expr) (define id expr))

(define-syntax-rule (fn #(arg ...) body ...)
  (#%plain-lambda (arg ...)
		  (begin body ...)))

(define-syntax-rule (defn id #(arg ...) body ...)
  (define id (fn #(arg ...) body ...)))

(define-syntax clojure:inner-let
  (syntax-rules () 
    ((_ (id v) . body)
     (let ((id v))
       (clojure:inner-let () . body)))
     ((_ (id v id2 ...) . body)
      (let ((id v))
	(clojure:inner-let (id2 ...) . body)))
    ((_ () . body)
     (let () . body))))

(define-syntax-rule (clojure:let #(bindings ...) . body)
  (let ()
    (clojure:inner-let (bindings ...) . body)))

(define-syntax clojure:inner-letfn
  (syntax-rules () 
    ((_ ((var #(args ...) init)) . body)
     (let ()
       (define var (#%plain-lambda (args ...) init))
       (let () . body)))
    ((_ ((var #(args ...) init) (var2 #(args2 ...) init2) ...) . body)
     (let () 
       (define var (#%plain-lambda (args ...) init))
       (clojure:inner-letfn ((var2 #(args2 ...) init2) ...) . body)))))

(define-syntax-rule (letfn #(bindings ...) . body)
  (let ()
    (clojure:inner-letfn (bindings ...) . body)))

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

(define true? 
  (lambda (x)
    (cond
     ((equal? x #t) #t)
     (else #f))))

(define false? 
  (lambda (x)
    (cond
     ((equal? x #f) #t)
     (else #f))))

(define-syntax reduce
  (syntax-rules ()
    ((_ proc #(e ...))
     (foldl proc 0 (list e ...)))
    ((_ proc val #(e ...))
     (foldl proc 0 (list val e ...)))
    ((_ proc coll)
     (cond
      ((vector? coll)
       (foldl proc 0 (vector->list coll)))))
    ((_ proc val coll)
     (cond
      ((vector? coll)
       (foldl proc 0 (cons val (vector->list coll))))))))

(provide println
	 vec
	 fn
	 get
	 pop
	 conj
	 defn
	 false?
         letfn
	 reduce
         str
	 true?
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
	  (null? nil?)
	  (sub1 dec)
	  (add1 inc)
	  (remainder rem)
	  (modulo mod)
	  (vector-copy subvec)
          (clojure:def def)
          (clojure:if if)))
