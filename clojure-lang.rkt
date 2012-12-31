#lang racket

(define (println input-string)
  (display input-string)) 

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
                     null
		     sub1
		     vector-copy
		     vector-ref)
         (rename-out 
          (clojure:do do)
          (clojure:cond cond)
	  (clojure:count count)
	  (vector-ref nth)
          (null nil)
	  (sub1 dec)
	  (add1 inc)
	  (vector-copy subvec)
          (clojure:def def)
          (clojure:if if)))
