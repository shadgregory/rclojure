#lang racket

(define (println input-string)
  (display input-string)) 

(define-syntax-rule (str string1 ...)
  (string-append string1 ...))

(define (clojure:if stmt branch1 (branch2 '()))
  (if (not branch2)
      (if (eq? stmt #t)
          branch1
          null)
      (if (eq? stmt #t)
          branch1
          branch2)))

(define-syntax-rule (clojure:do mexpr ...)
  (begin 
    mexpr ...))

(define-syntax-rule (vec a ...)
  (vector a ...))

(define-syntax-rule (clojure:def id expr)
  (define id expr))

(define-syntax-rule (fn #(arg ...) body ...)
  (#%plain-lambda (arg ...)
		  (begin body ...)))

(define-syntax-rule (defn id #(arg ...) body ...)
  (define id (#%plain-lambda (arg ...)
			     (begin body ...))))

(define-syntax clojure:cond
  (syntax-rules (:else)
    ((_ :else else-expr)
     (cond (else else-expr)))
    ((_ e1 e2 e3 ... :else else-expr)
     (if (= 0 (modulo (length '(e1 e2 e3 ...)) 2))
         (if e1 e2
             (clojure:cond e3 ... :else else-expr))
         (raise-syntax-error #f "cond requires an even number of forms")))))

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
	 pop
	 defn
         (except-out (all-from-out racket) 
                     if 
                     do 
                     car
                     cdr
                     null
                     cond
                     lambda
		     length
		     count
                     display
                     cond
		     vector-ref
                     begin)
         (rename-out 
          (clojure:do do)
          (clojure:cond cond)
	  (clojure:count count)
	  (vector-ref nth)
          (null nil)
          (clojure:def def)
          (clojure:if if)))
