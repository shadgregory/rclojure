#lang racket
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))
(require syntax/readerr)
(require data/queue)
(provide 
 (rename-out 
  (clj-read read)
  (clj-read-syntax read-syntax)))

(define (clj-read in)
  (parameterize ([current-readtable (make-clj-readtable)])
    (read in)))

(define (clj-read-syntax src in)
  (parameterize ([current-readtable (make-clj-readtable)])
    (read-syntax src in)))

(define (make-clj-readtable)
  (make-readtable (current-readtable)
                  #\[ 'terminating-macro read-vector))

(define read-vector
  (case-lambda
   [(ch in)
    (vec-read in)]
   [(ch in src line col pos)
    (vec-read-syntax src in)]))

(define (vec-read in)
  (syntax->datum (vec-read-syntax #f in)))

(define (atom? x) 
  (not (or (pair? x) (null? x) (vector? x))))

(define vec-append
  (lambda (v a)
    (let ((new-v (for/vector #:length (add1 (vector-length v)) ((element v)) element)))
      (vector-set! new-v (sub1 (vector-length new-v)) a)
      new-v)))

(define insert-list 
  (lambda (lst)
    (cond
     ((empty? lst) '())
     ((atom? (car lst))
      (cons (car lst) (insert-list (cdr lst))))
     ((pair? (car lst))
      (cons (cons 'list (car lst)) (insert-list (cdr lst)))))))

(define (vec-read-syntax src in)
  (let ((code (parsed-string in)))
    (eval (read (open-input-string code)) ns)))

(define (parsed-string in) 
  (define bracket-count 1) 
  (define code "[")
  (letrec ((get-code 
	    (lambda ()
	      (let ((ch (peek-char in)))
		(cond
		 ((equal? ch #\[)
		  (set! bracket-count (add1 bracket-count))
		  (set! code (string-append code (read-string 1 in)))
		  (get-code))
		 ((equal? ch #\])
		  (if (= 1 bracket-count)
		      (set! code (string-append code (read-string 1 in)))
		      (begin 
			(set! code (string-append code (read-string 1 in)))
			(set! bracket-count (sub1 bracket-count))
			(get-code))))
		 (else
		  (set! code (string-append code (read-string 1 in)))
		  (get-code)))))))
    (get-code))
  (set! code (regexp-replace* "]" code " ] "))
  (set! code (regexp-replace* "\\[" code "[ "))
  (set! code (regexp-replace* "'\\(" code "(list "))
  (set! code (regexp-replace* "\\)" code " ) "))
  (set! code (regexp-replace* "\\(" code "( "))
  (set! code (regexp-replace* "]" code ")"))
  (set! code (regexp-replace* "\\[" code "#("))
  code)

(define (parsed-list in)
  (define bracket-count 1)
  (define code "[")
  (letrec ((get-code 
	    (lambda ()
	      (let ((ch (peek-char in)))
		(cond
		 ((equal? ch #\[)
		  (set! bracket-count (add1 bracket-count))
		  (set! code (string-append code (read-string 1 in)))
		  (get-code))
		 ((equal? ch #\])
		  (if (= 1 bracket-count)
		      (set! code (string-append code (read-string 1 in)))
		      (begin 
			(set! code (string-append code (read-string 1 in)))
			(set! bracket-count (sub1 bracket-count))
			(get-code))))
		 (else
		  (set! code (string-append code (read-string 1 in)))
		  (get-code)))))))
    (get-code))
  (set! code (regexp-replace* "]" code " ] "))
  (set! code (regexp-replace* "\\[" code "[ "))
  (set! code (regexp-replace* "'\\(" code "(list "))
  (set! code (regexp-replace* "\\)" code " ) "))
  (set! code (regexp-replace* "\\(" code "( "))
  (set! code (regexp-replace* "]" code ")"))
  (set! code (regexp-replace* "\\[" code "#("))

  (for/list ((element (regexp-split #rx" +" code)))
    (cond
     ((string->number element)
      (string->number element))
     ((regexp-match #rx"^\".*\"$" element)
      (regexp-replace #rx"^\"(.*)\"$" element "\\1"))
     ((equal? element "]") element)
     ((equal? element "[") element)
     ((equal? element ")") element)
     ((equal? element "(") element)
     (else (string->symbol element)))))
