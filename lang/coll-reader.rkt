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
		  #\{ 'terminating-macro read-hash
                  #\[ 'terminating-macro read-vector
		  #\: 'non-terminating-macro (lambda (ch in . _)
					       (define body (string->symbol (string-append ":" (symbol->string (read in)))))
                                               ;; body is a symbol
					       `(quote ,body))))

(define read-hash
  (case-lambda
   [(ch in)
    (hash-read in)]
   [(ch in src line col pos)
    (hash-read-syntax src in)]))

(define read-vector
  (case-lambda
   [(ch in)
    (vec-read in)]
   [(ch in src line col pos)
    (vec-read-syntax src in)]))

(define (vec-read in)
  (syntax->datum (vec-read-syntax #f in)))

(define (hash-read in)
  (syntax->datum (hash-read-syntax #f in)))

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

(define (hash-read-syntax src in)
  (let ((code (parsed-hash in)))
    (eval (read (open-input-string code)) ns)))

(define (parsed-hash in) 
  (define curly-count 1) 
  (define code "{")
  (letrec ((get-code 
	    (lambda ()
	      (let ((ch (peek-char in)))
		(cond
		 ((equal? ch #\{)
		  (set! curly-count (add1 curly-count))
		  (set! code (string-append code (read-string 1 in)))
		  (get-code))
		 ((equal? ch #\})
		  (if (= 1 curly-count)
		      (set! code (string-append code (read-string 1 in)))
		      (begin 
			(set! code (string-append code (read-string 1 in)))
			(set! curly-count (sub1 curly-count))
			(get-code))))
		 (else
		  (set! code (string-append code (read-string 1 in)))
		  (get-code)))))))
    (get-code))
  (set! code (regexp-replace* #rx":([^ \n\t\r])" code "'\\1"))
  (set! code (regexp-replace* "{" code "(hash "))
  (set! code (regexp-replace* "\\}" code ")"))
  (cond ((> curly-count 1) (raise-syntax-error #f "mismatched curly braces")))
  code)

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
