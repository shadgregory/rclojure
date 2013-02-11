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
  (let* ((element-list (parsed-list in))
	 (bracket-count 0)
	 (paren-count 0)
         (in-list #f)
	 (list-string "")
	 (return-vector #()))
    (letrec ((read-list 
	      (lambda (lst q)
		(cond
		 ((empty? lst) 
		  return-vector)
                 ((and (equal? (car lst) "(") (equal? (second lst) 'list))
                  (set! in-list #t)
		  (set! paren-count (add1 paren-count))
                  (enqueue! q 'list)
		  (set! list-string (string-append list-string " (list "))
                  (read-list (cdr (cdr lst)) q)
                  )
                 ((and in-list (equal? (car lst) ")"))
		  (set! paren-count (sub1 paren-count))
		  (set! list-string (string-append list-string ")"))
		  (cond
		   ((= 0 paren-count)
		    (set! return-vector (vec-append 
					 return-vector  
					 (cons 'list 
					       (insert-list (eval (read 
								   (open-input-string list-string))
								  ns)))))))
                  (read-list (cdr lst) (make-queue)))
                 ((> paren-count 0)
		  (cond
		   ((string? (car lst))
		    (set! list-string (string-append list-string " " (car lst) " "))
		    )
		   ((symbol? (car lst))
		    (set! list-string (string-append list-string (symbol->string (car lst))))
		    )
		   ((number? (car lst))
		    (set! list-string (string-append list-string " "(number->string (car lst)) " "))
		    )
		   )
                  (enqueue! q (car lst))
                  (read-list (cdr lst) q))
		 ((equal? (car lst) "[")
		  (cond
		   ((> bracket-count 0)
		    (set! return-vector (vec-append return-vector #()))))
		  (set! bracket-count (add1 bracket-count))
		  (read-list (cdr lst) q))
		 ((equal? (car lst) "]")
		  (set! bracket-count (sub1 bracket-count))
		  (read-list (cdr lst) q))
		 ((= bracket-count 1) 
		  (set! return-vector (vector-append return-vector (vector (car lst))))
		  (read-list (cdr lst) q))
		 ((> bracket-count 1)
		  (if (vector? (vector-ref 
				return-vector 
				(sub1 (vector-length return-vector))))
		      (set! return-vector  
			    (vec-append
			     (vector-take return-vector 
					  (sub1 (vector-length return-vector)))
			     (vec-append
			      (vector-ref return-vector 
					  (sub1 (vector-length return-vector)))
			      (car lst))))
		      (set! return-vector 
			    (vector return-vector (vector (car lst)))))
		  (read-list (cdr lst) q))
		 ((= bracket-count 0)
		  return-vector)))))
      (read-list element-list (make-queue)))))

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

