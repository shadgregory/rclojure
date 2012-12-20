#lang racket
(require syntax/readerr)
(provide 
 (rename-out 
  (clj-read read)
  (clj-read-syntax read-syntax))
 )

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
    (vec-read-syntax src in)
    ]))

(define (vec-read in)
  (syntax->datum (vec-read-syntax #f in)))

(define (vec-read-syntax src in)
  (let* ((element-list (parsed-list in))
	 (bracket-count 0)
	 (return-vector #()))
    (letrec ((read-list 
	      (lambda (lst)
		(cond
		 ((empty? lst) return-vector)
		 ((equal? (car lst) "[")
		  (set! bracket-count (add1 bracket-count))
		  (read-list (cdr lst)))
		 ((equal? (car lst) "]")
		  (set! bracket-count (sub1 bracket-count))
		  (read-list (cdr lst)))
		 ((= bracket-count 1) 
		  (set! return-vector (vector-append return-vector (vector (car lst))))
		  (read-list (cdr lst)))
		 ((> bracket-count 1)
		  (if (vector? (vector-ref return-vector 
					   (sub1 (vector-length return-vector)))) 
		      (set! return-vector  
			    (vector
			     (vector-take return-vector 
					  (sub1 (vector-length return-vector)))
			     (vector-append 
			      (vector-ref return-vector 
					  (sub1 
					   (vector-length return-vector)))
			      (vector (car lst))))) 
		      (set! return-vector 
			    (vector
			     return-vector
			     (vector (car lst)))))
		  (read-list (cdr lst)))
		 ((= bracket-count 0) return-vector)))))
      (read-list element-list))))

(define (parsed-list in)
  (let ((code (string-append "[" (port->string in))))
    (set! code (regexp-replace "]" code " ] "))
    (set! code (regexp-replace* "\\[" code "[ "))
    (set! code (regexp-replace* #rx" *$" code ""))
     (for/list ((element (regexp-split #rx" +" code)))
       (if (not (string->number element))
	   element
	   (string->number element)))))
