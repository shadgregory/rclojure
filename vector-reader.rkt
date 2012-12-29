#lang racket
(require syntax/readerr)
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

(define vec-append
  (lambda (v a)
    (if (> (vector-length v) 0)
	(vector (vector->values v) a)
	(vector a))))

(define (vec-read-syntax src in)
  (let* ((element-list (parsed-list in))
	 (bracket-count 0)
	 (return-vector #()))
    (letrec ((read-list 
	      (lambda (lst)
		(cond
		 ((empty? lst) 
		  return-vector)
		 ((equal? (car lst) "[")
		  (cond
		   ((> bracket-count 0)
		    (set! return-vector (vec-append return-vector #()))))
		  (set! bracket-count (add1 bracket-count))
		  (read-list (cdr lst)))
		 ((equal? (car lst) "]")
		  (set! bracket-count (sub1 bracket-count))
		  (read-list (cdr lst)))
		 ((= bracket-count 1) 
		  (set! return-vector (vector-append return-vector (vector (car lst))))
		  (read-list (cdr lst)))
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
			    (vector
			     return-vector
			     (vector (car lst)))))
		  (read-list (cdr lst)))
		 ((= bracket-count 0) return-vector)))))
      (read-list element-list))))

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
  (set! code (regexp-replace* "]" code " ]"))
  (set! code (regexp-replace* "\\[" code "[ "))

  (for/list ((element (regexp-split #rx" +" code)))
    (cond
     ((string->number element)
      (string->number element))
     ((regexp-match #rx"^\".*\"$" element)
      (regexp-replace #rx"^\"(.*)\"$" element "\\1"))
     ((equal? element "]") element)
     ((equal? element "[") element)
     (else (string->symbol element)))))

