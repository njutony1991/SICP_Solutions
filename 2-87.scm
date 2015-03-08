(load "2-82.scm")
(define (install-polynomial-package)
  (define (make-poly variable termlist)
     (cons variable termlist))
  (define (variable poly)
     (car poly))
  (define (termlist poly)
     (cdr poly))
  
  (define (same-variable? v1 v2)
     (and (symbol? v1) (symbol? v2) (eq? v1 v2)))
  
  (define (zero-poly? poly)
    (define (zero-terms? termlist)
      (or (empty-termlist? termlist)
      	  (and (zero=? (coeff (first-term termlist)))
      	  	   (zero-terms? (rest-term termlist))))
  	)
	(zero-terms? (termlist poly)))

  (define (adjoin-term term term-list)
     (if (zero=? (coeff term))
         term-list
         (cons term term-list)))
  (define the-empty-termlist '())
  (define (first-term termlist)
     (car termlist))
  (define (rest-term termlist)
     (cdr termlist))
  (define (empty-termlist? termlist)
     (null? termlist))
  
  (define (make-term order coeff)
     (list order coeff))
  (define (order term)
     (car term))
  (define (coeff term)
     (cadr term))

  (define (add-terms l1 l2)
     (cond ((empty-termlist? l1) l2)
           ((empty-termlist? l2) l1)
       	   (else 
       	   	 (let ((t1 (first-term l1))
       	   	 	   (t2 (first-term l2))
       	   	 	   (r1 (rest-term l1))
       	   	 	   (r2 (rest-term l2)))
       	   	  	  (cond ((> (order t1) (order t2)) (adjoin-term t1 (add-terms r1 l2)))
       	   	  	        ((< (order t1) (order t2)) (adjoin-term t2 (add-terms l1 r2)))
       	   	  	    	(else 
       	   	  	    		(adjoin-term (make-term (order t1) (add (coeff t1) (coeff t2)))
       	   	  	    					 (add-terms r1 r2))))
       	   	 )
       	   )
     )
  )

  (define (negate-terms termlist)
    (if (empty-termlist? termlist)
        the-empty-termlist
        (let ((t1 (first-term termlist)))
        	(adjoin-term (make-term (order t1) (negate (coeff t1))) 
        				 (negate-terms (rest-term termlist))))))


  (define (mul-terms l1 l2)
    (if (empty-termlist? l1)
        the-empty-termlist
        (add-terms (mul-term-by-all-terms (first-term l1) l2)
        		   (mul-terms (rest-term l1) l2))))

  (define (mul-term-by-all-terms t1 l)
    (if (empty-termlist? l)
        the-empty-termlist
        (let ((t2 (first-term l)))
        	(adjoin-term
        		(make-term (+ (order t1) (order t2))
        				   (mul (coeff t1) (coeff t2)))
        		(mul-term-by-all-terms t1 (rest-term l))))))

  (define (add-poly p1 p2)
     (if (same-variable? (variable p1) (variable p2))
           (make-poly (variable p1) (add-terms (termlist p1)
           									   (termlist p2)))
           (error "Poly not in same var -- ADD-POLY" (list p1 p2))))

   (define (mul-poly p1 p2)
      (if (same-variable? (variable p1) (variable p2))
           (make-poly (variable p1) (mul-terms (termlist p1)
           									   (termlist p2)))
           (error "Poly not in same var -- MUL-POLY" (list p1 p2)))) 

    (define (tag p)
      (attach-tag 'polynomial p))

    (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
    (put 'zero=? '(polynomial) zero-poly?)
    (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))
    (put 'negate '(polynomial) (lambda (poly) (make-polynomial (variable poly)
    										  (negate-terms (termlist poly)))))

    (put 'sub '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 (negate p2)))))
    
    'done
)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(install-coercion-package)

(install-schemer-number-package)

(install-rational-package)

(install-polynomial-package)