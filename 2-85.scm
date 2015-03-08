;;(load "2-79.scm")
;;(load "2-83.scm")
(load "2-78.scm")

(define op-table* (make-equal-hash-table))

(define (put op type proc)
  (hash-table/put! op-table* (list op type) proc))

(define (get op type)
  (hash-table/get op-table* (list op type) '()))


(define coercion-table (make-equal-hash-table))

(define (put-coercion type1 type2 proc)
  (hash-table/put! coercion-table (list type1 type2) proc))

(define (get-coercion type1 type2)
  (hash-table/get coercion-table (list type1 type2) '()))

(define (install-coercion-package)
   (define (scheme-number->rational n)
     (make-rational (contents n) 1))

   (put-coercion 'scheme-number 'rational scheme-number->rational)
   'done
)

(define (install-schemer-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))

  (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))

  (put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))

  (put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))

  (put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))

  (put 'make 'scheme-number (lambda (x) (tag x)))

  (put 'equ? '(scheme-number scheme-number) =)

  (put 'raise '(scheme-number) (lambda (x) (make-rational x 1)))

  'done
)

(define (make-scheme-number x)
  ((get 'make 'scheme-number) x))

(define (install-rational-package)
  (define (numer x) (car x))

  (define (denom x) (cdr x))

  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))

  (define (tag x)
    (attach-tag 'rational x))

  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y)))
  )

  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y)))
  )

  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  (define (equal? x y)
    (= (* (numer x) (denom y)) (* (numer y) (denom x)))
  )

  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))

  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))

  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))

  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))

  (put 'equ? '(rational rational) equal?)

  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))

  ;; rational package
  (put 'project '(rational) (lambda (x) 
             (make-scheme-number (round (/ (numer x) (denom x))))))

  ;;(put 'raise 'rational (lambda (x) (make-real (/ (numer x) (denom x)))))
  'done
)

(define (make-rational n d)
  ((get 'make 'rational) n d)
)




(define (level type)
  (cond ((eq? type 'scheme-number) 0)
        ((eq? type 'rational) 1)
        ;;((eq? type 'real) 2)
        ;;((eq? type 'complex) 3)
  )
)





(define (raise x)
  (apply-generic 'raise x)
)


;;(put 'raise 'real (lambda (x) (make-from-real-imag x 0)))

(define (highest-type args)
  (if (null? (cdr args))
      (type-tag (car args))
      (let ((t1 (type-tag (car args)))
      		(t2 (highest-type (cdr args))))
      	(let ((l1 (level t1))
      		  (l2 (level t2)))
      		(if (> l1 l2) l1 l2))
      )
  )
)

(define (raise-to-level x type)
   (let ((target (level type)))
   	 (cond ((= (level (type-tag x)) target) x)
   	       ((> (level (type-tag x)) target) (error "ERROR raise-to-level CAST DOWN" x type))
   	       (else (raise-to-level (raise x) type))
   	 )
   )
)

(define (rasie-to-common args)
  (define (iter items result)
    (if (null? items)
        (reverse result)
        (iter (cdr items) (cons (raise-to-level (car items) 
        						(highest-type args)) result))))
  (display "raise-to-common")
  (iter args '())
)

;; complex package
;;(put 'project 'complex (lambda (x) (make-real (real-part x))))

(define (drop x)
  (display "drop\n")
  (let ((project-proc (get 'project (type-tag x))))
  	(if project-proc
  		  (let ((project-number (project-proc (contents x))))
  			  (if (equ? project-number (raise project-number))
  				    (drop project-number)
  				    x
  			  )
  		  )
  		  x
  	)
  )
)

(define (apply-generic op . args)
  (define (true-map proc sequence)
    (display "true-map-iter\n")
    (define (true-map-iter proc sequence result)
       (if (null? sequence)
           (reverse result)
           (let ((item (proc (car sequence))))
             (if item
                (true-map-iter proc (cdr sequence) (cons item result))
                #f
             )
           )
       )
    )
    (display "true-map\n")
    (true-map-iter proc sequence '())
  )

  (define (iter type-tags args)
    (display "iter\n")
    (if (null? type-tags)
        #f
        (let ((type1 (car type-tags)))
          (let ((filtered-args (true-map (lambda (x) 
                                            (let ((type2 (type-tag x)))
                                              (if (eq? type1 type2)
                                                  x
                                                  (let ((t2->t1 (get-coercion type2 type1)))
                                                    (if (null? t2->t1)
                                                        #f
                                                        (t2->t1 x)))))) args)))
            (or filtered-args (iter (cdr type-tags) args))
          )
        )
    )
  )

  (let ((type-tags (map type-tag args)))
  	(let ((proc (get op type-tags)))
  		(if (not (null? proc))
  			(drop (apply proc (map contents args)))
  			(let ((new-types (iter type-tag args)))
  				(if new-types
  					(apply apply-generic (cons op (iter type-tags args)))
  					(apply apply-generic (cons op (rasie-to-common args)))
  				)
  			)
  		  
  		)
  	)
  )
)

(define (equ? x y)
   (apply-generic 'equ? x y)
)

(define (add x y)
  (apply-generic 'add x y)
)

(define (sub x y)
  (apply-generic 'sub x y))

(define (mul x y)
  (apply-generic 'mul x y))

(define (div x y)
  (apply-generic 'div x y))

(install-rational-package)
(install-schemer-number-package)