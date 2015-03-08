(load "2-83.scm")
(define (level type)
  (cond ((eq? type 'integer) 0)
        ((eq? type 'rational) 1)
        ((eq? type 'real) 2)
        ((eq? type 'complex) 3)
  )
)

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
   (let (target (level type))
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
  (iter args '())
)

(define (apply-generic op . args)
  (define (true-map proc sequence)
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
    (true-map-iter proc sequence '())
  )

  (define (iter type-tags args)
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
  			(apply proc (map contents args))
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