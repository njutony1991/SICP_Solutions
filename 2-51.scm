(load "2-50.scm")
(load "2-46.scm")
(load "2-47.scm")
(load "2-48.scm")

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0 0.5)))
  	(let ((paint-left (transform-painter painter1 (make-vect 0 0)
  												  split-point
  												  (make-vect 0 1)))
  		  (paint-right (transform-painter painter2 split-point
  		  										   (make-vect 1 0)
  		  										   (make-vect 0.5 1)))
  		 )
  		(lambda (frame)
  		   (paint-left frame)
  		   (paint-right frame))
  	)
  )
)

(define (below1 painter1 painter2)
  (let ((split1 (make-vect 0 0.5))
  		(split2 (make-vect 1 0.5)))
  	(let ((paint-down 
  			(transform-painter painter1 (make-vect 0 0)
  										(make-vect 1 0)
  										split1))
  	      (paint-up
  	      	(transform-painter painter2 split1
  	      								split2
  	      								(make-vect 0 1)))
  		 )
  		 (lambda (frame) 
  		 	(paint-down frame)
  		 	(paint-up frame))
  	)
  )
)

(define (rotate-90 painter)
   (transform-painter painter (make-vect 0 1)
   							  (make-vect 0 0)
   							  (make-vect 1 1))
)

(define (below2 painter1 painter2)
  (rotate-270 (beside (rotate-90 painter1) (rotate-90 painter2)))
)