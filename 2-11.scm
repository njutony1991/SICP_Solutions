(load "2-7.scm")
(load "2-8.scm")

(define (mul-interval-improved x y)
  (let ((upper-x (upper-bound x))
  		(lower-x (lower-bound x))
  		(upper-y (upper-bound y))
  		(lower-y (lower-bound y)))
  	(cond ((and (> upper-x 0) (> lower-x 0) (> upper-y 0) (> lower-y 0))  ;x++ y++
  			(make-interval (* lower-y lower-x) (* upper-x upper-y)))
  	      ((and (> upper-x 0) (> lower-x 0) (> upper-y 0) (< lower-y 0))  ;x++ y-+
  	  		(make-interval (* lower-y upper-x) (* upper-x upper-y)))    
  	      ((and (> upper-x 0) (> lower-x 0) (< upper-y 0) (< lower-y 0))  ;x++ y--
  	      	(make-interval (* lower-y upper-x) (* lower-x upper-y)))
  	      ((and (> upper-x 0) (> 0 lower-x) (> upper-y 0) (> lower-y 0))  ;x-+ y++
  	      	(make-interval (* lower-x upper-y) (* upper-x upper-y)))
  	      ((and (> 0 upper-x) (> 0 lower-x) (> upper-y 0) (> lower-y 0))  ;x-- y++
  	      	(make-interval (* lower-x upper-y) (* upper-x lower-y)))
  	      ((and (> upper-x 0) (> 0 lower-x) (> upper-y 0) (> 0 lower-y))  ;x-+ y-+
  	      	(let ((p1 (* lower-x lower-y))
  	      		  (p2 (* lower-x upper-y))
  	      		  (p3 (* upper-x lower-y))
  	      		  (p4 (* upper-x upper-y)))
  	      		(make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))
  	      	)
  	      )
  	      ((and (> upper-x 0) (> 0 lower-x) (> 0 upper-y) (> 0 lower-y))  ;x-+ y--
  	      	(make-interval (* upper-x lower-y) (* lower-x lower-y)))
  	      ((and (> 0 upper-x) (> 0 lower-x) (> upper-y 0) (> 0 lower-y))  ;x-- y-+
  	        (make-interval (* lower-x upper-y) (* lower-x lower-y)))
  	      ((and (> 0 upper-x) (> 0 lower-x) (> 0 upper-y) (> 0 lower-y))  ;x-- y--
  	      	(make-interval (* upper-x upper-y) (* lower-x lower-y)))
  	      (else (error "can't be"))          
  	)
  )
)