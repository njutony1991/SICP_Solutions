(load "2-48.scm")
(load "2-46.scm")

(let ((bl (make-vect 0 0))
	  (br (make-vect 1 0))
	  (tl (make-vect 0 1))
	  (tr (make-vect 1 1))
	 )
	(define outline
		(segment-painter (list (make-segment br bl)
						   	   (make-segment tr tl)
						       (make-segment br tr)
						       (make-segment bl tl))))

	(define cross
	    (segment-painter (list (make-segment tl br) (make-segment tr bl))))

)

(let ((uh (make-vect 0.5 1))
	  (dh (make-vect 0.5 0))
	  (lh (make-vect 0 0.5))
	  (rh (make-vect 1 0.5))
	 )
	(define half
	  (segment-painter (list (make-segment lh uh)
	  						 (make-segment uh rh)
	  						 (make-segment rh dh)
	  						 (make-segment dh lh)))
	)
)